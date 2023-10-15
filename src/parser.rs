use memoir::traits::Parse;
use memoir::*;

use directories as dirs;

use crate::brush::BrushMode;
use crate::gfx::Rgba8;
use crate::platform;
use crate::script::{Argument, Command, Script};
use crate::session::{Direction, Mode, VisualState};

use std::ffi::OsString;
use std::str::FromStr;

pub type Error = memoir::result::Error;

impl Parse for Script {
    fn parser() -> Parser<Self> {
        get_script_parser(-1)
    }
}

fn get_script_parser(lookback: i32) -> Parser<Script> {
    assert!(
        lookback < 0,
        "lookback should start at -1 and go more negative"
    );
    // It's safe to unwrap this token here since token is non-whitespace
    // and the only command parse error is if the string is just whitespace.
    let command = token().map(|s| Command::from_str(&s).unwrap());

    command
        .skip(whitespace())
        .then(any::<Argument, Vec<Argument>>(get_argument_parser(
            lookback,
        )))
        .map(|(command, arguments)| Script { command, arguments })
        .label("<script>")
}

fn get_argument_parser(lookback: i32) -> Parser<Argument> {
    // TODO: this will explode the stack since each of these parsers call the other.
    //      we need a way to call lazily.  or implement Script::parser directly.
    // let script_arg = between('(', ')', get_script_parser(lookback - 1)).map(Argument::Script).label("<nested-script>");
    let string_arg = quoted().map(Argument::String).label("<string>");
    let color_arg = color().map(Argument::Color).label("<color>");
    let i64_arg = integer::<i64>().map(Argument::I64).label("<i64>");
    // TODO: evaluate any random string as a no-arg Script, besides comments

    peek(string_arg.or(color_arg).or(i64_arg))
        .skip(optional(whitespace()).skip(optional(comment())))
        .label("<argument>")
}

pub fn identifier() -> Parser<String> {
    many::<_, String>(satisfy(
        |c: char| c.is_ascii_alphabetic() || c == '/' || c == '-',
        "<identifier>",
    ))
    .label("<identifier>")
}

pub fn word() -> Parser<String> {
    many(letter())
}

pub fn token() -> Parser<String> {
    many::<_, String>(satisfy(|c| !c.is_whitespace(), "!<whitespace>"))
}

pub fn comment() -> Parser<String> {
    string("--")
        .skip(optional(whitespace()))
        .then(until(end()))
        .map(|(_, comment)| comment)
}

pub fn scale() -> Parser<u32> {
    symbol('@')
        .then(integer())
        .skip(symbol('x'))
        .label("@<scale>")
        .map(|(_, scale)| scale)
}

pub fn path() -> Parser<String> {
    token()
        .map(|input: String| {
            let mut path: OsString = input.clone().into();

            // Linux and BSD and MacOS use `~` to infer the home directory of a given user.
            if cfg!(unix) {
                // We have to do this dance because `Path::join` doesn't do what we want
                // if the input is for eg. "~/". We also can't use `Path::strip_prefix`
                // because it drops our trailing slash.
                if let Some('~') = input.chars().next() {
                    if let Some(base_dirs) = dirs::BaseDirs::new() {
                        path = base_dirs.home_dir().into();
                        path.push(&input['~'.len_utf8()..]);
                    }
                }
            }

            match path.to_str() {
                Some(p) => p.to_string(),
                None => panic!("invalid path: {:?}", path),
            }
        })
        .label("<path>")
}

// An implementation of `choice` which doesn't require parsing to the end.
// It also resets the input for each new option.
pub fn choose_prefix<T>(choices: Vec<Parser<T>>) -> Parser<T> {
    let label = choices
        .iter()
        .map(|p| p.label.clone())
        .collect::<Vec<_>>()
        .join(" | ");
    let expected = label.clone();

    Parser::new(
        move |input| {
            for p in &choices {
                match p.parse(input) {
                    Ok((result, rest)) => return Ok((result, rest)),
                    Err(_) => continue,
                }
            }
            Err((Error::expect(&expected, input), input))
        },
        label,
    )
}

impl Parse for platform::Key {
    fn parser() -> Parser<Self> {
        let alphanum = character().try_map(|c| {
            let key: platform::Key = c.into();

            if key == platform::Key::Unknown {
                return Err(format!("unknown key {:?}", c));
            } else if key == platform::Key::Space {
                return Err("use `<space>` for spacebar".to_string());
            }
            Ok(key)
        });

        let control = between('<', '>', any::<_, String>(letter())).try_map(|key| {
            let key = match key.as_str() {
                "up" => platform::Key::Up,
                "down" => platform::Key::Down,
                "left" => platform::Key::Left,
                "right" => platform::Key::Right,
                "ctrl" => platform::Key::Control,
                "alt" => platform::Key::Alt,
                "shift" => platform::Key::Shift,
                "space" => platform::Key::Space,
                "return" => platform::Key::Return,
                "backspace" => platform::Key::Backspace,
                "tab" => platform::Key::Tab,
                "end" => platform::Key::End,
                other => return Err(format!("unknown key <{}>", other)),
            };
            Ok(key)
        });

        control.or(alphanum).label("<key>")
    }
}

impl Parse for platform::Modifier {
    fn parser() -> Parser<Self> {
        peek(
            between('<', '>', any::<_, String>(letter())).try_map(|key| {
                let key = match key.as_str() {
                    "ctrl" => platform::Modifier::Control,
                    "alt" => platform::Modifier::Alt,
                    "shift" => platform::Modifier::Shift,
                    other => return Err(format!("unknown modifier <{}>", other)),
                };
                Ok(key)
            }),
        )
        .label("<mod>")
    }
}

impl Parse for platform::ModifiersState {
    fn parser() -> Parser<Self> {
        many::<_, Vec<platform::Modifier>>(param::<platform::Modifier>())
            .map(|modifiers| {
                let mut state = platform::ModifiersState {
                    shift: false,
                    alt: false,
                    ctrl: false,
                    meta: false,
                };
                for modifier in modifiers {
                    match modifier {
                        platform::Modifier::Control => state.ctrl = true,
                        platform::Modifier::Shift => state.shift = true,
                        platform::Modifier::Alt => state.alt = true,
                    }
                }
                state
            })
            .label("<mods>")
    }
}

impl Parse for platform::InputState {
    fn parser() -> Parser<Self> {
        word().try_map(|w| match w.as_str() {
            "pressed" => Ok(platform::InputState::Pressed),
            "released" => Ok(platform::InputState::Released),
            "repeated" => Ok(platform::InputState::Repeated),
            other => Err(format!("unknown input state: {}", other)),
        })
    }
}

impl Parse for Direction {
    fn parser() -> Parser<Self> {
        character()
            .try_map(|c| match c {
                '+' => Ok(Direction::Forward),
                '-' => Ok(Direction::Backward),
                _ => Err("direction must be either `+` or `-`"),
            })
            .label("+/-")
    }
}

pub fn param<T: Parse>() -> Parser<T> {
    T::parser()
}

pub fn color() -> Parser<Rgba8> {
    peek(
        token()
            .try_map(move |input| {
                if input.is_empty() {
                    return Err("expected color".to_owned());
                }
                // TODO: support e.g. #fff and #abc
                if input.len() < 7 {
                    return Err(format!("{:?} is not a valid color value", input));
                }
                let (s, alpha) = input.split_at(7);

                match Rgba8::from_str(s) {
                    Ok(color) => {
                        if let Ok((a, _)) = symbol('/')
                            .then(rational::<f64>())
                            .map(|(_, a)| a)
                            .parse(alpha)
                        {
                            Ok(color.alpha((a * std::u8::MAX as f64) as u8))
                        } else {
                            Ok(color)
                        }
                    }
                    Err(_) => Err(format!("malformed color value `{}`", s)),
                }
            })
            .label("<color>"),
    )
}

impl Parse for BrushMode {
    fn parser() -> Parser<Self> {
        Parser::new(
            |input| {
                let (id, p) = identifier().parse(input)?;
                match id.as_str() {
                    "erase" => Ok((BrushMode::Erase, p)),
                    "multi" => Ok((BrushMode::Multi, p)),
                    "perfect" => Ok((BrushMode::Perfect, p)),
                    "xsym" => Ok((BrushMode::XSym, p)),
                    "ysym" => Ok((BrushMode::YSym, p)),
                    "xray" => Ok((BrushMode::XRay, p)),
                    "line" => optional(whitespace())
                        .then(optional(natural()))
                        .parse(p)
                        .map(|((_, snap), p)| (BrushMode::Line(snap), p)),
                    mode => Err((
                        memoir::result::Error::new(format!("unknown brush mode '{}'", mode)),
                        input,
                    )),
                }
            },
            "<mode>",
        )
    }
}

impl Parse for Mode {
    fn parser() -> Parser<Self> {
        Parser::new(
            |input| {
                let (id, p) = identifier().parse(input)?;
                match id.as_str() {
                    "command" => Ok((Mode::Command, p)),
                    "normal" => Ok((Mode::Normal, p)),
                    "visual" => Ok((Mode::Visual(VisualState::default()), p)),
                    "present" => Ok((Mode::Present, p)),
                    mode => Err((
                        memoir::result::Error::new(format!("unknown mode: {}", mode)),
                        input,
                    )),
                }
            },
            "<mode>",
        )
    }
}

pub fn quoted() -> Parser<String> {
    single_quoted().or(double_quoted())
}

pub fn single_quoted() -> Parser<String> {
    between('\'', '\'', until(symbol('\'')))
}

pub fn double_quoted() -> Parser<String> {
    between('"', '"', until(symbol('"')))
}

pub fn paths() -> Parser<Vec<String>> {
    any::<_, Vec<String>>(path().skip(optional(whitespace()))).label("<path>..")
}

pub fn setting() -> Parser<String> {
    identifier().label("<setting>")
}

pub fn tuple<O>(x: Parser<O>, y: Parser<O>) -> Parser<(O, O)> {
    x.skip(whitespace()).then(y)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_paths() {
        let p = paths();

        let (out, rest) = p.parse("path/one.png path/two.png path/three.png").unwrap();

        assert_eq!(rest, "");
        assert_eq!(out, vec!["path/one.png", "path/two.png", "path/three.png"]);
    }

    #[test]
    fn test_color() {
        let p = color().skip(whitespace()).then(color());

        let ((a, b), rest) = p.parse("#ffaa44/0.5 #141414").unwrap();

        assert_eq!(rest, "");
        assert_eq!(a, Rgba8::new(0xff, 0xaa, 0x44, 127));
        assert_eq!(b, Rgba8::new(0x14, 0x14, 0x14, 255));
    }

    #[test]
    fn test_quotes() {
        let q = quoted();

        let (result, rest) = q.parse("\"hello world!\"").unwrap();
        assert_eq!(rest, "");
        assert_eq!(result, "hello world!".to_string());

        let (result, rest) = q.parse("'cast of cats'").unwrap();
        assert_eq!(rest, "");
        assert_eq!(result, "cast of cats".to_string());
    }

    #[test]
    fn test_script_multiple_simple_args() {
        let p = Script::parser();

        let (result, rest) = p.parse("if -3024 'hello' #123456 \"world\"").unwrap();
        assert_eq!(rest, "");
        assert_eq!(
            result,
            Script {
                command: Command::If,
                arguments: vec![
                    Argument::I64(-3024),
                    Argument::String("hello".to_string()),
                    Argument::Color(Rgba8 {
                        r: 0x12,
                        g: 0x34,
                        b: 0x56,
                        a: 0xff
                    }),
                    Argument::String("world".to_string()),
                ],
            }
        );
    }

    // TODO: test script with comments

    #[test]
    fn test_modifiers_parser_success() {
        let p = platform::ModifiersState::parser();

        assert_eq!(
            p.parse("<shift>").unwrap().0,
            platform::ModifiersState {
                shift: true,
                alt: false,
                ctrl: false,
                meta: false
            }
        );
        assert_eq!(
            p.parse("<ctrl>").unwrap().0,
            platform::ModifiersState {
                shift: false,
                alt: false,
                ctrl: true,
                meta: false
            }
        );
        assert_eq!(
            p.parse("<alt>").unwrap().0,
            platform::ModifiersState {
                shift: false,
                alt: true,
                ctrl: false,
                meta: false
            }
        );
        assert_eq!(
            p.parse("<shift><ctrl>").unwrap().0,
            platform::ModifiersState {
                shift: true,
                alt: false,
                ctrl: true,
                meta: false
            }
        );
        assert_eq!(
            p.parse("<ctrl><alt>").unwrap().0,
            platform::ModifiersState {
                shift: false,
                alt: true,
                ctrl: true,
                meta: false
            }
        );
        assert_eq!(
            p.parse("<alt><shift>").unwrap().0,
            platform::ModifiersState {
                shift: true,
                alt: true,
                ctrl: false,
                meta: false
            }
        );
        assert_eq!(
            p.parse("<ctrl><alt><shift>").unwrap().0,
            platform::ModifiersState {
                shift: true,
                alt: true,
                ctrl: true,
                meta: false
            }
        );
    }

    #[test]
    fn test_modifiers_parser_failure() {
        let p = platform::ModifiersState::parser();

        // We don't want ModifiersState to be greedy and assume that nothing is allowed as a blank modifier, otherwise it will match anything.
        assert!(p.parse("").is_err());
        assert!(p.parse("asdf").is_err());
        assert!(p.parse("<tab>").is_err());
    }

    #[test]
    fn test_modifiers_add_key_parser() {
        let p = platform::ModifiersState::parser().then(param::<platform::Key>());

        assert_eq!(
            p.parse("<ctrl>a").unwrap().0,
            (
                platform::ModifiersState {
                    shift: false,
                    alt: false,
                    ctrl: true,
                    meta: false
                },
                platform::Key::A
            )
        );
        assert_eq!(
            p.parse("<ctrl><shift>p").unwrap().0,
            (
                platform::ModifiersState {
                    shift: true,
                    alt: false,
                    ctrl: true,
                    meta: false
                },
                platform::Key::P
            )
        );
        assert!(p.parse("<ctrl>").is_err());
        assert!(p.parse("<ctrl><shift>").is_err());
        assert!(p.parse("<ctrl> ").is_err());
    }

    #[test]
    fn test_modifiers_with_key_or_plain_key_parser() {
        let p = peek(platform::ModifiersState::parser().then(param::<platform::Key>()))
            .or(param::<platform::Key>().map(|key| (platform::ModifiersState::default(), key)));

        assert_eq!(
            p.parse("<ctrl>a").unwrap().0,
            (
                platform::ModifiersState {
                    shift: false,
                    alt: false,
                    ctrl: true,
                    meta: false
                },
                platform::Key::A
            )
        );
        assert_eq!(
            p.parse("<ctrl>").unwrap().0,
            (
                platform::ModifiersState {
                    shift: false,
                    alt: false,
                    ctrl: false,
                    meta: false
                },
                platform::Key::Control
            )
        );
    }
}
