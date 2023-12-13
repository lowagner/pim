use memoir::traits::Parse;
use memoir::*;

use directories as dirs;

use crate::brush::BrushMode;
use crate::command::*;
use crate::gfx::Rgba8;
use crate::platform;
use crate::script::{Argument, Input, Script, Use};
use crate::session::{Direction, Mode, Visual};

use std::ffi::OsString;
use std::path::MAIN_SEPARATOR;
use std::str::FromStr;

pub type Error = memoir::result::Error;

/*
    Creating your own commands, and how to run them:

        :set 'cmd_name' (the_cmd $0 123)
    =>  :cmd_name 5     -- calls `the_cmd` with arguments `5` `123`

    where $0 means to use the argument at index 0.  Similarly for $1, etc.

    The reason we use $0 for the first argument is to avoid off-by-one errors while
    indexing the arguments internally within this script interpreter.

    If we used $0 for the function name, this would make it more bash-like, and we
    could call recursively like this:
        :const 'factorial' (if (positive $1) (* $1 ($0 (+ $1 -1))) 1)
    But we want to avoid off-by-one errors, so if we want to support this, we will create
    our own re-evaluation function, e.g., $$.
        :const 'factorial' (if (positive $0) (* $0 ($$ (+ $0 -1))) 1)

    Note we also put parentheses before the function name so that in the common case,
    where we're running just one function, we don't need to use parentheses.
    E.g., `my_function my_arg_0 my_arg_1 my_arg_etc` instead of
    `my_function(my_arg_0 my_arg1 my_arg_etc)` (NOT VALID).
*/
impl Parse for Script {
    fn parser() -> Parser<Self> {
        get_script_parser(0)
    }
}

fn get_script_parser(lookback: i32) -> Parser<Script> {
    // TODO: add some logic for seeing a comma, turning things into a `run-all`.
    // e.g., `fg (pc 1), bg (pc 2)`
    // TODO: i think it might be better to convert it into an arguments vector.
    // ideally we make it possible to run something on pressing a button, e.g.,
    // `f0, f1, (f2 arg0 arg1), ...` and then use those to populate the arguments
    // for the script on release, e.g., $0 -> f0's result, $1 -> f1's result, etc.
    // i.e., make it a `Script` without a command.
    // TODO: if we do something like `-cmd` then convert to something like (* -1 cmd)
    special_command()
        .skip(optional(whitespace()))
        .then(any::<Argument, Vec<Argument>>(
            get_argument_parser(lookback).skip(optional(whitespace()).skip(optional(comment()))),
        ))
        .map(|(special, arguments)| match special {
            SpecialCommand::Command(command) => Script { command, arguments },
            SpecialCommand::Script(mut script) => {
                script.arguments.extend_from_slice(&arguments);
                script
            }
        })
        .label("<script>")
}

// NOTE: There's not a huge point to exposing this as an Argument::parser()
// due to the fact that `Use` depends on lookback.
fn get_argument_parser(lookback: i32) -> Parser<Argument> {
    Parser::new(
        move |input| {
            // Prevent stack overflow (script within argument recursion).
            // Only spin up another script parser if we're within a new `()` block.
            if input.starts_with('(') {
                let nested_script_parser = get_script_parser(lookback - 1);
                return match nested_script_parser.parse(&input[1..]) {
                    Ok((nested_script, rest)) if rest.starts_with(')') => {
                        Ok((Argument::Script(nested_script), &rest[1..]))
                    }
                    Ok((nested_script, rest)) if rest.is_empty() => {
                        eprint!(
                            "argument with nested script didn't terminate: `{}` -> `{}`\n",
                            input, rest
                        );
                        Ok((Argument::Script(nested_script), rest))
                    }
                    Ok((_, rest)) => {
                        eprint!(
                            "unexpected end of nested script: `{}` -> `{}`\n",
                            input, rest
                        );
                        Err(("unexpected end of nested script".into(), input))
                    }
                    Err(err) => Err(err),
                };
            }

            let i64_arg = integer::<i64>().map(Argument::I64).label("<i64>");
            let color_arg = color().map(Argument::Color).label("<color>");
            let string_arg = peek(implicit_path().or(quoted()))
                .map(Argument::String)
                .label("<string>");
            // TODO: maybe look for an optional `-[0-9]+` (e.g., full use `$1-4`) to do lookback -4.
            // if we see a use-case for looking back on arguments not scripted in.
            let use_arg = symbol('$')
                .then(natural::<u32>())
                .map(move |(_symbol, index)| Argument::Use(Use { index, lookback }));
            let input_arg = param::<Input>().map(Argument::Input).label("<input>");
            // Evaluate any random token as a command in a no-arg Script.
            // This allows things like `bg fg` to switch the background
            // to the foreground color.  E.g., zero-arg functions are
            // typically getters.
            let zero_arg_script = special_command().map(|special| match special {
                SpecialCommand::Command(c) => Argument::Script(Script::zero_arg(c)),
                SpecialCommand::Script(s) => Argument::Script(s),
            });

            peek(
                i64_arg
                    .or(color_arg)
                    .or(string_arg)
                    .or(use_arg)
                    .or(input_arg)
                    .or(zero_arg_script),
            )
            .label("<argument>")
            .parse(input)
        },
        "<argument>",
    )
}

fn special_command() -> Parser<SpecialCommand> {
    token().map(|s| {
        let postfix = if s.ends_with("++") {
            1
        } else if s.ends_with("--") {
            -1
        } else {
            0
        };
        if postfix != 0 {
            let pre_postfix = &s[0..s.len() - 2];
            if pre_postfix.len() == 0 {
                // User may have overrode ++ or -- as a separate command;
                // let that "fall through" to the default case of interpreting it as a command.
            } else {
                // This should be safe because pre_postfix.len() > 0;
                // the only command parse error is if the string is just whitespace.
                let command = Command::from_str(&pre_postfix).unwrap();
                // User put in something like `f++` which should be interpreted as `f (+ f 1)`.
                return SpecialCommand::Script(Script {
                    command: command.clone(),
                    arguments: vec![Argument::Script(Script {
                        command: Command::Sum,
                        arguments: vec![
                            Argument::Script(Script::zero_arg(command)),
                            Argument::I64(postfix),
                        ],
                    })],
                });
            }
        }
        // It's safe to unwrap this token here since token is non-whitespace
        // and the only command parse error is if the string is just whitespace.
        SpecialCommand::Command(Command::from_str(&s).unwrap())
    })
}

enum SpecialCommand {
    Command(Command),
    Script(Script),
}

pub fn identifier() -> Parser<String> {
    many::<_, String>(satisfy(
        |c: char| c.is_ascii_alphabetic() || c == '/' || c == '-' || c == '%',
        "<identifier>",
    ))
    .label("<identifier>")
}

pub fn word() -> Parser<String> {
    many(letter())
}

/// The next run of consecutive, non-white-space characters that aren't `(` or `)`.
/// We need to avoid () so that we can create scripts like this:
/// `const 'my-script' (fg #123456)` and not get `(fg` lumped in one token
/// and `#123456)` in another.
pub fn token() -> Parser<String> {
    many::<_, String>(satisfy(
        |c| !c.is_whitespace() && c != '(' && c != ')',
        "!<whitespace>",
    ))
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

// TODO: i think we can remove this at some point and use `implicit_path` instead.
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

// TODO: rename to `path` eventually.
fn implicit_path() -> Parser<String> {
    peek(
        token()
            .try_map(|input: String| {
                let evidence = input.contains(MAIN_SEPARATOR) || input.contains(".");

                // Linux and BSD and MacOS use `~` to infer the home directory of a given user.
                let maybe_path: Option<OsString> = if cfg!(unix) {
                    // We have to do this dance because `Path::join` doesn't do what we want
                    // if the input is for eg. "~/". We also can't use `Path::strip_prefix`
                    // because it drops our trailing slash.
                    if input.starts_with('~') {
                        let base_dirs = dirs::BaseDirs::new().unwrap();
                        let mut path: OsString = base_dirs.home_dir().into();
                        path.push(&input['~'.len_utf8()..]);
                        Some(path)
                    } else if evidence {
                        Some(input.clone().into())
                    } else {
                        None
                    }
                } else if evidence {
                    Some(input.clone().into())
                } else {
                    None
                };
                match maybe_path {
                    None => Err("probably not a path"),
                    Some(path) => Ok(path.to_str().unwrap().to_string()),
                }
            })
            .label("<path>"),
    )
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
        get_key_parser(false)
    }
}

fn get_key_parser(tag_required: bool) -> Parser<platform::Key> {
    let tagged_key = between('<', '>', any::<_, String>(letter())).try_map(move |key| {
        let key = match key.as_str() {
            "up" => platform::Key::Up,
            "down" => platform::Key::Down,
            "left" => platform::Key::Left,
            "right" => platform::Key::Right,
            "backspace" => platform::Key::Backspace,
            "return" => platform::Key::Return,
            "space" => platform::Key::Space,
            "tab" => platform::Key::Tab,
            "esc" => platform::Key::Escape,
            "escape" => platform::Key::Escape,
            "ins" => platform::Key::Insert,
            "insert" => platform::Key::Insert,
            "del" => platform::Key::Delete,
            "delete" => platform::Key::Delete,
            "home" => platform::Key::Home,
            "end" => platform::Key::End,
            "pgdown" => platform::Key::PageDown,
            "pagedown" => platform::Key::PageDown,
            "pgup" => platform::Key::PageUp,
            "pageup" => platform::Key::PageUp,
            "shift" => platform::Key::Shift,
            "ctrl" => platform::Key::Control,
            "alt" => platform::Key::Alt,
            "meta" => platform::Key::Meta,
            other => {
                if tag_required {
                    let key = char::from_str(other);
                    if key.is_ok() {
                        let key: platform::Key = key.unwrap().into();
                        if key != platform::Key::Unknown {
                            return Ok(key);
                        }
                    }
                }
                return Err(format!("unknown key <{}>", other));
            }
        };
        Ok(key)
    });

    if tag_required {
        tagged_key
    } else {
        let alphanum = character().try_map(|c| {
            let key: platform::Key = c.into();

            if key == platform::Key::Unknown {
                return Err(format!("unknown key {:?}", c));
            } else if key == platform::Key::Space {
                return Err("use `<space>` for spacebar".to_string());
            }
            Ok(key)
        });
        tagged_key.or(alphanum).label("<key>")
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
                    "meta" => platform::Modifier::Meta,
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
                        platform::Modifier::Meta => state.meta = true,
                    }
                }
                state
            })
            .label("<mods>")
    }
}

impl Parse for Input {
    fn parser() -> Parser<Self> {
        // TODO: mouse button
        // TODO: mouse wheel
        // TODO: mouse move
        // TODO: modifiers with a rune, e.g., `<shift>'ö'`, becomes Input::Rune(mods, char)
        let modifiers = param::<platform::ModifiersState>();
        // TODO: the "tag" part of the key should be optional if modifiers were present.
        let tagged_key = get_key_parser(true);
        peek(optional(modifiers).then(tagged_key))
            .map(|(mods, key)| {
                let mods = mods.unwrap_or_default();
                Input::Key(mods, key)
            })
            .label("<input>")
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

// TODO: "of" might make more sense than "param"
pub fn param<T: Parse>() -> Parser<T> {
    T::parser()
}

pub fn color() -> Parser<Rgba8> {
    peek(
        token()
            .try_map(|input| Rgba8::from_str(&input))
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
                    "visual" => Ok((Mode::Visual(Visual::default()), p)),
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
    use crate::platform::{Key, ModifiersState};
    use crate::settings::*;

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

        let ((a, b), rest) = p.parse("#ffaa447f #141414").unwrap();

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

    #[test]
    fn test_script_no_args() {
        let p = Script::parser();

        let (result, rest) = p.parse("bg").unwrap();

        assert_eq!(rest, "");
        assert_eq!(
            result,
            Script {
                command: Command::ColorSetting(ColorSetting::Background),
                arguments: vec![],
            }
        );
    }

    #[test]
    fn test_script_with_nested_scripts() {
        let p = Script::parser();

        let (result, rest) = p
            .parse("if $10 (hello 'world' (hi 42 \"earth\" $5)) (hey 'moon' -7 $93)")
            .unwrap();

        assert_eq!(rest, "");
        assert_eq!(
            result,
            Script {
                command: Command::If,
                arguments: vec![
                    Argument::Use(Use {
                        index: 10,
                        lookback: 0 // note this would be invalid
                    }),
                    Argument::Script(Script {
                        command: Command::Evaluate("hello".to_string()),
                        arguments: vec![
                            Argument::String("world".to_string()),
                            Argument::Script(Script {
                                command: Command::Evaluate("hi".to_string()),
                                arguments: vec![
                                    Argument::I64(42),
                                    Argument::String("earth".to_string()),
                                    Argument::Use(Use {
                                        index: 5,
                                        lookback: -2
                                    }),
                                ],
                            }),
                        ],
                    }),
                    Argument::Script(Script {
                        command: Command::Evaluate("hey".to_string()),
                        arguments: vec![
                            Argument::String("moon".to_string()),
                            Argument::I64(-7),
                            Argument::Use(Use {
                                index: 93,
                                lookback: -1
                            }),
                        ],
                    }),
                ],
            }
        );
    }

    #[test]
    fn test_script_with_short_nested_script_and_comment() {
        let p = Script::parser();

        let (result, rest) = p.parse("fg (bg) -- 123 'do stuff'").unwrap();

        assert_eq!(rest, "");
        assert_eq!(
            result,
            Script {
                command: Command::ColorSetting(ColorSetting::Foreground),
                arguments: vec![Argument::Script(Script {
                    command: Command::ColorSetting(ColorSetting::Background),
                    arguments: vec![]
                }),],
            }
        );
    }

    #[test]
    fn test_script_converts_unknown_tokens_to_zero_arg_scripts() {
        let p = Script::parser();

        let (result, rest) = p.parse("p x1 y2 (fg swap3)").unwrap();

        assert_eq!(rest, "");
        assert_eq!(
            result,
            Script {
                command: Command::Paint,
                arguments: vec![
                    Argument::Script(Script {
                        command: Command::Evaluate("x1".to_string()),
                        arguments: vec![]
                    }),
                    Argument::Script(Script {
                        command: Command::Evaluate("y2".to_string()),
                        arguments: vec![]
                    }),
                    Argument::Script(Script {
                        command: Command::ColorSetting(ColorSetting::Foreground),
                        arguments: vec![Argument::Script(Script {
                            command: Command::Evaluate("swap3".to_string()),
                            arguments: vec![]
                        }),],
                    }),
                ],
            }
        );
    }

    #[test]
    fn test_script_with_unfinished_nested_script() {
        let p = Script::parser();

        let (result, rest) = p.parse("fg (hello 'world' 123").unwrap();

        assert_eq!(rest, "");
        assert_eq!(
            result,
            Script {
                command: Command::ColorSetting(ColorSetting::Foreground),
                arguments: vec![Argument::Script(Script {
                    command: Command::Evaluate("hello".to_string()),
                    arguments: vec![Argument::String("world".to_string()), Argument::I64(123),],
                }),],
            }
        );
    }

    #[test]
    fn test_script_with_unfinished_nested_script_with_comment() {
        let p = Script::parser();

        let (result, rest) = p.parse("fg (hello 'world' -123 -- comment").unwrap();

        assert_eq!(rest, "");
        assert_eq!(
            result,
            Script {
                command: Command::ColorSetting(ColorSetting::Foreground),
                arguments: vec![Argument::Script(Script {
                    command: Command::Evaluate("hello".to_string()),
                    arguments: vec![Argument::String("world".to_string()), Argument::I64(-123),],
                }),],
            }
        );
    }

    #[test]
    fn test_script_can_reserialize() {
        let start = "paint $0 (y2 $1) (if condition (fg $2) (chill (bg $2)))".to_string();
        let p = Script::parser();

        let (result, rest) = p.parse(&start).unwrap();

        assert_eq!(rest, "");
        assert_eq!(format!("{}", result), start);
    }

    #[test]
    fn test_script_can_do_postfix_increment() {
        let p = Script::parser();

        let (result, rest) = p.parse("f++").unwrap();

        assert_eq!(rest, "");
        assert_eq!(
            result,
            Script {
                command: Command::I64Setting(I64Setting::FrameIndex),
                arguments: vec![Argument::Script(Script {
                    command: Command::Sum,
                    arguments: vec![
                        Argument::Script(Script::zero_arg(Command::I64Setting(
                            I64Setting::FrameIndex
                        ))),
                        Argument::I64(1),
                    ],
                }),],
            }
        );
    }

    #[test]
    fn test_script_can_do_postfix_increment_in_a_script() {
        let p = Script::parser();

        let (result, rest) = p.parse("+ f-width++ 123").unwrap();

        assert_eq!(rest, "");
        assert_eq!(
            result,
            Script {
                command: Command::Sum,
                arguments: vec![
                    Argument::Script(Script {
                        command: Command::I64Setting(I64Setting::FrameWidth),
                        arguments: vec![Argument::Script(Script {
                            command: Command::Sum,
                            arguments: vec![
                                Argument::Script(Script::zero_arg(Command::I64Setting(
                                    I64Setting::FrameWidth
                                ))),
                                Argument::I64(1),
                            ],
                        }),],
                    }),
                    Argument::I64(123),
                ],
            }
        );
    }

    #[test]
    fn test_script_can_do_postfix_decrement_with_extraneous_argument() {
        let p = Script::parser();

        // Note this is probably garbage that won't be used by normal commands,
        // but just in case it will be added as a second argument.
        let (result, rest) = p.parse("foo-- 'asdf'").unwrap();

        assert_eq!(rest, "");
        assert_eq!(
            result,
            Script {
                command: Command::Evaluate("foo".to_string()),
                arguments: vec![
                    Argument::Script(Script {
                        command: Command::Sum,
                        arguments: vec![
                            Argument::Script(Script::zero_arg(Command::Evaluate(
                                "foo".to_string()
                            ))),
                            Argument::I64(-1),
                        ],
                    }),
                    Argument::String("asdf".to_string()),
                ],
            }
        );
    }

    #[test]
    fn test_script_can_parse_input() {
        let p = Script::parser();

        let (result, rest) = p.parse("asdf <ctrl><e> <shift><alt><home>").unwrap();

        assert_eq!(rest, "");
        assert_eq!(
            result,
            Script {
                command: Command::Evaluate("asdf".to_string()),
                arguments: vec![
                    Argument::Input(Input::Key(ModifiersState::CTRL, Key::E)),
                    Argument::Input(Input::Key(ModifiersState::ALT_SHIFT, Key::Home)),
                ],
            }
        );
    }

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

    #[test]
    fn test_implicit_path_parses_directories() {
        let p = implicit_path();

        if cfg!(unix) {
            assert_eq!(
                p.parse("~/hello").unwrap().0,
                dirs::BaseDirs::new()
                    .unwrap()
                    .home_dir()
                    .to_str()
                    .unwrap()
                    .to_string()
                    + "/hello"
            );

            assert_eq!(p.parse("hello/"), Ok(("hello/".to_string(), "")));
        } else {
            assert_eq!(p.parse("hello\\"), Ok(("hello\\".to_string(), "")));
        }
    }

    #[test]
    fn test_implicit_path_parses_files_with_extensions() {
        let p = implicit_path();
        assert_eq!(
            p.parse("this-is-a-great-file.png"),
            Ok(("this-is-a-great-file.png".to_string(), ""))
        );
        assert_eq!(p.parse(".pimrc hi"), Ok((".pimrc".to_string(), " hi")));
    }

    #[test]
    fn test_implicit_path_fails_for_strings_without_separators_or_periods() {
        let p = implicit_path();
        assert!(p.parse("hello").is_err());
        assert!(p.parse("hi-hello").is_err());
        assert!(p.parse("hi++hello").is_err());
    }

    #[test]
    fn test_input_parser() {
        let p = param::<Input>();

        // TODO: enable this as a Input::Key(mods, platform::Key::Shift) input
        assert!(p.parse("<shift>").is_err());
        assert_eq!(
            p.parse("<shift><e>"),
            Ok((
                Input::Key(platform::ModifiersState::SHIFT, platform::Key::E),
                ""
            ))
        );

        assert!(p.parse("<shift><ctrl>").is_err());
        assert_eq!(
            p.parse("<shift><ctrl><home>..."),
            Ok((
                Input::Key(platform::ModifiersState::CTRL_SHIFT, platform::Key::Home),
                "..."
            ))
        );

        assert!(p.parse("<shift><ctrl><alt>").is_err());
        assert_eq!(
            p.parse("<alt><shift><ctrl><pgup>asdf"),
            Ok((
                Input::Key(
                    platform::ModifiersState::CTRL_ALT_SHIFT,
                    platform::Key::PageUp
                ),
                "asdf"
            ))
        );

        assert!(p.parse("<meta><shift><ctrl><alt>").is_err());
        assert_eq!(
            p.parse("<alt><meta><shift><ctrl><pagedown>whoa"),
            Ok((
                Input::Key(
                    platform::ModifiersState::CTRL_ALT_SHIFT_META,
                    platform::Key::PageDown
                ),
                "whoa"
            ))
        );

        assert!(p.parse("<hamburger>").is_err());
        assert_eq!(
            p.parse("<end>y"),
            Ok((
                Input::Key(platform::ModifiersState::default(), platform::Key::End),
                "y"
            ))
        );
        assert_eq!(
            p.parse("<q>soup"),
            Ok((
                Input::Key(platform::ModifiersState::default(), platform::Key::Q),
                "soup"
            ))
        );
    }
}
