use crate::gfx::Rgba8;
use crate::palette::Palette;

use claim::assert_ok;

use std::collections::HashMap;
use std::fmt;
use std::mem;
use std::str::FromStr;

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

TODO: add `cx`/`cy` as functions/commands to get cursor position x/y
TODO: or maybe `mx`/`my` for mouse x/y.  could do both.
*/

/// The commands that are possible in a script.
// Note these are designed to be quickly cloned, so don't put large amounts of data in them.
#[derive(PartialEq, Debug, Clone)]
pub enum Command {
    /// *Does not evaluate* $0, returns info about the script/command instead.
    Help,

    /// Creates a notification message with a string that combines all arguments.
    Echo,
    /// Creates an error with a string that combines all arguments.
    Error,

    /// Evaluates all `Argument`s in turn, returning early if any is an Err.
    /// Keeps track of the first argument's result (the `push`) regardless of
    /// any errors later, and when done with the other arguments, reaches into
    /// the first argument and runs it as a script with the `push` result,
    /// i.e., which returns the variable to its original value, i.e., the `pop`.
    /// NOTE: the first argument should be a script which is setting the pushed value.
    /// E.g., this will change the foreground color for the duration of the script,
    /// but then reset it after the script is done:
    /// `push/pop (fg #123456) do_stuff_with_new_fg`
    PushPop,
    /// Evaluates each Argument in turn, returning early if any is an Err,
    /// returning Ok with last result otherwise.  Will return any errors
    /// from earlier arguments since that prevents the last argument from
    /// executing.
    RunAll,

    // Evaluates the first argument, then uses it as an index into the remaining arguments.
    // TODO: option 1: if 0, don't run anything.  1, 2, etc. index the arguments directly as $1, $2, etc.
    // TODO: option 2: if 0, run argument $1.  1, 2, etc. index the arguments at offset $2, $3, etc.
    // TODO: RunIndex,
    /// Runs $0, returns 0 if truthy, 1 if falsy.
    Not,
    /// Runs $0, checks if it's truthy, then evaluates $1 if so, otherwise $2.
    /// Note that $1 and $2 are optional.
    // TODO: if $2 is missing, return I64(0); if $1 is missing, return I64(1)
    If,
    /// Returns 1 if $0 is even, otherwise 0.  This is an error if $0 does
    /// not evaluate to an integer.
    // TODO: evaluate all arguments as a sum, initialize to 0
    Even,
    /// Returns 1 if $0 is odd, otherwise 0.  This is an error if $0 does
    /// not evaluate to an integer.
    // TODO: evaluate all arguments as a sum, initialize to 0
    Odd,

    /// Sums all arguments, using $0 as the type to return.
    /// E.g., if $0 is an integer, then $1, $2, etc. will be cast to integers.
    Sum,
    /// Multiplies all arguments.  Each argument will be cast to an integer.
    /// If no arguments are present, returns 1.
    Product,

    /// Gets the value of a variable that is known to the compiler; evaluates it.
    /// This will also evaluate scripts, etc., returning an argument that evaluates as
    /// a value, i.e., `argument.is_value()` is true.
    Evaluate(String),
    // Gets the value of a named variable at $0; technically evaluating it.
    // TODO: this won't work well with a Script due to an offset that doesn't happen with Evaluate.
    //      E.g., `:get 'my-fn' Arg1 Arg2` will be shifted from `$my-fn Arg1 Arg2`.
    //      $0 will refer to 'my-fn' in the former, while $0 will refer to Arg1 in the latter.
    //      let's keep $0 as the first argument (Arg1), so we'll need to copy the argument array for `GetVariable`.
    //      probably the easiest thing to do is "LoadVariableName" and "EvaluateVariableName" with some extra state.
    //      The other thing that's hard to do with this is if someone uses `Get/Execute "if" ...`
    //      for any of the primitive operations; we'd need to handle those specially.
    // GetVariable,
    /// Sets a variable with name $0 to the value at $1, allowing future changes.
    /// E.g., `:set 'my-mut' 123` will set `my-mut` to evaluate to 123.
    /// You can also define lambda functions with e.g., `:set 'my-fn' (fg $0)`
    SetVariable,
    /// Sets a variable with name $0 to the value at $1, and doesn't allow future changes to this variable.
    /// E.g., `:const 'my-const' 123` will set `my-const` to evaluate to 123.
    /// You can also define lambda functions with e.g., `:const 'my-fn' (fg $0)`
    ConstVariable,
    /// Uses $0 to alias the name of a variable at $1, and doesn't allow future changes to this alias name.
    /// E.g., `:alias 'my-alias' 'fg'` will set `my-alias` to act as `fg`.
    /// Note that the alias is constant, but what the variable it points to can change.
    CreateAlias,

    // TODO: Map: uses mode $0, with keybinding at $1, to evaluate command at $2.  optional command at $3 for release
    /// Returns the current mode, changing it to what's in $0 if present and valid.
    Mode,

    /// Getter/swapper for whether we animate the frames.
    Animate,
    /// Getter/swapper for the UI Scale (e.g., palette boxes and command line), as a percentage.
    UiScale,

    /// Setter for the width x height of each frame, using $0 for width and $1 for height.
    /// If either $0 or $1 is null, it keeps that dimension the same; if both are null,
    /// it crops the frames to content.  Returns the number of pixels in one frame, i.e.,
    /// width * height, from *before* the operation.
    FrameResize,
    /// Getter/swapper for the width of each frame.  If $0 is null, this returns the current width;
    /// if $0 is an integer, sets the frame width to $0 and returns the old value.
    FrameWidth,
    /// Getter/swapper for the height of each frame.  Compare with FrameWidth.
    FrameHeight,
    /// Getter/swapper for the current frame index.  If $0 is null, returns the current frame;
    /// if $0 is an integer, sets the current frame to that value and returns the old value.
    /// Note that we mod by the number of frames, so `f -1` will go to the last frame.
    FrameIndex,
    // TODO: FrameClone
    // TODO: FrameDelete
    // TODO: FrameSwap
    // TODO: FrameShift, moves the animation over one to start one frame down

    // TODO: FitPixelWidth, FitPixelHeight; zoom to fit that many pixels within the screen based on available area
    /// Uses $0 to set the foreground color, if not null, and returns the old value.
    /// If $0 is null, returns the current foreground color without changing it.
    /// If $1 is a valid color, e.g., a hex color (#123456) or a palette index (0),
    /// then the foreground color will be updated to that color and the old color
    /// will be returned (in hex).  For example:
    ///     `fg 1`          sets foreground color to what is in palette 1
    ///     `fg #fedcba`    sets the foreground color to #fedcba
    ///     `fg`            just returns the foreground color without changing it
    ForegroundColor,
    /// Same as `ForegroundColor` but for the background color.  For example:
    ///     `bg 0`          sets background color to what is in palette 0
    ///     `bg #012345`    sets the background color to #012345
    ///     `bg`            just returns the background color without changing it
    BackgroundColor,
    /// Uses $0 for x, $1 for y, and $2 as an optional color (defaults to foreground color).
    /// E.g., `paint 5 10` to paint the pixel at (5, 10) with the foreground color,
    /// and `paint 11 12 #123456` to paint the pixel at (11, 12) with #123456.
    /// Note that an integer for the color also works as an index to the palette.
    Paint,

    /// Getter/swapper for brush size.  If $0 is null, returns current brush size;
    /// if $0 is an integer, sets the brush size to that and returns the old brush size.
    BrushSize,
    /// Getter/swapper for BrushMode.  E.g., without an argument, will return the current value;
    /// with an argument, will set/unset the BrushMode if $0 is truthy/falsey.  In the latter case,
    /// the command will return the old value.  For specific values, `b/erase` to get/swap erase
    /// mode, `b/multi` to get/swap multi-frame drawing, etc.  Note that `b/line` can take an integer
    /// argument for the angle (in degrees) to snap to when drawing a line; 0 for no snapping.
    BrushMode(BrushMode),
    // TODO: BrushReset,

    /// Versions of quit, see enum `Quit`.
    Quit(Quit),
}

impl Command {
    pub fn is_built_in(&self) -> bool {
        !matches!(self, Self::Evaluate(_))
    }

    pub fn dynamic(&self) -> Option<String> {
        match self {
            Command::Evaluate(string) => Some(string.clone()),
            _ => None,
        }
    }
}

impl fmt::Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Command::Help => write!(f, "?"),
            Command::Echo => write!(f, "echo"),
            Command::Error => write!(f, "error"),
            Command::PushPop => write!(f, "push/pop"),
            Command::RunAll => write!(f, "run/all"),
            Command::Not => write!(f, "not"),
            Command::If => write!(f, "if"),
            Command::Even => write!(f, "even"),
            Command::Odd => write!(f, "odd"),
            Command::Sum => write!(f, "+"),
            Command::Product => write!(f, "*"),
            Command::Evaluate(value) => write!(f, "{}", value),
            Command::SetVariable => write!(f, "set"),
            Command::ConstVariable => write!(f, "const"),
            Command::CreateAlias => write!(f, "alias"),
            Command::Mode => write!(f, "mode"),
            Command::Animate => write!(f, "a"),
            Command::UiScale => write!(f, "ui/scale%"),
            Command::FrameResize => write!(f, "f/resize"),
            Command::FrameWidth => write!(f, "f/width"),
            Command::FrameHeight => write!(f, "f/height"),
            Command::FrameIndex => write!(f, "f"),
            Command::ForegroundColor => write!(f, "fg"),
            Command::BackgroundColor => write!(f, "bg"),
            Command::Paint => write!(f, "p"),
            Command::BrushSize => write!(f, "b/size"),
            Command::BrushMode(BrushMode::Erase) => write!(f, "b/erase"),
            Command::BrushMode(BrushMode::Multi) => write!(f, "b/multi"),
            Command::BrushMode(BrushMode::Perfect) => write!(f, "b/perfect"),
            Command::BrushMode(BrushMode::XSym) => write!(f, "b/xsym"),
            Command::BrushMode(BrushMode::YSym) => write!(f, "b/ysym"),
            Command::BrushMode(BrushMode::XRay) => write!(f, "b/xray"),
            Command::BrushMode(BrushMode::Line) => write!(f, "b/line"),
            Command::Quit(Quit::Safe) => write!(f, "q"),
            Command::Quit(Quit::AllSafe) => write!(f, "qa"),
            Command::Quit(Quit::Forced) => write!(f, "q!"),
            Command::Quit(Quit::AllForced) => write!(f, "qa!"),
        }
    }
}

impl FromStr for Command {
    type Err = EmptyCommandParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.trim() {
            "?" => Ok(Command::Help),
            "echo" => Ok(Command::Echo),
            "error" => Ok(Command::Error),
            "push/pop" => Ok(Command::PushPop),
            "run/all" => Ok(Command::RunAll),
            "not" => Ok(Command::Not),
            "if" => Ok(Command::If),
            "even" => Ok(Command::Even),
            "odd" => Ok(Command::Odd),
            "+" => Ok(Command::Sum),
            "*" => Ok(Command::Product),
            "set" => Ok(Command::SetVariable),
            "const" => Ok(Command::ConstVariable),
            "alias" => Ok(Command::CreateAlias),
            "mode" => Ok(Command::Mode),
            "a" => Ok(Command::Animate),
            "ui/scale%" => Ok(Command::UiScale),
            "f/resize" => Ok(Command::FrameResize),
            "f/width" => Ok(Command::FrameWidth),
            "f/height" => Ok(Command::FrameHeight),
            "f" => Ok(Command::FrameIndex),
            "fg" => Ok(Command::ForegroundColor),
            "bg" => Ok(Command::BackgroundColor),
            "p" => Ok(Command::Paint),
            "b/size" => Ok(Command::BrushSize),
            "b/erase" => Ok(Command::BrushMode(BrushMode::Erase)),
            "b/multi" => Ok(Command::BrushMode(BrushMode::Multi)),
            "b/perfect" => Ok(Command::BrushMode(BrushMode::Perfect)),
            "b/xsym" => Ok(Command::BrushMode(BrushMode::XSym)),
            "b/ysym" => Ok(Command::BrushMode(BrushMode::YSym)),
            "b/xray" => Ok(Command::BrushMode(BrushMode::XRay)),
            "b/line" => Ok(Command::BrushMode(BrushMode::Line)),
            "q" => Ok(Command::Quit(Quit::Safe)),
            "qa" => Ok(Command::Quit(Quit::AllSafe)),
            "q!" => Ok(Command::Quit(Quit::Forced)),
            "qa!" => Ok(Command::Quit(Quit::AllForced)),
            name => {
                if name.len() > 0 {
                    Ok(Command::Evaluate(name.to_string()))
                } else {
                    Err(EmptyCommandParseError)
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct EmptyCommandParseError;

// TODO: rename BrushMode to BrushOption
/// Brush mode. Any number of these modes can be active at once.
// TODO: update `brush.rs` to these values once `script.rs` takes over `cmd.rs`;\
// move the argument on `brush::BrushMode::Line` into the Brush class instance itself.
#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Debug)]
pub enum BrushMode {
    /// Erase pixels.
    Erase,
    /// Draw on all frames at once.
    Multi,
    /// Pixel-perfect mode.
    Perfect,
    /// X-Symmetry mode.
    XSym,
    /// Y-Symmetry mode.
    YSym,
    /// X-Ray mode.
    XRay,
    /// Confine line angles to multiples of this value.
    Line,
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Quit {
    /// The default, Quit::Safe, via `quit` or `q`, quits this view if saved.
    Safe,
    /// Quit all views, via `qa`, if safe to do so (work is saved).
    AllSafe,
    /// Force quit this view, via `q!`.
    Forced,
    /// Force quit all views, via `qa!`.
    AllForced,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Script {
    pub command: Command,
    pub arguments: Arguments,
}

impl Script {
    pub fn zero_arg(command: Command) -> Self {
        Script {
            command,
            arguments: vec![],
        }
    }

    pub fn run(&self, runner: &mut dyn ScriptRunner) -> ArgumentResult {
        runner.run(vec![self])
    }
}

fn serialize_script(script: &Script, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", script.command)?;
    for a in &script.arguments {
        write!(f, " {}", Serialize::Argument(&a))?;
    }
    fmt::Result::Ok(())
}

pub type Arguments = Vec<Argument>;
pub type ArgumentResult = Result<Argument, String>;

#[derive(PartialEq, Debug, Clone)]
pub enum Argument {
    // Value-based arguments.
    Null,

    // TODO: for Scale, use an I64 with the number of pixels desired in the X/Y direction.
    // E.g., we can have PixelsX/PixelsY or width/height
    I64(i64),
    Color(Rgba8),
    String(String),
    // TODO: for keys (e.g. a-z, <tab>, <backspace>, <ctrl>, etc.)
    //      and also mods (<ctrl>a, <alt>c, etc.)
    // Note that commands that take Input should also take a String that is a single
    // character long, e.g., 'A' or 'ö'.
    // Input(Input),

    // Non-value-based (AKA evaluatable) arguments follow.
    // TODO: we should be able to pause execution, e.g., for an alert box to confirm an action

    // An argument that is itself the output of another Script.
    // Note that these are never memoized and lazily evaluated
    // (i.e., only if needed; e.g., `b` will never be evaluated
    // if `a` evaluates to false in `if a b c`), so this script
    // can be called multiple times as a lambda function.
    Script(Script),

    // An argument used to define a command so it can be called with other arguments elsewhere.
    //          :command 'cmd_name' (the_cmd $0 123)
    //      =>  :cmd_name 5     -- calls `the_cmd` with arguments `5` `123`
    // The new `cmd_name` script looks like this:
    //          Script { Command::Lookup("the_cmd"), Argument::[Use(0), I64(123)] }
    // NOTE: if you ask for an argument beyond the length of the argument list,
    // it will return Argument::Null.
    Use(Use),
}

impl Argument {
    pub fn is_value(&self) -> bool {
        !matches!(self, Self::Script(_) | Self::Use(_))
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Null => false,
            Self::I64(value) => *value != 0,
            Self::Color(value) => *value != Rgba8::TRANSPARENT,
            Self::String(value) => *value != "",
            _ => panic!("unimplemented"),
        }
    }

    /// Returns an i64 if possible, coercing Null to 0.
    pub fn get_i64(&self, what_for: &str) -> I64Result {
        match self {
            Argument::Null => Ok(0),
            Argument::I64(value) => Ok(*value),
            result => Err(format!("invalid i64 {}: {}", what_for, result)),
        }
    }

    /// Returns an optional i64 if the argument is I64 or Null.
    pub fn get_optional_i64(&self, what_for: &str) -> OptionalI64Result {
        match self {
            Argument::Null => Ok(None),
            Argument::I64(value) => Ok(Some(*value)),
            result => Err(format!("invalid i64 {}: {}", what_for, result)),
        }
    }

    pub fn get_string(&self, what_for: &str) -> StringResult {
        // TODO: we probably can coerce integers, etc., to strings
        match self {
            Argument::Null => Ok("".to_string()),
            Argument::String(value) => Ok(value.clone()),
            result => Err(format!("invalid string {}: {}", what_for, result)),
        }
    }

    pub fn get_script(&self, what_for: &str) -> ScriptResult {
        // TODO: theoretically we can create everyone as a script if
        // we have an identity command (e.g., `identity $0` resolves to $0).
        match self {
            Argument::Script(value) => Ok(value.clone()),
            result => Err(format!("invalid script {}: {}", what_for, result)),
        }
    }
}

impl fmt::Display for Argument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Argument::Null => write!(f, "null"),
            Argument::I64(value) => write!(f, "{}", *value),
            Argument::Color(value) => write!(f, "{}", *value),
            Argument::String(value) => write!(f, "'{}'", *value),
            Argument::Script(script) => {
                let mut check = write!(f, "{{command: `{}`, arguments: [", script.command);
                if check.is_err() {
                    return check;
                }
                for arg in &script.arguments {
                    check = write!(f, "{}, ", arg);
                    if check.is_err() {
                        return check;
                    }
                }
                write!(f, "]}}")
            }
            Argument::Use(use_argument) => write!(f, "${}", use_argument.index),
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub struct Use {
    /// Index of argument on the script that we'll use to evaluate this argument.
    pub index: u32,
    /// Number of stacks to look back for the script to use.  Should be < 0.
    /// -1 implies looking back one script.
    // Note we're using a negative number to more easily distinguish from index.
    pub lookback: i32,
}

pub type OptionalI64Result = Result<Option<i64>, String>;
pub type I64Result = Result<i64, String>;
pub type StringResult = Result<String, String>;
pub type ScriptResult = Result<Script, String>;

fn serialize_argument(argument: &Argument, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match argument {
        Argument::Null => write!(f, "null"),
        Argument::I64(value) => write!(f, "{}", *value),
        Argument::Color(value) => write!(f, "{}", *value),
        Argument::String(value) => write!(f, "'{}'", *value),
        Argument::Script(script) if script.arguments.len() == 0 => {
            write!(f, "{}", script.command)
        }
        Argument::Script(script) => {
            write!(f, "({})", Serialize::Script(&script))
        }
        // TODO: consider adding lookback here (${}{}), since lookback is negative.
        //      i think we'll be ok because we implement the parser ourselves and
        //      we always decrement lookback when going into a new script.
        //      there shouldn't be any ambiguity if we copy paste the resulting script.
        Argument::Use(use_argument) => write!(f, "${}", use_argument.index),
    }
}

// TODO: we probably can just use fmt::Display for the nice serialization,
// and fmt::Debug for the debug display version.
pub enum Serialize<'a> {
    Argument(&'a Argument),
    Script(&'a Script),
    ArgumentResult(&'a ArgumentResult),
}

impl<'a> fmt::Display for Serialize<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Serialize::Argument(argument) => serialize_argument(argument, f),
            Serialize::Script(script) => serialize_script(script, f),
            Serialize::ArgumentResult(result) => match result {
                Err(string) => {
                    if string.is_empty() {
                        write!(f, "(error)")
                    } else {
                        write!(f, "(error `{}`)", string)
                    }
                }
                Ok(argument) => serialize_argument(argument, f),
            },
        }
    }
}

/// It is recommended to use the macro `script_runner!` to add the relevant
/// common code for evaluating scripts to your ScriptRunner.
/// You will be able to customize the required methods (e.g., `paint`).
pub trait ScriptRunner {
    /// Executes the script on the top of the stack, using lower parts of the stack
    /// to trace `Argument::Use` arguments.  Note that the lowest script in the stack
    /// should *not* have any `Argument::Use` arguments.  We purposely move/copy this
    /// stack because all sorts of redirection is possible, i.e., due to `Argument::Use`
    /// or additional nested scripts being run via `Argument::Script`.
    fn run(&mut self, script_stack: Vec<&Script>) -> ArgumentResult;
}

#[macro_export] // meta programming
macro_rules! script_runner {
    ( $name:ident ) => {
        impl $name {
            // TODO: could add other common methods.

            /// Display a message to the user. Also logs.
            pub fn message<D: fmt::Display>(&mut self, msg: D, t: MessageType) {
                // TODO: if an existing message is present, we should wait a bit
                // and then show the new message later, a la toasts.  we can log right away.
                self.message = Message::new(msg, t);
                self.message.log();
            }

            fn message_clear(&mut self) {
                self.message = Message::default();
            }
        }

        impl ScriptRunner for $name {
            fn run(&mut self, script_stack: Vec<&Script>) -> ArgumentResult {
                if script_stack.len() == 0 {
                    return Ok(Argument::Null);
                }
                let script: &Script = &script_stack[script_stack.len() - 1];
                let command = script.command.clone();
                self.begin_script_command(command.clone());
                let result = match command.clone() {
                    Command::Help => {
                        if script.arguments.len() != 1 {
                            self.message(self.variables.describe(Command::Help), MessageType::Info);
                        } else {
                            match &script.arguments[0] {
                                Argument::Script(nested_script) => {
                                    if nested_script.arguments.len() == 0 {
                                        self.message(
                                            self.variables.describe(nested_script.command.clone()),
                                            MessageType::Info,
                                        );
                                    } else {
                                        self.message(
                                            format!(
                                                "({}) -- a script",
                                                Serialize::Script(nested_script)
                                            ),
                                            MessageType::Info,
                                        );
                                    }
                                }
                                Argument::Use(use_arg) => self.message(
                                    format!(
                                        "-- a way to evaluate an argument at position {}; \
                                        e.g., `const 'ten' (sum $0 10)` will add 10 to argument 0, \
                                        and can be called via `ten 123` which will return `133`",
                                        use_arg.index
                                    ),
                                    MessageType::Info,
                                ),
                                a => self.message(
                                    format!("{} -- an argument", Serialize::Argument(a)),
                                    MessageType::Info,
                                ),
                            }
                        }
                        Ok(Argument::Null)
                    }
                    Command::Echo => {
                        let mut string = String::new();
                        for i in 0..script.arguments.len() {
                            let result = self.script_evaluate(&script_stack, Evaluate::Index(i as u32));
                            let next_string = format!("{}", Serialize::ArgumentResult(&result));
                            if i > 0 {
                                string.push_str(" ");
                            }
                            string.push_str(&next_string);
                        }
                        self.message(string.clone(), MessageType::Echo);
                        Ok(Argument::String(string))
                    }
                    Command::Error => {
                        let mut string = String::new();
                        for i in 0..script.arguments.len() {
                            let result = self.script_evaluate(&script_stack, Evaluate::Index(i as u32));
                            let next_string = format!("{}", Serialize::ArgumentResult(&result));
                            if i > 0 {
                                string.push_str(" ");
                            }
                            string.push_str(&next_string);
                        }
                        Err(string)
                    }
                    Command::PushPop => {
                        if script.arguments.len() == 0 {
                            return Err("push/pop (fg #12345) do_stuff_with_pushed_fg".to_string())
                        }
                        let reset_script = match &script.arguments[0] {
                            Argument::Script(argument_script) => {
                                // The first argument is a script that "pushes" some value to a variable,
                                // but we try to make commands "swappers" that will return the old value
                                // if you set a new value.  The returned value will be swapped back later:
                                let reset_value = self.script_evaluate(&script_stack, Evaluate::Index(0))?;

                                // The last command we'll run pops the variable, or resets it to what it returned here:
                                Script {
                                    command: argument_script.command.clone(),
                                    arguments: vec![reset_value],
                                }
                            }
                            _ => return Err("push/pop needs a script as its first argument".to_string())
                        };
                        for i in 1..script.arguments.len() {
                            if self.script_evaluate(&script_stack, Evaluate::Index(i as u32)).is_err() {
                                break;
                            }
                        }
                        // We shouldn't need much context on the reset script; it doesn't
                        // have any arguments like Argument::Use or Argument::Script internally.
                        reset_script.run(self)
                    }
                    Command::RunAll => {
                        let mut value = self.script_evaluate(&script_stack, Evaluate::Index(0))?;
                        for i in 1..script.arguments.len() {
                            value = self.script_evaluate(&script_stack, Evaluate::Index(i as u32))?;
                        }
                        Ok(value)
                    }
                    Command::Not => {
                        let value = self.script_evaluate(&script_stack, Evaluate::Index(0))?;
                        Ok(Argument::I64(if value.is_truthy() { 0 } else { 1 }))
                    }
                    Command::If => {
                        let conditional =
                            self.script_evaluate(&script_stack, Evaluate::Index(0))?;
                        if conditional.is_truthy() {
                            self.script_evaluate(&script_stack, Evaluate::Index(1))
                        } else {
                            self.script_evaluate(&script_stack, Evaluate::Index(2))
                        }
                    }
                    Command::Even => {
                        let value = self
                            .script_evaluate(&script_stack, Evaluate::Index(0))?
                            .get_i64("for even")?;
                        Ok(Argument::I64(if value % 2 == 0 { 1 } else { 0 }))
                    }
                    Command::Odd => {
                        let value = self
                            .script_evaluate(&script_stack, Evaluate::Index(0))?
                            .get_i64("for odd")?;
                        Ok(Argument::I64(if value % 2 == 1 { 1 } else { 0 }))
                    }
                    Command::Sum => {
                        let starting_value =
                            self.script_evaluate(&script_stack, Evaluate::Index(0))?;
                        let argument_count = script.arguments.len() as u32;
                        match starting_value {
                            Argument::I64(i64_value) => {
                                let mut sum = i64_value;
                                for index in 1..argument_count {
                                    sum += self
                                        .script_evaluate(&script_stack, Evaluate::Index(index))?
                                        .get_i64("for sum")?;
                                }
                                Ok(Argument::I64(sum))
                            }
                            // TODO: implement some of these sums (i.e., for other value-like args)
                            _ => Err("sum for things besides i64 not implemented yet".to_string()),
                        }
                    }
                    Command::Product => {
                        let mut product: i64 = 1;
                        for i in 0..script.arguments.len() {
                            match self.script_evaluate(
                                &script_stack,
                                Evaluate::Index(i as u32),
                            )?.get_optional_i64("for product")? {
                                Some(result) => {product *= result;},
                                None => {}, // skip nulls, they act as if they aren't arguments.
                            }
                        }
                        Ok(Argument::I64(product))
                    }
                    Command::Evaluate(name) => {
                        // Need to make a clone here since executing commands could
                        // modify `self`, which could break the reference here.
                        let argument: Argument = self.variables.get(name);

                        // TODO: revisit if this isn't useful for non-test ScriptRunners.
                        // Not using `self.script_evaluate` mostly for avoiding the extra WhatRan in tests:
                        evaluate(self, &script_stack, Evaluate::Argument(&argument))
                    }
                    // Note that variables can technically be `Script`s.
                    // E.g., if we do `set "var-name" (echo $0 ; echo $1)`, then we
                    // can call with `var-name 'hello' 'world'`.  This is the way
                    // we create new commands.
                    Command::SetVariable => self.variables.set_from_script(&script),
                    Command::ConstVariable => self.variables.set_from_script(&script),
                    Command::CreateAlias => self.variables.set_from_script(&script),
                    Command::Mode => {
                        let value = self
                            .script_evaluate(&script_stack, Evaluate::Index(0))?
                            .get_string("for mode")?;
                        Ok(Argument::String(self.get_or_swap_mode(value)?))
                    }
                    Command::Animate => {
                        let value = self
                            .script_evaluate(&script_stack, Evaluate::Index(0))?
                            .get_optional_i64("for animate")?;
                        Ok(Argument::I64(self.get_or_swap_animate(value)?))
                    }
                    Command::UiScale => {
                        let value = self
                            .script_evaluate(&script_stack, Evaluate::Index(0))?
                            .get_optional_i64("for scale%")?;
                        Ok(Argument::I64(self.get_or_swap_ui_scale(value)?))
                    }
                    Command::FrameResize => {
                        let optional_width = self
                            .script_evaluate(&script_stack, Evaluate::Index(0))?
                            .get_optional_i64("for frame width")?;
                        let optional_height = self
                            .script_evaluate(&script_stack, Evaluate::Index(1))?
                            .get_optional_i64("for frame height")?;
                        let old_width = self.get_or_swap_frame_width(None)?;
                        let old_height = self.get_or_swap_frame_height(None)?;
                        if optional_width.is_some() && optional_height.is_some() {
                            self.resize_frames(optional_width.unwrap(), optional_height.unwrap())?;
                        } else if optional_width.is_some() {
                            self.resize_frames(optional_width.unwrap(), old_height)?;
                        } else if optional_height.is_some() {
                            self.resize_frames(old_width, optional_height.unwrap())?;
                        } else {
                            // TODO: Crop to content
                            return Err("`f/resize` without arguments is not yet implemented".to_string());
                        }
                        Ok(Argument::I64(old_width * old_height))
                    }
                    Command::FrameWidth => {
                        let value = self
                            .script_evaluate(&script_stack, Evaluate::Index(0))?
                            .get_optional_i64("for frame width")?;
                        Ok(Argument::I64(self.get_or_swap_frame_width(value)?))
                    }
                    Command::FrameHeight => {
                        let value = self
                            .script_evaluate(&script_stack, Evaluate::Index(0))?
                            .get_optional_i64("for frame height")?;
                        Ok(Argument::I64(self.get_or_swap_frame_height(value)?))
                    }
                    Command::FrameIndex => {
                        let value = self
                            .script_evaluate(&script_stack, Evaluate::Index(0))?
                            .get_optional_i64("for frame")?;
                        Ok(Argument::I64(self.get_or_swap_frame_index(value)?))
                    }
                    Command::ForegroundColor => {
                        let color_arg = self.script_evaluate(&script_stack, Evaluate::Index(0))?;
                        get_or_swap_color(&mut self.fg, &self.palette, color_arg)
                    }
                    Command::BackgroundColor => {
                        let color_arg = self.script_evaluate(&script_stack, Evaluate::Index(0))?;
                        get_or_swap_color(&mut self.bg, &self.palette, color_arg)
                    }
                    Command::Paint => {
                        let x = self
                            .script_evaluate(&script_stack, Evaluate::Index(0))?
                            .get_i64("for coordinate")?;
                        let y = self
                            .script_evaluate(&script_stack, Evaluate::Index(1))?
                            .get_i64("for coordinate")?;

                        let mut color = self.fg; // default to foreground color
                        let color_arg = self.script_evaluate(&script_stack, Evaluate::Index(2))?;
                        get_or_swap_color(&mut color, &self.palette, color_arg)?;

                        self.script_paint(x, y, color)
                    }
                    Command::BrushSize => {
                        let optional = self
                            .script_evaluate(&script_stack, Evaluate::Index(0))?
                            .get_optional_i64("for brush size")?;
                        let old_size = self.brush.size as i64;
                        let new_size = match optional {
                            None => return Ok(Argument::I64(old_size)),
                            Some(value) => value,
                        };
                        self.brush.size = new_size.max(1) as usize;
                        Ok(Argument::I64(old_size))
                    }
                    Command::BrushMode(mode) => {
                        let argument = self
                            .script_evaluate(&script_stack, Evaluate::Index(0))?
                            .get_optional_i64("for brush mode")?;
                        // TODO: clean up when script::BrushMode folds into BrushMode
                        let mut maybe_result = None;
                        let brush_mode = match mode {
                            BrushMode::Erase => brush::BrushMode::Erase,
                            BrushMode::Multi => brush::BrushMode::Multi,
                            BrushMode::Perfect => brush::BrushMode::Perfect,
                            BrushMode::XSym => brush::BrushMode::XSym,
                            BrushMode::YSym => brush::BrushMode::YSym,
                            BrushMode::XRay => brush::BrushMode::XRay,
                            BrushMode::Line => {
                                maybe_result = Some(
                                    if let Some(brush::BrushMode::Line(snap)) = self.brush.line_mode() {
                                        snap.unwrap_or(0) as i64
                                    } else {
                                        0
                                    },
                                );
                                brush::BrushMode::Line(Some(argument.unwrap_or(0) as u32))
                            }
                        };
                        let result = maybe_result.unwrap_or(self.brush.is_set(brush_mode) as i64);
                        if let Some(value) = argument {
                            if value == 0 {
                                self.brush.unset(brush_mode);
                            } else {
                                self.brush.set(brush_mode);
                            }
                        }
                        Ok(Argument::I64(result))
                    }
                    Command::Quit(q) => {
                        self.script_quit(q);
                        Ok(Argument::Null)
                    }
                };
                // TODO: move the above `match` into its own function so
                // this method always gets called:
                self.end_script_command(command);
                result
            }
        }
    };
}

/// Essentially a getter/swapper for color, with a Null argument making
/// this behave as a getter and a non-Null argument as a swapper, i.e.,
/// we return the old value as we swap in the new value.
/// We will use the palette in case an integer argument is passed in.
pub fn get_or_swap_color(
    maybe_set_color: &mut Rgba8,
    palette: &Palette,
    color_result: Argument,
) -> ArgumentResult {
    match color_result {
        // Without an argument, we're expecting to get the current color.
        Argument::Null => Ok(Argument::Color(maybe_set_color.clone())),
        // With a (valid) argument, we're expecting to set the new color
        // and return the old color.
        Argument::I64(value) => {
            // Purposely underflow here for negative values;
            // that will just become a very large positive number,
            // and we'll check the palette size first.
            let palette_index = value as usize;
            if palette_index < palette.size() {
                let mut color = palette.colors[palette_index];
                mem::swap(&mut color, maybe_set_color);
                return Ok(Argument::Color(color));
            }
            Err(format!("no palette color with index {}", value))
        }
        Argument::Color(mut color) => {
            mem::swap(&mut color, maybe_set_color);
            Ok(Argument::Color(color))
        }
        arg => Err(format!("invalid argument to get_or_swap_color: {}", arg)),
    }
}

pub enum Evaluate<'a> {
    /// Pass an argument directly to evaluate.
    Argument(&'a Argument),
    /// Pass an index into the most recent script's argument list
    /// to evaluate that argument.
    Index(u32),
}

/// Executes a script argument, following trails like `Argument::Use`
/// down to previous arguments in the script stack, as well as nested
/// scripts via `Argument::Script`.
// In this function we don't copy the script stack, but will do it
// internally if we need to, e.g., due to evaluating any nested
// scripts via `Argument::Script`.
//
// Technical details on why we need a stack of scripts.  For example:
//    :command 'paint5' (paint $0 $1 5)
//    :command 'dot5' (paint5 $2 $0)
//
// `dot5` needs to map external argument 2 to paint's argument 0,
// and external argument 0 to paint's argument 1.
//    let paint5 = Script { command: Paint, arguments: [Use(0, -1), Use(1, -1), I64(5, -1)] };
//    let dot5 = Script { command: Evaluate("paint5"), arguments: [Use(2, -1), Use(0, -1)] };
//
// The "easy" fix is to just push to a script stack:
//    let root = Script { command: Root, arguments: [/*external-arguments*/] };
//    let script_stack = [root, dot5, paint5];
//
pub fn evaluate(
    runner: &mut dyn ScriptRunner,
    script_stack: &Vec<&Script>,
    mut evaluate_this: Evaluate,
) -> ArgumentResult {
    assert!(script_stack.len() > 0);
    if script_stack.len() > 1000 {
        return Err("script call stack overflow".to_string());
    }
    let mut script_index: usize = script_stack.len() - 1;
    let mut iteration = 0;
    loop {
        let argument: &Argument = match evaluate_this {
            Evaluate::Argument(evaluate_this_argument) => evaluate_this_argument,
            Evaluate::Index(evaluate_this_index) => {
                let script: &Script = &script_stack[script_index];
                if evaluate_this_index as usize >= script.arguments.len() {
                    return Ok(Argument::Null);
                }
                &script.arguments[evaluate_this_index as usize]
            }
        };
        if argument.is_value() {
            return Ok(argument.clone());
        }
        match argument {
            Argument::Script(nested_script) => {
                let mut new_stack = vec![];
                // TODO: add test for this (i.e., Use() + Script()`.
                // We need to only grab the stack from 0 to script_index.
                new_stack.extend_from_slice(&script_stack[0..script_index + 1]);
                new_stack.push(&nested_script);
                return runner.run(new_stack);
            }
            Argument::Use(use_argument) => {
                if use_argument.lookback >= 0 {
                    // This can happen if we parse a script that's self referential `do stuff $0 $1`
                    return Err(format!(
                        "${} should only be used inside a script",
                        use_argument.index
                    ));
                }
                let subtract: usize = -use_argument.lookback as usize;
                script_index -= subtract;
                // Check for underflow:
                if script_index > script_stack.len() {
                    // For debugging, reset script_index:
                    script_index += subtract;
                    return Err(format!(
                        "lookback went too far ({}) but we're on script {}",
                        use_argument.lookback, script_index
                    ));
                }
                evaluate_this = Evaluate::Index(use_argument.index);
            }
            // is_value() and this matcher should be disjoint.
            _ => panic!("Invalid argument, not evaluatable: {}", *argument),
        }
        iteration += 1;
        if iteration > 1000 {
            return Err("script took too many iterations".to_string());
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Variable {
    // A variable that can be reassigned:
    Mutable(Argument),
    // A variable that can't be reassigned:
    Const(Argument),
    // To look up a variable and remap it:
    Alias(String),
    // Built-in function, e.g., `if`, `fg`, `paint`, etc.:
    // The string is for the `help` function to explain what the command does.
    BuiltIn(String),
}

impl Variable {
    pub fn is_mutable(&self) -> bool {
        matches!(self, Self::Mutable(_))
    }
}

pub struct Variables {
    map: HashMap<String, Variable>,
}

impl Variables {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    fn add_built_in(&mut self, command: Command, description: &str) {
        assert_ok!(self.set(
            format!("{}", command),
            Variable::BuiltIn(description.to_string()),
        ));
    }

    pub fn describe(&self, command: Command) -> String {
        let name = format!("{}", command);
        match &self.map.get(&name) {
            None => format!("{} is unknown", name),
            Some(Variable::BuiltIn(description)) => format!("-- {}", description),
            Some(Variable::Mutable(a)) => format!("{} -- mutable", Serialize::Argument(a)),
            Some(Variable::Const(a)) => format!("{} -- const", Serialize::Argument(a)),
            Some(Variable::Alias(a)) => format!("-- alias of {}", a),
        }
    }

    pub fn with_built_ins() -> Self {
        let mut variables = Variables::new();

        variables.add_built_in(
            Command::Help,
            "explains what $0 does without evaluating it, \
            e.g., `? paint` to explain what the `paint` command does",
        );
        // TODO: consider serializing strings without '', at least for echo/error.
        // we need them when serializing a script.
        variables.add_built_in(
            Command::Echo,
            "evaluates all arguments, joining them into a string to print, \
            e.g., `echo 'oh no' fg` will show a message of `'oh no' {foreground-color}`",
        );
        variables.add_built_in(
            Command::Error,
            "evaluates all arguments, joining them into an error string, \
            e.g., `error 'oh no' fg` will return an error of `'oh no' {foreground-color}`",
        );
        variables.add_built_in(
            Command::PushPop,
            "first argument should be a script that swaps in a value to a variable, \
            all other arguments will be evaluated,
            and afterwards the variable will be reset; \
            e.g., `push/pop (fg #123456) (paint 3 4)` to set `fg` temporarily",
        );
        variables.add_built_in(
            Command::RunAll,
            "evaluates all arguments until any error, \
            returning the last evaluated argument; \
            e.g., `run/all 'take me' (fg #123456)` returns the previous foreground color",
        );
        variables.add_built_in(
            Command::Not,
            "if $0 evaluates to truthy, returns 0, otherwise 1, \
            e.g., `not 3` returns 0 and `not 0` returns 1",
        );
        variables.add_built_in(
            Command::If,
            "if $0 evaluates to truthy, evaluates $1, otherwise $2, \
            e.g., `if 'hi' 'world' 3` returns 'world'",
        );
        variables.add_built_in(
            Command::Even,
            "if $0 evaluates to even, returns 1, otherwise 0, \
            e.g., `even 23` returns 0",
        );
        variables.add_built_in(
            Command::Odd,
            "if $0 evaluates to odd, returns 1, otherwise 0, \
            e.g., `odd 23` returns 1",
        );
        variables.add_built_in(
            Command::Sum,
            "adds all evaluated arguments together into the type of $0, \
            e.g., `+ 1 2 3 4` to return 10",
        );
        variables.add_built_in(
            Command::Product,
            "multiplies all evaluated arguments together, \
            e.g., `* -1 2 3 4` to return -24",
        );
        variables.add_built_in(
            Command::SetVariable,
            "creates/updates a mutable variable with name $0 to the value of $1, unevaluated, \
            e.g., `set 'paint5' (paint $0 $1 5)` to create a lambda",
        );
        variables.add_built_in(
            Command::ConstVariable,
            "creates an immutable variable with name $0 and value $1, unevaluated, \
            e.g., `const 'greet' (echo 'hello, world')` to create a lambda",
        );
        variables.add_built_in(
            Command::CreateAlias,
            "creates an alias with name $0 that evaluates name $1 when called, \
            e.g., `alias 'fgc' 'fg'` to add `fgc` as an alias for `fg`",
        );
        variables.add_built_in(
            Command::Mode,
            "getter/swapper for the current mode if $0 is null/present, \
            e.g., `mode 'normal'` to go to normal mode",
        );
        variables.add_built_in(
            Command::Animate,
            "getter/swapper for toggling animation if $0 is null/present, \
            e.g., `a on` to turn on animation",
        );
        variables.add_built_in(
            Command::UiScale,
            "getter/swapper for current UI scale percentage if $0 is null/present, \
            e.g., `ui/scale% 150` to set UI to 1.5x",
        );
        variables.add_built_in(
            Command::FrameResize,
            "sets frame size, or crops to content if no arguments, \
            e.g., `f/resize 12 34` to set to 12 pixels wide and 34 pixels high",
        );
        variables.add_built_in(
            Command::FrameWidth,
            "getter/swapper for the frame width if $0 is null/present, \
            e.g., `f/width 123` to set to 123 pixels wide",
        );
        variables.add_built_in(
            Command::FrameHeight,
            "getter/swapper for the frame height if $0 is null/present, \
            e.g., `f/height 456` to set to 456 pixels high",
        );
        variables.add_built_in(
            Command::FrameIndex,
            "getter/swapper for the frame index if $0 is null/present, \
            e.g., `f 3` to switch to the fourth frame (0-indexed), \
            or `f -1` to switch to the last frame (wrap around)",
        );
        variables.add_built_in(
            Command::ForegroundColor,
            "getter/swapper for foreground color if $0 is null/present, \
            e.g., `fg 3` to set foreground color to palette 3",
        );
        variables.add_built_in(
            Command::BackgroundColor,
            "getter/swapper for background color if $0 is null/present, \
            e.g., `bg #123456` to set background color to #123456",
        );
        variables.add_built_in(
            Command::Paint,
            "paints coordinates ($0, $1) with color $2, defaulting to foreground, \
            e.g., `p 3 4 #765432`",
        );
        variables.add_built_in(
            Command::BrushSize,
            "getter/swapper for brush size if $0 is null/present, \
            e.g., `b/size 15` to set to 15",
        );
        variables.add_built_in(
            Command::BrushMode(BrushMode::Erase),
            "getter/swapper for erase brush option if $0 is null/present, \
            e.g., `b/erase on` to turn on",
        );
        variables.add_built_in(
            Command::BrushMode(BrushMode::Multi),
            "getter/swapper for multi-frame brush option if $0 is null/present, \
            e.g., `b/multi off` to turn off",
        );
        variables.add_built_in(
            Command::BrushMode(BrushMode::Perfect),
            "getter/swapper for pixel-perfect brush option if $0 is null/present, \
            e.g., `b/perfect 0` to turn off",
        );
        variables.add_built_in(
            Command::BrushMode(BrushMode::XSym),
            "getter/swapper for draw with x-symmetry brush option if $0 is null/present, \
            e.g., `b/xsym false` to turn off",
        );
        variables.add_built_in(
            Command::BrushMode(BrushMode::YSym),
            "getter/swapper for draw with y-symmetry brush option if $0 is null/present, \
            e.g., `b/ysym true` to turn on",
        );
        variables.add_built_in(
            Command::BrushMode(BrushMode::XRay),
            "getter/swapper for x-ray (see underlying pixel) brush option if $0 is null/present, \
            e.g., `b/xray 1` to turn on",
        );
        variables.add_built_in(
            Command::BrushMode(BrushMode::Line),
            "getter/swapper for line angle brush option if $0 is null/present, \
            e.g., `b/line 30` to set to 30 degrees, `b/line 0` to turn off",
        );
        variables.add_built_in(
            Command::Quit(Quit::Safe),
            "quits the current view if it has been saved",
        );
        variables.add_built_in(
            Command::Quit(Quit::AllSafe),
            "quits all views if they have been saved",
        );
        variables.add_built_in(
            Command::Quit(Quit::Forced),
            "quits current view even if it hasn't been saved",
        );
        variables.add_built_in(
            Command::Quit(Quit::AllForced),
            "quits all views even if they haven't been saved",
        );
        assert_ok!(variables.set("run".to_string(), Variable::BuiltIn("TODO".to_string())));

        // Helpful constants.
        assert_ok!(variables.set("null".to_string(), Variable::Const(Argument::Null)));
        assert_ok!(variables.set("on".to_string(), Variable::Const(Argument::I64(1))));
        assert_ok!(variables.set("off".to_string(), Variable::Const(Argument::I64(0))));
        assert_ok!(variables.set("true".to_string(), Variable::Const(Argument::I64(1))));
        assert_ok!(variables.set("false".to_string(), Variable::Const(Argument::I64(0))));
        // TODO: add `normal` as a Const variable to the string "normal",
        // same for other modes that you can set.

        // Helpful scripts.
        // TODO: make this a Script parser so we can use it for other things.
        // e.g., `a/delay++` should do this same logic for `a/delay`:
        assert_ok!(variables.set(
            "f++".to_string(),
            Variable::Const(Argument::Script(Script {
                command: Command::FrameIndex,
                arguments: vec![Argument::Script(Script {
                    command: Command::Sum,
                    arguments: vec![
                        Argument::Script(Script::zero_arg(Command::FrameIndex)),
                        Argument::I64(1),
                    ],
                })],
            }))
        ));
        assert_ok!(variables.set(
            "f--".to_string(),
            Variable::Const(Argument::Script(Script {
                command: Command::FrameIndex,
                arguments: vec![Argument::Script(Script {
                    command: Command::Sum,
                    arguments: vec![
                        Argument::Script(Script::zero_arg(Command::FrameIndex)),
                        Argument::I64(-1),
                    ],
                })],
            }))
        ));
        assert_ok!(variables.set(
            "swap".to_string(),
            Variable::Const(Argument::Script(Script {
                command: Command::ForegroundColor,
                arguments: vec![Argument::Script(Script {
                    command: Command::BackgroundColor,
                    arguments: vec![Argument::Script(Script::zero_arg(Command::ForegroundColor))],
                })],
            })),
        ));
        // TODO: add "red", "blue", etc. as Mutable color variables
        // e.g., add `red 1`, `red 2`, etc., using OkLab colors

        assert_ok!(variables.set("multiply".to_string(), Variable::Alias("*".to_string())));
        assert_ok!(variables.set("product".to_string(), Variable::Alias("*".to_string())));
        assert_ok!(variables.set("sum".to_string(), Variable::Alias("+".to_string())));
        assert_ok!(variables.set("paint".to_string(), Variable::Alias("p".to_string())));
        assert_ok!(variables.set("f/index".to_string(), Variable::Alias("f".to_string())));
        assert_ok!(variables.set("quit".to_string(), Variable::Alias("q".to_string())));
        assert_ok!(variables.set("quit!".to_string(), Variable::Alias("q!".to_string())));
        for c in [
            "size", "erase", "multi", "perfect", "xsym", "ysym", "xray", "line",
        ] {
            assert_ok!(variables.set(format!("brush/{}", c), Variable::Alias(format!("b/{}", c))));
        }
        variables
    }

    /// Returns the Argument represented by this name, returning Null if not present in the map.
    // Notice that we resolve aliases here so that you'll definitely get an argument.
    pub fn get(&self, mut name: String) -> Argument {
        for _iteration in 1..24 {
            match &self.map.get(&name) {
                None => return Argument::Null,
                Some(variable) => {
                    match variable {
                        Variable::Const(arg) => return arg.clone(),
                        Variable::Mutable(arg) => return arg.clone(),
                        Variable::Alias(alias) => {
                            name = alias.clone();
                        }
                        Variable::BuiltIn(_) => {
                            // TODO: figure out how we want to do this; if we eventually
                            // support calling functions like this:
                            // `prepare 'if'; evaluate $0 $1 $2` to run `if` dynamically.
                            panic!("not sure how you got here!  built-ins should be handled elsewhere.");
                        }
                    }
                }
            }
        }
        eprint!("don't nest aliases this much!\n");
        return Argument::Null;
    }

    /// Sets the variable, returning the old value if it was present in the map.
    /// NOTE! Will return an error if the variable was present and const.
    // TODO: rename `swap_in`
    pub fn set(&mut self, name: String, variable: Variable) -> ArgumentResult {
        match &variable {
            Variable::Alias(alias) => {
                if name == *alias {
                    return Err(format!(
                        "alias can't be self referential: {} -> {}",
                        name, *alias
                    ));
                }
            }
            _ => {}
        }
        match self.map.get(&name) {
            None => {}
            Some(var) => match var {
                Variable::Mutable(_) => {
                    if !variable.is_mutable() {
                        eprint!(
                            "overwriting mutable variable `{}` with a const or alias\n",
                            name
                        );
                    }
                }
                Variable::Alias(_) => return Err(format!("alias `{}` is not reassignable", name)),
                Variable::Const(_) => {
                    return Err(format!("variable `{}` is not reassignable", name))
                }
                Variable::BuiltIn(_) => {
                    return Err(format!("built-in `{}` is not reassignable", name))
                }
            },
        }
        // TODO: optimization for aliases of aliases, we could drill down since aliases
        // are constant.  e.g., X -> Y -> Z should collapse to X -> Z and Y -> Z.
        // if we create X -> Y first, then Y -> Z, the next time we mutate self
        // and notice that X -> Y which is itself an alias, we can update to X -> Z.
        self.map
            .insert(name, variable)
            .map_or(Ok(Argument::Null), |v| match v {
                Variable::Mutable(var) => Ok(var),
                _ => panic!("i thought we checked for this already"),
            })
    }

    pub fn set_from_script(&mut self, script: &Script) -> ArgumentResult {
        if script.arguments.len() == 0 {
            return Err("setting a variable requires at least one argument".to_string());
        }

        let name = match &script.arguments[0] {
            Argument::String(argument_name) => argument_name.clone(),
            _ => return Err("variable name must be a known string".to_string()),
        };

        let value_argument = if script.arguments.len() >= 2 {
            script.arguments[1].clone()
        } else {
            Argument::Null
        };
        let value = match script.command {
            Command::ConstVariable => Variable::Const(value_argument),
            Command::SetVariable => Variable::Mutable(value_argument),
            Command::CreateAlias => {
                match value_argument {
                    Argument::String(alias) => Variable::Alias(alias),
                    // TODO: we can probably relax this at some point,
                    // but i don't see a huge use case for it.
                    _ => return Err("alias must be to a known string".to_string()),
                }
            }
            _ => return Err("script is invalid for setting a variable".to_string()),
        };

        self.set(name, value)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::brush::{self, Brush};
    use crate::message::*;

    #[test]
    fn test_argument_values() {
        // value-type arguments:
        assert!(Argument::Null.is_value());
        assert!(Argument::I64(123).is_value());
        assert!(Argument::String("asdf".to_string()).is_value());
        assert!(Argument::Color(Rgba8::RED).is_value());

        // non-value arguments; these need further evaluation to be used:
        assert_eq!(
            Argument::Script(Script {
                command: Command::Quit(Quit::Forced),
                arguments: vec![]
            })
            .is_value(),
            false
        );
        assert_eq!(
            Argument::Use(Use {
                index: 3,
                lookback: -1
            })
            .is_value(),
            false
        );
    }

    #[derive(PartialEq, Debug, Clone)]
    enum WhatRan {
        Begin(Command),
        End(Command),
        Evaluated(ArgumentResult),
        Mocked(String),
    }

    #[derive(PartialEq, Debug, Clone, Copy)]
    struct Painted {
        x: i64,
        y: i64,
        color: Rgba8,
    }

    struct TestRunner {
        test_what_ran: Vec<WhatRan>,
        variables: Variables,
        brush: Brush,
        fg: Rgba8,
        bg: Rgba8,
        palette: Palette,
        message: Message,
    }

    script_runner! {TestRunner}
    impl TestRunner {
        const PAINT_RETURN_COLOR: Rgba8 = Rgba8 {
            r: 254,
            g: 253,
            b: 252,
            a: 251,
        };
        fn new() -> Self {
            let mut palette = Palette::new(12.0, 50);
            palette.add(Rgba8::BLACK);
            palette.add(Rgba8 {
                r: 11,
                g: 15,
                b: 19,
                a: 0xff,
            });
            palette.add(Rgba8 {
                r: 22,
                g: 25,
                b: 28,
                a: 0xff,
            });
            palette.add(Rgba8 {
                r: 33,
                g: 35,
                b: 37,
                a: 0xff,
            });
            palette.add(Rgba8 {
                r: 44,
                g: 45,
                b: 46,
                a: 0xff,
            });
            palette.add(Rgba8 {
                r: 55,
                g: 55,
                b: 55,
                a: 0xff,
            });
            palette.add(Rgba8 {
                r: 66,
                g: 65,
                b: 64,
                a: 0xff,
            });
            palette.add(Rgba8 {
                r: 77,
                g: 75,
                b: 73,
                a: 0xff,
            });
            palette.add(Rgba8 {
                r: 88,
                g: 85,
                b: 82,
                a: 0xff,
            });
            palette.add(Rgba8 {
                r: 99,
                g: 95,
                b: 91,
                a: 0xff,
            });
            palette.add(Rgba8::WHITE);
            Self {
                palette,
                brush: Brush::default(),
                test_what_ran: Vec::new(),
                variables: Variables::with_built_ins(),
                fg: Rgba8::WHITE,
                bg: Rgba8::BLACK,
                message: Message::default(),
            }
        }

        fn begin_script_command(&mut self, command: Command) {
            self.test_what_ran.push(WhatRan::Begin(command));
        }

        fn end_script_command(&mut self, command: Command) {
            self.test_what_ran.push(WhatRan::End(command));
        }

        // We wrap evaluate for tests to get `WhatRan`.
        fn script_evaluate(
            &mut self,
            script_stack: &Vec<&Script>,
            evaluate_this: Evaluate,
        ) -> ArgumentResult {
            let result = evaluate(self, script_stack, evaluate_this);
            self.test_what_ran.push(WhatRan::Evaluated(result.clone()));
            result
        }

        fn script_paint(&mut self, x: i64, y: i64, color: Rgba8) -> ArgumentResult {
            self.test_what_ran
                .push(WhatRan::Mocked(format!("p {} {} {}", x, y, color)));
            // In the implementation, return the color that was under the cursor.
            Ok(Argument::Color(Self::PAINT_RETURN_COLOR))
        }

        fn get_or_swap_mode(&mut self, mode: String) -> StringResult {
            self.test_what_ran
                .push(WhatRan::Mocked(format!("mode {}", mode)));
            Ok(mode)
        }

        fn get_or_swap_animate(&mut self, value: Option<i64>) -> I64Result {
            self.test_what_ran
                .push(WhatRan::Mocked(format!("a {:?}", value)));
            Ok(1)
        }

        fn get_or_swap_ui_scale(&mut self, value: Option<i64>) -> I64Result {
            self.test_what_ran
                .push(WhatRan::Mocked(format!("ui/scale% {:?}", value)));
            Ok(100)
        }

        fn resize_frames(&mut self, width: i64, height: i64) -> Result<(), String> {
            self.test_what_ran
                .push(WhatRan::Mocked(format!("f/resize {} {}", width, height)));
            Ok(())
        }

        fn get_or_swap_frame_width(&mut self, value: Option<i64>) -> I64Result {
            self.test_what_ran
                .push(WhatRan::Mocked(format!("f/width {:?}", value)));
            Ok(987)
        }

        fn get_or_swap_frame_height(&mut self, value: Option<i64>) -> I64Result {
            self.test_what_ran
                .push(WhatRan::Mocked(format!("f/height {:?}", value)));
            Ok(321)
        }

        fn get_or_swap_frame_index(&mut self, value: Option<i64>) -> I64Result {
            self.test_what_ran
                .push(WhatRan::Mocked(format!("f {:?}", value)));
            Ok(404)
        }

        fn script_quit(&mut self, quit: Quit) {
            self.test_what_ran
                .push(WhatRan::Mocked(format!("quit{{{:?}}}", quit)));
        }
    }

    #[test]
    fn test_script_run_ok() {
        let script = Script {
            command: Command::Quit(Quit::Safe),
            arguments: Vec::new(),
        };

        let mut test_runner = TestRunner::new();
        let result = script.run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::Quit(Quit::Safe)),
                WhatRan::Mocked("quit{Safe}".to_string()),
                WhatRan::End(Command::Quit(Quit::Safe)),
            ])
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_script_run_taking_too_many_arguments() {
        let script = Script {
            command: Command::ForegroundColor,
            arguments: Vec::new(),
        };

        let mut test_runner = TestRunner::new();
        let result = script.run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::ForegroundColor),
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::End(Command::ForegroundColor),
            ])
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_script_not_can_execute_truthy_correctly() {
        let mut test_runner = TestRunner::new();
        let result = Script {
            command: Command::Not,
            arguments: Vec::from([Argument::I64(123)]),
        }
        .run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::Not),
                WhatRan::Evaluated(Ok(Argument::I64(123))),
                WhatRan::End(Command::Not),
            ])
        );
        assert_eq!(result, Ok(Argument::I64(0)));
    }

    #[test]
    fn test_script_not_can_execute_falsey_correctly() {
        let mut test_runner = TestRunner::new();
        let result = Script {
            command: Command::Not,
            arguments: Vec::from([Argument::String("".to_string())]),
        }
        .run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::Not),
                WhatRan::Evaluated(Ok(Argument::String("".to_string()))),
                WhatRan::End(Command::Not),
            ])
        );
        assert_eq!(result, Ok(Argument::I64(1)));
    }

    #[test]
    fn test_script_if_can_execute_true() {
        let script = Script {
            command: Command::If,
            arguments: Vec::from([
                Argument::I64(1),
                Argument::Script(Script {
                    command: Command::ForegroundColor,
                    arguments: Vec::from([Argument::Color(Rgba8::RED)]),
                }),
            ]),
        };

        let mut test_runner = TestRunner::new();
        let result = script.run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::If),
                WhatRan::Evaluated(Ok(Argument::I64(1))),
                WhatRan::Begin(Command::ForegroundColor),
                WhatRan::Evaluated(Ok(Argument::Color(Rgba8::RED))),
                WhatRan::End(Command::ForegroundColor),
                // This second one comes as a result of returning this from the foreground;
                // this is what it was previously:
                WhatRan::Evaluated(Ok(Argument::Color(Rgba8::WHITE))),
                WhatRan::End(Command::If),
            ])
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_script_if_can_execute_false_and_returns_null_without_enough_arguments() {
        let script = Script {
            command: Command::If,
            arguments: Vec::from([
                Argument::I64(0),
                // NOTE! not enough arguments
            ]),
        };

        let mut test_runner = TestRunner::new();
        let result = script.run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::If),
                WhatRan::Evaluated(Ok(Argument::I64(0))),
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::End(Command::If),
            ])
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_script_if_can_execute_false_and_succeed_without_else() {
        let script = Script {
            command: Command::If,
            arguments: Vec::from([
                Argument::I64(0),
                Argument::Color(Rgba8::BLACK), // the truthy block.
                                               // the else block is optional
            ]),
        };

        let mut test_runner = TestRunner::new();
        let result = script.run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::If),
                WhatRan::Evaluated(Ok(Argument::I64(0))),
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::End(Command::If),
            ])
        );
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some(Argument::Null));
    }

    #[test]
    fn test_script_if_can_execute_false_and_succeed_with_else() {
        let script = Script {
            command: Command::If,
            arguments: Vec::from([
                Argument::I64(0),
                Argument::Color(Rgba8::BLACK), // the truthy block.
                Argument::Color(Rgba8::BLUE),  // the falsey block.
            ]),
        };

        let mut test_runner = TestRunner::new();
        let result = script.run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::If),
                WhatRan::Evaluated(Ok(Argument::I64(0))),
                WhatRan::Evaluated(Ok(Argument::Color(Rgba8::BLUE))),
                WhatRan::End(Command::If),
            ])
        );
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some(Argument::Color(Rgba8::BLUE)));
    }

    #[test]
    fn test_script_push_pop_works() {
        let new_color = Rgba8::RED;
        let script = Script {
            command: Command::PushPop,
            arguments: Vec::from([
                Argument::Script(Script {
                    command: Command::ForegroundColor,
                    arguments: Vec::from([Argument::Color(new_color)]),
                }),
                Argument::Script(Script::zero_arg(Command::ForegroundColor)),
            ]),
        };
        let mut test_runner = TestRunner::new();
        let initial_color = Rgba8 {
            r: 123,
            g: 45,
            b: 6,
            a: 0xff,
        };
        test_runner.fg = initial_color;

        let result = script.run(&mut test_runner);

        // Very important: FG is reset to initial color:
        assert_eq!(test_runner.fg, initial_color);
        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::PushPop),
                // Evaluating the "push" part of PushPop; pushing a new color:
                WhatRan::Begin(Command::ForegroundColor),
                WhatRan::Evaluated(Ok(Argument::Color(new_color))),
                WhatRan::End(Command::ForegroundColor),
                WhatRan::Evaluated(Ok(Argument::Color(initial_color))),
                // Now running the later arguments:
                WhatRan::Begin(Command::ForegroundColor),
                WhatRan::Evaluated(Ok(Argument::Null)), // no argument to fg, getter.
                WhatRan::End(Command::ForegroundColor),
                // Foreground color inside the commands is evaluating to this:
                WhatRan::Evaluated(Ok(Argument::Color(new_color))),
                // Now we are evaluating the "pop" part of PushPop; resetting color:
                WhatRan::Begin(Command::ForegroundColor),
                WhatRan::Evaluated(Ok(Argument::Color(initial_color))),
                WhatRan::End(Command::ForegroundColor),
                WhatRan::End(Command::PushPop),
            ])
        );
        // As a consequence of the swapper logic, PushPop will return what the value
        // was *inside* the script.
        assert_eq!(result, Ok(Argument::Color(new_color)));
    }

    #[test]
    fn test_script_push_pop_successfully_resets_even_if_error() {
        let new_color = Rgba8::BLACK;
        let script = Script {
            command: Command::PushPop,
            arguments: Vec::from([
                Argument::Script(Script {
                    command: Command::BackgroundColor,
                    arguments: Vec::from([Argument::Color(new_color)]),
                }),
                Argument::Script(Script::zero_arg(Command::BackgroundColor)),
                // This will be an error:
                Argument::Use(Use {
                    lookback: 0,
                    index: 1234,
                }),
                // Ensure we don't evaluate this:
                Argument::I64(4042),
            ]),
        };
        let mut test_runner = TestRunner::new();
        let initial_color = Rgba8 {
            r: 12,
            g: 34,
            b: 56,
            a: 0xff,
        };
        test_runner.bg = initial_color;

        let result = script.run(&mut test_runner);

        // Very important: BG is reset to initial color:
        assert_eq!(test_runner.bg, initial_color);
        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::PushPop),
                // Evaluating the "push" part of PushPop; pushing a new color:
                WhatRan::Begin(Command::BackgroundColor),
                WhatRan::Evaluated(Ok(Argument::Color(new_color))),
                WhatRan::End(Command::BackgroundColor),
                WhatRan::Evaluated(Ok(Argument::Color(initial_color))),
                // Now running the later arguments:
                WhatRan::Begin(Command::BackgroundColor),
                WhatRan::Evaluated(Ok(Argument::Null)), // no argument to bg, getter.
                WhatRan::End(Command::BackgroundColor),
                // Background color inside the commands is evaluating to this:
                WhatRan::Evaluated(Ok(Argument::Color(new_color))),
                // Trying something that will error:
                WhatRan::Evaluated(Err("$1234 should only be used inside a script".to_string())),
                // NOTE: I64(4042) should NOT be evaluated!
                // Now we are evaluating the "pop" part of PushPop; resetting color:
                WhatRan::Begin(Command::BackgroundColor),
                WhatRan::Evaluated(Ok(Argument::Color(initial_color))),
                WhatRan::End(Command::BackgroundColor),
                WhatRan::End(Command::PushPop),
            ])
        );
        // As a consequence of the swapper logic, PushPop will return what the value
        // was *inside* the script.
        assert_eq!(result, Ok(Argument::Color(new_color)));
    }

    #[test]
    fn test_script_push_pop_fails_if_first_argument_is_not_a_script() {
        let script = Script {
            command: Command::PushPop,
            arguments: Vec::from([
                Argument::I64(0),
                Argument::Script(Script::zero_arg(Command::BackgroundColor)),
            ]),
        };
        let mut test_runner = TestRunner::new();

        let result = script.run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([WhatRan::Begin(Command::PushPop),])
        );
        assert_eq!(
            result,
            Err("push/pop needs a script as its first argument".to_string())
        );
    }

    #[test]
    fn test_script_run_all_exits_early_on_failure() {
        let script = Script {
            command: Command::RunAll,
            arguments: Vec::from([
                Argument::Script(Script::zero_arg(Command::ForegroundColor)),
                // Not much point to a non-script argument in RunAll unless you refer to
                // it later, e.g., via $1 or similar.
                Argument::I64(32),
                Argument::Script(Script {
                    command: Command::Paint,
                    arguments: vec![
                        Argument::I64(101),
                        Argument::I64(202),
                        Argument::Color(Rgba8::BLUE),
                    ],
                }),
                Argument::Script(Script::zero_arg(Command::Error)),
                // Should not evaluate this:
                Argument::Script(Script::zero_arg(Command::BackgroundColor)),
            ]),
        };
        let mut test_runner = TestRunner::new();

        let result = script.run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::RunAll),
                WhatRan::Begin(Command::ForegroundColor),
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::End(Command::ForegroundColor),
                WhatRan::Evaluated(Ok(Argument::Color(test_runner.fg))),
                WhatRan::Evaluated(Ok(Argument::I64(32))),
                WhatRan::Begin(Command::Paint),
                WhatRan::Evaluated(Ok(Argument::I64(101))),
                WhatRan::Evaluated(Ok(Argument::I64(202))),
                WhatRan::Evaluated(Ok(Argument::Color(Rgba8::BLUE))),
                WhatRan::Mocked("p 101 202 #0000ff".to_string()),
                WhatRan::End(Command::Paint),
                WhatRan::Evaluated(Ok(Argument::Color(TestRunner::PAINT_RETURN_COLOR))),
                WhatRan::Begin(Command::Error),
                WhatRan::End(Command::Error),
                WhatRan::Evaluated(Err("".to_string())),
            ])
        );
        assert_eq!(result, Err("".to_string()));
    }

    #[test]
    fn test_script_run_all_returns_ok_result() {
        let script = Script {
            command: Command::RunAll,
            arguments: Vec::from([
                Argument::Script(Script::zero_arg(Command::ForegroundColor)),
                Argument::Script(Script::zero_arg(Command::BackgroundColor)),
                Argument::I64(33),
            ]),
        };
        let mut test_runner = TestRunner::new();

        let result = script.run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::RunAll),
                WhatRan::Begin(Command::ForegroundColor),
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::End(Command::ForegroundColor),
                WhatRan::Evaluated(Ok(Argument::Color(test_runner.fg))),
                WhatRan::Begin(Command::BackgroundColor),
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::End(Command::BackgroundColor),
                WhatRan::Evaluated(Ok(Argument::Color(test_runner.bg))),
                WhatRan::Evaluated(Ok(Argument::I64(33))),
                WhatRan::End(Command::RunAll),
            ])
        );
        assert_eq!(result, Ok(Argument::I64(33)));
    }

    #[test]
    fn test_script_echo_stringifies_the_evaluation_of_all_arguments() {
        let script = Script {
            command: Command::Echo,
            arguments: vec![
                Argument::I64(123),
                Argument::String("jkl;".to_string()),
                Argument::Script(Script::zero_arg(Command::Error)),
                Argument::Script(Script {
                    command: Command::RunAll,
                    arguments: vec![Argument::Null, Argument::String("xyz".to_string()), Argument::I64(456)],
                }),
                Argument::Script(Script {
                    command: Command::Error,
                    arguments: vec![
                        Argument::Use(Use {
                            lookback: -1,
                            index: 0,
                        }),
                        Argument::Null,
                        Argument::Color(Rgba8::RED),
                    ],
                }),
                Argument::Script(Script::zero_arg(Command::ForegroundColor)),
            ],
        };
        let mut test_runner = TestRunner::new();

        let result = script.run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::Echo),
                WhatRan::Evaluated(Ok(Argument::I64(123))),
                WhatRan::Evaluated(Ok(Argument::String("jkl;".to_string()))),
                WhatRan::Begin(Command::Error),
                WhatRan::End(Command::Error),
                WhatRan::Evaluated(Err("".to_string())), // result of Err
                WhatRan::Begin(Command::RunAll),
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::Evaluated(Ok(Argument::String("xyz".to_string()))),
                WhatRan::Evaluated(Ok(Argument::I64(456))),
                WhatRan::End(Command::RunAll),
                WhatRan::Evaluated(Ok(Argument::I64(456))), // result of RunAll
                WhatRan::Begin(Command::Error),
                WhatRan::Evaluated(Ok(Argument::I64(123))), // Use $0
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::Evaluated(Ok(Argument::Color(Rgba8::RED))),
                WhatRan::End(Command::Error),
                WhatRan::Evaluated(Err("123 null #ff0000".to_string())),
                WhatRan::Begin(Command::ForegroundColor),
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::End(Command::ForegroundColor),
                WhatRan::Evaluated(Ok(Argument::Color(test_runner.fg))),
                WhatRan::End(Command::Echo),
            ])
        );
        let message = "123 'jkl;' (error) 456 (error `123 null #ff0000`) #ffffff".to_string();
        assert_eq!(
            result,
            Ok(Argument::String(message.clone()))
        );
        assert_eq!(test_runner.message, 
            Message {
                string: message,
                message_type: MessageType::Echo,
            }
        );
    }

    #[test]
    fn test_script_error_stringifies_the_evaluation_of_all_arguments() {
        let script = Script {
            command: Command::Error,
            arguments: vec![
                Argument::I64(123),
                Argument::String("asdf".to_string()),
                Argument::Script(Script::zero_arg(Command::Error)),
                Argument::Script(Script {
                    command: Command::RunAll,
                    arguments: vec![Argument::I64(456), Argument::Null],
                }),
                Argument::Script(Script {
                    command: Command::Error,
                    arguments: vec![
                        Argument::Null,
                        Argument::Use(Use {
                            lookback: -1,
                            index: 0,
                        }),
                        Argument::Color(Rgba8::GREEN),
                    ],
                }),
                Argument::Script(Script::zero_arg(Command::BackgroundColor)),
            ],
        };
        let mut test_runner = TestRunner::new();

        let result = script.run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::Error),
                WhatRan::Evaluated(Ok(Argument::I64(123))),
                WhatRan::Evaluated(Ok(Argument::String("asdf".to_string()))),
                WhatRan::Begin(Command::Error),
                WhatRan::End(Command::Error),
                WhatRan::Evaluated(Err("".to_string())), // result of Err
                WhatRan::Begin(Command::RunAll),
                WhatRan::Evaluated(Ok(Argument::I64(456))),
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::End(Command::RunAll),
                WhatRan::Evaluated(Ok(Argument::Null)), // result of RunAll
                WhatRan::Begin(Command::Error),
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::Evaluated(Ok(Argument::I64(123))), // Use $0
                WhatRan::Evaluated(Ok(Argument::Color(Rgba8::GREEN))),
                WhatRan::End(Command::Error),
                WhatRan::Evaluated(Err("null 123 #00ff00".to_string())),
                WhatRan::Begin(Command::BackgroundColor),
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::End(Command::BackgroundColor),
                WhatRan::Evaluated(Ok(Argument::Color(test_runner.bg))),
                WhatRan::End(Command::Error),
            ])
        );
        assert_eq!(
            result,
            Err("123 'asdf' (error) null (error `null 123 #00ff00`) #000000".to_string())
        );
    }

    #[test]
    fn test_script_can_look_back_multiple_script_arguments() {
        let mut test_runner = TestRunner::new();
        let function = Script {
            command: Command::If,
            arguments: vec![
                Argument::Script(Script {
                    command: Command::Even,
                    arguments: vec![Argument::Script(Script {
                        command: Command::Sum,
                        arguments: vec![
                            Argument::Use(Use {
                                index: 0,
                                lookback: -3,
                            }),
                            // Put a null in here to ensure we don't stop the sum at null:
                            Argument::Null,
                            Argument::I64(0),
                            Argument::Use(Use {
                                index: 1,
                                lookback: -3,
                            }),
                        ],
                    })],
                }),
                Argument::Color(Rgba8::BLUE),
                Argument::Color(Rgba8::GREEN),
            ],
        };
        let checkerboard = "checkerboard".to_string();
        _ = test_runner.variables.set(
            checkerboard.clone(),
            Variable::Const(Argument::Script(function)),
        );

        let script = Script {
            command: Command::Evaluate(checkerboard.clone()),
            arguments: vec![Argument::I64(3), Argument::I64(5)], // (x, y) coordinates
        };
        let result = script.run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            vec![
                WhatRan::Begin(Command::Evaluate(checkerboard.clone())),
                WhatRan::Begin(Command::If),
                WhatRan::Begin(Command::Even),
                WhatRan::Begin(Command::Sum),
                WhatRan::Evaluated(Ok(Argument::I64(3))),
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::Evaluated(Ok(Argument::I64(0))),
                WhatRan::Evaluated(Ok(Argument::I64(5))),
                // result of the sum:
                WhatRan::End(Command::Sum),
                WhatRan::Evaluated(Ok(Argument::I64(8))),
                // result of even
                WhatRan::End(Command::Even),
                WhatRan::Evaluated(Ok(Argument::I64(1))),
                // final result
                WhatRan::Evaluated(Ok(Argument::Color(Rgba8::BLUE))),
                WhatRan::End(Command::If),
                WhatRan::End(Command::Evaluate(checkerboard.clone())),
            ]
        );
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some(Argument::Color(Rgba8::BLUE)));
    }

    #[test]
    fn test_script_empty_multiply_returns_unity() {
        let mut test_runner = TestRunner::new();

        assert_eq!(
            Script {
                command: Command::Product,
                arguments: vec![],
            }
            .run(&mut test_runner),
            Ok(Argument::I64(1))
        );

        assert_eq!(
            test_runner.test_what_ran,
            vec![
                WhatRan::Begin(Command::Product),
                WhatRan::End(Command::Product),
            ]
        );
    }

    #[test]
    fn test_script_nulls_in_multiply_are_ignored() {
        let mut test_runner = TestRunner::new();

        assert_eq!(
            Script {
                command: Command::Product,
                arguments: vec![Argument::Null],
            }
            .run(&mut test_runner),
            Ok(Argument::I64(1))
        );

        assert_eq!(
            test_runner.test_what_ran,
            vec![
                WhatRan::Begin(Command::Product),
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::End(Command::Product),
            ]
        );
    }

    #[test]
    fn test_script_multiply_works_correctly() {
        let mut test_runner = TestRunner::new();

        assert_eq!(
            Script {
                command: Command::Product,
                arguments: vec![
                    Argument::Null,
                    Argument::Script(Script {
                        command: Command::Sum,
                        arguments: vec![Argument::I64(3), Argument::Null, Argument::I64(27)]
                    }),
                    Argument::I64(100),
                    Argument::I64(5),
                ],
            }
            .run(&mut test_runner),
            Ok(Argument::I64(30 * 100 * 5))
        );

        assert_eq!(
            test_runner.test_what_ran,
            vec![
                WhatRan::Begin(Command::Product),
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::Begin(Command::Sum),
                WhatRan::Evaluated(Ok(Argument::I64(3))),
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::Evaluated(Ok(Argument::I64(27))),
                WhatRan::End(Command::Sum),
                WhatRan::Evaluated(Ok(Argument::I64(30))),
                WhatRan::Evaluated(Ok(Argument::I64(100))),
                WhatRan::Evaluated(Ok(Argument::I64(5))),
                WhatRan::End(Command::Product),
            ]
        );
    }

    // TODO: add test for Odd

    #[test]
    fn test_script_allows_variable_arguments() {
        let variable_name = "cabbage".to_string();
        let script = Script {
            command: Command::If,
            arguments: vec![
                Argument::Script(Script {
                    command: Command::Evaluate(variable_name.clone()),
                    arguments: vec![],
                }),
                Argument::Script(Script {
                    command: Command::ForegroundColor,
                    arguments: vec![Argument::Color(Rgba8::BLACK)],
                }),
                Argument::Script(Script {
                    command: Command::BackgroundColor,
                    arguments: vec![Argument::Color(Rgba8::RED)],
                }),
            ],
        };
        let mut test_runner = TestRunner::new();

        assert!(test_runner
            .variables
            .set(variable_name.clone(), Variable::Mutable(Argument::I64(1)))
            .is_ok());
        let mut result = script.run(&mut test_runner);
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some(Argument::Color(Rgba8::WHITE))); // previous FG color
        assert_eq!(test_runner.fg, Rgba8::BLACK);

        assert!(test_runner
            .variables
            .set(variable_name.clone(), Variable::Mutable(Argument::I64(0)))
            .is_ok());
        result = script.run(&mut test_runner);
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some(Argument::Color(Rgba8::BLACK))); // previous BG color
        assert_eq!(test_runner.bg, Rgba8::RED);

        assert_eq!(
            test_runner.test_what_ran,
            vec![
                // First run:
                WhatRan::Begin(Command::If),
                WhatRan::Begin(Command::Evaluate(variable_name.clone())),
                WhatRan::End(Command::Evaluate(variable_name.clone())),
                WhatRan::Evaluated(Ok(Argument::I64(1))),
                WhatRan::Begin(Command::ForegroundColor),
                WhatRan::Evaluated(Ok(Argument::Color(Rgba8::BLACK))),
                WhatRan::End(Command::ForegroundColor),
                WhatRan::Evaluated(Ok(Argument::Color(Rgba8::WHITE))), // previous FG color
                WhatRan::End(Command::If),
                // Second run:
                WhatRan::Begin(Command::If),
                WhatRan::Begin(Command::Evaluate(variable_name.clone())),
                WhatRan::End(Command::Evaluate(variable_name.clone())),
                WhatRan::Evaluated(Ok(Argument::I64(0))),
                WhatRan::Begin(Command::BackgroundColor),
                WhatRan::Evaluated(Ok(Argument::Color(Rgba8::RED))),
                WhatRan::End(Command::BackgroundColor),
                WhatRan::Evaluated(Ok(Argument::Color(Rgba8::BLACK))), // previous BG color
                WhatRan::End(Command::If),
            ]
        );
    }

    #[test]
    fn test_script_allows_setting_mutable_variables() {
        let variable_name = "forkroot".to_string();
        let mut script = Script {
            command: Command::SetVariable,
            arguments: Vec::from([
                Argument::String(variable_name.clone()),
                Argument::Script(Script {
                    command: Command::ForegroundColor,
                    arguments: vec![Argument::Use(Use {
                        index: 1,
                        lookback: -1,
                    })],
                }),
            ]),
        };
        let mut test_runner = TestRunner::new();

        let mut result = script.run(&mut test_runner);
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some(Argument::Null)); // variable wasn't anything previously

        script = Script {
            command: Command::Evaluate(variable_name.clone()),
            arguments: vec![
                Argument::Color(Rgba8::RED),
                Argument::Color(Rgba8::BLUE),
                Argument::Color(Rgba8::BLACK),
            ],
        };
        result = script.run(&mut test_runner);
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some(Argument::Color(Rgba8::WHITE))); // previous value of FG
        assert_eq!(test_runner.fg, Rgba8::BLUE);

        assert_eq!(
            test_runner.test_what_ran,
            vec![
                // First run:
                WhatRan::Begin(Command::SetVariable),
                WhatRan::End(Command::SetVariable),
                // Second run:
                WhatRan::Begin(Command::Evaluate(variable_name.clone())),
                WhatRan::Begin(Command::ForegroundColor),
                WhatRan::Evaluated(Ok(Argument::Color(Rgba8::BLUE))),
                WhatRan::End(Command::ForegroundColor),
                WhatRan::End(Command::Evaluate(variable_name.clone())),
            ]
        );
    }

    #[test]
    fn test_script_allows_setting_const_variables() {
        let variable_name = "artichoke".to_string();
        let mut script = Script {
            command: Command::ConstVariable,
            arguments: Vec::from([
                Argument::String(variable_name.clone()),
                Argument::Script(Script {
                    command: Command::BackgroundColor,
                    arguments: vec![Argument::Use(Use {
                        index: 2,
                        lookback: -1,
                    })],
                }),
            ]),
        };
        let mut test_runner = TestRunner::new();

        let mut result = script.run(&mut test_runner);
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some(Argument::Null)); // variable wasn't anything previously

        script = Script {
            command: Command::Evaluate(variable_name.clone()),
            arguments: vec![
                Argument::Color(Rgba8::RED),
                Argument::Color(Rgba8::BLUE),
                Argument::Color(Rgba8::BLACK),
            ],
        };
        result = script.run(&mut test_runner);
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some(Argument::Color(Rgba8::BLACK)));

        assert_eq!(
            test_runner.test_what_ran,
            vec![
                // First run:
                WhatRan::Begin(Command::ConstVariable),
                WhatRan::End(Command::ConstVariable),
                // Second run:
                WhatRan::Begin(Command::Evaluate(variable_name.clone())),
                WhatRan::Begin(Command::BackgroundColor),
                WhatRan::Evaluated(Ok(Argument::Color(Rgba8::BLACK))),
                WhatRan::End(Command::BackgroundColor),
                WhatRan::End(Command::Evaluate(variable_name.clone())),
            ]
        );
    }

    // TODO: test that we don't recurse infinitely if a function calls itself

    #[test]
    fn test_evaluate_can_follow_path_of_use_arguments() {
        let function1 = "dot5".to_string();
        let function2 = "paint5".to_string();
        let mut test_runner = TestRunner::new();
        assert_eq!(
            test_runner.variables.set(
                function1.clone(),
                Variable::Const(Argument::Script(Script {
                    command: Command::Evaluate(function2.to_string()),
                    arguments: vec![
                        Argument::Use(Use {
                            index: 2,
                            lookback: -1,
                        }),
                        Argument::Use(Use {
                            index: 0,
                            lookback: -1,
                        }),
                    ],
                })),
            ),
            Ok(Argument::Null)
        );
        assert_eq!(
            test_runner.variables.set(
                function2.clone(),
                Variable::Const(Argument::Script(Script {
                    command: Command::Paint,
                    arguments: vec![
                        Argument::Use(Use {
                            index: 0,
                            lookback: -1,
                        }),
                        Argument::Use(Use {
                            index: 1,
                            lookback: -1,
                        }),
                        Argument::I64(5),
                    ],
                })),
            ),
            Ok(Argument::Null)
        );
        let script = Script {
            command: Command::Evaluate(function1.clone()),
            arguments: vec![
                Argument::I64(10), // used as x coordinate in paint
                Argument::String("not used anywhere".to_string()),
                Argument::I64(37), // used as y coordinate in paint
            ],
        };

        assert_eq!(
            script.run(&mut test_runner),
            Ok(Argument::Color(TestRunner::PAINT_RETURN_COLOR))
        );

        assert_eq!(
            test_runner.test_what_ran,
            vec![
                // First function/lambda:
                WhatRan::Begin(Command::Evaluate(function1.clone())),
                // Second function/lambda:
                WhatRan::Begin(Command::Evaluate(function2.clone())),
                // Actual built-in function:
                WhatRan::Begin(Command::Paint),
                WhatRan::Evaluated(Ok(Argument::I64(37))), // x coordinate
                WhatRan::Evaluated(Ok(Argument::I64(10))), // y coordinate
                WhatRan::Evaluated(Ok(Argument::I64(5))),  // color palette
                WhatRan::Mocked(format!("p 37 10 {}", test_runner.palette.colors[5])),
                WhatRan::End(Command::Paint),
                WhatRan::End(Command::Evaluate(function2.clone())),
                WhatRan::End(Command::Evaluate(function1.clone())),
            ]
        );
    }

    #[test]
    fn test_evaluate_brush_erase_via_getter() {
        let mut test_runner = TestRunner::new();

        test_runner.brush.set(brush::BrushMode::Erase);
        assert_eq!(
            Script {
                command: Command::BrushMode(BrushMode::Erase),
                arguments: vec![]
            }
            .run(&mut test_runner),
            Ok(Argument::I64(1))
        );
        assert_eq!(test_runner.brush.is_set(brush::BrushMode::Erase), true); // doesn't change it

        test_runner.brush.unset(brush::BrushMode::Erase);
        assert_eq!(
            Script {
                command: Command::BrushMode(BrushMode::Erase),
                arguments: vec![Argument::Null]
            }
            .run(&mut test_runner),
            Ok(Argument::I64(0))
        );
        assert_eq!(test_runner.brush.is_set(brush::BrushMode::Erase), false); // doesn't change it
    }

    #[test]
    fn test_evaluate_brush_erase_via_swapper() {
        let mut test_runner = TestRunner::new();

        test_runner.brush.set(brush::BrushMode::Erase);
        assert_eq!(
            Script {
                command: Command::BrushMode(BrushMode::Erase),
                arguments: vec![Argument::I64(0)]
            }
            .run(&mut test_runner),
            Ok(Argument::I64(1)) // returns old value
        );
        assert_eq!(test_runner.brush.is_set(brush::BrushMode::Erase), false); // does change it

        test_runner.brush.unset(brush::BrushMode::Erase);
        assert_eq!(
            Script {
                command: Command::BrushMode(BrushMode::Erase),
                arguments: vec![Argument::I64(1)]
            }
            .run(&mut test_runner),
            Ok(Argument::I64(0)) // returns old value
        );
        assert_eq!(test_runner.brush.is_set(brush::BrushMode::Erase), true); // does change it
    }

    // TODO: Other BrushMode tests

    #[test]
    fn test_evaluate_brush_line_via_getter() {
        let mut test_runner = TestRunner::new();

        test_runner.brush.set(brush::BrushMode::Line(Some(15)));
        assert_eq!(
            Script {
                command: Command::BrushMode(BrushMode::Line),
                arguments: vec![Argument::Null]
            }
            .run(&mut test_runner),
            Ok(Argument::I64(15))
        );
        assert_eq!(
            test_runner.brush.line_mode(),
            Some(brush::BrushMode::Line(Some(15)))
        ); // doesn't change it

        test_runner.brush.unset(brush::BrushMode::Line(None));
        assert_eq!(
            Script {
                command: Command::BrushMode(BrushMode::Line),
                arguments: vec![]
            }
            .run(&mut test_runner),
            Ok(Argument::I64(0))
        );
        assert_eq!(test_runner.brush.line_mode(), None); // doesn't change it
    }

    #[test]
    fn test_evaluate_brush_line_via_swapper() {
        let mut test_runner = TestRunner::new();

        test_runner.brush.set(brush::BrushMode::Line(Some(15)));
        assert_eq!(
            Script {
                command: Command::BrushMode(BrushMode::Line),
                arguments: vec![Argument::I64(77)]
            }
            .run(&mut test_runner),
            Ok(Argument::I64(15))
        );
        assert_eq!(
            test_runner.brush.line_mode(),
            Some(brush::BrushMode::Line(Some(77)))
        ); // changes it

        assert_eq!(
            Script {
                command: Command::BrushMode(BrushMode::Line),
                arguments: vec![Argument::I64(0)]
            }
            .run(&mut test_runner),
            Ok(Argument::I64(77)) // was 77
        );
        assert_eq!(test_runner.brush.line_mode(), None); // changes change it
    }

    #[test]
    fn test_evaluate_brush_size_via_getter() {
        let mut test_runner = TestRunner::new();

        test_runner.brush.size = 123456;
        assert_eq!(
            Script {
                command: Command::BrushSize,
                arguments: vec![]
            }
            .run(&mut test_runner),
            Ok(Argument::I64(123456))
        );
        assert_eq!(test_runner.brush.size, 123456); // doesn't change it

        test_runner.brush.size = 1234567;
        assert_eq!(
            Script {
                command: Command::BrushSize,
                arguments: vec![Argument::Null]
            }
            .run(&mut test_runner),
            Ok(Argument::I64(1234567))
        );
        assert_eq!(test_runner.brush.size, 1234567); // doesn't change it
    }

    #[test]
    fn test_evaluate_brush_size_via_swapper() {
        let mut test_runner = TestRunner::new();

        test_runner.brush.size = 123;
        assert_eq!(
            Script {
                command: Command::BrushSize,
                arguments: vec![Argument::I64(777)]
            }
            .run(&mut test_runner),
            Ok(Argument::I64(123)) // returns old value
        );
        assert_eq!(test_runner.brush.size, 777); // sets new value

        // Can't go below 1
        test_runner.brush.size = 1234;
        assert_eq!(
            Script {
                command: Command::BrushSize,
                arguments: vec![Argument::I64(0)]
            }
            .run(&mut test_runner),
            Ok(Argument::I64(1234)) // returns old value
        );
        assert_eq!(test_runner.brush.size, 1); // sets new value
    }

    #[test]
    fn test_evaluate_errors_when_use_lookback_is_invalid() {
        let script = Script {
            command: Command::If,
            arguments: vec![
                Argument::Use(Use {
                    lookback: 0,
                    index: 1,
                }),
                Argument::String("reference me".to_string()),
                Argument::Use(Use {
                    lookback: 0,
                    index: 2,
                }), // this would be self-referential
            ],
        };
        let mut test_runner = TestRunner::new();

        assert_eq!(
            script.run(&mut test_runner),
            Err("$1 should only be used inside a script".to_string())
        );

        assert_eq!(
            test_runner.test_what_ran,
            vec![
                WhatRan::Begin(Command::If),
                WhatRan::Evaluated(Err("$1 should only be used inside a script".to_string()))
            ]
        );
    }

    #[test]
    fn test_script_help_is_great() {
        let mut test_runner = TestRunner::new();

        let result = Script {
            command: Command::Help,
            arguments: vec![],
        }
        .run(&mut test_runner);
        assert_eq!(
            test_runner.message,
            Message {
                string: "-- explains what $0 does without evaluating it, \
                        e.g., `? paint` to explain what the `paint` command does"
                    .to_string(),
                message_type: MessageType::Info,
            }
        );
        assert_eq!(result, Ok(Argument::Null));

        let result = Script {
            command: Command::Help,
            arguments: vec![Argument::Script(Script::zero_arg(Command::If))],
        }
        .run(&mut test_runner);
        assert_eq!(
            test_runner.message,
            Message {
                string: "-- if $0 evaluates to truthy, evaluates $1, otherwise $2, \
                        e.g., `if 'hi' 'world' 3` returns 'world'"
                    .to_string(),
                message_type: MessageType::Info,
            }
        );
        assert_eq!(result, Ok(Argument::Null));

        let result = Script {
            command: Command::Help,
            arguments: vec![Argument::Use(Use {
                index: 3,
                lookback: 0,
            })],
        }
        .run(&mut test_runner);
        assert_eq!(
            test_runner.message,
            Message {
                string: "-- a way to evaluate an argument at position 3; \
                        e.g., `const 'ten' (sum $0 10)` will add 10 to argument 0, \
                        and can be called via `ten 123` which will return `133`"
                    .to_string(),
                message_type: MessageType::Info,
            }
        );
        assert_eq!(result, Ok(Argument::Null));

        let result = Script {
            command: Command::Help,
            arguments: vec![Argument::String("blah".to_string())],
        }
        .run(&mut test_runner);
        assert_eq!(
            test_runner.message,
            Message {
                string: "'blah' -- an argument".to_string(),
                message_type: MessageType::Info,
            }
        );
        assert_eq!(result, Ok(Argument::Null));

        let result = Script {
            command: Command::Help,
            arguments: vec![Argument::Script(Script {
                command: Command::If,
                arguments: vec![Argument::I64(2)],
            })],
        }
        .run(&mut test_runner);
        assert_eq!(
            test_runner.message,
            Message {
                string: "(if 2) -- a script".to_string(),
                message_type: MessageType::Info,
            }
        );
        assert_eq!(result, Ok(Argument::Null));
    }

    #[test]
    fn test_variables_set_cannot_overwrite_built_ins() {
        let mut variables = Variables::with_built_ins();
        assert_eq!(
            variables.set("if".to_string(), Variable::Mutable(Argument::I64(123)),),
            Err("built-in `if` is not reassignable".to_string())
        );
        assert_eq!(
            variables.set("fg".to_string(), Variable::Const(Argument::I64(123)),),
            Err("built-in `fg` is not reassignable".to_string())
        );
        assert_eq!(
            variables.set("p".to_string(), Variable::Alias("super-paint".to_string()),),
            Err("built-in `p` is not reassignable".to_string())
        );
        assert_eq!(
            variables.set(
                "const".to_string(),
                Variable::BuiltIn("try to override".to_string())
            ),
            Err("built-in `const` is not reassignable".to_string())
        );
    }

    #[test]
    fn test_variables_has_some_const_defaults() {
        let mut variables = Variables::with_built_ins();
        let mut check_variable = |name: &str, value: Argument| {
            assert_eq!(variables.get(name.to_string()), value);
            assert_eq!(
                variables.set(name.to_string(), Variable::Mutable(Argument::I64(123))),
                Err(format!("variable `{}` is not reassignable", name))
            );
        };
        check_variable("null", Argument::Null);
        check_variable("on", Argument::I64(1));
        check_variable("off", Argument::I64(0));
        check_variable("true", Argument::I64(1));
        check_variable("false", Argument::I64(0));
    }

    #[test]
    fn test_variables_can_serialize_built_ins() {
        let variables = Variables::with_built_ins();
        let swap = variables
            .get("swap".to_string())
            .get_script("for test")
            .unwrap();
        assert_eq!(
            format!("{}", Serialize::Script(&swap)),
            "fg (bg fg)".to_string()
        );

        let next_frame = variables
            .get("f++".to_string())
            .get_script("for test")
            .unwrap();
        assert_eq!(
            format!("{}", Serialize::Script(&next_frame)),
            "f (+ f 1)".to_string()
        );

        let previous_frame = variables
            .get("f--".to_string())
            .get_script("for test")
            .unwrap();
        assert_eq!(
            format!("{}", Serialize::Script(&previous_frame)),
            "f (+ f -1)".to_string()
        );
    }

    #[test]
    fn test_variables_can_add_const_variables() {
        let mut variables = Variables::new();

        let var_name = "wyzx".to_string();
        // Nothing in variables yet:
        assert_eq!(variables.get(var_name.clone()), Argument::Null);

        // First assignment is fine:
        assert_eq!(
            variables.set(var_name.clone(), Variable::Const(Argument::I64(456))),
            // The previous value of this variable was null:
            Ok(Argument::Null)
        );
        assert_eq!(variables.get(var_name.clone()), Argument::I64(456));

        // Can't reassign:
        assert_eq!(
            variables.set(var_name.clone(), Variable::Const(Argument::I64(789))),
            Err(format!("variable `{}` is not reassignable", var_name))
        );
        assert_eq!(variables.get(var_name.clone()), Argument::I64(456)); // No change!
    }

    #[test]
    fn test_variables_can_set_mutable_variables() {
        let mut variables = Variables::new();

        let var_name = "cdef".to_string();
        // Nothing in variables yet:
        assert_eq!(variables.get(var_name.clone()), Argument::Null);

        // First assignment is fine:
        assert_eq!(
            variables.set(var_name.clone(), Variable::Mutable(Argument::I64(456))),
            // The previous value of this variable was null:
            Ok(Argument::Null)
        );
        assert_eq!(variables.get(var_name.clone()), Argument::I64(456));

        // Can't reassign:
        assert_eq!(
            variables.set(var_name.clone(), Variable::Mutable(Argument::I64(789))),
            // Returns the old value:
            Ok(Argument::I64(456))
        );
        assert_eq!(variables.get(var_name.clone()), Argument::I64(789));
    }

    #[test]
    fn test_variables_can_resolve_aliases() {
        let mut variables = Variables::new();

        let x = "X".to_string();
        let y = "Y".to_string();
        let z = "Z".to_string();

        _ = variables.set(x.clone(), Variable::Alias(y.clone()));
        // If Y isn't defined yet:
        assert_eq!(variables.get(x.clone()), Argument::Null);

        _ = variables.set(y.clone(), Variable::Alias(z.clone()));
        // Z isn't defined yet, either:
        assert_eq!(variables.get(x.clone()), Argument::Null);
        assert_eq!(variables.get(y.clone()), Argument::Null);

        _ = variables.set(z.clone(), Variable::Const(Argument::I64(123)));
        assert_eq!(variables.get(x.clone()), Argument::I64(123));
        assert_eq!(variables.get(y.clone()), Argument::I64(123));
        assert_eq!(variables.get(z.clone()), Argument::I64(123)); // not an alias, but should make sense!
    }

    #[test]
    fn test_variables_errors_on_self_referential_alias() {
        let mut variables = Variables::new();

        let name = "xyz".to_string();

        assert_eq!(
            variables.set(name.clone(), Variable::Alias(name.clone())),
            Err(format!(
                "alias can't be self referential: {} -> {}",
                name.clone(),
                name.clone()
            ))
        )
    }

    #[test]
    fn test_variables_set_from_mutable_script() {
        let mut variables = Variables::new();
        let var_name = "blast-turtle".to_string();
        let lambda = Script {
            command: Command::ForegroundColor,
            arguments: vec![Argument::I64(30)],
        };
        let mut script = Script {
            command: Command::SetVariable,
            arguments: vec![
                Argument::String(var_name.clone()),
                Argument::Script(lambda.clone()),
            ],
        };

        assert_eq!(variables.set_from_script(&script), Ok(Argument::Null));
        assert_eq!(
            variables.get(var_name.clone()),
            Argument::Script(lambda.clone())
        );

        // Double check that we can mutate:
        script.arguments[1] = Argument::I64(-123);
        assert_eq!(
            variables.set_from_script(&script),
            Ok(Argument::Script(lambda))
        );
        assert_eq!(variables.get(var_name), Argument::I64(-123));
    }

    #[test]
    fn test_variables_set_from_const_script() {
        let mut variables = Variables::new();
        let var_name = "blast-dog".to_string();
        let lambda = Script {
            command: Command::ForegroundColor,
            arguments: vec![Argument::I64(29)],
        };
        let mut script = Script {
            command: Command::ConstVariable,
            arguments: vec![
                Argument::String(var_name.clone()),
                Argument::Script(lambda.clone()),
            ],
        };

        assert_eq!(variables.set_from_script(&script), Ok(Argument::Null));
        assert_eq!(
            variables.get(var_name.clone()),
            Argument::Script(lambda.clone())
        );

        // Double check that we cannot reassign:
        script.arguments[1] = Argument::I64(603);
        assert_eq!(
            variables.set_from_script(&script),
            Err(format!(
                "variable `{}` is not reassignable",
                var_name.clone()
            ))
        );
        assert_eq!(variables.get(var_name), Argument::Script(lambda));
    }

    #[test]
    fn test_variables_set_from_alias_script() {
        let mut variables = Variables::new();
        let alias_name = "blast-cat".to_string();
        let alias_value = "blast-frog".to_string();
        let mut script = Script {
            command: Command::CreateAlias,
            arguments: vec![
                Argument::String(alias_name.clone()),
                Argument::String(alias_value.clone()),
            ],
        };

        assert_eq!(variables.set_from_script(&script), Ok(Argument::Null));
        assert_eq!(
            *variables.map.get(&alias_name).expect("should be here"),
            Variable::Alias(alias_value.clone())
        );
        let mut aliased_value = Argument::String("hello".to_string());
        assert_eq!(
            variables.set(
                alias_value.clone(),
                Variable::Mutable(aliased_value.clone()),
            ),
            Ok(Argument::Null)
        );
        assert_eq!(variables.get(alias_name.clone()), aliased_value.clone());

        // Double check that we cannot reassign:
        script.arguments[1] = Argument::String("change-to-this".to_string());
        assert_eq!(
            variables.set_from_script(&script),
            Err(format!(
                "alias `{}` is not reassignable",
                alias_name.clone()
            ))
        );
        assert_eq!(
            *variables.map.get(&alias_name).expect("should be here"),
            Variable::Alias(alias_value.clone())
        );
        assert_eq!(variables.get(alias_name.clone()), aliased_value.clone());
        // But we can modify the underlying value:
        aliased_value = Argument::I64(8675309);
        assert_eq!(
            variables.set(
                alias_value.clone(),
                Variable::Mutable(aliased_value.clone()),
            ),
            Ok(Argument::String("hello".to_string()))
        );
        assert_eq!(variables.get(alias_name), aliased_value);
    }

    #[test]
    fn test_get_or_swap_color_works_with_palette() {
        let mut palette = Palette::new(12.0, 50);
        palette.add(Rgba8 {
            r: 111,
            g: 22,
            b: 3,
            a: 255,
        });
        palette.add(Rgba8 {
            r: 44,
            g: 55,
            b: 66,
            a: 255,
        });

        // The color we'll try to modify.
        let mut color = Rgba8 {
            r: 255,
            g: 254,
            b: 253,
            a: 255,
        };
        let mut trailing_color = color;

        // going "under" the palette size:
        assert_eq!(
            get_or_swap_color(&mut color, &palette, Argument::I64(-1)),
            Err("no palette color with index -1".to_string())
        );
        assert_eq!(color, trailing_color); // no change

        // being within palette size:
        assert_eq!(
            get_or_swap_color(&mut color, &palette, Argument::I64(1)),
            Ok(Argument::Color(trailing_color))
        );
        assert_eq!(color, palette.colors[1]);
        trailing_color = color;

        // going over the palette size:
        assert_eq!(
            get_or_swap_color(&mut color, &palette, Argument::I64(2)),
            Err("no palette color with index 2".to_string())
        );
        assert_eq!(color, trailing_color); // no change

        // another test within palette size:
        assert_eq!(
            get_or_swap_color(&mut color, &palette, Argument::I64(0)),
            Ok(Argument::Color(trailing_color))
        );
        assert_eq!(color, palette.colors[0]);
    }

    #[test]
    fn test_get_or_swap_color_works_with_pure_colors() {
        // Palette should be ignored.
        let palette = Palette::new(12.0, 50);

        // The color we'll try to modify.
        let mut color = Rgba8 {
            r: 255,
            g: 254,
            b: 253,
            a: 255,
        };
        let mut trailing_color = color;

        // Test once:
        let mut new_color = Rgba8 {
            r: 53,
            g: 27,
            b: 13,
            a: 0xff,
        };
        assert_eq!(
            get_or_swap_color(&mut color, &palette, Argument::Color(new_color)),
            Ok(Argument::Color(trailing_color))
        );
        assert_eq!(color, new_color);
        trailing_color = color;

        // Test again:
        new_color = Rgba8 {
            r: 101,
            g: 1,
            b: 77,
            a: 0xff,
        };
        assert_eq!(
            get_or_swap_color(&mut color, &palette, Argument::Color(new_color)),
            Ok(Argument::Color(trailing_color))
        );
        assert_eq!(color, new_color);
    }

    #[test]
    fn test_get_or_swap_color_returns_current_color_with_null_argument() {
        // Palette should be ignored.
        let palette = Palette::new(12.0, 50);

        // The color we'll try to modify.
        let mut color = Rgba8 {
            r: 255,
            g: 254,
            b: 253,
            a: 255,
        };
        let initial_color = color;

        assert_eq!(
            get_or_swap_color(&mut color, &palette, Argument::Null),
            Ok(Argument::Color(initial_color))
        );
        assert_eq!(color, initial_color);
    }

    #[test]
    fn test_get_or_swap_color_returns_error_with_invalid_arguments() {
        // Palette should be ignored.
        let palette = Palette::new(12.0, 50);

        // The color we'll try to modify.
        let mut color = Rgba8 {
            r: 255,
            g: 254,
            b: 253,
            a: 255,
        };
        let initial_color = color;

        // String doesn't work.
        assert_eq!(
            get_or_swap_color(&mut color, &palette, Argument::String("asdf".to_string())),
            Err("invalid argument to get_or_swap_color: 'asdf'".to_string())
        );
        assert_eq!(color, initial_color);

        // Script doesn't work:
        assert_eq!(
            get_or_swap_color(
                &mut color,
                &palette,
                Argument::Script(Script {
                    command: Command::ForegroundColor,
                    arguments: vec![]
                })
            ),
            Err(
                "invalid argument to get_or_swap_color: {command: `fg`, arguments: []}".to_string()
            )
        );
        assert_eq!(color, initial_color);

        // Use doesn't work:
        assert_eq!(
            get_or_swap_color(
                &mut color,
                &palette,
                Argument::Use(Use {
                    index: 1234,
                    lookback: -1
                })
            ),
            Err("invalid argument to get_or_swap_color: $1234".to_string())
        );
        assert_eq!(color, initial_color);
    }

    #[test]
    fn test_command_parsing() {
        assert_eq!(Command::from_str("?"), Ok(Command::Help));
        assert_eq!(Command::from_str("push/pop"), Ok(Command::PushPop));
        assert_eq!(Command::from_str("run/all"), Ok(Command::RunAll));
        assert_eq!(Command::from_str("not"), Ok(Command::Not));
        assert_eq!(Command::from_str("if"), Ok(Command::If));
        assert_eq!(Command::from_str("even"), Ok(Command::Even));
        assert_eq!(Command::from_str("odd"), Ok(Command::Odd));
        assert_eq!(Command::from_str("+"), Ok(Command::Sum));
        assert_eq!(Command::from_str("*"), Ok(Command::Product));
        assert_eq!(Command::from_str("set"), Ok(Command::SetVariable));
        assert_eq!(Command::from_str("const"), Ok(Command::ConstVariable));
        assert_eq!(Command::from_str("alias"), Ok(Command::CreateAlias));
        assert_eq!(Command::from_str("fg"), Ok(Command::ForegroundColor));
        assert_eq!(Command::from_str("bg"), Ok(Command::BackgroundColor));
        assert_eq!(Command::from_str("p"), Ok(Command::Paint));
        assert_eq!(Command::from_str("q"), Ok(Command::Quit(Quit::Safe)));
        assert_eq!(Command::from_str("qa"), Ok(Command::Quit(Quit::AllSafe)));
        assert_eq!(Command::from_str("q!"), Ok(Command::Quit(Quit::Forced)));
        assert_eq!(Command::from_str("qa!"), Ok(Command::Quit(Quit::AllForced)));
        assert_eq!(
            Command::from_str("gnarly345"),
            Ok(Command::Evaluate("gnarly345".to_string()))
        );
        assert_eq!(Command::from_str(""), Err(EmptyCommandParseError));
    }

    #[test]
    fn test_can_serialize_complicated_function() {
        let script = Script {
            command: Command::If,
            arguments: vec![
                Argument::Use(Use {
                    index: 0,
                    lookback: -1,
                }),
                Argument::Script(Script {
                    command: Command::Paint,
                    arguments: vec![
                        Argument::Script(Script::zero_arg(Command::Evaluate("x1".to_string()))),
                        Argument::Script(Script::zero_arg(Command::Evaluate("y2".to_string()))),
                        Argument::Use(Use {
                            index: 1,
                            lookback: -2,
                        }),
                    ],
                }),
                Argument::Script(Script {
                    command: Command::Evaluate("jangles".to_string()),
                    arguments: vec![
                        Argument::Script(Script {
                            command: Command::ForegroundColor,
                            arguments: vec![Argument::Use(Use {
                                index: 2,
                                lookback: -3,
                            })],
                        }),
                        Argument::Script(Script::zero_arg(Command::Quit(Quit::Safe))),
                        Argument::Use(Use {
                            index: 3,
                            lookback: -2,
                        }),
                    ],
                }),
            ],
        };

        assert_eq!(
            format!("{}", Serialize::Script(&script)),
            "if $0 (p x1 y2 $1) (jangles (fg $2) q $3)".to_string()
        );
    }
}
