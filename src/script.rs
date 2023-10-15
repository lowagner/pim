use crate::gfx::Rgba8;
use crate::palette::Palette;

use std::collections::HashMap;
use std::fmt;
use std::mem;
use std::str::FromStr;

/*
TODO: how is parsing going to work here?

        :set 'cmd_name' (the_cmd $0 123)
    =>  :cmd_name 5     -- calls `the_cmd` with arguments `5` `123`

        $0          -- evaluate argument at index 0.  similarly for $1, etc.
        run         -- evaluates next argument, uses it as index as to which argument to run
                    -- (index starts at 0 for the first argument after the argument index.)
        truthy      -- returns 1 if all arguments are truthy otherwise 0

    based on the above, `if` can be built like this:
        -- note the truthy clause happens second since `run (truthy $0)`
        -- will run argument 0 for false and argument 1 for truthy:

    option 1:
        :set 'if' (run (truthy $0) $2 $1)
    option 2 (NOT USED!)
        :set('if' run(truthy($0) $2 $1))
    with option 1 we can avoid parentheses if we use a simple command.

    then we can do `if (conditional) (truthy) (falsy)`.

TODO: add test/functionality for parsing like this:
        :set 'check' (paint cx cy (if (even (sum cx cy)) fg bg))
    i.e., `fg` and `bg` here are variables that we evaluate without any arguments.
    we could create a specialized argument for this, but i think it's better to
    just parse as `Argument::Script({ command: Evaluate("fg"), args: [] })`.

TODO: add `cx`/`cy` as functions/commands to get cursor position x/y

TODO: we should probably have an error if people try to overwrite 'if', 'set', etc.

TODO: make push have an automatic pop at the end of a script runner.
*/

/// The commands that are possible in a script.
// Note these are designed to be quickly cloned, so don't put large amounts of data in them.
#[derive(PartialEq, Debug, Clone)]
pub enum Command {
    /// Runs $0, checks if it's truthy, then evaluates $1 if so, otherwise $2.
    /// Note that $1 and $2 are optional.
    // TODO: if $2 is missing, return I64(0); if $1 is missing, return I64(1)
    If,
    /// Returns 1 if $0 is even, otherwise 0.  This is an error if $0 does
    /// not evaluate to an integer.
    Even,
    /// Returns 1 if $0 is odd, otherwise 0.  This is an error if $0 does
    /// not evaluate to an integer.
    Odd,
    /// Sums all arguments, using $0 as the type to return.
    /// E.g., if $0 is an integer, then $1, $2, etc. will be cast to integers.
    Sum,

    // Evaluates each Argument in turn, returning early if any is an Err,
    // returning Ok with last result otherwise.
    // TODO: Sequential,
    // Evaluates the first argument, then uses it as an index into the remaining arguments.
    // TODO: option 1: if 0, don't run anything.  1, 2, etc. index the arguments directly as $1, $2, etc.
    // TODO: option 2: if 0, run argument $1.  1, 2, etc. index the arguments at offset $2, $3, etc.
    // TODO: RunIndex,
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

    // TODO: UiScale (1,2,3,4).  TODO: make it a percentage?  e.g., `ui/scale% 100` for scale=1, etc.
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

    // TODO: Shift: moves the animation over one to start one frame down
    /// Returns the current mode, changing it to what's in $0 if present and valid.
    Mode,

    /// Versions of quit, see enum `Quit`.
    Quit(Quit),
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

impl fmt::Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Command::If => write!(f, "if"),
            Command::Even => write!(f, "even"),
            Command::Odd => write!(f, "odd"),
            Command::Sum => write!(f, "sum"),
            Command::Evaluate(value) => write!(f, "{}", value),
            Command::SetVariable => write!(f, "set"),
            Command::ConstVariable => write!(f, "const"),
            Command::CreateAlias => write!(f, "alias"),
            Command::ForegroundColor => write!(f, "fg"),
            Command::BackgroundColor => write!(f, "bg"),
            Command::Paint => write!(f, "paint"),
            Command::Mode => write!(f, "mode"),
            Command::Quit(Quit::Safe) => write!(f, "q"),
            Command::Quit(Quit::AllSafe) => write!(f, "qa"),
            Command::Quit(Quit::Forced) => write!(f, "q!"),
            Command::Quit(Quit::AllForced) => write!(f, "qa!"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct EmptyCommandParseError;

impl FromStr for Command {
    type Err = EmptyCommandParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.trim() {
            "if" => Ok(Command::If),
            "even" => Ok(Command::Even),
            "odd" => Ok(Command::Odd),
            "sum" => Ok(Command::Sum),
            "set" => Ok(Command::SetVariable),
            "const" => Ok(Command::ConstVariable),
            "alias" => Ok(Command::CreateAlias),
            "fg" => Ok(Command::ForegroundColor),
            "bg" => Ok(Command::BackgroundColor),
            "paint" => Ok(Command::Paint),
            "mode" => Ok(Command::Mode),
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

#[derive(PartialEq, Debug, Clone)]
pub struct Script {
    pub command: Command,
    pub arguments: Arguments,
}

impl Script {
    fn run(&self, runner: &mut dyn ScriptRunner) -> ArgumentResult {
        runner.run(vec![self])
    }

    // TODO: add method to print Script back as a command sequence
}

pub type Arguments = Vec<Argument>;
pub type ArgumentResult = Result<Argument, String>;

#[derive(PartialEq, Debug, Clone)]
pub enum Argument {
    // Value-based arguments.
    Null,

    // TODO: for Scale, use an I64 with the number of pixels desired in the X/Y direction.
    // E.g., we can have PixelsX/PixelsY or width/height
    // TODO: convert "true"/"false" to 1/0 in an I64.
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

#[derive(PartialEq, Debug, Clone, Copy)]
pub struct Use {
    /// Index of argument on the script that we'll use to evaluate this argument.
    pub index: u32,
    /// Number of stacks to look back for the script to use.  Should be < 0.
    /// -1 implies looking back one script.
    // Note we're using a negative number to more easily distinguish from index.
    pub lookback: i32,
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

    pub fn get_i64(&self, what_for: &str) -> I64Result {
        // TODO: add parsing for strings, etc.
        match self {
            Argument::Null => Ok(0),
            Argument::I64(value) => return Ok(*value),
            result => return Err(format!("invalid i64 {}: {}", what_for, result)),
        }
    }

    pub fn get_string(&self, what_for: &str) -> StringResult {
        // TODO: add parsing for other things.
        match self {
            Argument::Null => Ok("".to_string()),
            Argument::String(value) => return Ok(value.clone()),
            result => return Err(format!("invalid string {}: {}", what_for, result)),
        }
    }
}

pub type I64Result = Result<i64, String>;
pub type StringResult = Result<String, String>;

impl fmt::Display for Argument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
            Self::I64(value) => write!(f, "{}", *value),
            Self::Color(value) => write!(f, "{}", *value),
            Self::String(value) => write!(f, "'{}'", *value),
            Self::Script(script) => {
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
            Self::Use(use_argument) => write!(f, "${}", use_argument.index),
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
            // TODO: could add common methods (if we wanted).
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
                        let argument_count = script_stack.last().unwrap().arguments.len() as u32;
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
                    Command::ForegroundColor => {
                        let color_arg = self.script_evaluate(&script_stack, Evaluate::Index(0))?;
                        get_or_set_color(&mut self.fg, &self.palette, color_arg)
                    }
                    Command::BackgroundColor => {
                        let color_arg = self.script_evaluate(&script_stack, Evaluate::Index(0))?;
                        get_or_set_color(&mut self.bg, &self.palette, color_arg)
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
                        _ = get_or_set_color(&mut color, &self.palette, color_arg)?;

                        self.script_paint(x, y, color)
                    }
                    Command::Mode => {
                        let mode = self
                            .script_evaluate(&script_stack, Evaluate::Index(0))?
                            .get_string("for mode")?;
                        Ok(Argument::String(self.get_or_set_mode(mode)?))
                    }
                    Command::Quit(q) => {
                        self.script_quit(q);
                        Ok(Argument::Null)
                    }
                };
                self.end_script_command(command);
                result
            }
        }
    };
}

/// Essentially a getter/setter for color, with a Null argument making
/// this behave as a getter and a non-Null argument as a setter, though
/// we return the old value as we swap in the new value.
/// We will use the palette in case an integer argument is passed in.
pub fn get_or_set_color(
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
        arg => Err(format!("invalid argument to get_or_set_color: {}", arg)),
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
                assert!(use_argument.lookback < 0);
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
    BuiltIn,
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

    pub fn with_built_ins() -> Self {
        // TODO: should probably add a few const variables like `null`, etc.
        let mut variables = Variables::new();
        variables.set("if".to_string(), Variable::BuiltIn);
        variables.set("even".to_string(), Variable::BuiltIn);
        variables.set("odd".to_string(), Variable::BuiltIn);
        variables.set("sum".to_string(), Variable::BuiltIn);
        variables.set("set".to_string(), Variable::BuiltIn);
        variables.set("const".to_string(), Variable::BuiltIn);
        variables.set("alias".to_string(), Variable::BuiltIn);
        variables.set("fg".to_string(), Variable::BuiltIn);
        variables.set("bg".to_string(), Variable::BuiltIn);
        variables.set("paint".to_string(), Variable::BuiltIn);
        variables.set("mode".to_string(), Variable::BuiltIn);
        variables.set("quit".to_string(), Variable::BuiltIn);
        variables.set("?".to_string(), Variable::BuiltIn);
        variables.set("run".to_string(), Variable::BuiltIn);

        variables.set("quit".to_string(), Variable::Alias("q".to_string()));
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
                        Variable::BuiltIn => {
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
                Variable::BuiltIn => {
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
        fg: Rgba8,
        bg: Rgba8,
        palette: Palette,
        test_painted: Vec<Painted>,
    }

    script_runner! {TestRunner}
    impl TestRunner {
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
                test_what_ran: Vec::new(),
                variables: Variables::with_built_ins(),
                fg: Rgba8::WHITE,
                bg: Rgba8::BLACK,
                test_painted: vec![],
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
            self.test_painted.push(Painted { x, y, color });
            // NOTE! It is not required to return this color, you could do
            // something fancier (e.g., return the color that was under the cursor).
            Ok(Argument::Color(color))
        }

        fn get_or_set_mode(&mut self, mode: String) -> StringResult {
            // Actually switch modes
            Ok(mode)
        }

        fn script_quit(&mut self, _quit: Quit) {
            // Actually quit
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
    fn test_script_can_look_back_multiple_script_arguments() {
        // TODO: test parsing from `(if (even (sum $0 $1)) color0 color1)`
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

        let mut result = script.run(&mut test_runner);
        assert!(result.is_ok());
        assert_eq!(
            result.ok(),
            Some(Argument::Color(test_runner.palette.colors[5]))
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
                WhatRan::End(Command::Paint),
                WhatRan::End(Command::Evaluate(function2.clone())),
                WhatRan::End(Command::Evaluate(function1.clone())),
            ]
        );
        assert_eq!(
            test_runner.test_painted,
            vec![Painted {
                x: 37,
                y: 10,
                color: test_runner.palette.colors[5]
            }],
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
            variables.set(
                "paint".to_string(),
                Variable::Alias("super-paint".to_string()),
            ),
            Err("built-in `paint` is not reassignable".to_string())
        );
        assert_eq!(
            variables.set("const".to_string(), Variable::BuiltIn,),
            Err("built-in `const` is not reassignable".to_string())
        );
    }

    #[test]
    fn test_get_or_set_color_works_with_palette() {
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
            get_or_set_color(&mut color, &palette, Argument::I64(-1)),
            Err("no palette color with index -1".to_string())
        );
        assert_eq!(color, trailing_color); // no change

        // being within palette size:
        assert_eq!(
            get_or_set_color(&mut color, &palette, Argument::I64(1)),
            Ok(Argument::Color(trailing_color))
        );
        assert_eq!(color, palette.colors[1]);
        trailing_color = color;

        // going over the palette size:
        assert_eq!(
            get_or_set_color(&mut color, &palette, Argument::I64(2)),
            Err("no palette color with index 2".to_string())
        );
        assert_eq!(color, trailing_color); // no change

        // another test within palette size:
        assert_eq!(
            get_or_set_color(&mut color, &palette, Argument::I64(0)),
            Ok(Argument::Color(trailing_color))
        );
        assert_eq!(color, palette.colors[0]);
    }

    #[test]
    fn test_get_or_set_color_works_with_pure_colors() {
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
            get_or_set_color(&mut color, &palette, Argument::Color(new_color)),
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
            get_or_set_color(&mut color, &palette, Argument::Color(new_color)),
            Ok(Argument::Color(trailing_color))
        );
        assert_eq!(color, new_color);
    }

    #[test]
    fn test_get_or_set_color_returns_current_color_with_null_argument() {
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
            get_or_set_color(&mut color, &palette, Argument::Null),
            Ok(Argument::Color(initial_color))
        );
        assert_eq!(color, initial_color);
    }

    #[test]
    fn test_get_or_set_color_returns_error_with_invalid_arguments() {
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

        // String doesn't work.  TODO: would it be good to add e.g., 'red', 'blue', etc. someday?
        assert_eq!(
            get_or_set_color(&mut color, &palette, Argument::String("asdf".to_string())),
            Err("invalid argument to get_or_set_color: 'asdf'".to_string())
        );
        assert_eq!(color, initial_color);

        // Script doesn't work:
        assert_eq!(
            get_or_set_color(
                &mut color,
                &palette,
                Argument::Script(Script {
                    command: Command::ForegroundColor,
                    arguments: vec![]
                })
            ),
            Err("invalid argument to get_or_set_color: {command: `fg`, arguments: []}".to_string())
        );
        assert_eq!(color, initial_color);

        // Use doesn't work:
        assert_eq!(
            get_or_set_color(
                &mut color,
                &palette,
                Argument::Use(Use {
                    index: 1234,
                    lookback: -1
                })
            ),
            Err("invalid argument to get_or_set_color: $1234".to_string())
        );
        assert_eq!(color, initial_color);
    }

    #[test]
    fn test_command_parsing() {
        assert_eq!(Command::from_str("if"), Ok(Command::If));
        assert_eq!(Command::from_str("even"), Ok(Command::Even));
        assert_eq!(Command::from_str("odd"), Ok(Command::Odd));
        assert_eq!(Command::from_str("sum"), Ok(Command::Sum));
        assert_eq!(Command::from_str("set"), Ok(Command::SetVariable));
        assert_eq!(Command::from_str("const"), Ok(Command::ConstVariable));
        assert_eq!(Command::from_str("alias"), Ok(Command::CreateAlias));
        assert_eq!(Command::from_str("fg"), Ok(Command::ForegroundColor));
        assert_eq!(Command::from_str("bg"), Ok(Command::BackgroundColor));
        assert_eq!(Command::from_str("paint"), Ok(Command::Paint));
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
}
