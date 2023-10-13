use crate::gfx::Rgba8;
use crate::palette::Palette;

use std::collections::HashMap;
use std::fmt;
use std::mem;

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

        :set 'if' ( run (truthy $0) $2 $1 )

    then we can do `if (conditional) (truthy) (falsy)`.

TODO: we should probably have an error if people try to overwrite 'if', 'set', etc.

TODO: make push have an automatic pop at the end of a script runner.
*/

/// The commands that are possible in a script.
// Note these are designed to be quickly cloned, so don't put large amounts of data in them.
#[derive(PartialEq, Debug, Clone)]
pub enum Command {
    // A sort of throw-away command to make going through arguments
    // from a script stack more easy.  Do not use outside of this file.
    // See `Script::run` and `evaluate(...)` for more info.
    // The associated arguments for this command are the arguments that
    // the user passes in to run the script.
    Root,

    If,
    // Evaluates each Argument in turn, returning early if any is an Err,
    // returning Ok with last result otherwise.
    // TODO: Sequential,
    // Evaluates the first argument, then uses it as an index into the remaining arguments.
    // TODO: option 1: if 0, don't run anything.  1, 2, etc. index the arguments directly as $1, $2, etc.
    // TODO: option 2: if 0, run argument $1.  1, 2, etc. index the arguments at offset $2, $3, etc.
    // TODO: RunIndex,
    /// Gets the value of a variable that is known to the compiler; evaluates it.
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

    // TODO: UiScale (1,2,3,4)
    // TODO: FitPixelWidth, FitPixelHeight; zoom to fit that many pixels within the screen based on available area
    ForegroundColor,
    BackgroundColor,

    // TODO: Shift: moves the animation over one to start one frame down
    // TODO: maybe a `Lookup` command.
    Quit,
}

impl fmt::Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Root => write!(f, ":"),
            Self::If => write!(f, "if"),
            Self::Evaluate(value) => write!(f, "${}", value),
            Self::SetVariable => write!(f, "set"),
            Self::ConstVariable => write!(f, "const"),
            Self::CreateAlias => write!(f, "alias"),
            Self::ForegroundColor => write!(f, "fg"),
            Self::BackgroundColor => write!(f, "bg"),
            Self::Quit => write!(f, "quit"),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Script {
    pub command: Command,
    pub arguments: Arguments,
}

impl Script {
    fn run(&self, runner: &mut dyn ScriptRunner, arguments: Arguments) -> ArgumentResult {
        let root = Script {
            command: Command::Root,
            arguments,
        };
        runner.run(vec![&root, self])
    }

    // TODO: add method to print Script back as a command sequence
}

pub type Arguments = Vec<Argument>;
pub type ArgumentResult = Result<Argument, String>;

#[derive(PartialEq, Debug, Clone)]
pub enum Argument {
    // Value-based arguments.
    Null,
    // TODO: convert "true"/"false" to 1/0 in an I64.
    I64(i64),
    // TODO: for Scale, use an I64 with the number of pixels desired in the X/Y direction.
    // E.g., we can have PixelsX/PixelsY or width/height
    String(String),
    Color(Rgba8),

    // Non-value-based (AKA evaluatable) arguments follow.
    // TODO: we probably can have a zero-arg Variable here,
    //      e.g., Variable(String) which will be executed without any arguments.
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
    Use(usize),
}

impl Argument {
    pub fn is_value(&self) -> bool {
        !matches!(self, Self::Script(_) | Self::Use(_))
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Null => false,
            Self::I64(value) => *value != 0,
            Self::String(value) => *value != "",
            Self::Color(value) => *value != Rgba8::TRANSPARENT,
            _ => panic!("unimplemented"),
        }
    }
}

impl fmt::Display for Argument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
            Self::I64(value) => write!(f, "{}", *value),
            Self::String(value) => write!(f, "'{}'", *value),
            Self::Color(value) => write!(f, "{}", *value),
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
            Self::Use(value) => write!(f, "${}", *value),
        }
    }
}

// We're using this instead of a closure because rust closures are hard to type correctly.
pub trait ScriptRunner {
    /// Executes the script with external arguments in `external_arguments`.
    // Executes the script on the top of the stack, using lower parts of the stack
    // to trace `Argument::Use` arguments.  Note that the lowest script in the stack
    // should *not* have any `Argument::Use` arguments.  We purposely move/copy this
    // stack because all sorts of redirection is possible, i.e., due to `Argument::Use`
    // or additional nested scripts being run via `Argument::Script`.
    fn run(&mut self, script_stack: Vec<&Script>) -> ArgumentResult;
}

/// Essentially a getter/setter for color, with a Null argument making
/// this behave as a getter and a non-Null argument as a setter, though
/// we return the old value as we swap in the new value.
/// We will use the palette in case an integer argument is passed in.
pub fn get_or_set_color(
    maybe_set_color: &mut Rgba8,
    palette: &Palette,
    color_result: ArgumentResult,
) -> ArgumentResult {
    if color_result.is_err() {
        return color_result;
    }
    match color_result.unwrap() {
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
    Index(usize),
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
//    let paint5 = Script { command: Paint, arguments: [Use(0), Use(1), I64(5)] };
//    let dot5 = Script { command: Evaluate("paint5"), arguments: [Use(2), Use(0)] };
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
    let mut script_index: usize = script_stack.len();
    let mut iteration = 0;
    while script_index > 0 {
        script_index -= 1;
        iteration += 1;
        if iteration > 1000 {
            return Err("script took too many iterations".to_string());
        }
        let argument: &Argument = match evaluate_this {
            Evaluate::Argument(evaluate_this_argument) => evaluate_this_argument,
            Evaluate::Index(evaluate_this_index) => {
                let script: &Script = &script_stack[script_index];
                if evaluate_this_index >= script.arguments.len() {
                    return Ok(Argument::Null);
                }
                &script.arguments[evaluate_this_index]
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
            Argument::Use(previous_stack_argument_index) => {
                evaluate_this = Evaluate::Index(*previous_stack_argument_index);
            }
            // is_value() and this matcher should be disjoint.
            _ => panic!("Invalid argument, not evaluatable: {}", *argument),
        }
    }
    Err("external arguments to script should not include $0, $1, etc.".to_string())
}

#[derive(PartialEq, Debug, Clone)]
pub enum Variable {
    // A variable that can be reassigned:
    Mutable(Argument),
    // A variable that can't be reassigned:
    Const(Argument),
    // To look up a variable and remap it:
    Alias(String),
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
        // TODO: should probably add a few const variables like `null`, etc.
        Self {
            map: HashMap::new(),
        }
    }

    /// Returns the Argument represented by this name, returning Null if not present in the map.
    // Notice that we resolve aliases here so that you'll definitely get an argument.
    pub fn get(&self, mut name: String) -> Argument {
        for _iteration in 1..24 {
            match &self.map.get(&name) {
                None => return Argument::Null,
                Some(variable) => match variable {
                    Variable::Const(arg) => return arg.clone(),
                    Variable::Mutable(arg) => return arg.clone(),
                    Variable::Alias(alias) => {
                        name = alias.clone();
                    }
                },
            }
        }
        eprint!("don't nest aliases this much!\n");
        return Argument::Null;
    }

    /// Sets the variable, returning the old value if it was present in the map.
    /// WARNING! Will return an error if the variable is const.
    pub fn set(&mut self, name: String, variable: Variable) -> ArgumentResult {
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
                _ => return Err(format!("variable `{}` is not reassignable", name)),
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
                command: Command::Quit,
                arguments: vec![]
            })
            .is_value(),
            false
        );
        assert_eq!(Argument::Use(3).is_value(), false);
    }

    #[derive(PartialEq, Debug, Clone)]
    enum WhatRan {
        Command(Command),
        Argument(ArgumentResult),
    }

    struct TestRunner {
        test_what_ran: Vec<WhatRan>,
        variables: Variables,
    }

    impl ScriptRunner for TestRunner {
        fn run(&mut self, script_stack: Vec<&Script>) -> ArgumentResult {
            if script_stack.len() == 0 {
                return Ok(Argument::Null);
            }
            let script: &Script = &script_stack[script_stack.len() - 1];
            let command = script.command.clone();
            self.test_what_ran.push(WhatRan::Command(command.clone()));
            match command {
                Command::Root => panic!("root command should not be run"),
                Command::If => {
                    let conditional = self.evaluate(&script_stack, Evaluate::Index(0));
                    if conditional.is_err() {
                        conditional
                    } else if conditional.unwrap().is_truthy() {
                        self.evaluate(&script_stack, Evaluate::Index(1))
                    } else {
                        self.evaluate(&script_stack, Evaluate::Index(2))
                    }
                }
                Command::Evaluate(name) => {
                    // Need to make a clone here since executing commands could
                    // modify `self`, which could break the reference here.
                    let argument: Argument = self.variables.get(name);

                    // No need to double up on WhatRan, just run directly:
                    evaluate(self, &script_stack, Evaluate::Argument(&argument))
                }
                // Note that variables can technically be `Script`s.
                // E.g., if we do `set "var-name" (echo $0 ; echo $1)`, then we
                // can call with `var-name 'hello' 'world'`.  This is the way
                // we create new commands.
                Command::SetVariable => self.variables.set_from_script(&script),
                Command::ConstVariable => self.variables.set_from_script(&script),
                Command::CreateAlias => self.variables.set_from_script(&script),
                Command::ForegroundColor => self.evaluate(&script_stack, Evaluate::Index(0)),
                Command::BackgroundColor => self.evaluate(&script_stack, Evaluate::Index(0)),
                Command::Quit => Ok(Argument::Null),
            }
        }
    }

    impl TestRunner {
        fn new() -> Self {
            Self {
                test_what_ran: Vec::new(),
                variables: Variables::new(),
            }
        }

        // You probably don't need to wrap the `evaluate` function.
        // We do it for tests to get `WhatRan`.
        fn evaluate(
            &mut self,
            script_stack: &Vec<&Script>,
            evaluate_this: Evaluate,
        ) -> ArgumentResult {
            let result = evaluate(self, script_stack, evaluate_this);
            self.test_what_ran.push(WhatRan::Argument(result.clone()));
            result
        }
    }

    #[test]
    fn test_script_run_ok() {
        let script = Script {
            command: Command::Quit,
            arguments: Vec::new(),
        };

        let mut test_runner = TestRunner::new();
        let arguments = vec![];
        let result = script.run(&mut test_runner, arguments);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([WhatRan::Command(Command::Quit)])
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
        let arguments = vec![];
        let result = script.run(&mut test_runner, arguments);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Command(Command::ForegroundColor),
                WhatRan::Argument(Ok(Argument::Null)),
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
        let arguments = vec![];
        let result = script.run(&mut test_runner, arguments);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Command(Command::If),
                WhatRan::Argument(Ok(Argument::I64(1))),
                WhatRan::Command(Command::ForegroundColor),
                WhatRan::Argument(Ok(Argument::Color(Rgba8::RED))),
                // This second one comes as a result of returning this from the `if`
                WhatRan::Argument(Ok(Argument::Color(Rgba8::RED))),
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
        let arguments = vec![];
        let result = script.run(&mut test_runner, arguments);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Command(Command::If),
                WhatRan::Argument(Ok(Argument::I64(0))),
                WhatRan::Argument(Ok(Argument::Null)),
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
        let arguments = vec![];
        let result = script.run(&mut test_runner, arguments);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Command(Command::If),
                WhatRan::Argument(Ok(Argument::I64(0))),
                WhatRan::Argument(Ok(Argument::Null)),
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
        let arguments = vec![];
        let result = script.run(&mut test_runner, arguments);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Command(Command::If),
                WhatRan::Argument(Ok(Argument::I64(0))),
                WhatRan::Argument(Ok(Argument::Color(Rgba8::BLUE))),
            ])
        );
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some(Argument::Color(Rgba8::BLUE)));
    }

    #[test]
    fn test_script_allows_external_arguments() {
        let script = Script {
            command: Command::If,
            arguments: Vec::from([Argument::I64(0), Argument::Use(0), Argument::Use(1)]),
        };

        let mut test_runner = TestRunner::new();
        let arguments = vec![
            Argument::Script(Script {
                command: Command::ForegroundColor,
                arguments: vec![Argument::Color(Rgba8::BLACK)],
            }),
            Argument::Script(Script {
                command: Command::BackgroundColor,
                arguments: vec![Argument::Color(Rgba8::RED)],
            }),
        ];
        let result = script.run(&mut test_runner, arguments);

        assert_eq!(
            test_runner.test_what_ran,
            vec![
                WhatRan::Command(Command::If),
                WhatRan::Argument(Ok(Argument::I64(0))),
                WhatRan::Command(Command::BackgroundColor),
                WhatRan::Argument(Ok(Argument::Color(Rgba8::RED))),
                WhatRan::Argument(Ok(Argument::Color(Rgba8::RED)))
            ]
        );
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some(Argument::Color(Rgba8::RED)));
    }

    #[test]
    fn test_script_allows_variable_arguments() {
        let variable_name = "cabbage".to_string();
        let script = Script {
            command: Command::If,
            arguments: Vec::from([
                Argument::Script(Script {
                    command: Command::Evaluate(variable_name.clone()),
                    arguments: vec![],
                }),
                Argument::Use(0),
                Argument::Use(1),
            ]),
        };
        let mut test_runner = TestRunner::new();
        let arguments = vec![
            Argument::Script(Script {
                command: Command::ForegroundColor,
                arguments: vec![Argument::Color(Rgba8::BLACK)],
            }),
            Argument::Script(Script {
                command: Command::BackgroundColor,
                arguments: vec![Argument::Color(Rgba8::RED)],
            }),
        ];

        test_runner
            .variables
            .set(variable_name.clone(), Variable::Mutable(Argument::I64(1)));
        let mut result = script.run(&mut test_runner, arguments.clone());
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some(Argument::Color(Rgba8::BLACK)));

        test_runner
            .variables
            .set(variable_name.clone(), Variable::Mutable(Argument::I64(0)));
        result = script.run(&mut test_runner, arguments);
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some(Argument::Color(Rgba8::RED)));

        assert_eq!(
            test_runner.test_what_ran,
            vec![
                // First run:
                WhatRan::Command(Command::If),
                WhatRan::Command(Command::Evaluate(variable_name.clone())),
                WhatRan::Argument(Ok(Argument::I64(1))),
                WhatRan::Command(Command::ForegroundColor),
                WhatRan::Argument(Ok(Argument::Color(Rgba8::BLACK))),
                WhatRan::Argument(Ok(Argument::Color(Rgba8::BLACK))),
                // Second run:
                WhatRan::Command(Command::If),
                WhatRan::Command(Command::Evaluate(variable_name.clone())),
                WhatRan::Argument(Ok(Argument::I64(0))),
                WhatRan::Command(Command::BackgroundColor),
                WhatRan::Argument(Ok(Argument::Color(Rgba8::RED))),
                WhatRan::Argument(Ok(Argument::Color(Rgba8::RED))),
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
                    arguments: vec![Argument::Use(1)],
                }),
            ]),
        };
        let mut test_runner = TestRunner::new();

        let mut result = script.run(&mut test_runner, vec![]);
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
        result = script.run(&mut test_runner, vec![]);
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some(Argument::Color(Rgba8::BLUE)));

        assert_eq!(
            test_runner.test_what_ran,
            vec![
                // First run:
                WhatRan::Command(Command::SetVariable),
                // Second run:
                WhatRan::Command(Command::Evaluate(variable_name.clone())),
                WhatRan::Command(Command::ForegroundColor),
                WhatRan::Argument(Ok(Argument::Color(Rgba8::BLUE))),
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
                    arguments: vec![Argument::Use(2)],
                }),
            ]),
        };
        let mut test_runner = TestRunner::new();

        let mut result = script.run(&mut test_runner, vec![]);
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
        result = script.run(&mut test_runner, vec![]);
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some(Argument::Color(Rgba8::BLACK)));

        assert_eq!(
            test_runner.test_what_ran,
            vec![
                // First run:
                WhatRan::Command(Command::ConstVariable),
                // Second run:
                WhatRan::Command(Command::Evaluate(variable_name.clone())),
                WhatRan::Command(Command::BackgroundColor),
                WhatRan::Argument(Ok(Argument::Color(Rgba8::BLACK))),
            ]
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
        variables.set(
            alias_value.clone(),
            Variable::Mutable(aliased_value.clone()),
        );
        assert_eq!(variables.get(alias_name.clone()), aliased_value.clone());

        // Double check that we cannot reassign:
        script.arguments[1] = Argument::String("change-to-this".to_string());
        assert_eq!(
            variables.set_from_script(&script),
            Err(format!(
                "variable `{}` is not reassignable",
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
        variables.set(
            alias_value.clone(),
            Variable::Mutable(aliased_value.clone()),
        );
        assert_eq!(variables.get(alias_name), aliased_value);
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
            get_or_set_color(&mut color, &palette, Ok(Argument::I64(-1))),
            Err("no palette color with index -1".to_string())
        );
        assert_eq!(color, trailing_color); // no change

        // being within palette size:
        assert_eq!(
            get_or_set_color(&mut color, &palette, Ok(Argument::I64(1))),
            Ok(Argument::Color(trailing_color))
        );
        assert_eq!(color, palette.colors[1]);
        trailing_color = color;

        // going over the palette size:
        assert_eq!(
            get_or_set_color(&mut color, &palette, Ok(Argument::I64(2))),
            Err("no palette color with index 2".to_string())
        );
        assert_eq!(color, trailing_color); // no change

        // another test within palette size:
        assert_eq!(
            get_or_set_color(&mut color, &palette, Ok(Argument::I64(0))),
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
            get_or_set_color(&mut color, &palette, Ok(Argument::Color(new_color))),
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
            get_or_set_color(&mut color, &palette, Ok(Argument::Color(new_color))),
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
            get_or_set_color(&mut color, &palette, Ok(Argument::Null)),
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
            get_or_set_color(
                &mut color,
                &palette,
                Ok(Argument::String("asdf".to_string()))
            ),
            Err("invalid argument to get_or_set_color: 'asdf'".to_string())
        );
        assert_eq!(color, initial_color);

        // Script doesn't work:
        assert_eq!(
            get_or_set_color(
                &mut color,
                &palette,
                Ok(Argument::Script(Script {
                    command: Command::Root,
                    arguments: vec![]
                }))
            ),
            Err("invalid argument to get_or_set_color: {command: `:`, arguments: []}".to_string())
        );
        assert_eq!(color, initial_color);

        // Use doesn't work:
        assert_eq!(
            get_or_set_color(&mut color, &palette, Ok(Argument::Use(1234))),
            Err("invalid argument to get_or_set_color: $1234".to_string())
        );
        assert_eq!(color, initial_color);
    }
}
