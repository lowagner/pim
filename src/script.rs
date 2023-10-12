use crate::gfx::Rgba8;

use std::fmt;

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

#[derive(PartialEq, Debug, Clone)]
pub enum Command {
    // A sort of throw-away command to make going through arguments
    // from a script stack more easy.  Do not use outside of this file.
    // See `Script::run` and `evaluate(...)` for more info.
    // The associated arguments for this command are the arguments that
    // the user passes in to run the script.
    Root,

    If,
    // Runs each inner Command in turn, returning early if any is an Err,
    // returning Ok with last result otherwise.
    // TODO: Sequential(Vec<Command>),

    // Gets the value of a variable that is known to the compiler; evaluates it.
    Evaluate(String),
    // Gets the value of a named variable at $0; technically evaluating it.
    // TODO: this won't work well with a Script due to an offset that doesn't happen with Evaluate.
    //      E.g., `:get 'my-fn' Arg1 Arg2` will be shifted from `$my-fn Arg1 Arg2`.
    //      $0 will refer to 'my-fn' in the former, while $0 will refer to Arg1 in the latter.
    //      let's keep $0 as the first argument (Arg1), so we'll need to copy the argument array for `GetVariable`.
    //      probably the easiest thing to do is "LoadVariableName" and "EvaluateVariableName" with some extra state.
    // GetVariable,
    // Sets the value of a named variable at $0 to $1
    SetVariable,

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
            Self::ForegroundColor => write!(f, "fg"),
            Self::BackgroundColor => write!(f, "bg"),
            Self::Quit => write!(f, "quit"),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Script {
    command: Command,
    arguments: Arguments,
}

impl Script {
    fn run(&self, runner: &mut dyn ScriptRunner, arguments: Arguments) -> ArgumentResult {
        let root = Script { command: Command::Root, arguments };
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
    // Note that these are lazily evaluated and never memoized, so this script
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
                let mut check = write!(
                    f,
                    "Script {{command: {}, arguments: Vec2::from([",
                    script.command
                );
                if check.is_err() {
                    return check;
                }
                for arg in &script.arguments {
                    check = write!(f, "{}, ", arg);
                    if check.is_err() {
                        return check;
                    }
                }
                write!(f, "])}}")
            }
            _ => write!(f, "???"),
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
// Note we purposely move/copy the script stack because all sorts
// of redirection are possible, i.e., due to `Argument::Use`
// or additional nested scripts being run via `Argument::Script`.
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
            },
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
            _ => panic!(
                "Invalid argument, not evaluatable: {}",
                *argument
            ),
        }
    }
    Err("external arguments to script should not include $0, $1, etc.".to_string())
}

#[cfg(test)]
mod test {
    use super::*;
    use std::collections::HashMap;

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
        variables: HashMap<String, Argument>,
    }

    impl ScriptRunner for TestRunner {
        fn run(&mut self, script_stack: Vec<&Script>) -> ArgumentResult {
            if script_stack.len() == 0 { return Ok(Argument::Null); }
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
                },
                Command::Evaluate(name) => {
                    // Need to make a clone here since executing commands could
                    // modify `self`, which could break the reference here.
                    let argument: Argument = self.get_variable(name);
                    // No need to double up on WhatRan, just run directly:
                    evaluate(self, &script_stack, Evaluate::Argument(&argument))
                },
                Command::SetVariable => {
                    let maybe_name = self.evaluate(&script_stack, Evaluate::Index(0));
                    if maybe_name.is_err() {
                        return maybe_name;
                    }
                    let name_argument = maybe_name.unwrap();
                    match name_argument {
                        // Note that variables[name] could technically be a Script.
                        // You can call `get "var-name"` with extra arguments,
                        // e.g., `get "var-name" 1 #aabbcc` and do something with them
                        // if e.g., `set "var-name" (echo $0 ; echo $1)`.  I.e., this
                        // is the way we create new commands.
                        Argument::String(name) => {
                            self.variables.insert(
                                name,
                                if script.arguments.len() >= 2 {
                                    script.arguments[1].clone()
                                } else {
                                    Argument::Null
                                },
                            );
                            Ok(Argument::Null)
                        }
                        _ => Err("Invalid type for variable name".to_string()),
                    }
                },
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
                variables: HashMap::new(),
            }
        }

        fn get_variable(&self, name: String) -> Argument {
            self.variables
                    .get(&name)
                    .map_or(Argument::Null, |o| o.clone())
        }

        // You probably don't need to wrap the `evaluate` function.
        // We do it for tests to get `WhatRan`.
        fn evaluate(
            &mut self,
            script_stack: &Vec<&Script>,
            evaluate_this: Evaluate
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
            .insert(variable_name.clone(), Argument::I64(1));
        let mut result = script.run(&mut test_runner, arguments.clone());
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some(Argument::Color(Rgba8::BLACK)));

        test_runner
            .variables
            .insert(variable_name.clone(), Argument::I64(0));
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
}
