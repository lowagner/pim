use crate::gfx::Rgba8;

use std::fmt;

/*
TODO: how is parsing going to work here?
TODO: how do we pull in arguments to a new command?  cmd-line syntax: `$0`, `$1`, etc.

        :command 'cmd_name' (the_cmd $0 123)
    =>  :cmd_name 5     -- calls `the_cmd` with arguments `5` `123`

        $0          -- evaluate argument at index 0.  similarly for $1, etc.

        run_index   -- evaluates next argument, uses it as index as to which argument to run
                    -- (index starts at 0 for the first argument after the argument index.)
                    -- afterwards, sets the argument index to the end.
        truthy      -- returns 1 if next argument is truthy otherwise 0

    based on the above, the "optional else" `if` can be built like this:
        -- note the truthy clause happens second since `run (truthy $0)`
        -- will run argument 0 for false and argument 1 for truthy:

        :command 'if' ( run (truthy $0) $2 $1 )

    notice that "next argument" or "skip argument" do *not* apply to
    the script's list of arguments; they apply to arguments being supplied
    to the script externally.

TODO: nested scripts may become confusing.  we probably need to map indices for the external arguments.
    For example:
        :command 'paint5' (paint $0 $1 5)
        :command 'dot5' paint5 $2 $0

    needs to map external argument 2 to paint's argument 0, external argument 0 to paint's argument 1.

        paint5: Script { command: Paint, arguments: [Use(0), Use(1), I64(5)] }
        dot5: Script { command: Evaluate("paint5"), arguments: [Use(2), Use(0)] }

    note that we need to evaluate paint5.arguments with dot5.arguments as "external"
    but then dot5.arguments need to link to truly external arguments.

    could just push to a script stack:

        root: Script { command: Root, arguments [external-arguments] }
        [root, dot5, paint5].

TODO: we should maybe use variables for writing commands.
e.g., `:set 'if' (run (truth $0) $2 $1)` should be fine.
then we can do `:get 'if' (conditional) (truthy) (falsey)` or just `if (conditional) (truthy) (falsy)`

TODO: make push have an automatic pop at the end of a script runner.
*/

#[derive(PartialEq, Debug, Clone)]
pub enum Command {
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
    fn run(&self, runner: &mut dyn ScriptRunner, arguments: &Arguments) -> ArgumentResult {
        runner.run(self, arguments)
    }
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
    // Note that the `external_arguments` should *not* have any `Argument::Use` arguments;
    // But that `script.arguments` can have these, which will use the external arguments.
    fn run(&mut self, script: &Script, external_arguments: &Arguments) -> ArgumentResult;
}

pub fn evaluate_script_argument(
    runner: &mut dyn ScriptRunner,
    script: &Script,
    argument_index: usize,
    external_arguments: &Arguments,
) -> ArgumentResult {
    if argument_index >= script.arguments.len() {
        return Ok(Argument::Null);
    }
    return evaluate_argument(
        runner,
        &script.arguments[argument_index],
        external_arguments,
    );
}

pub fn evaluate_argument(
    runner: &mut dyn ScriptRunner,
    script_argument: &Argument,
    external_arguments: &Arguments,
) -> ArgumentResult {
    if script_argument.is_value() {
        return Ok(script_argument.clone());
    }

    let mut evaluatable_argument = Argument::Null;
    match &script_argument {
        Argument::Script(nested_script) => {
            let result = runner.run(&nested_script, external_arguments);
            if result.is_err() {
                // TODO: add test for this.
                return result;
            }
            evaluatable_argument = result.unwrap();
        }
        Argument::Use(external_index) => {
            let result = evaluate_external_argument(runner, external_arguments, *external_index);
            if result.is_err() {
                // TODO: add test for this.
                return result;
            }
            evaluatable_argument = result.unwrap();
        }
        _ => panic!(
            "Invalid argument, not evaluatable: {}",
            evaluatable_argument
        ),
    }
    assert!(
        evaluatable_argument.is_value(),
        "Nested scripts not supported ATM"
    );
    return Ok(evaluatable_argument);
}

// Purposely private.  This should only be called via `evaluate_script_argument`.
fn evaluate_external_argument(
    runner: &mut dyn ScriptRunner,
    external_arguments: &Arguments,
    external_index: usize,
) -> ArgumentResult {
    if external_index >= external_arguments.len() {
        return Ok(Argument::Null);
    }
    if external_arguments[external_index].is_value() {
        return Ok(external_arguments[external_index].clone());
    }

    match &external_arguments[external_index] {
        Argument::Script(nested_script) => runner.run(&nested_script, external_arguments),
        Argument::Use(_) => panic!("External arguments should not have Argument::Use"),
        _ => panic!(
            "Invalid argument, not evaluatable: {}",
            external_arguments[external_index]
        ),
    }
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
        fn run(&mut self, script: &Script, arguments: &Arguments) -> ArgumentResult {
            let command = script.command.clone();
            self.test_what_ran.push(WhatRan::Command(command.clone()));
            match command {
                Command::If => {
                    let conditional = self.evaluate_argument(script, 0, arguments);
                    if conditional.is_err() {
                        conditional
                    } else if conditional.unwrap().is_truthy() {
                        self.evaluate_argument(script, 1, arguments)
                    } else {
                        self.evaluate_argument(script, 2, arguments)
                    }
                }
                Command::Evaluate(name) => {
                    let variable = self.get_variable(name);
                    evaluate_argument(self, &variable, arguments)
                }
                Command::SetVariable => {
                    let maybe_name = self.evaluate_argument(script, 0, arguments);
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
                                if arguments.len() >= 2 {
                                    arguments[1].clone()
                                } else {
                                    Argument::Null
                                },
                            );
                            Ok(Argument::Null)
                        }
                        _ => Err("Invalid type for variable name".to_string()),
                    }
                }
                Command::ForegroundColor => self.evaluate_argument(script, 0, arguments),
                Command::BackgroundColor => self.evaluate_argument(script, 0, arguments),
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

        // You probably don't need to wrap the `evaluate_argument` function.
        // We do it for tests to get `WhatRan`.
        fn evaluate_argument(
            &mut self,
            script: &Script,
            index: usize,
            external_arguments: &Arguments,
        ) -> ArgumentResult {
            let result = evaluate_script_argument(self, script, index, external_arguments);
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
        let result = script.run(&mut test_runner, &arguments);

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
        let result = script.run(&mut test_runner, &arguments);

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
        let result = script.run(&mut test_runner, &arguments);

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
        let result = script.run(&mut test_runner, &arguments);

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
        let result = script.run(&mut test_runner, &arguments);

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
        let result = script.run(&mut test_runner, &arguments);

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
        let result = script.run(&mut test_runner, &arguments);

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
        let result = script.run(&mut test_runner, &arguments);

        test_runner
            .variables
            .insert(variable_name.clone(), Argument::I64(0));
        let result = script.run(&mut test_runner, &arguments);

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
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some(Argument::Color(Rgba8::RED)));
    }
}
