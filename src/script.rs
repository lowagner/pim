use crate::gfx::Rgba8;

use std::fmt;

/*
TODO: how is parsing going to work here?
TODO: how do we pull in arguments to a new command?  cmd-line syntax: `$0`, `$1`, etc.
NOTE we need permanence of argument position (can't "next arg") because conditionals get confusing.
especially nested scripts which might get executed out of order.
TODO: we want a lazy evaluator: don't execute an Argument::Script unless needed, but then only once.
maybe replace inline from a mutable Vec of Arguments.

        :command 'cmd_name' (the_cmd $0 123)
    =>  :cmd_name 5     -- calls `the_cmd` with arguments `5` `123`

        $0          -- evaluate argument at index 0.  similarly for $1, etc.
        $0?         -- evaluate argument at index 1 if present.  similarly for $1?, etc.
        $#          -- number of arguments

        run_index   -- evaluates next argument, uses it as index as to which argument to run
                    -- (index starts at 0 for the first argument after the argument index.)
                    -- afterwards, sets the argument index to the end.
        truthy      -- returns 1 if next argument is truthy otherwise 0

    based on the above, the "optional else" `if` can be built like this:
        -- note the truthy clause happens second since `run (truthy $0)`
        -- will run argument 0 for false and argument 1 for truthy:

        :command 'if' ( run (truthy $0) $2? $1 )

    notice that "next argument" or "skip argument" do *not* apply to
    the script's list of arguments; they apply to arguments being supplied
    to the script externally.
*/

#[derive(PartialEq, Debug, Clone)]
pub enum Command {
    Quit,
    ForegroundColor,
    If,
    // TODO: Shift: moves the animation over one to start one frame down
}

impl fmt::Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Quit => write!(f, "quit"),
            Self::ForegroundColor => write!(f, "fg"),
            Self::If => write!(f, "if"),
            _ => write!(f, "???"),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Script {
    command: Command,
    arguments: Vec<Argument>,
}

impl Script {
    pub fn execute(&self, executor: &mut dyn ScriptExecutor) -> ArgumentResult {
        let mut runner = ScriptRunner::new(&self);
        runner.run(executor)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Argument {
    Null,
    // TODO: convert "true"/"false" to 1/0 in an I64.
    I64(i64),
    // TODO: for Scale, use an I64 with the number of pixels desired in the X/Y direction.
    // E.g., we can have PixelsX/PixelsY or width/height
    Str(String),
    Rgba8(Rgba8),
    // TODO: maybe we need an `Argument::End` for end-of-input.
    // TODO: we probably need an `Argument::SupplyNext` for user-created commands
    // TODO: we should be able to pause execution, e.g., for an alert box to confirm an action
    Script(Script),
}

impl fmt::Display for Argument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
            Self::I64(value) => write!(f, "{}", *value),
            Self::Str(value) => write!(f, "{}", *value),
            Self::Rgba8(value) => write!(f, "{}", *value),
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

impl Argument {
    pub fn is_value(&self) -> bool {
        !matches!(self, Self::Script(_))
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Null => false,
            Self::I64(value) => *value != 0,
            Self::Str(value) => *value != "",
            Self::Rgba8(value) => *value != Rgba8::TRANSPARENT,
            _ => panic!("unimplemented"),
        }
    }
}

pub type ArgumentResult = Result<Argument, String>;

// We're using this instead of a closure because rust closures are hard to type correctly.
pub trait ScriptExecutor {
    fn execute(&mut self, runner: &mut ScriptRunner) -> ArgumentResult;
}

pub struct ScriptRunner<'a> {
    script: &'a Script,
    argument_index: usize,
}

impl<'a> ScriptRunner<'a> {
    fn new(script: &'a Script) -> Self {
        Self {
            script,
            argument_index: 0,
        }
    }

    pub fn command(&self) -> Command {
        self.script.command.clone()
    }

    pub fn has_another_argument(&self) -> bool {
        self.argument_index < self.script.arguments.len()
    }

    pub fn skip_argument(&mut self) -> Result<(), String> {
        if self.argument_index < self.script.arguments.len() {
            self.argument_index += 1;
            Ok(())
        } else {
            return Err(format!("`{}` required another argument", self.command()));
        }
    }

    /// Ensures returning a non-Script-based Argument by executing
    /// any nested scripts embedded in the argument.
    pub fn evaluate_argument(&mut self, executor: &mut dyn ScriptExecutor) -> ArgumentResult {
        let mut next_argument = || {
            let current_argument_index = self.argument_index;
            if self.argument_index < self.script.arguments.len() {
                self.argument_index += 1;
                Ok(self.script.arguments[current_argument_index].clone())
            } else {
                Err(format!("`{}` required another argument", self.command()))
            }
        };
        let mut result = next_argument();
        while !result.is_err() {
            let argument = result.unwrap();
            match argument {
                Argument::Script(script) => {
                    result = script.execute(executor);
                }
                _ => return Ok(argument),
            }
        }
        result
    }

    fn run(&mut self, executor: &mut dyn ScriptExecutor) -> ArgumentResult {
        let result = executor.execute(self);

        if self.argument_index == self.script.arguments.len() {
            result
        } else {
            Err(format!(
                "`{}` did not use up all the arguments",
                self.command()
            ))
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[derive(PartialEq, Debug, Clone)]
    enum WhatRan {
        Command(Command),
        Argument(ArgumentResult),
    }

    struct TestExecutor {
        test_what_ran: Vec<WhatRan>,
    }

    impl ScriptExecutor for TestExecutor {
        fn execute(&mut self, runner: &mut ScriptRunner) -> ArgumentResult {
            let command = runner.command();
            self.test_what_ran.push(WhatRan::Command(command.clone()));
            match command {
                Command::Quit => Ok(Argument::Null),
                Command::ForegroundColor => self.evaluate_argument(runner),
                Command::If => {
                    // TODO: turn this into a macro
                    let conditional = self.evaluate_argument(runner);
                    if conditional.is_err() {
                        return conditional;
                    }
                    if conditional.unwrap().is_truthy() {
                        let result = self.evaluate_argument(runner);
                        if runner.has_another_argument() {
                            // To avoid errors with not using up arguments; i.e.,
                            // we support an optional `else` clause.
                            let check = runner.skip_argument();
                            // We checked if an argument was present before skipping;
                            // this should be safe.
                            assert!(check.is_ok());
                        }
                        return result;
                    } else {
                        let check = runner.skip_argument();
                        if check.is_err() {
                            // We expected another argument to `if` besides just the conditional.
                            return Err("Expected `if` to have a truthy block".to_string());
                        }
                        // To support an optional `else` clause, only execute if present.
                        if runner.has_another_argument() {
                            return self.evaluate_argument(runner);
                        }
                        return Ok(Argument::Null);
                    }
                }
            }
        }
    }

    impl TestExecutor {
        fn new() -> Self {
            Self {
                test_what_ran: Vec::new(),
            }
        }

        // You probably don't need to wrap your next argument function.
        // We do it for tests to get `WhatRan`.
        fn evaluate_argument(&mut self, runner: &mut ScriptRunner) -> ArgumentResult {
            let result = runner.evaluate_argument(self);
            self.test_what_ran.push(WhatRan::Argument(result.clone()));
            result
        }
    }

    #[test]
    fn test_script_run_leaving_too_many_arguments() {
        let script = Script {
            command: Command::Quit,
            arguments: Vec::from([Argument::I64(-3), Argument::I64(12)]),
        };

        let mut test_executor = TestExecutor::new();
        let result = script.execute(&mut test_executor);

        assert_eq!(
            test_executor.test_what_ran,
            Vec::from([WhatRan::Command(Command::Quit)])
        );
        assert_eq!(
            result.err(),
            Some("`quit` did not use up all the arguments".to_string())
        );
    }

    #[test]
    fn test_script_run_ok() {
        let script = Script {
            command: Command::Quit,
            arguments: Vec::new(),
        };

        let mut test_executor = TestExecutor::new();
        let result = script.execute(&mut test_executor);

        assert_eq!(
            test_executor.test_what_ran,
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

        let mut test_executor = TestExecutor::new();
        let result = script.execute(&mut test_executor);

        let error_string = "`fg` required another argument".to_string();
        assert_eq!(
            test_executor.test_what_ran,
            Vec::from([
                WhatRan::Command(Command::ForegroundColor),
                WhatRan::Argument(Err(error_string.clone())),
            ])
        );
        assert_eq!(result.err(), Some(error_string));
    }

    #[test]
    fn test_script_if_can_execute_true() {
        let script = Script {
            command: Command::If,
            arguments: Vec::from([
                Argument::I64(1),
                Argument::Script(Script {
                    command: Command::ForegroundColor,
                    arguments: Vec::from([Argument::Rgba8(Rgba8::RED)]),
                }),
            ]),
        };

        let mut test_executor = TestExecutor::new();
        let result = script.execute(&mut test_executor);

        assert_eq!(
            test_executor.test_what_ran,
            Vec::from([
                WhatRan::Command(Command::If),
                WhatRan::Argument(Ok(Argument::I64(1))),
                WhatRan::Command(Command::ForegroundColor),
                WhatRan::Argument(Ok(Argument::Rgba8(Rgba8::RED))),
                // This second one comes as a result of returning this from the `if`
                WhatRan::Argument(Ok(Argument::Rgba8(Rgba8::RED))),
            ])
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_script_if_can_execute_false_and_fail_without_enough_arguments() {
        let mut script = Script {
            command: Command::If,
            arguments: Vec::from([
                Argument::I64(0),
                // NOTE! not enough arguments
            ]),
        };

        let mut test_executor = TestExecutor::new();
        let result = script.execute(&mut test_executor);

        assert_eq!(
            test_executor.test_what_ran,
            Vec::from([
                WhatRan::Command(Command::If),
                WhatRan::Argument(Ok(Argument::I64(0))),
            ])
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_script_if_can_execute_false_and_succeed_without_else() {
        let mut script = Script {
            command: Command::If,
            arguments: Vec::from([
                Argument::I64(0),
                Argument::Rgba8(Rgba8::BLACK), // the truthy block.
                                               // the else block is optional
            ]),
        };

        let mut test_executor = TestExecutor::new();
        let result = script.execute(&mut test_executor);

        assert_eq!(
            test_executor.test_what_ran,
            Vec::from([
                WhatRan::Command(Command::If),
                WhatRan::Argument(Ok(Argument::I64(0))),
            ])
        );
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some(Argument::Null));
    }

    #[test]
    fn test_script_if_can_execute_false_and_succeed_with_else() {
        let mut script = Script {
            command: Command::If,
            arguments: Vec::from([
                Argument::I64(0),
                Argument::Rgba8(Rgba8::BLACK), // the truthy block.
                Argument::Rgba8(Rgba8::BLUE),  // the falsey block.
            ]),
        };

        let mut test_executor = TestExecutor::new();
        let result = script.execute(&mut test_executor);

        assert_eq!(
            test_executor.test_what_ran,
            Vec::from([
                WhatRan::Command(Command::If),
                WhatRan::Argument(Ok(Argument::I64(0))),
                WhatRan::Argument(Ok(Argument::Rgba8(Rgba8::BLUE))),
            ])
        );
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some(Argument::Rgba8(Rgba8::BLUE)));
    }
}
