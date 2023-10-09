use crate::gfx::Rgba8;

use std::fmt;

#[derive(PartialEq, Debug, Clone)]
pub enum Command {
    Quit,
    ForegroundColor,
}

impl fmt::Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Quit => write!(f, "quit"),
            Self::ForegroundColor => write!(f, "fg"),
            _ => write!(f, "???"),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Script {
    command: Command,
    arguments: Vec<Argument>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Argument {
    Null,
    Bool(bool),
    I64(i64),
    F64(f64),
    Str(String),
    Rgba8(Rgba8),
    If,
    // TODO: we should be able to pause execution, e.g., for an alert box to confirm an action
    Script(Script),
}

impl Argument {
    pub fn is_value(&self) -> bool {
        !matches!(
            self,
            Self::If | Self::Script(_)
        )
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Null => false,
            Self::Bool(value) => *value,
            Self::I64(value) => *value != 0,
            Self::F64(value) => *value != 0.,
            Self::Str(value) => *value != "",
            Self::Rgba8(value) => *value != Rgba8::TRANSPARENT,
            _ => panic!("unimplemented")
        }
    }
}

pub struct ScriptRunner<'a> {
    script: &'a Script,
    argument_index: usize,
}

pub type ArgumentResult = Result<Argument, String>;

// We're using this instead of a closure because rust closures are hard to type correctly.
pub trait ScriptExecutor {
    fn execute(&mut self, runner: &mut ScriptRunner) -> ArgumentResult;
}

impl Script {
    pub fn execute(&self, executor: &mut dyn ScriptExecutor) -> ArgumentResult {
        let mut runner = ScriptRunner::new(&self);
        runner.run(executor)
    }
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
        let mut execute_argument = |mut result: ArgumentResult| {
            while !result.is_err() {
                let argument = result.unwrap();
                match argument {
                    Argument::Script(script) => {
                        result = script.execute(executor);
                    },
                    Argument::If => {
                        // TODO: need to update a state machine `if_state`; can't call closure from itself
                        let condition = execute_argument();
                        if condition.is_err() { return condition; }
                        if condition.unwrap().is_truthy() {
                            result = execute_argument();
                            // Skip the `else` only if present:
                            if self.argument_index < self.script.arguments.len() {
                                self.skip_argument();
                            }
                        } else {
                            self.skip_argument();
                            // Execute the `else` only if present:
                            result = if self.argument_index < self.script.arguments.len() {
                                execute_argument()
                            } else {
                                Ok(Argument::Null)
                            }
                        }
                        return result;
                    },
                    _ => return Ok(argument),
                }
            }
            result
        };
        execute_argument(next_argument())
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
            arguments: Vec::from([Argument::I64(-3), Argument::Bool(true)]),
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
    fn test_script_run_skipping_not_enough_arguments() {
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
}
