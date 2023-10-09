use crate::gfx::Rgba8;

use std::fmt;

#[derive(PartialEq, Debug, Clone)]
pub enum Command {
    Quit,
    ForegroundColor,
    If,
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

#[derive(PartialEq, Debug, Clone)]
pub enum Argument {
    Null,
    Bool(bool),
    I64(i64),
    F64(f64),
    Str(String),
    Rgba8(Rgba8),
    // TODO: we should be able to pause execution, e.g., for an alert box to confirm an action
    Script(Script),
}

impl fmt::Display for Argument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
            Self::Bool(value) => write!(f, "{}", if *value { "true" } else { "false" }),
            Self::I64(value) => write!(f, "{}", *value),
            Self::F64(value) => write!(f, "{}", *value),
            Self::Str(value) => write!(f, "{}", *value),
            Self::Rgba8(value) => write!(f, "{}", *value),
            Self::Script(script) => {
                let mut check = write!(f, "Script {{command: {}, arguments: Vec2::from([", script.command);
                if check.is_err() { return check; }
                for arg in &script.arguments {
                    check = write!(f, "{}, ", arg);
                    if check.is_err() { return check; }
                }
                write!(f, "])}}")
            },
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
                },
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
                    // TODO: see if we can make this a helper function
                    let conditional = self.evaluate_argument(runner);
                    // TODO: finalize with error
                    if conditional.is_err() { return conditional; }
                    if conditional.unwrap().is_truthy() {
                        eprint!("heyo inside if {}\n", command);
                        let result = self.evaluate_argument(runner);
                        eprint!("heyo inside if after {}\n", result.clone().unwrap());
                        if runner.has_another_argument() {
                            // To avoid errors with not using up arguments; i.e.,
                            // we support an optional `else` clause.
                            runner.skip_argument();
                        }
                        return result;
                    } else {
                        runner.skip_argument();
                        // To support an optional `else` clause, only execute if present.
                        if runner.has_another_argument() {
                            return self.evaluate_argument(runner);
                        }
                        return Ok(Argument::Null);
                    }
                },
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
            if result.is_ok() {
                eprint!("heyo inside evaluate argument {}\n", result.clone().unwrap());
            }
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
    fn test_script_run_ok() {
        let script = Script {
            command: Command::Quit,
            arguments: Vec::new(),
        };

        let mut test_executor = TestExecutor::new();
        let result = script.execute(&mut test_executor);

        assert_eq!(
            test_executor.test_what_ran,
            Vec::from([
                WhatRan::Command(Command::Quit)
                ])
        );
        assert!(
            result.is_ok()
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
    fn test_script_if_can_execute_if() {
        let script = Script {
            command: Command::If,
            arguments: Vec::from([Argument::Bool(true), Argument::Script(Script {
                command: Command::ForegroundColor,
                arguments: Vec::from([Argument::Rgba8(Rgba8::RED)]),
            })]),
        };

        let mut test_executor = TestExecutor::new();
        let result = script.execute(&mut test_executor);

        assert_eq!(
            test_executor.test_what_ran,
            Vec::from([
                WhatRan::Command(Command::If),
                WhatRan::Argument(Ok(Argument::Bool(true))),
                WhatRan::Command(Command::ForegroundColor),
                WhatRan::Argument(Ok(Argument::Rgba8(Rgba8::RED))),
                // TODO: we should try to avoid 
                //WhatRan::Argument(Ok(Argument::Rgba8(Rgba8::RED))),
            ])
        );
        assert!(result.is_ok());
    }
}
