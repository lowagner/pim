use crate::command::*;
use crate::gfx::Rgba8;
use crate::palette::Palette;
use crate::platform::{Key, ModifiersState, MouseButton};
use crate::session::Tool;
use crate::settings::*;

use claim::assert_ok;

use std::collections::HashMap;
use std::fmt;
use std::path::MAIN_SEPARATOR;

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

impl fmt::Display for Script {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.command)?;
        for a in &self.arguments {
            write!(f, " {}", a)?;
        }
        fmt::Result::Ok(())
    }
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
    // For keys (e.g. <a>-<z>, <tab>, <backspace>, <ctrl>, etc.) and also modifiers
    // (<shift><ctrl><a>, <alt><c>, etc.)
    // Note that commands that take Input should also take a String that is a single
    // character long, e.g., 'A' or 'ö'.
    Input(Input),

    // Non-value-based (AKA evaluatable) arguments follow.
    // TODO: we should be able to pause execution, e.g., for an alert box to confirm an action

    // An argument that is itself the output of another Script.
    // Note that these are never memoized and are always lazily evaluated
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

    pub fn is_null(&self) -> bool {
        !matches!(self, Self::Null)
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

    // TODO: probably should have `to_X` versions where we consume this argument.
    //       e.g., this is probably most important for String and Script.
    /// Returns an i64 if possible.
    pub fn get_i64(&self, what_for: &str) -> I64Result {
        match self {
            Argument::I64(value) => Ok(*value),
            result => Err(format!("invalid i64 {}: {}", what_for, result)),
        }
    }

    /// Returns an optional i64 if the argument is I64 or Null.
    pub fn get_optional_i64(&self, what_for: &str) -> OptionalI64Result {
        match self {
            Argument::Null => Ok(None),
            arg => Ok(Some(arg.get_i64(what_for)?)),
        }
    }

    pub fn get_optional_string(&self, what_for: &str) -> OptionalStringResult {
        match self {
            Argument::Null => Ok(None),
            arg => Ok(Some(arg.get_string(what_for)?)),
        }
    }

    pub fn get_string(&self, what_for: &str) -> StringResult {
        match self {
            Argument::String(value) => Ok(value.clone()),
            result => Err(format!("invalid string {}: {}", what_for, result)),
        }
    }

    pub fn get_color(&self, palette: &Palette) -> ColorResult {
        match self {
            Argument::I64(value) => {
                // Purposely underflow here for negative values;
                // that will just become a very large positive number,
                // and we'll check the palette size first.
                let palette_index = *value as usize;
                if palette_index < palette.size() {
                    Ok(palette.colors[palette_index])
                } else {
                    Err(format!("no palette color with index {}", value))
                }
            }
            Argument::Color(color) => Ok(*color),
            // TODO: maybe parse strings like 'red' or 'orange'
            arg => Err(format!("invalid argument cannot convert to color: {}", arg)),
        }
    }

    pub fn get_optional_color(&self, palette: &Palette) -> OptionalColorResult {
        match self {
            Argument::Null => Ok(None),
            arg => Ok(Some(arg.get_color(palette)?)),
        }
    }

    pub fn get_input(&self) -> InputResult {
        match self {
            Argument::String(string) => Ok(Input::Rune(
                ModifiersState::default(),
                get_single_rune(&string)?,
            )),
            Argument::Input(input) => Ok(input.clone()),
            result => Err(format!("invalid input: {}", result)),
        }
    }

    pub fn get_optional_input(&self) -> OptionalInputResult {
        match self {
            Argument::Null => Ok(None),
            arg => Ok(Some(arg.get_input()?)),
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

    pub fn get_optional_script(&self, what_for: &str) -> OptionalScriptResult {
        match self {
            Argument::Null => Ok(None),
            arg => Ok(Some(arg.get_script(what_for)?)),
        }
    }
}

/// Returns the rune if the string comprises only a single char.
pub fn get_single_rune(string: &String) -> Result<char, String> {
    // TODO: probably can use char::from_str with `use std::str::FromStr;` import.
    //       but we need to convert to the String error.
    // We want only the first char, but only if the string is one char long.
    let mut chars = string.chars();
    let rune = chars.next();
    if rune.is_none() {
        return Err(format!("invalid rune: <empty-string>"));
    }
    let rune = rune.unwrap();
    if chars.next().is_some() {
        return Err(format!("invalid rune: '{}'", string));
    }
    Ok(rune)
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

#[derive(Eq, PartialEq, Debug, Clone, Copy, Hash)]
pub enum Input {
    // TODO: rename ModifiersState -> Modifiers at some point.
    /// A special key like the `e` key (ignoring modifiers) or `<home>`.
    Key(ModifiersState, Key),
    /// A mouse button like left-mouse button (lmb).
    MouseButton(ModifiersState, MouseButton),
    /// Mouse wheel was scrolled.  Multiplies the platform input by 100,
    /// so e.g., scrolling up/down is +-100.
    MouseWheel(ModifiersState, i32),
    /// A letter/character/rune, e.g., 'A' or 'ö', without modifiers.
    /// We can't handle released state here, just the pressed state.
    Rune(ModifiersState, char),
}

pub type VoidResult = Result<(), String>;
pub type OptionalI64Result = Result<Option<i64>, String>;
pub type I64Result = Result<i64, String>;
pub type OptionalStringResult = Result<Option<String>, String>;
pub type ColorResult = Result<Rgba8, String>;
pub type OptionalColorResult = Result<Option<Rgba8>, String>;
pub type InputResult = Result<Input, String>;
pub type OptionalInputResult = Result<Option<Input>, String>;
pub type StringResult = Result<String, String>;
pub type ScriptResult = Result<Script, String>;
pub type OptionalScriptResult = Result<Option<Script>, String>;

impl fmt::Display for Argument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Argument::Null => write!(f, "null"),
            Argument::I64(value) => write!(f, "{}", *value),
            Argument::Color(value) => write!(f, "{}", *value),
            Argument::String(value) => write!(f, "'{}'", *value),
            Argument::Input(Input::Key(mods, key)) => write!(f, "{}<{}>", mods, key),
            Argument::Input(Input::MouseButton(mods, btn)) => write!(f, "{}<{}>", mods, btn),
            Argument::Input(Input::MouseWheel(mods, val)) => write!(f, "{}<{{{}}}>", mods, val),
            Argument::Input(Input::Rune(mods, rune)) => {
                if *rune == '\'' {
                    write!(f, "{}\"'\"", mods)
                } else {
                    write!(f, "{}'{}'", mods, rune)
                }
            }
            Argument::Script(script) if script.arguments.len() == 0 => {
                write!(f, "{}", script.command)
            }
            Argument::Script(script) => {
                write!(f, "({})", script)
            }
            // TODO: consider adding lookback here (${}{}), since lookback is negative.
            //      i think we'll be ok because we implement the parser ourselves and
            //      we always decrement lookback when going into a new script.
            //      there shouldn't be any ambiguity if we copy paste the resulting script.
            Argument::Use(use_argument) => write!(f, "${}", use_argument.index),
        }
    }
}

pub fn result_to_string(result: ArgumentResult) -> String {
    match result {
        Err(string) => {
            if string.is_empty() {
                "(error)".to_string()
            } else {
                format!("(error `{}`)", string)
            }
        }
        Ok(argument) => format!("{}", argument),
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

    /// Executes a script on the top of the stack but using `command` to override the
    /// `script.command` value.  This is e.g., for an aliased Variable::BuiltIn.
    fn run_command(
        &mut self,
        command: &Command,
        script: &Script,
        script_stack: Vec<&Script>,
    ) -> ArgumentResult;
}

#[macro_export] // meta programming
macro_rules! script_runner {
    ( $name:ident ) => {
        impl $name {
            // Can add other common methods for ScriptRunner here.

            /// Sums the top of the script stack's arguments together.
            /// Converts a Null sum to 0.
            fn sum(&mut self, script_stack: &Vec<&Script>) -> ArgumentResult {
                let mut value =
                    self.script_evaluate(&script_stack, Evaluate::Index(0))?;
                let script: &Script = &script_stack[script_stack.len() - 1];
                let argument_count = script.arguments.len() as u32;
                for index in 1..argument_count {
                    let next = self
                        .script_evaluate(&script_stack, Evaluate::Index(index))?;
                    value = match value {
                        Argument::Null => next,
                        Argument::I64(number) => Argument::I64(number +
                            next.get_optional_i64("for i64 sum")?.unwrap_or(0)),
                        // TODO: implement some of these sums (i.e., for other value-like args)
                        _ => return Err("sum for things besides i64 not implemented yet".to_string()),
                    }
                }
                match value {
                    Argument::Null => Ok(Argument::I64(0)),
                    arg => Ok(arg),
                }
            }
        }

        impl ScriptRunner for $name {
            fn run(&mut self, script_stack: Vec<&Script>) -> ArgumentResult {
                if script_stack.len() == 0 {
                    return Ok(Argument::Null);
                }
                let script: &Script = &script_stack[script_stack.len() - 1];
                let command = script.command.clone();
                self.begin_script_command(&command.clone());
                let result = self.run_command(&command, script, script_stack);
                self.end_script_command(&command);
                result
            }

            fn run_command(
                &mut self,
                command: &Command,
                script: &Script,
                script_stack: Vec<&Script>,
            ) -> ArgumentResult {
                match command {
                    Command::NoOp => Ok(Argument::Null),
                    Command::Help => {
                        if script.arguments.len() != 1 {
                            Ok(Argument::String(self.variables.describe(Command::Help)))
                        } else {
                            match &script.arguments[0] {
                                Argument::Script(nested_script) => {
                                    if nested_script.arguments.len() == 0 {
                                        Ok(Argument::String(self.variables.describe(nested_script.command.clone())))
                                    } else {
                                        Ok(Argument::String(
                                            format!("({}) -- a script", nested_script),
                                        ))
                                    }
                                }
                                Argument::Use(use_arg) => Ok(Argument::String(
                                    format!(
                                        "-- a way to evaluate an argument at position {}; \
                                        e.g., `const 'ten' (sum $0 10)` will add 10 to argument 0, \
                                        and can be called via `ten 123` which will return `133`",
                                        use_arg.index
                                    ),
                                )),
                                // TODO: break down by argument type.  e.g., Argument::String is a string argument
                                a => Ok(Argument::String(
                                    format!("{} -- an argument", a),
                                )),
                            }
                        }
                    }
                    Command::Echo => {
                        let mut string = String::new();
                        for i in 0..script.arguments.len() {
                            let result = self.script_evaluate(&script_stack, Evaluate::Index(i as u32));
                            let next_string = result_to_string(result);
                            if i > 0 {
                                string.push_str(" ");
                            }
                            string.push_str(&next_string);
                        }
                        Ok(Argument::String(string))
                    }
                    Command::Error => {
                        let mut string = String::new();
                        for i in 0..script.arguments.len() {
                            let result = self.script_evaluate(&script_stack, Evaluate::Index(i as u32));
                            let next_string = result_to_string(result);
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
                        let value = self.sum(&script_stack)?.get_i64("for even")?;
                        Ok(Argument::I64(if value % 2 == 0 { 1 } else { 0 }))
                    }
                    Command::Odd => {
                        let value = self.sum(&script_stack)?.get_i64("for odd")?;
                        Ok(Argument::I64(if value % 2 == 1 { 1 } else { 0 }))
                    }
                    Command::Sum => self.sum(&script_stack),
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
                        match self.variables.get(name) {
                            Get::Argument(argument) => {
                                // TODO: revisit if this isn't useful for non-test ScriptRunners.
                                // Not using `self.script_evaluate` mostly for avoiding the extra WhatRan in tests:
                                evaluate(self, &script_stack, Evaluate::Argument(&argument))
                            }
                            Get::Command(command) => {
                                // This was an alias, we need to rerun the current script with the
                                // aliased command substituting for the `script.command`.
                                self.run_command(&command, script, script_stack)
                            }
                        }
                    }
                    // Note that variables can technically be `Script`s.
                    // E.g., if we do `set "var-name" (echo $0 ; echo $1)`, then we
                    // can call with `var-name 'hello' 'world'`.  This is the way
                    // we create new commands.
                    Command::SetVariable => self.variables.set_from_script(&script),
                    Command::ConstVariable => self.variables.set_from_script(&script),
                    Command::CreateAlias => self.variables.set_from_script(&script),
                    Command::Bind(bind) => {
                        let input = self.script_evaluate(
                            &script_stack,
                            Evaluate::Index(0),
                        )?.get_input()?;
                        let get_optional_script = |index: usize, for_what: &str| {
                            match &script.arguments.get(index) {
                                None => Ok(None),
                                Some(Argument::Script(script)) => Ok(Some(script.clone())),
                                Some(_) => Err(format!("not a script at argument {} {}", index, for_what)),
                            }
                        };
                        let on_trigger = get_optional_script(1, "for trigger")?.ok_or(
                            "need script for trigger at argument 1".to_string(),
                        )?;
                        let on_release = get_optional_script(2, "for release")?;

                        let modes = match bind {
                            Bind::Modes => vec![Mode::Normal, Mode::Select(Select::Selecting)],
                            Bind::Normal => vec![Mode::Normal],
                            Bind::Select => vec![Mode::Select(Select::Selecting)],
                            Bind::Help => vec![Mode::Help],
                        };

                        let key_binding = KeyBinding {
                            input,
                            on_trigger,
                            on_release,
                            display: Some(format!("{}", script)),
                        };

                        for mode in modes {
                            self.bind_key(mode, key_binding.clone());
                        }
                        Ok(Argument::Null)
                    },
                    Command::ColorSetting(setting) => {
                        let optional_color = self.script_evaluate(
                            &script_stack,
                            Evaluate::Index(0),
                        )?.get_optional_color(&self.palette)?;
                        let old_value = self.get_color_setting(*setting);
                        match optional_color {
                            Some(new_value) if new_value != old_value => {
                                self.set_color_setting(*setting, new_value)?;
                            }
                            _ => {}
                        }
                        Ok(Argument::Color(old_value))
                    }
                    Command::StringSetting(setting) => {
                        let argument = self
                            .script_evaluate(&script_stack, Evaluate::Index(0))?
                            .get_optional_string("for string setting")?;
                        let old_value = self.get_string_setting(*setting);
                        match argument {
                            Some(new_value) if new_value != old_value => {
                                self.set_string_setting(*setting, new_value)?;
                            }
                            _ => {}
                        }
                        Ok(Argument::String(old_value))
                    }
                    Command::I64Setting(setting) => {
                        let argument = self
                            .script_evaluate(&script_stack, Evaluate::Index(0))?
                            .get_optional_i64("for i64 setting")?;
                        let old_value = self.get_i64_setting(*setting);
                        match argument {
                            Some(new_value) if new_value != old_value => {
                                self.set_i64_setting(*setting, old_value, new_value)?;
                            }
                            _ => {}
                        }
                        Ok(Argument::I64(old_value))
                    }
                    Command::WithoutArguments(for_what) => {
                        // Classic taste, but zero arguments.
                        self.script_zero(*for_what)
                    }
                    Command::UsingOptionalI64(for_what) => {
                        let optional_i64 = self
                            .script_evaluate(&script_stack, Evaluate::Index(0))?
                            .get_optional_i64("for optional i64 command")?;
                        // TODO: return ArgumentResult here:
                        self.script_optional_i64(*for_what, optional_i64)?;
                        Ok(Argument::Null)
                    }
                    Command::UsingOptionalColor(for_what) => {
                        let optional = self.script_evaluate(&script_stack, Evaluate::Index(0))?
                            .get_optional_color(&self.palette)?;
                        self.script_optional_color(*for_what, optional)
                    }
                    Command::UsingStrings(strings_for) => {
                        let mut strings = vec![];
                        for i in 0..script.arguments.len() {
                            if let Some(string) = self.script_evaluate(
                                &script_stack,
                                Evaluate::Index(i as u32),
                            )?.get_optional_string("for strings command")? {
                                strings.push(string);
                            }
                        }
                        self.script_strings(*strings_for, strings)
                    }
                    Command::UsingTwoI64s(for_what) => {
                        let x = self
                            .script_evaluate(&script_stack, Evaluate::Index(0))?
                            .get_optional_i64("for coordinate X")?
                            .unwrap_or_default();
                        let y = self
                            .script_evaluate(&script_stack, Evaluate::Index(1))?
                            .get_optional_i64("for coordinate Y")?
                            .unwrap_or_default();

                        self.script_two_i64s(*for_what, x, y)
                    }
                    Command::PaletteColor => {
                        // TODO: pull out palette_index logic into a helper function
                        //       so we can re-use it for PaletteRepaint
                        let old_palette_size = self.palette.size();
                        let palette_index = match self.script_evaluate(
                            &script_stack, Evaluate::Index(0)
                        )?.get_optional_i64("for palette index")? {
                            None => return Err(format!("{} needs $0 for an palette index", command)),
                            Some(i64_value) => {
                                let index = i64_value as usize;
                                while index >= self.palette.colors.len() {
                                    self.palette.colors.push(Rgba8::BLACK);
                                }
                                index
                            }
                        };
                        let old_color = self.palette.colors[palette_index];
                        let color_arg = self.script_evaluate(&script_stack, Evaluate::Index(1))?
                            .get_optional_color(&self.palette)?;
                        if let Some(color) = color_arg {
                            self.palette.colors[palette_index] = color;
                        }
                        if old_palette_size != self.palette.size() {
                            self.center_palette();
                        }
                        Ok(Argument::Color(old_color))
                    }
                    Command::PaletteAddColors => {
                        let mut result = None;
                        for i in 0..script.arguments.len() {
                            let color = self.script_evaluate(&script_stack, Evaluate::Index(i as u32))?.get_color(&self.palette)?;
                            result = Some(self.palette.add(color) as i64);
                        }
                        self.center_palette();
                        match result {
                            None => Err("use `p-add #123 #456` to add colors to the palette".to_string()),
                            Some(value) => Ok(Argument::I64(value)),
                        }
                    }
                    Command::PaletteAddGradient => {
                        let color_start = self.script_evaluate(&script_stack, Evaluate::Index(0))?.get_color(&self.palette)?;
                        let color_end = self.script_evaluate(&script_stack, Evaluate::Index(1))?.get_color(&self.palette)?;
                        let gradient_count = self.script_evaluate(&script_stack, Evaluate::Index(2))?.get_optional_i64("for gradient count")?.unwrap_or(5);
                        if gradient_count < 0 {
                            return Err(format!("gradient count should be >= 0, got {}", gradient_count));
                        }
                        let result = self.palette.gradient(color_start, color_end, gradient_count as usize);
                        self.center_palette();
                        Ok(Argument::I64(result as i64))
                    }
                    Command::PaletteSort => {
                        self.palette.sort();
                        Ok(Argument::Null)
                    }
                    Command::PaletteAddViewColors => {
                        let before = self.palette.colors.len() as i64;
                        self.add_view_colors();
                        let after = self.palette.colors.len() as i64;
                        Ok(Argument::I64(after - before))
                    }
                    Command::PaletteWriteToFile => {
                        // TODO: consider *not* evaluating; create a method on Argument that
                        //      returns the string in a Command::Evaluate(String) *or* the
                        //      a string if someone wrote 'filepath'
                        //      we could have a "ForceEvaluate" command that nests the evaluate
                        //      e.g., `p-write (identity (whatever-you-want-here))` so that
                        //      you could still have an evaluated result here.
                        //      Alternatively: convert any token with a / or a . in it into
                        //      a string (or a number if . and just digits).  <--= Do this.
                        //      we should throw an error in any `set` `const` `alias` command
                        //      if the name has a . or / in it.
                        let path = self.script_evaluate(
                            &script_stack,
                            Evaluate::Index(0),
                        )?.get_string("for palette file path")?;
                        match self.palette.write(path) {
                            Ok(text) => Ok(Argument::String(text)),
                            Err(err) => Err(err),
                        }
                    }
                    Command::PaletteClear => {
                        let mut remove_colors = HashSet::new();
                        // Since we're removing colors from the palette, if we see any palette indices,
                        // we need to resolve those colors before deleting them (since erasing entries
                        // on the fly could shift later indices).
                        for i in 0..script.arguments.len() {
                            match self.script_evaluate(
                                &script_stack,
                                Evaluate::Index(i as u32),
                            )?.get_optional_color(&self.palette)? {
                                Some(color) => {remove_colors.insert(color);},
                                None => {}, // ignore nulls
                            }
                        }
                        let previous_count = self.palette.colors.len() as i64;
                        if remove_colors.len() > 0 {
                            self.palette.colors.retain(|c| !remove_colors.contains(c));
                        } else {
                            // If we had no arguments, then we clear the whole palette.
                            self.palette.colors.clear();
                        }
                        let new_count = self.palette.colors.len() as i64;
                        Ok(Argument::I64(previous_count - new_count))
                    }
                    Command::Paint => {
                        let x = self
                            .script_evaluate(&script_stack, Evaluate::Index(0))?
                            .get_i64("for paint coordinate")?;
                        let y = self
                            .script_evaluate(&script_stack, Evaluate::Index(1))?
                            .get_i64("for paint coordinate")?;
                        let color = self.script_evaluate(&script_stack, Evaluate::Index(2))?
                            .get_optional_color(&self.palette)?
                            .unwrap_or(self.fg); // default to foreground color

                        self.script_paint(x, y, color)
                    }
                    Command::Bucket => {
                        let x = self
                            .script_evaluate(&script_stack, Evaluate::Index(0))?
                            .get_i64("for bucket coordinate")?;
                        let y = self
                            .script_evaluate(&script_stack, Evaluate::Index(1))?
                            .get_i64("for bucket coordinate")?;
                        let color = self.script_evaluate(&script_stack, Evaluate::Index(2))?
                            .get_optional_color(&self.palette)?
                            .unwrap_or(self.fg); // default to foreground color

                        self.script_bucket(x, y, color)
                    }
                    Command::Line => {
                        let coords = self.get_active_view_mouse_coords();
                        let x0 = self
                            .script_evaluate(&script_stack, Evaluate::Index(0))?
                            .get_optional_i64("for line start coordinate")?.unwrap_or(coords.x as i64);
                        let y0 = self
                            .script_evaluate(&script_stack, Evaluate::Index(1))?
                            .get_optional_i64("for line start coordinate")?.unwrap_or(coords.y as i64);
                        let x1 = self
                            .script_evaluate(&script_stack, Evaluate::Index(2))?
                            .get_optional_i64("for line end coordinate")?.unwrap_or(coords.x as i64);
                        let y1 = self
                            .script_evaluate(&script_stack, Evaluate::Index(3))?
                            .get_optional_i64("for line end coordinate")?.unwrap_or(coords.y as i64);
                        let color = self.script_evaluate(&script_stack, Evaluate::Index(4))?
                            .get_optional_color(&self.palette)?
                            .unwrap_or(self.fg); // default to foreground color

                        self.script_line(x0, y0, x1, y1, color)?;
                        Ok(Argument::Null)
                    }
                    Command::MouseX => {
                        let coords = self.get_active_view_mouse_coords();
                        // TODO: do something fun with Argument 0 if it's an I64 -- e.g., add a random
                        // offset from -I64 to I64
                        Ok(Argument::I64(coords.point.x as i64))
                    }
                    Command::MouseY => {
                        let coords = self.get_active_view_mouse_coords();
                        // TODO: do something fun with Argument 0 if it's an I64 -- e.g., add a random
                        // offset from -I64 to I64
                        Ok(Argument::I64(coords.point.y as i64))
                    }
                    Command::Write => {
                        let arg0 = self
                            .script_evaluate(&script_stack, Evaluate::Index(0))?.get_optional_string("for path")?;
                        let arg1 = self
                            .script_evaluate(&script_stack, Evaluate::Index(1))?.get_optional_i64("for scale")?;
                        self.script_write(arg0, arg1)
                    }
                    Command::Quit(q) => self.script_quit(*q),
                }
            }
        }
    };
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

// TODO: move `variable` stuff into its own variable.rs file.
#[derive(PartialEq, Debug, Clone)]
pub enum Variable {
    // A variable that can be reassigned:
    Mutable(Argument),
    // A variable that can't be reassigned:
    Const(Argument),
    // Built-in function, e.g., `if`, `fg`, `paint`, etc.:
    // The string is for the `help` function to explain what the command does.
    BuiltIn(Command, String),
    // To look up a variable and remap it:
    Alias(String),
}

impl Variable {
    pub fn is_mutable(&self) -> bool {
        matches!(self, Self::Mutable(_))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Get {
    Argument(Argument),
    Command(Command),
}

impl Get {
    pub fn to_argument(self) -> Argument {
        match self {
            Get::Argument(a) => a,
            _ => panic!("i am not an argument"),
        }
    }

    pub fn to_command(self) -> Command {
        match self {
            Get::Command(c) => c,
            _ => panic!("i am not an command"),
        }
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

    /// Returns the Argument represented by this name, returning a string if not present in the map.
    /// Returning the string of the name is a bit brittle, but it might be expected if you
    /// are trying to open a file name (without the .png extension), to avoid using quotes.
    // Notice that we resolve aliases here so that you'll definitely get an argument.
    pub fn get<'a>(&'a self, mut name: &'a String) -> Get {
        for _iteration in 1..24 {
            match &self.map.get(name) {
                None => return Get::Argument(Argument::String(name.clone())),
                Some(variable) => match variable {
                    Variable::Const(arg) => return Get::Argument(arg.clone()),
                    Variable::Mutable(arg) => return Get::Argument(arg.clone()),
                    Variable::BuiltIn(command, _) => return Get::Command(command.clone()),
                    Variable::Alias(alias) => {
                        name = alias;
                    }
                },
            }
        }
        eprint!("don't nest aliases this much!\n");
        return Get::Argument(Argument::Null);
    }

    /// Sets the variable, returning the old value if it was present in the map.
    /// NOTE! Will return an error if the variable was present and const.
    pub fn set(&mut self, name: String, variable: Variable) -> ArgumentResult {
        // TODO: don't let name contain `<` or `>` as these are interpreted as modifiers/keys
        //       we can probably get away with only checking the beginning as that's what the parser would do.
        // TODO: also avoid containing `(`, `)`, `{` `}`, or `[` `]`
        // TODO: disallow `-` from starting commands
        if name.contains(MAIN_SEPARATOR) || name.contains(".") {
            return Err(format!(
                "variable names should not contain `{}` or `.` which indicate paths",
                MAIN_SEPARATOR
            ));
        }
        if name.contains(" ") {
            return Err("variable names should not contain spaces".to_string());
        }
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
                Variable::BuiltIn(_, _) => {
                    return Err(format!("built-in `{}` is not reassignable", name));
                }
                Variable::Const(_) => {
                    return Err(format!("variable `{}` is not reassignable", name));
                }
                Variable::Alias(_) => {
                    return Err(format!("alias `{}` is not reassignable", name));
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

    fn add_built_in(&mut self, command: Command, description: &str) {
        let command_name = format!("{}", command);
        let command_description = description.replace("$$", &command_name);
        assert_ok!(self.set(
            command_name,
            Variable::BuiltIn(command, command_description)
        ));
    }

    pub fn help(&self) -> Vec<String> {
        self.map
            .iter()
            .map(|(name, value)| Self::describe_variable(&name, &value))
            .collect()
    }

    pub fn describe(&self, command: Command) -> String {
        let name = format!("{}", command);
        if let Some(var) = &self.map.get(&name) {
            Self::describe_variable(&name, &var)
        } else {
            format!("{} is unknown", name)
        }
    }

    fn describe_variable(name: &String, var: &Variable) -> String {
        match var {
            Variable::BuiltIn(_, description) => format!("{} -- {}", name, description),
            Variable::Mutable(a) => format!("{} -- mutable {}", name, a),
            Variable::Const(a) => format!("{} -- const {}", name, a),
            Variable::Alias(a) => format!("{} -- alias of {}", name, a),
        }
    }

    pub fn with_built_ins() -> Self {
        let mut variables = Variables::new();

        variables.add_built_in(
            Command::NoOp,
            "no operation, does nothing, but useful to ensure a binding is not repeatable, \
            e.g., `bind <x> (fg (bg fg)) $$` to ensure holding x won't keep swapping fg<->bg",
        );
        variables.add_built_in(
            Command::Help,
            "explains what $0 does without evaluating it, \
            e.g., `$$ paint` to explain what the `paint` command does",
        );
        // TODO: consider serializing strings without '', at least for echo/error.
        // we need them when serializing a script.
        variables.add_built_in(
            Command::Echo,
            "evaluates all arguments, joining them into a string to print, \
            e.g., `$$ 'oh no' fg` will show a message of `'oh no' {foreground-color}`",
        );
        variables.add_built_in(
            Command::Error,
            "evaluates all arguments, joining them into an error string, \
            e.g., `$$ 'oh no' fg` will return an error of `'oh no' {foreground-color}`",
        );
        variables.add_built_in(
            Command::PushPop,
            "first argument should be a script that swaps in a value to a variable, \
            all other arguments will be evaluated,
            and afterwards the variable will be reset; \
            e.g., `$$ (fg #123456) (paint 3 4)` to set `fg` temporarily",
        );
        variables.add_built_in(
            Command::RunAll,
            "evaluates all arguments until any error, \
            returning the last evaluated argument; \
            e.g., `$$ 'take me' (fg #123456)` returns the previous foreground color",
        );
        variables.add_built_in(
            Command::Not,
            "if $0 evaluates to truthy, returns 0, otherwise 1, \
            e.g., `$$ 3` returns 0 and `not 0` returns 1",
        );
        variables.add_built_in(
            Command::If,
            "if $0 evaluates to truthy, evaluates $1, otherwise $2, \
            e.g., `$$ 'hi' 'world' 3` returns 'world'",
        );
        variables.add_built_in(
            Command::Even,
            "if the sum of all evaluated arguments is even, returns 1, otherwise 0, \
            e.g., `$$ 22 3 4` returns 0",
        );
        variables.add_built_in(
            Command::Odd,
            "if the sum of all evaluated arguments is odd, returns 1, otherwise 0, \
            e.g., `$$ 20 10 7` returns 1",
        );
        variables.add_built_in(
            Command::Sum,
            "adds all evaluated arguments together into the type of $0, \
            e.g., `$$ 1 2 3 4` to return 10",
        );
        variables.add_built_in(
            Command::Product,
            "multiplies all evaluated arguments together, \
            e.g., `$$ -1 2 3 4` to return -24",
        );
        variables.add_built_in(
            Command::SetVariable,
            "creates/updates a mutable variable with name $0 to the value of $1, unevaluated, \
            e.g., `$$ 'paint5' (paint $0 $1 5)` to create a lambda",
        );
        variables.add_built_in(
            Command::ConstVariable,
            "creates an immutable variable with name $0 and value $1, unevaluated, \
            e.g., `$$ 'greet' (echo 'hello, world')` to create a lambda",
        );
        variables.add_built_in(
            Command::CreateAlias,
            "creates an alias with name $0 that evaluates name $1 when called, \
            e.g., `$$ 'fgc' 'fg'` to add `fgc` as an alias for `fg`",
        );
        variables.add_built_in(
            Command::Bind(Bind::Modes),
            "adds input $0 binding with script $1 on press, optional $2 on release, \
            to standard modes, e.g., `$$ <ctrl><z> undo` to undo, or \
            `$$ <shift><p> (p cx cy #123456) (p cx cy #654321)` to paint",
        );
        variables.add_built_in(
            Command::Bind(Bind::Normal),
            "adds input $0 binding with script $1 on press, optional $2 on release, \
            to normal mode, e.g., `$$ <ctrl><z> undo` to undo, or \
            `$$ <shift><p> (p cx cy #123456) (p cx cy #654321)` to paint",
        );
        variables.add_built_in(
            Command::Bind(Bind::Select),
            "adds input $0 binding with script $1 on press, optional $2 on release, \
            to select mode, e.g., `$$ <ctrl><z> undo` to undo, or \
            `$$ <shift><p> (p cx cy #123456) (p cx cy #654321)` to paint",
        );
        variables.add_built_in(
            Command::Bind(Bind::Help),
            "adds input $0 binding with script $1 on press, optional $2 on release, \
            to help mode, e.g., `$$ '?' (mode 'normal')` to go back to normal mode, or \
            `$$ <space> (tool pant) (tool $0)` to pan",
        );
        variables.add_built_in(
            Command::ColorSetting(ColorSetting::UiBackground),
            "getter/swapper for the current UI background color if $0 is null/present, \
            e.g., `$$ #123` to make the background color #123",
        );
        variables.add_built_in(
            Command::ColorSetting(ColorSetting::UiGrid),
            "getter/swapper for the current UI grid color if $0 is null/present, \
            e.g., `$$ 4` to set the grid color to palette color 4. \
            e.g., use `grid 16` to enable seeing a 16x16 grid",
        );
        variables.add_built_in(
            Command::ColorSetting(ColorSetting::Foreground),
            "getter/swapper for foreground color if $0 is null/present, \
            e.g., `$$ 3` to set foreground color to palette 3",
        );
        variables.add_built_in(
            Command::ColorSetting(ColorSetting::Background),
            "getter/swapper for background color if $0 is null/present, \
            e.g., `$$ #123456` to set background color to #123456",
        );
        variables.add_built_in(
            Command::StringSetting(StringSetting::Mode),
            "getter/swapper for the current mode if $0 is null/present, \
            e.g., `$$ 'normal'` to go to normal mode",
        );
        variables.add_built_in(
            Command::StringSetting(StringSetting::PresentWorkingDirectory),
            "getter/swapper for the present working directory if $0 is null/present, \
            e.g., `$$ '/home/whatever'` to change directories",
        );
        variables.add_built_in(
            Command::StringSetting(StringSetting::ConfigDirectory),
            "getter/swapper for the config file directory if $0 is null/present, \
            e.g., `$$ '/home/whatever'` to change directories",
        );
        variables.add_built_in(
            Command::I64Setting(I64Setting::Debug),
            "getter/swapper for debug mode if $0 is null/present, \
            e.g., `$$ on` to turn on debug",
        );
        variables.add_built_in(
            Command::I64Setting(I64Setting::UiAnimate),
            "getter/swapper for toggling animation if $0 is null/present, \
            e.g., `$$ on` to turn on animation",
        );
        variables.add_built_in(
            Command::I64Setting(I64Setting::UiChecker),
            "getter/swapper for how big the checkerboard is if $0 is null/present, \
            e.g., `$$ 0` to turn off checkerboard transparency, `$$ 5` to set to 5x5 pixels",
        );
        variables.add_built_in(
            Command::I64Setting(I64Setting::UiGrid),
            "getter/swapper for the size of a grid if $0 is null/present, \
            e.g., `$$ 8` to show an 8x8 grid, `$$ off` to hide",
        );
        variables.add_built_in(
            Command::I64Setting(I64Setting::UiScalePercentage),
            "getter/swapper for current UI scale percentage if $0 is null/present, \
            e.g., `$$ 150` to set UI to 1.5x",
        );
        variables.add_built_in(
            Command::I64Setting(I64Setting::UiOffsetX),
            "getter/swapper for current UI x offset if $0 is null/present, \
            e.g., `$$ 32` to set X pixel offset to 32",
        );
        variables.add_built_in(
            Command::I64Setting(I64Setting::UiOffsetY),
            "getter/swapper for current UI y offset if $0 is null/present, \
            e.g., `$$ 45` to set Y pixel offset to 45",
        );
        variables.add_built_in(
            Command::I64Setting(I64Setting::Tool),
            "getter/swapper for the current tool if $0 is null/present, \
            e.g., `$$ pant` to set to \"pan\"; also buckett, samplert, brusht \
            (remove final t for what tool it is)",
        );
        variables.add_built_in(
            Command::I64Setting(I64Setting::PaletteHeight),
            "getter/swapper for current palette height if $0 is null/present, \
            e.g., `$$ 5` to set show 5 colors squares vertically",
        );
        variables.add_built_in(
            Command::I64Setting(I64Setting::UiZoom),
            "getter/swapper for current UI zoom if $0 is null/present, \
            e.g., `$$ 32` to set zoom to 32x",
        );
        variables.add_built_in(
            Command::I64Setting(I64Setting::ViewIndex),
            "getter/swapper for current UI view index if $0 is null/present, \
            e.g., `$$++` to increment the view",
        );
        variables.add_built_in(
            Command::I64Setting(I64Setting::CursorXRay),
            "getter/swapper for x-ray to see pixel under cursor if $0 is null/present, \
            e.g., `$$ 1` to turn on",
        );
        variables.add_built_in(
            Command::I64Setting(I64Setting::BrushSize),
            "getter/swapper for brush size if $0 is null/present, \
            e.g., `$$ 15` to set to 15",
        );
        variables.add_built_in(
            Command::I64Setting(I64Setting::BrushErase),
            "getter/swapper for erase brush option if $0 is null/present, \
            e.g., `$$ on` to turn on",
        );
        variables.add_built_in(
            Command::I64Setting(I64Setting::BrushMultiFrame),
            "getter/swapper for multi-frame brush option if $0 is null/present, \
            e.g., `$$ off` to turn off",
        );
        variables.add_built_in(
            Command::I64Setting(I64Setting::BrushPixelPerfect),
            "getter/swapper for pixel-perfect brush option if $0 is null/present, \
            e.g., `$$ 0` to turn off",
        );
        variables.add_built_in(
            Command::I64Setting(I64Setting::BrushXSymmetry),
            "getter/swapper for draw with X-symmetry brush option if $0 is null/present, \
            e.g., `$$ false` to turn off",
        );
        variables.add_built_in(
            Command::I64Setting(I64Setting::BrushYSymmetry),
            "getter/swapper for draw with Y-symmetry brush option if $0 is null/present, \
            e.g., `$$ true` to turn on",
        );
        variables.add_built_in(
            Command::I64Setting(I64Setting::BrushLineAngle),
            "getter/swapper for line angle brush option if $0 is null/present, \
            e.g., `$$ 30` to set to 30 degrees, `$$ 0` to turn off",
        );
        variables.add_built_in(
            Command::I64Setting(I64Setting::AnimationDelay),
            "getter/swapper for the current view's animation delay if $0 is null/present, \
            e.g., `$$ 300` to set the delay to 300 ms, \
            or `$$` to return the current delay without changing it",
        );
        variables.add_built_in(
            Command::I64Setting(I64Setting::FrameIndex),
            "getter/swapper for the frame index if $0 is null/present, \
            e.g., `$$ 3` to switch to the fourth frame (0-indexed), \
            or `$$ -1` to switch to the last frame (wrap around)",
        );
        variables.add_built_in(
            Command::I64Setting(I64Setting::FrameWidth),
            "getter/swapper for the frame width if $0 is null/present, \
            e.g., `$$ 123` to set to 123 pixels wide",
        );
        variables.add_built_in(
            Command::I64Setting(I64Setting::FrameHeight),
            "getter/swapper for the frame height if $0 is null/present, \
            e.g., `$$ 456` to set to 456 pixels high",
        );
        variables.add_built_in(
            Command::I64Setting(I64Setting::ImageSplit),
            "getter/swapper for the number of animation frames if $0 is null/present, \
            without resizing the image; i.e., the desired amount must divide the image width. \
            e.g., `$$ 2` to split the image into 2 animation frames.",
        );
        variables.add_built_in(
            Command::I64Setting(I64Setting::History),
            "getter/swapper for the current view's history ID, \
            e.g., `$$` get the current ID which can be used to restore it later.",
        );
        variables.add_built_in(
            Command::I64Setting(I64Setting::NormalMode),
            "getter/swapper for the whether normal mode is active, \
            e.g., `$$ 1` to switch to normal mode",
        );
        variables.add_built_in(
            Command::WithoutArguments(ZeroArgumentsFor::Reset),
            "`$$` resets all settings",
        );
        variables.add_built_in(
            Command::WithoutArguments(ZeroArgumentsFor::SelectionToClipboardSize),
            "`$$` ensures selection has same size as clipboard",
        );
        variables.add_built_in(
            Command::WithoutArguments(ZeroArgumentsFor::SelectionExpand),
            "`$$` expands selection to fill the overlapped frames",
        );
        variables.add_built_in(
            Command::WithoutArguments(ZeroArgumentsFor::SelectionErase),
            "`$$` deletes what's in the selection",
        );
        variables.add_built_in(
            Command::WithoutArguments(ZeroArgumentsFor::SelectionCopy),
            "`$$` copies the selection",
        );
        variables.add_built_in(
            Command::WithoutArguments(ZeroArgumentsFor::SelectionCut),
            "`$$` copies the selection and clears the area",
        );
        variables.add_built_in(
            Command::WithoutArguments(ZeroArgumentsFor::SelectionSwapCut),
            "`$$` pastes what was in the clipboard and cuts what was in the selection",
        );
        variables.add_built_in(
            Command::WithoutArguments(ZeroArgumentsFor::SelectionPaste),
            "`$$` pastes what was copied in a previous selection",
        );
        variables.add_built_in(
            Command::WithoutArguments(ZeroArgumentsFor::SelectionMirrorX),
            "`$$` pastes a horizontally flipped version of the clipboard selection",
        );
        variables.add_built_in(
            Command::WithoutArguments(ZeroArgumentsFor::SelectionMirrorY),
            "`$$` pastes a vertically flipped version of the clipboard selection",
        );
        variables.add_built_in(
            Command::UsingOptionalI64(OptionalI64For::FrameAdd),
            "adds a blank frame after index $0, or the current frame if $0 is null, \
            e.g., `$$ -1` to add a frame at the end",
        );
        variables.add_built_in(
            Command::UsingOptionalI64(OptionalI64For::FrameInsert),
            "adds a blank frame before index $0, or the current frame if $0 is null, \
            e.g., `$$ 0` to add a frame at the start",
        );
        variables.add_built_in(
            Command::UsingOptionalI64(OptionalI64For::FrameClone),
            "clones the frame at index $0, or the current frame if $0 is null, \
            e.g., `$$ 2` to clone the third frame",
        );
        variables.add_built_in(
            Command::UsingOptionalI64(OptionalI64For::FrameRemove),
            "removes the frame at index $0, or the current frame if $0 is null, \
            e.g., `$$ 0` to remove the first frame",
        );
        variables.add_built_in(
            Command::UsingOptionalI64(OptionalI64For::FrameShift),
            "shifts the starting frame right by $0 frames, defaulting to 1, with wrap-around, \
            e.g., `$$ -1` to move the first frame left (to the end of the animation)",
        );
        variables.add_built_in(
            Command::UsingOptionalI64(OptionalI64For::FrameShiftX),
            "shifts each frame's columns right by $0 pixels, defaulting to 1, with wrap-around, \
            e.g., `$$ -1` to move the first column left (to the end of the frame)",
        );
        variables.add_built_in(
            Command::UsingOptionalI64(OptionalI64For::FrameShiftY),
            "shifts each frame's rows down by $0 pixels, defaulting to 1, with wrap-around, \
            e.g., `$$ -1` to move the first row up (to the bottom of the frame)",
        );
        variables.add_built_in(
            Command::UsingOptionalI64(OptionalI64For::Undo),
            "calls undo with an optional number of times to repeat, defaulting to 1, \
            e.g., `$$` to undo once",
        );
        variables.add_built_in(
            Command::UsingOptionalI64(OptionalI64For::Redo),
            "calls redo with an optional number of times to repeat, defaulting to 1, \
            e.g., `$$ 2` to redo two times",
        );
        variables.add_built_in(
            Command::UsingOptionalColor(OptionalColorFor::SelectionClear),
            "uses an optional color at $0 (defaulting to transparent) to \
            erase everything in the selection and to replace all pixels, \
            e.g., `$$ #123` to fill with color #123",
        );
        variables.add_built_in(
            Command::UsingStrings(StringsFor::ChangeDirectory),
            "changes directory to that in $0 if non-null, or the initial working directory otherwise, \
            e.g., `$$ 'subdir'` to move into the 'subdir' directory",
        );
        variables.add_built_in(
            Command::UsingStrings(StringsFor::Source),
            "runs all commands in the files specified by the arguments, \
            e.g., `$$ 'hi.pim'` to execute 'hi.pim'",
        );
        variables.add_built_in(
            Command::UsingStrings(StringsFor::Edit),
            "loads all images in the files specified by the arguments, \
            e.g., `$$ 'hi.png' 'hello.png'` to load both files",
        );
        variables.add_built_in(
            Command::UsingStrings(StringsFor::Concatenate),
            "concatenates all images in the files specified by the arguments, \
            e.g., `$$ 'hi.png' 'hello.png'` to put them side by side",
        );
        variables.add_built_in(
            Command::UsingStrings(StringsFor::ListFiles),
            "lists files in the current directory that match all arguments, \
            e.g., `$$ hi world` match `hi-world.png` and `world.hi`",
        );
        variables.add_built_in(
            Command::UsingTwoI64s(TwoI64sFor::Pan),
            "moves the view horizontally+vertically, \
            e.g., `$$ 11 23` pans (11, 23) in (X, Y) coordinates",
        );
        variables.add_built_in(
            Command::UsingTwoI64s(TwoI64sFor::FrameResize),
            "sets frame size, or crops to content if no arguments, \
            e.g., `$$ 12 34` to set to 12 pixels wide and 34 pixels high",
        );
        variables.add_built_in(
            Command::UsingTwoI64s(TwoI64sFor::FrameSwap),
            "swaps frames $0 and $1, \
            e.g., `$$ 2 4` to swap the third and fifth frame",
        );
        variables.add_built_in(
            Command::UsingTwoI64s(TwoI64sFor::SelectionDelta),
            "expand or shrink the selection box by ($0, $1) with defaults of 0, \
            e.g., `$$ 5 -3` to increase 5 in width and decrease 3 in height",
        );
        variables.add_built_in(
            Command::UsingTwoI64s(TwoI64sFor::SelectionDeltaSymmetric),
            "expand or shrink the selection box in a symmetric way by 2*($0, $1) with defaults of 0, \
            e.g., `$$ -4 +11` to shrink by 8 in width and expand by 22 in height",
        );
        variables.add_built_in(
            Command::UsingTwoI64s(TwoI64sFor::SelectionMove),
            "moves the selection box by ($0, $1) with defaults of 0, \
            e.g., `$$ 7 -9` to move +7 in X and -9 in Y",
        );
        variables.add_built_in(
            Command::UsingTwoI64s(TwoI64sFor::SelectionShift),
            "moves the pixels in the selection box by ($0, $1) with defaults of 0, \
            e.g., `$$ 7 -9` to move +7 in X and -9 in Y, moving the box as well",
        );
        variables.add_built_in(
            Command::PaletteColor,
            "getter/swapper for palette color $0 if $1 is null/present, \
            e.g., `$$ 7 #123456` to set palette color 7 to #123456",
        );
        variables.add_built_in(
            Command::PaletteAddColors,
            "adds all colors specified by arguments to the palette, \
            e.g., `$$ #987654` to ensure #987654 is in the palette",
        );
        variables.add_built_in(
            Command::PaletteAddGradient,
            "palette gradient from color $0 to color $1, with an \
            optional $2 (defaulting to 5) added colors\
            e.g., `$$ #404 #00aa33` to add 5 colors to the palette",
        );
        variables.add_built_in(
            Command::PaletteSort,
            "sorts the palette roughly by hue and lightness \
            e.g., `$$` and all arguments are ignored",
        );
        variables.add_built_in(
            Command::PaletteAddViewColors,
            "adds colors from the current view into the palette \
            e.g., `$$` and all arguments are ignored",
        );
        variables.add_built_in(
            Command::PaletteWriteToFile,
            "writes palette to file specified by $0 \
            e.g., `$$ 'warm.palette'` to write to the file warm.palette",
        );
        variables.add_built_in(
            Command::PaletteClear,
            "clears entire palette if no arguments, otherwise colors specified \
            by the arguments (index or color), \
            e.g., `$$ 1 #456` to remove palette entry 1 and #456 from palette",
        );
        variables.add_built_in(
            Command::Paint,
            "paints coordinates ($0, $1) with optional color $2, defaulting to foreground, \
            e.g., `$$ 3 4 #765432` to paint coordinate (3, 4) color #765432",
        );
        variables.add_built_in(
            Command::Bucket,
            "flood-fills starting at coordinates ($0, $1) with optional color $2, defaulting \
            to foreground, e.g., `$$ 3 4 #765432` to start flood-filling at (3, 4)",
        );
        variables.add_built_in(
            Command::Line,
            "draws a line starting at ($0, $1) going to ($2, $3) with optional color $4, defaulting \
            to foreground, e.g., `$$ 3 4 15 16 #765432` to draw from (3, 4) to (15, 16)",
        );
        variables.add_built_in(
            Command::MouseX,
            "returns the mouse X position for the current view. \
            e.g., `p $$ 5` paints the foreground color at coordinates (mouse-x, 5)",
        );
        variables.add_built_in(
            Command::MouseY,
            "returns the mouse Y position for the current view. \
            e.g., `p 3 $$` paints the foreground color at coordinates (3, mouse-y)",
        );
        variables.add_built_in(
            Command::Write,
            "writes the current image to storage, with optional file name and optional scale, \
            e.g., `$$` to save to current file, `$$ anim.gif` to save as a gif, and
            `$$ what.png 3` to save as a 3x-scaled png",
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
        variables.add_built_in(
            Command::Quit(Quit::AfterWrite),
            "saves the current view before quitting",
        );
        assert_ok!(variables.set(
            "run".to_string(),
            Variable::BuiltIn(Command::Error, "TODO".to_string())
        ));

        // Helpful constants.
        assert_ok!(variables.set("null".to_string(), Variable::Const(Argument::Null)));
        assert_ok!(variables.set("on".to_string(), Variable::Const(Argument::I64(1))));
        assert_ok!(variables.set("off".to_string(), Variable::Const(Argument::I64(0))));
        assert_ok!(variables.set("true".to_string(), Variable::Const(Argument::I64(1))));
        assert_ok!(variables.set("false".to_string(), Variable::Const(Argument::I64(0))));
        // TODO: consider how convenient for users it is to add `m` when `normal` might suffice, then maybe...
        // TODO: add `normalm` as a Const variable to the string "normal",
        // same for other modes that you can set, with `m` as a suffix.
        // TODO: link `normal` as a command to `mode normalm`.
        // TODO: add `home` as a const variable to the home directory

        assert_ok!(variables.set(
            "swap-fg-bg".to_string(),
            Variable::Const(Argument::Script(Script {
                command: Command::ColorSetting(ColorSetting::Foreground),
                arguments: vec![Argument::Script(Script {
                    command: Command::ColorSetting(ColorSetting::Background),
                    arguments: vec![Argument::Script(Script::zero_arg(Command::ColorSetting(
                        ColorSetting::Foreground
                    )))],
                })],
            })),
        ));
        // TODO: add "red", "blue", etc. as Mutable color variables
        // e.g., add `red 1`, `red 2`, etc., using OkLab colors

        assert_ok!(variables.set("cwd".to_string(), Variable::Alias("pwd".to_string())));
        assert_ok!(variables.set("multiply".to_string(), Variable::Alias("*".to_string())));
        assert_ok!(variables.set("product".to_string(), Variable::Alias("*".to_string())));
        assert_ok!(variables.set("sum".to_string(), Variable::Alias("+".to_string())));
        assert_ok!(variables.set("anim".to_string(), Variable::Alias("ui-a".to_string())));
        assert_ok!(variables.set(
            "ui-background".to_string(),
            Variable::Alias("ui-bg".to_string())
        ));
        assert_ok!(variables.set(
            "ui-checker".to_string(),
            Variable::Alias("checker".to_string())
        ));
        assert_ok!(variables.set("ui-grid".to_string(), Variable::Alias("grid".to_string())));
        assert_ok!(variables.set(
            "ui-grid-color".to_string(),
            Variable::Alias("grid-color".to_string())
        ));
        assert_ok!(variables.set(
            "scale%".to_string(),
            Variable::Alias("ui-scale%".to_string())
        ));
        assert_ok!(variables.set("foreground".to_string(), Variable::Alias("fg".to_string())));
        assert_ok!(variables.set("background".to_string(), Variable::Alias("bg".to_string())));
        assert_ok!(variables.set("paint".to_string(), Variable::Alias("p".to_string())));
        assert_ok!(variables.set("bucket".to_string(), Variable::Alias("b".to_string())));
        assert_ok!(variables.set("line".to_string(), Variable::Alias("l".to_string())));
        assert_ok!(variables.set("mouse-x".to_string(), Variable::Alias("mx".to_string())));
        assert_ok!(variables.set("mouse-y".to_string(), Variable::Alias("my".to_string())));
        assert_ok!(variables.set("cursor-x".to_string(), Variable::Alias("mx".to_string())));
        assert_ok!(variables.set("cursor-y".to_string(), Variable::Alias("my".to_string())));
        assert_ok!(variables.set("cx".to_string(), Variable::Alias("mx".to_string())));
        assert_ok!(variables.set("cy".to_string(), Variable::Alias("my".to_string())));
        assert_ok!(variables.set("f-add".to_string(), Variable::Alias("fa".to_string())));
        assert_ok!(variables.set("f-insert".to_string(), Variable::Alias("fi".to_string())));
        assert_ok!(variables.set("f-clone".to_string(), Variable::Alias("fc".to_string())));
        assert_ok!(variables.set("f-index".to_string(), Variable::Alias("f".to_string())));
        assert_ok!(variables.set("f-remove".to_string(), Variable::Alias("fr".to_string())));
        assert_ok!(variables.set("f-shift".to_string(), Variable::Alias("shift".to_string())));
        assert_ok!(variables.set("f-shift-x".to_string(), Variable::Alias("fsx".to_string())));
        assert_ok!(variables.set("f-shift-y".to_string(), Variable::Alias("fsy".to_string())));
        assert_ok!(variables.set("fw".to_string(), Variable::Alias("f-width".to_string())));
        assert_ok!(variables.set("fh".to_string(), Variable::Alias("f-height".to_string())));
        assert_ok!(variables.set("s-clear".to_string(), Variable::Alias("clear".to_string())));
        assert_ok!(variables.set("s-erase".to_string(), Variable::Alias("erase".to_string())));
        assert_ok!(variables.set("s-copy".to_string(), Variable::Alias("copy".to_string())));
        assert_ok!(variables.set("s-cut".to_string(), Variable::Alias("cut".to_string())));
        assert_ok!(variables.set(
            "s-swapcut".to_string(),
            Variable::Alias("swapcut".to_string())
        ));
        assert_ok!(variables.set("s-paste".to_string(), Variable::Alias("paste".to_string())));
        assert_ok!(variables.set(
            "s-mirrorx".to_string(),
            Variable::Alias("mirrorx".to_string())
        ));
        assert_ok!(variables.set(
            "s-mirrory".to_string(),
            Variable::Alias("mirrory".to_string())
        ));
        assert_ok!(variables.set(
            "s-flipx".to_string(),
            Variable::Alias("mirrorx".to_string())
        ));
        assert_ok!(variables.set(
            "s-flipy".to_string(),
            Variable::Alias("mirrory".to_string())
        ));
        assert_ok!(variables.set("slice".to_string(), Variable::Alias("split".to_string())));
        assert_ok!(variables.set("edit".to_string(), Variable::Alias("e".to_string())));
        assert_ok!(variables.set(
            "s-symdelta".to_string(),
            Variable::Alias("s-delta2".to_string())
        ));
        assert_ok!(variables.set(
            "s-deltasym".to_string(),
            Variable::Alias("s-delta2".to_string())
        ));
        assert_ok!(variables.set(
            "merge".to_string(),
            Variable::Const(Argument::Script(Script {
                command: Command::I64Setting(I64Setting::ImageSplit),
                arguments: vec![Argument::I64(1)],
            }))
        ));
        assert_ok!(variables.set("tool".to_string(), Variable::Alias("t".to_string())));
        assert_ok!(variables.set(
            "brusht".to_string(),
            Variable::Const(Argument::I64(Tool::Brush as i64))
        ));
        assert_ok!(variables.set(
            "buckett".to_string(),
            Variable::Const(Argument::I64(Tool::FloodFill as i64))
        ));
        assert_ok!(variables.set(
            "samplert".to_string(),
            Variable::Const(Argument::I64(Tool::Sampler as i64))
        ));
        assert_ok!(variables.set(
            "pant".to_string(),
            Variable::Const(Argument::I64(Tool::Pan as i64))
        ));
        assert_ok!(variables.set("view".to_string(), Variable::Alias("v".to_string())));
        assert_ok!(variables.set("quit".to_string(), Variable::Alias("q".to_string())));
        assert_ok!(variables.set("quit!".to_string(), Variable::Alias("q!".to_string())));
        for c in [
            "size", "erase", "multi", "perfect", "xsym", "ysym", "xray", "line",
        ] {
            assert_ok!(variables.set(format!("brush-{}", c), Variable::Alias(format!("b-{}", c))));
        }
        variables
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::gfx::Point;
    use crate::message::*;
    use crate::session::{KeyBinding, Mode, Select};
    use crate::view::ViewExtent;
    use std::collections::HashSet;
    use strum::IntoEnumIterator;

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
        SideEffect(String),
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
        message: Message,
    }

    script_runner! {TestRunner}
    impl TestRunner {
        const PAN_PIXELS: i64 = 32;
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
                test_what_ran: Vec::new(),
                variables: Variables::with_built_ins(),
                fg: Rgba8::WHITE,
                bg: Rgba8::BLACK,
                message: Message::default(),
            }
        }

        fn begin_script_command(&mut self, command: &Command) {
            self.test_what_ran.push(WhatRan::Begin(command.clone()));
        }

        fn end_script_command(&mut self, command: &Command) {
            self.test_what_ran.push(WhatRan::End(command.clone()));
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

        fn script_bucket(&mut self, x: i64, y: i64, color: Rgba8) -> ArgumentResult {
            self.test_what_ran
                .push(WhatRan::Mocked(format!("b {} {} {}", x, y, color)));
            // In the implementation, return the color that was under the cursor.
            Ok(Argument::Color(Self::PAINT_RETURN_COLOR))
        }

        fn script_line(&mut self, x0: i64, y0: i64, x1: i64, y1: i64, color: Rgba8) -> VoidResult {
            self.test_what_ran.push(WhatRan::Mocked(format!(
                "l {} {} {} {} {}",
                x0, y0, x1, y1, color
            )));
            Ok(())
        }

        fn add_view_colors(&mut self) {
            self.test_what_ran
                .push(WhatRan::Mocked("p-view".to_string()));
        }

        fn get_color_setting(&self, setting: ColorSetting) -> Rgba8 {
            match setting {
                ColorSetting::Foreground => self.fg,
                ColorSetting::Background => self.bg,
                _ => Rgba8 {
                    r: 99,
                    g: 88,
                    b: 77,
                    a: 66,
                },
            }
        }

        fn set_color_setting(&mut self, setting: ColorSetting, value: Rgba8) -> VoidResult {
            match setting {
                ColorSetting::Foreground => {
                    self.fg = value;
                }
                ColorSetting::Background => {
                    self.bg = value;
                }
                _ => {
                    self.test_what_ran
                        .push(WhatRan::Mocked(format!("set{:?}({})", setting, value)));
                }
            }
            Ok(())
        }

        fn get_string_setting(&self, _setting: StringSetting) -> String {
            "asdf".to_string()
        }

        fn set_string_setting(&mut self, setting: StringSetting, value: String) -> VoidResult {
            self.test_what_ran
                .push(WhatRan::Mocked(format!("set{:?}({})", setting, value)));
            Ok(())
        }

        fn get_i64_setting(&self, _setting: I64Setting) -> i64 {
            1234567890
        }

        fn set_i64_setting(
            &mut self,
            setting: I64Setting,
            old_value: i64,
            new_value: i64,
        ) -> VoidResult {
            self.test_what_ran.push(WhatRan::Mocked(format!(
                "set{:?}({} -> {})",
                setting, old_value, new_value
            )));
            Ok(())
        }

        pub fn get_active_view_mouse_coords(&self) -> Point<ViewExtent, f32> {
            Point::new(7733.0, 6644.0)
        }

        pub fn script_zero(&mut self, for_what: ZeroArgumentsFor) -> ArgumentResult {
            self.test_what_ran
                .push(WhatRan::Mocked(format!("{:?}", for_what)));
            Ok(Argument::Null)
        }

        pub fn script_optional_i64(
            &mut self,
            for_what: OptionalI64For,
            optional: Option<i64>,
        ) -> VoidResult {
            self.test_what_ran
                .push(WhatRan::Mocked(format!("{:?}{{{:?}}}", for_what, optional)));
            Ok(())
        }

        pub fn script_optional_color(
            &mut self,
            for_what: OptionalColorFor,
            optional: Option<Rgba8>,
        ) -> ArgumentResult {
            self.test_what_ran
                .push(WhatRan::Mocked(format!("{:?}{{{:?}}}", for_what, optional)));
            Ok(Argument::Null)
        }

        pub fn script_two_i64s(&mut self, for_what: TwoI64sFor, x: i64, y: i64) -> ArgumentResult {
            self.test_what_ran
                .push(WhatRan::Mocked(format!("{:?}{{{}, {}}}", for_what, x, y,)));
            Ok(Argument::Null)
        }

        fn script_strings(&mut self, for_what: StringsFor, strings: Vec<String>) -> ArgumentResult {
            for string in strings {
                self.test_what_ran
                    .push(WhatRan::Mocked(format!("{:?}{{{:?}}}", for_what, string)));
            }
            Ok(Argument::Null)
        }

        fn center_palette(&mut self) {
            self.test_what_ran
                .push(WhatRan::SideEffect("center_palette".to_string()));
        }

        pub fn bind_key(&mut self, mode: Mode, binding: KeyBinding) {
            self.test_what_ran
                .push(WhatRan::Mocked(format!("bind{:?}{:?}", mode, binding)));
        }

        fn script_write(&mut self, path: Option<String>, scale: Option<i64>) -> ArgumentResult {
            self.test_what_ran
                .push(WhatRan::Mocked(format!("write{{{:?}, {:?}}}", path, scale)));
            Ok(Argument::String("wrote some stuff".into()))
        }

        fn script_quit(&mut self, quit: Quit) -> ArgumentResult {
            self.test_what_ran
                .push(WhatRan::Mocked(format!("quit{{{:?}}}", quit)));
            Ok(Argument::Null)
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
            command: Command::ColorSetting(ColorSetting::Foreground),
            arguments: Vec::new(),
        };

        let mut test_runner = TestRunner::new();
        let result = script.run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::ColorSetting(ColorSetting::Foreground)),
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::End(Command::ColorSetting(ColorSetting::Foreground)),
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
                    command: Command::ColorSetting(ColorSetting::Foreground),
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
                WhatRan::Begin(Command::ColorSetting(ColorSetting::Foreground)),
                WhatRan::Evaluated(Ok(Argument::Color(Rgba8::RED))),
                WhatRan::End(Command::ColorSetting(ColorSetting::Foreground)),
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
        let palette_index: usize = 3;
        let script = Script {
            command: Command::PushPop,
            arguments: Vec::from([
                Argument::Script(Script {
                    command: Command::ColorSetting(ColorSetting::Foreground),
                    arguments: Vec::from([Argument::I64(palette_index as i64)]),
                }),
                Argument::Script(Script::zero_arg(Command::ColorSetting(
                    ColorSetting::Foreground,
                ))),
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
                WhatRan::Begin(Command::ColorSetting(ColorSetting::Foreground)),
                WhatRan::Evaluated(Ok(Argument::I64(palette_index as i64))),
                WhatRan::End(Command::ColorSetting(ColorSetting::Foreground)),
                WhatRan::Evaluated(Ok(Argument::Color(initial_color))),
                // Now running the later arguments:
                WhatRan::Begin(Command::ColorSetting(ColorSetting::Foreground)),
                WhatRan::Evaluated(Ok(Argument::Null)), // no argument to fg, getter.
                WhatRan::End(Command::ColorSetting(ColorSetting::Foreground)),
                // Foreground color inside the commands is evaluating to this:
                WhatRan::Evaluated(Ok(Argument::Color(
                    test_runner.palette.colors[palette_index]
                ))),
                // Now we are evaluating the "pop" part of PushPop; resetting color:
                WhatRan::Begin(Command::ColorSetting(ColorSetting::Foreground)),
                WhatRan::Evaluated(Ok(Argument::Color(initial_color))),
                WhatRan::End(Command::ColorSetting(ColorSetting::Foreground)),
                WhatRan::End(Command::PushPop),
            ])
        );
        // As a consequence of the swapper logic, PushPop will return what the value
        // was *inside* the script.
        assert_eq!(
            result,
            Ok(Argument::Color(test_runner.palette.colors[palette_index]))
        );
    }

    #[test]
    fn test_script_push_pop_successfully_resets_even_if_error() {
        let new_color = Rgba8::BLACK;
        let script = Script {
            command: Command::PushPop,
            arguments: Vec::from([
                Argument::Script(Script {
                    command: Command::ColorSetting(ColorSetting::Background),
                    arguments: Vec::from([Argument::Color(new_color)]),
                }),
                Argument::Script(Script::zero_arg(Command::ColorSetting(
                    ColorSetting::Background,
                ))),
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
                WhatRan::Begin(Command::ColorSetting(ColorSetting::Background)),
                WhatRan::Evaluated(Ok(Argument::Color(new_color))),
                WhatRan::End(Command::ColorSetting(ColorSetting::Background)),
                WhatRan::Evaluated(Ok(Argument::Color(initial_color))),
                // Now running the later arguments:
                WhatRan::Begin(Command::ColorSetting(ColorSetting::Background)),
                WhatRan::Evaluated(Ok(Argument::Null)), // no argument to bg, getter.
                WhatRan::End(Command::ColorSetting(ColorSetting::Background)),
                // Background color inside the commands is evaluating to this:
                WhatRan::Evaluated(Ok(Argument::Color(new_color))),
                // Trying something that will error:
                WhatRan::Evaluated(Err("$1234 should only be used inside a script".to_string())),
                // NOTE: I64(4042) should NOT be evaluated!
                // Now we are evaluating the "pop" part of PushPop; resetting color:
                WhatRan::Begin(Command::ColorSetting(ColorSetting::Background)),
                WhatRan::Evaluated(Ok(Argument::Color(initial_color))),
                WhatRan::End(Command::ColorSetting(ColorSetting::Background)),
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
                Argument::Script(Script::zero_arg(Command::ColorSetting(
                    ColorSetting::Background,
                ))),
            ]),
        };
        let mut test_runner = TestRunner::new();

        let result = script.run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::PushPop),
                WhatRan::End(Command::PushPop),
            ])
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
                Argument::Script(Script::zero_arg(Command::ColorSetting(
                    ColorSetting::Foreground,
                ))),
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
                Argument::Script(Script::zero_arg(Command::ColorSetting(
                    ColorSetting::Background,
                ))),
            ]),
        };
        let mut test_runner = TestRunner::new();

        let result = script.run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::RunAll),
                WhatRan::Begin(Command::ColorSetting(ColorSetting::Foreground)),
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::End(Command::ColorSetting(ColorSetting::Foreground)),
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
                WhatRan::End(Command::RunAll),
            ])
        );
        assert_eq!(result, Err("".to_string()));
    }

    #[test]
    fn test_script_run_all_returns_ok_result() {
        let script = Script {
            command: Command::RunAll,
            arguments: Vec::from([
                Argument::Script(Script::zero_arg(Command::ColorSetting(
                    ColorSetting::Foreground,
                ))),
                Argument::Script(Script::zero_arg(Command::ColorSetting(
                    ColorSetting::Background,
                ))),
                Argument::I64(33),
            ]),
        };
        let mut test_runner = TestRunner::new();

        let result = script.run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::RunAll),
                WhatRan::Begin(Command::ColorSetting(ColorSetting::Foreground)),
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::End(Command::ColorSetting(ColorSetting::Foreground)),
                WhatRan::Evaluated(Ok(Argument::Color(test_runner.fg))),
                WhatRan::Begin(Command::ColorSetting(ColorSetting::Background)),
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::End(Command::ColorSetting(ColorSetting::Background)),
                WhatRan::Evaluated(Ok(Argument::Color(test_runner.bg))),
                WhatRan::Evaluated(Ok(Argument::I64(33))),
                WhatRan::End(Command::RunAll),
            ])
        );
        assert_eq!(result, Ok(Argument::I64(33)));
    }

    #[test]
    fn test_script_source_returns_null() {
        let script = Script {
            command: Command::UsingStrings(StringsFor::Source),
            arguments: Vec::from([
                Argument::String("/home/wow/my.pim".to_string()),
                Argument::String("/great/my-other.pim".to_string()),
            ]),
        };
        let mut test_runner = TestRunner::new();

        let result = script.run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::UsingStrings(StringsFor::Source)),
                WhatRan::Evaluated(Ok(Argument::String("/home/wow/my.pim".to_string()))),
                WhatRan::Evaluated(Ok(Argument::String("/great/my-other.pim".to_string()))),
                WhatRan::Mocked("Source{\"/home/wow/my.pim\"}".to_string()),
                WhatRan::Mocked("Source{\"/great/my-other.pim\"}".to_string()),
                WhatRan::End(Command::UsingStrings(StringsFor::Source)),
            ])
        );
        assert_eq!(result, Ok(Argument::Null));
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
                    arguments: vec![
                        Argument::Null,
                        Argument::String("xyz".to_string()),
                        Argument::I64(456),
                    ],
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
                Argument::Script(Script::zero_arg(Command::ColorSetting(
                    ColorSetting::Foreground,
                ))),
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
                WhatRan::Begin(Command::ColorSetting(ColorSetting::Foreground)),
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::End(Command::ColorSetting(ColorSetting::Foreground)),
                WhatRan::Evaluated(Ok(Argument::Color(test_runner.fg))),
                WhatRan::End(Command::Echo),
            ])
        );
        let message = "123 'jkl;' (error) 456 (error `123 null #ff0000`) #ffffff".to_string();
        assert_eq!(result, Ok(Argument::String(message.clone())));
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
                Argument::Script(Script::zero_arg(Command::ColorSetting(
                    ColorSetting::Background,
                ))),
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
                WhatRan::Begin(Command::ColorSetting(ColorSetting::Background)),
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::End(Command::ColorSetting(ColorSetting::Background)),
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
    fn test_script_even_sums_and_can_look_back_multiple_script_arguments() {
        let mut test_runner = TestRunner::new();
        let function = Script {
            command: Command::If,
            arguments: vec![
                Argument::Script(Script {
                    command: Command::Even,
                    arguments: vec![
                        Argument::Use(Use {
                            index: 0,
                            lookback: -2,
                        }),
                        // Put a null in here to ensure we don't stop the even sum at null:
                        Argument::Null,
                        Argument::I64(0),
                        Argument::Use(Use {
                            index: 1,
                            lookback: -2,
                        }),
                    ],
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
                WhatRan::Evaluated(Ok(Argument::I64(3))),
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::Evaluated(Ok(Argument::I64(0))),
                WhatRan::Evaluated(Ok(Argument::I64(5))),
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
    fn test_script_odd_sums_and_evaluates_all_arguments() {
        let mut test_runner = TestRunner::new();

        let result = Script {
            command: Command::Odd,
            arguments: vec![
                // Put a null in here to ensure we don't stop the odd sum at null:
                Argument::Null,
                Argument::Script(Script::zero_arg(Command::Sum)), // should be 0
                Argument::Script(Script::zero_arg(Command::Product)), // should be 1
                Argument::Script(Script {
                    command: Command::Product,
                    arguments: vec![Argument::I64(3), Argument::I64(4), Argument::I64(5)],
                }),
            ],
        }
        .run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            vec![
                WhatRan::Begin(Command::Odd),
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::Begin(Command::Sum),
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::End(Command::Sum),
                WhatRan::Evaluated(Ok(Argument::I64(0))), // null sum is 0
                WhatRan::Begin(Command::Product),
                WhatRan::End(Command::Product),
                WhatRan::Evaluated(Ok(Argument::I64(1))), // empty product is 1
                WhatRan::Begin(Command::Product),
                WhatRan::Evaluated(Ok(Argument::I64(3))),
                WhatRan::Evaluated(Ok(Argument::I64(4))),
                WhatRan::Evaluated(Ok(Argument::I64(5))),
                WhatRan::End(Command::Product),
                WhatRan::Evaluated(Ok(Argument::I64(60))),
                WhatRan::End(Command::Odd),
            ]
        );
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some(Argument::I64(1))); // 0 + 1 + 60 should be odd
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
                    command: Command::ColorSetting(ColorSetting::Foreground),
                    arguments: vec![Argument::Color(Rgba8::BLACK)],
                }),
                Argument::Script(Script {
                    command: Command::ColorSetting(ColorSetting::Background),
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
                WhatRan::Begin(Command::ColorSetting(ColorSetting::Foreground)),
                WhatRan::Evaluated(Ok(Argument::Color(Rgba8::BLACK))),
                WhatRan::End(Command::ColorSetting(ColorSetting::Foreground)),
                WhatRan::Evaluated(Ok(Argument::Color(Rgba8::WHITE))), // previous FG color
                WhatRan::End(Command::If),
                // Second run:
                WhatRan::Begin(Command::If),
                WhatRan::Begin(Command::Evaluate(variable_name.clone())),
                WhatRan::End(Command::Evaluate(variable_name.clone())),
                WhatRan::Evaluated(Ok(Argument::I64(0))),
                WhatRan::Begin(Command::ColorSetting(ColorSetting::Background)),
                WhatRan::Evaluated(Ok(Argument::Color(Rgba8::RED))),
                WhatRan::End(Command::ColorSetting(ColorSetting::Background)),
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
                    command: Command::ColorSetting(ColorSetting::Foreground),
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
                WhatRan::Begin(Command::ColorSetting(ColorSetting::Foreground)),
                WhatRan::Evaluated(Ok(Argument::Color(Rgba8::BLUE))),
                WhatRan::End(Command::ColorSetting(ColorSetting::Foreground)),
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
                    command: Command::ColorSetting(ColorSetting::Background),
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
                WhatRan::Begin(Command::ColorSetting(ColorSetting::Background)),
                WhatRan::Evaluated(Ok(Argument::Color(Rgba8::BLACK))),
                WhatRan::End(Command::ColorSetting(ColorSetting::Background)),
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
                WhatRan::Evaluated(Err("$1 should only be used inside a script".to_string())),
                WhatRan::End(Command::If),
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
            result,
            Ok(Argument::String(
                "? -- explains what $0 does without evaluating it, \
            e.g., `? paint` to explain what the `paint` command does"
                    .to_string()
            ))
        );

        let result = Script {
            command: Command::Help,
            arguments: vec![Argument::Script(Script::zero_arg(Command::If))],
        }
        .run(&mut test_runner);
        assert_eq!(
            result,
            Ok(Argument::String(
                "if -- if $0 evaluates to truthy, evaluates $1, otherwise $2, \
            e.g., `if 'hi' 'world' 3` returns 'world'"
                    .to_string(),
            ))
        );

        let result = Script {
            command: Command::Help,
            arguments: vec![Argument::Script(Script::zero_arg(Command::StringSetting(
                StringSetting::Mode,
            )))],
        }
        .run(&mut test_runner);
        assert_eq!(
            result,
            Ok(Argument::String(
                "mode -- getter/swapper for the current mode if $0 is null/present, \
                e.g., `mode 'normal'` to go to normal mode"
                    .to_string(),
            ))
        );

        let result = Script {
            command: Command::Help,
            arguments: vec![Argument::Use(Use {
                index: 3,
                lookback: 0,
            })],
        }
        .run(&mut test_runner);
        assert_eq!(
            result,
            Ok(Argument::String(
                "-- a way to evaluate an argument at position 3; \
                e.g., `const 'ten' (sum $0 10)` will add 10 to argument 0, \
                and can be called via `ten 123` which will return `133`"
                    .to_string(),
            ))
        );

        let result = Script {
            command: Command::Help,
            arguments: vec![Argument::String("blah".to_string())],
        }
        .run(&mut test_runner);
        assert_eq!(
            result,
            Ok(Argument::String("'blah' -- an argument".to_string()))
        );

        let result = Script {
            command: Command::Help,
            arguments: vec![Argument::Script(Script {
                command: Command::If,
                arguments: vec![Argument::I64(2)],
            })],
        }
        .run(&mut test_runner);
        assert_eq!(
            result,
            Ok(Argument::String("(if 2) -- a script".to_string()))
        );
    }

    #[test]
    fn test_script_can_add_new_palette_color() {
        let new_color = Rgba8 {
            r: 5,
            g: 225,
            b: 75,
            a: 33,
        };
        let mut test_runner = TestRunner::new();
        let initial_palette_size = test_runner.palette.colors.len();

        let result = Script {
            command: Command::PaletteAddColors,
            arguments: vec![Argument::Color(new_color)],
        }
        .run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::PaletteAddColors),
                WhatRan::Evaluated(Ok(Argument::Color(new_color))),
                WhatRan::SideEffect("center_palette".to_string()),
                WhatRan::End(Command::PaletteAddColors),
            ])
        );
        assert_eq!(
            result,
            Ok(Argument::I64(test_runner.palette.colors.len() as i64 - 1))
        );
        assert_eq!(test_runner.palette.colors.len(), initial_palette_size + 1);
    }

    #[test]
    fn test_script_ignores_adding_already_present_palette_color() {
        let mut test_runner = TestRunner::new();
        let already_present_index = 3;
        let already_present = test_runner.palette.colors[already_present_index];
        let initial_palette_size = test_runner.palette.colors.len();

        let result = Script {
            command: Command::PaletteAddColors,
            arguments: vec![Argument::Color(already_present)],
        }
        .run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::PaletteAddColors),
                WhatRan::Evaluated(Ok(Argument::Color(already_present))),
                WhatRan::SideEffect("center_palette".to_string()),
                WhatRan::End(Command::PaletteAddColors),
            ])
        );
        assert_eq!(result, Ok(Argument::I64(already_present_index as i64)));
        assert_eq!(test_runner.palette.colors.len(), initial_palette_size);
    }

    #[test]
    fn test_script_can_add_gradient_using_existing_palette_color_and_new_color() {
        let new_color = Rgba8 {
            r: 5,
            g: 225,
            b: 75,
            a: 100,
        };
        let mut test_runner = TestRunner::new();
        let initial_palette_size = test_runner.palette.colors.len();

        let result = Script {
            command: Command::PaletteAddGradient,
            arguments: vec![
                Argument::Color(new_color),
                Argument::I64(3), // existing palette color
                                  // Use the default "Null" number of gradient colors to add
            ],
        }
        .run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::PaletteAddGradient),
                WhatRan::Evaluated(Ok(Argument::Color(new_color))),
                WhatRan::Evaluated(Ok(Argument::I64(3))),
                WhatRan::Evaluated(Ok(Argument::Null)), // number of colors
                WhatRan::SideEffect("center_palette".to_string()),
                WhatRan::End(Command::PaletteAddGradient),
            ])
        );
        assert_eq!(
            result,
            // Only adds 4 new palette entries since color 3 was already present.
            Ok(Argument::I64(4))
        );
        assert_eq!(test_runner.palette.colors.len(), initial_palette_size + 4);
        assert_eq!(
            test_runner.palette.colors[initial_palette_size..initial_palette_size + 4],
            vec![
                Rgba8 {
                    r: 5,
                    g: 225,
                    b: 75,
                    a: 100
                },
                Rgba8 {
                    r: 20,
                    g: 157,
                    b: 65,
                    a: 139
                },
                Rgba8 {
                    r: 30,
                    g: 104,
                    b: 55,
                    a: 178
                },
                Rgba8 {
                    r: 33,
                    g: 64,
                    b: 46,
                    a: 216
                }
            ]
        );
    }

    #[test]
    fn test_script_can_add_gradient_with_specified_color_count() {
        let new_color = Rgba8 {
            r: 200,
            g: 0,
            b: 100,
            a: 0,
        };
        let mut test_runner = TestRunner::new();
        let initial_palette_size = test_runner.palette.colors.len();

        let result = Script {
            command: Command::PaletteAddGradient,
            arguments: vec![
                Argument::I64(5), // existing palette color
                Argument::Color(new_color),
                Argument::I64(4),
            ],
        }
        .run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::PaletteAddGradient),
                WhatRan::Evaluated(Ok(Argument::I64(5))),
                WhatRan::Evaluated(Ok(Argument::Color(new_color))),
                WhatRan::Evaluated(Ok(Argument::I64(4))), // number of colors
                WhatRan::SideEffect("center_palette".to_string()),
                WhatRan::End(Command::PaletteAddGradient),
            ])
        );
        assert_eq!(
            result,
            // Only adds 3 new palette entries since color 5 was already present.
            Ok(Argument::I64(3))
        );
        assert_eq!(test_runner.palette.colors.len(), initial_palette_size + 3);
        assert_eq!(
            test_runner.palette.colors[initial_palette_size..initial_palette_size + 3],
            vec![
                Rgba8 {
                    r: 99,
                    g: 38,
                    b: 69,
                    a: 170
                },
                Rgba8 {
                    r: 147,
                    g: 20,
                    b: 84,
                    a: 85
                },
                Rgba8 {
                    r: 200,
                    g: 0,
                    b: 100,
                    a: 0
                }
            ]
        );
    }

    #[test]
    fn test_script_clear_palette_without_arguments() {
        let mut test_runner = TestRunner::new();
        let initial_palette_size = test_runner.palette.colors.len();

        let result = Script {
            command: Command::PaletteClear,
            arguments: vec![],
        }
        .run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::PaletteClear),
                WhatRan::End(Command::PaletteClear),
            ])
        );
        assert_eq!(result, Ok(Argument::I64(initial_palette_size as i64)));
        assert_eq!(test_runner.palette.colors.len(), 0);
    }

    #[test]
    fn test_script_clear_palette_with_null_arguments_still_clears_all() {
        let mut test_runner = TestRunner::new();
        let initial_palette_size = test_runner.palette.colors.len();

        let result = Script {
            command: Command::PaletteClear,
            arguments: vec![Argument::Null],
        }
        .run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::PaletteClear),
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::End(Command::PaletteClear),
            ])
        );
        assert_eq!(result, Ok(Argument::I64(initial_palette_size as i64)));
        assert_eq!(test_runner.palette.colors.len(), 0);
    }

    #[test]
    fn test_script_clear_palette_with_arguments() {
        let mut test_runner = TestRunner::new();
        let initial_palette_size = test_runner.palette.colors.len();
        let remove_color_1 = test_runner.palette.colors[1];
        let remove_color_2 = test_runner.palette.colors[2];
        let unknown_color = Rgba8 {
            r: 1,
            g: 2,
            b: 3,
            a: 4,
        };
        assert_eq!(test_runner.palette.colors.contains(&remove_color_1), true);
        assert_eq!(test_runner.palette.colors.contains(&remove_color_2), true);
        assert_eq!(test_runner.palette.colors.contains(&unknown_color), false);

        let result = Script {
            command: Command::PaletteClear,
            arguments: vec![
                Argument::Null,
                // Make sure we naively delete a color before an indexed color,
                // so that we ensure that both are removed correctly.
                Argument::Color(remove_color_1),
                Argument::I64(2),
                // This is a bogus color that's not present; but it shouldn't throw.
                Argument::Color(unknown_color),
            ],
        }
        .run(&mut test_runner);

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::PaletteClear),
                WhatRan::Evaluated(Ok(Argument::Null)),
                WhatRan::Evaluated(Ok(Argument::Color(remove_color_1))),
                WhatRan::Evaluated(Ok(Argument::I64(2))),
                WhatRan::Evaluated(Ok(Argument::Color(unknown_color))),
                WhatRan::End(Command::PaletteClear),
            ])
        );
        assert_eq!(test_runner.palette.colors.contains(&remove_color_1), false);
        assert_eq!(test_runner.palette.colors.contains(&remove_color_2), false);
        assert_eq!(test_runner.palette.colors.contains(&unknown_color), false);
        assert_eq!(result, Ok(Argument::I64(2)));
        assert_eq!(test_runner.palette.colors.len(), initial_palette_size - 2);
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
                Variable::BuiltIn(Command::Even, "try to override".to_string())
            ),
            Err("built-in `const` is not reassignable".to_string())
        );

        for color_setting in ColorSetting::iter() {
            let name = format!("{}", Command::ColorSetting(color_setting));
            assert_eq!(
                variables.set(
                    name.clone(),
                    Variable::BuiltIn(Command::If, "should not override".to_string())
                ),
                Err(format!("built-in `{}` is not reassignable", name))
            );
        }
        for string_setting in StringSetting::iter() {
            let name = format!("{}", Command::StringSetting(string_setting));
            assert_eq!(
                variables.set(
                    name.clone(),
                    Variable::BuiltIn(Command::If, "should not override".to_string())
                ),
                Err(format!("built-in `{}` is not reassignable", name))
            );
        }
        for i64_setting in I64Setting::iter() {
            let name = format!("{}", Command::I64Setting(i64_setting));
            assert_eq!(
                variables.set(
                    name.clone(),
                    Variable::BuiltIn(Command::Odd, "should not override".to_string())
                ),
                Err(format!("built-in `{}` is not reassignable", name))
            );
        }
        for what_for in ZeroArgumentsFor::iter() {
            let name = format!("{}", Command::WithoutArguments(what_for));
            assert_eq!(
                variables.set(
                    name.clone(),
                    Variable::BuiltIn(Command::Even, "should not override".to_string())
                ),
                Err(format!("built-in `{}` is not reassignable", name))
            );
        }
        for optional_i64_for in OptionalI64For::iter() {
            let name = format!("{}", Command::UsingOptionalI64(optional_i64_for));
            assert_eq!(
                variables.set(
                    name.clone(),
                    Variable::BuiltIn(Command::Odd, "should not override".to_string())
                ),
                Err(format!("built-in `{}` is not reassignable", name))
            );
        }
        for for_what in OptionalColorFor::iter() {
            let name = format!("{}", Command::UsingOptionalColor(for_what));
            assert_eq!(
                variables.set(
                    name.clone(),
                    Variable::BuiltIn(Command::Odd, "should not override".to_string())
                ),
                Err(format!("built-in `{}` is not reassignable", name))
            );
        }
        for strings_for in StringsFor::iter() {
            let name = format!("{}", Command::UsingStrings(strings_for));
            assert_eq!(
                variables.set(
                    name.clone(),
                    Variable::BuiltIn(Command::Odd, "should not override".to_string())
                ),
                Err(format!("built-in `{}` is not reassignable", name))
            );
        }
        for what_for in TwoI64sFor::iter() {
            let name = format!("{}", Command::UsingTwoI64s(what_for));
            assert_eq!(
                variables.set(
                    name.clone(),
                    Variable::BuiltIn(Command::Odd, "should not override".to_string())
                ),
                Err(format!("built-in `{}` is not reassignable", name))
            );
        }
    }

    #[test]
    fn test_variables_has_some_const_defaults() {
        let mut variables = Variables::with_built_ins();
        let mut check_variable = |name: &str, value: Argument| {
            let name_string = name.to_string();
            assert_eq!(variables.get(&name_string), Get::Argument(value));
            assert_eq!(
                variables.set(name_string, Variable::Mutable(Argument::I64(123))),
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
        let name = "swap-fg-bg".to_string();
        let swap = variables
            .get(&name)
            .to_argument()
            .get_script("for test")
            .unwrap();
        assert_eq!(format!("{}", swap), "fg (bg fg)".to_string());
    }

    #[test]
    fn test_variables_can_run_built_in_scripts() {
        let mut test_runner = TestRunner::new();

        assert_eq!(
            Script {
                command: Command::Evaluate("b".to_string()),
                arguments: vec![
                    Argument::I64(11),
                    Argument::I64(22),
                    Argument::Color(Rgba8::GREEN),
                ],
            }
            .run(&mut test_runner),
            Ok(Argument::Color(TestRunner::PAINT_RETURN_COLOR))
        );

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::Evaluate("b".to_string())),
                WhatRan::Evaluated(Ok(Argument::I64(11))),
                WhatRan::Evaluated(Ok(Argument::I64(22))),
                WhatRan::Evaluated(Ok(Argument::Color(Rgba8::GREEN))),
                WhatRan::Mocked("b 11 22 #00ff00".to_string()),
                WhatRan::End(Command::Evaluate("b".to_string())),
            ])
        );
    }

    #[test]
    fn test_variables_can_run_conditional_built_in_scripts() {
        let mut test_runner = TestRunner::new();

        assert_eq!(
            Script {
                command: Command::Evaluate("if".to_string()),
                arguments: vec![
                    Argument::I64(0),
                    Argument::Color(Rgba8::RED),
                    Argument::Color(Rgba8::GREEN),
                ],
            }
            .run(&mut test_runner),
            Ok(Argument::Color(Rgba8::GREEN))
        );

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::Evaluate("if".to_string())),
                WhatRan::Evaluated(Ok(Argument::I64(0))),
                WhatRan::Evaluated(Ok(Argument::Color(Rgba8::GREEN))),
                WhatRan::End(Command::Evaluate("if".to_string())),
            ])
        );
    }

    #[test]
    fn test_variables_can_run_aliased_scripts() {
        let mut test_runner = TestRunner::new();

        assert_eq!(
            Script {
                command: Command::Evaluate("paint".to_string()),
                arguments: vec![
                    Argument::I64(101),
                    Argument::I64(202),
                    Argument::Color(Rgba8::BLUE),
                ],
            }
            .run(&mut test_runner),
            Ok(Argument::Color(TestRunner::PAINT_RETURN_COLOR))
        );

        assert_eq!(
            test_runner.test_what_ran,
            Vec::from([
                WhatRan::Begin(Command::Evaluate("paint".to_string())),
                WhatRan::Evaluated(Ok(Argument::I64(101))),
                WhatRan::Evaluated(Ok(Argument::I64(202))),
                WhatRan::Evaluated(Ok(Argument::Color(Rgba8::BLUE))),
                WhatRan::Mocked("p 101 202 #0000ff".to_string()),
                WhatRan::End(Command::Evaluate("paint".to_string())),
            ])
        );
    }

    #[test]
    fn test_variables_can_add_const_variables() {
        let mut variables = Variables::new();

        let var_name = "wyzx".to_string();
        // Nothing useful in variables yet:
        assert_eq!(
            variables.get(&var_name).to_argument(),
            Argument::String(var_name.clone())
        );

        // First assignment is fine:
        assert_eq!(
            variables.set(var_name.clone(), Variable::Const(Argument::I64(456))),
            // The previous value of this variable was null:
            Ok(Argument::Null)
        );
        assert_eq!(variables.get(&var_name).to_argument(), Argument::I64(456));

        // Can't reassign:
        assert_eq!(
            variables.set(var_name.clone(), Variable::Const(Argument::I64(789))),
            Err(format!("variable `{}` is not reassignable", var_name))
        );
        assert_eq!(variables.get(&var_name).to_argument(), Argument::I64(456)); // No change!
    }

    #[test]
    fn test_variables_can_set_mutable_variables() {
        let mut variables = Variables::new();

        let var_name = "cdef".to_string();
        // Nothing in variables yet, so it'll use the name as the return string:
        assert_eq!(
            variables.get(&var_name).to_argument(),
            Argument::String(var_name.clone())
        );

        // First assignment is fine:
        assert_eq!(
            variables.set(var_name.clone(), Variable::Mutable(Argument::I64(456))),
            // The previous value of this variable was null:
            Ok(Argument::Null)
        );
        assert_eq!(variables.get(&var_name), Get::Argument(Argument::I64(456)));

        // Can't reassign:
        assert_eq!(
            variables.set(var_name.clone(), Variable::Mutable(Argument::I64(789))),
            // Returns the old value:
            Ok(Argument::I64(456))
        );
        assert_eq!(variables.get(&var_name).to_argument(), Argument::I64(789));
    }

    #[test]
    fn test_variables_complains_about_separators() {
        let mut variables = Variables::new();
        let name = format!("whatever{}this-is", MAIN_SEPARATOR);

        assert_eq!(
            variables.set(name.clone(), Variable::Mutable(Argument::I64(3))),
            Err(format!(
                "variable names should not contain `{}` or `.` which indicate paths",
                MAIN_SEPARATOR
            ))
        );
    }

    #[test]
    fn test_variables_complains_about_periods() {
        let mut variables = Variables::new();
        let name = "yoyo.123".to_string();

        assert_eq!(
            variables.set(name.clone(), Variable::Mutable(Argument::I64(3))),
            Err(format!(
                "variable names should not contain `{}` or `.` which indicate paths",
                MAIN_SEPARATOR
            ))
        );
    }

    #[test]
    fn test_variables_complains_about_spaces() {
        let mut variables = Variables::new();
        let name = "asdf jkl;".to_string();

        assert_eq!(
            variables.set(name.clone(), Variable::Mutable(Argument::I64(3))),
            Err("variable names should not contain spaces".to_string())
        );
    }

    #[test]
    fn test_variables_can_resolve_aliases() {
        let mut variables = Variables::new();

        let x = "X".to_string();
        let y = "Y".to_string();
        let z = "Z".to_string();

        _ = variables.set(x.clone(), Variable::Alias(y.clone()));
        // If Y isn't defined yet, so it'll resolve to Y's name:
        assert_eq!(variables.get(&x).to_argument(), Argument::String(y.clone()));

        _ = variables.set(y.clone(), Variable::Alias(z.clone()));
        // Z isn't defined yet, either, so it'll resolve to Z's name:
        assert_eq!(variables.get(&x).to_argument(), Argument::String(z.clone()));
        assert_eq!(variables.get(&y).to_argument(), Argument::String(z.clone()));

        _ = variables.set(z.clone(), Variable::Const(Argument::I64(123)));
        assert_eq!(variables.get(&x).to_argument(), Argument::I64(123));
        assert_eq!(variables.get(&y).to_argument(), Argument::I64(123));
        assert_eq!(variables.get(&z).to_argument(), Argument::I64(123)); // not an alias, but should make sense!
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
            command: Command::ColorSetting(ColorSetting::Foreground),
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
            variables.get(&var_name).to_argument(),
            Argument::Script(lambda.clone())
        );

        // Double check that we can mutate:
        script.arguments[1] = Argument::I64(-123);
        assert_eq!(
            variables.set_from_script(&script),
            Ok(Argument::Script(lambda))
        );
        assert_eq!(variables.get(&var_name).to_argument(), Argument::I64(-123));
    }

    #[test]
    fn test_variables_set_from_const_script() {
        let mut variables = Variables::new();
        let var_name = "blast-dog".to_string();
        let lambda = Script {
            command: Command::ColorSetting(ColorSetting::Foreground),
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
            variables.get(&var_name).to_argument(),
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
        assert_eq!(
            variables.get(&var_name).to_argument(),
            Argument::Script(lambda)
        );
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
        assert_eq!(
            variables.get(&alias_name).to_argument(),
            aliased_value.clone()
        );

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
        assert_eq!(
            variables.get(&alias_name).to_argument(),
            aliased_value.clone()
        );
        // But we can modify the underlying value:
        aliased_value = Argument::I64(8675309);
        assert_eq!(
            variables.set(
                alias_value.clone(),
                Variable::Mutable(aliased_value.clone()),
            ),
            Ok(Argument::String("hello".to_string()))
        );
        assert_eq!(variables.get(&alias_name).to_argument(), aliased_value);
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
                            command: Command::ColorSetting(ColorSetting::Foreground),
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
            format!("{}", script),
            "if $0 (p x1 y2 $1) (jangles (fg $2) q $3)".to_string()
        );
    }

    #[test]
    fn test_argument_string_can_become_input() {
        assert_eq!(
            Argument::String("a".to_string()).get_input(),
            Ok(Input::Rune(ModifiersState::default(), 'a'))
        );

        assert_eq!(
            Argument::String("Ö".to_string()).get_input(),
            Ok(Input::Rune(ModifiersState::default(), 'Ö'))
        );

        // But only if it's exactly one char long:
        assert_eq!(
            Argument::String("xy".to_string()).get_input(),
            Err("invalid rune: 'xy'".to_string())
        );

        assert_eq!(
            Argument::String("".to_string()).get_input(),
            Err("invalid rune: <empty-string>".to_string())
        );
    }
}
