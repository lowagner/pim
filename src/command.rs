use crate::autocomplete::{self, Autocomplete, FileCompleter, FileCompleterOpts};
use crate::history::History;
use crate::parser::*;
use crate::script::Script;
use crate::settings::*;

use memoir::*;
use strum_macros::EnumIter;

use std::fmt;
use std::path::Path;
use std::str::FromStr;

pub const COMMENT: char = '-';

/*
syntax:
    * `cmd arg_or_expression1 arg_or_expression2 ...`.  Each command
    can consume a variable number of args, but usually specified beforehand.

    * `(cmd ...)`: evaluate the command `cmd` with args `...`, so that you
    can nest commands: `cmd1 (cmd2 ...) (cmd3 ...)` will pass the output
    of command `cmd2` and `cmd3` to `cmd1`.

    TODO:
    * `cmd arg ;` forces the command to stop consuming arguments after `arg`.
    This is most useful if you want to chain multiple commands in one command,
    e.g., `cmd1 args1... ; cmd2 args2... ; cmd3 args3...`.

    TODO:
    * `if (cmd) (true-branch) (false-branch)`
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
    /// Returns 1 if the sum of all arguments is even, otherwise 0.
    Even,
    /// Returns 1 if the sum of all arguments is odd, otherwise 0.
    Odd,
    // TODO: LessThan: evaluates $0, ensures that it's less than $1 (defaults to 0).
    // if there are more arguments: ensures that $1 is less than $2.
    // etc. for $n less than ${n+1}, returning false early if any
    // later is greater.
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
    // TODO: we should define a way to evaluate something and put its value into a variable;
    //       everything below is lazily evaluated.  e.g., `set-val 'my-var' (...)`
    //       or maybe `set-eval 'my-var' (...)`
    // TODO: convert these to `NamedScript(NamedScript)` command, e.g., with a String and a Script as arguments.
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

    /// Uses $0 for the input binding, to run the script at $1 on press and
    /// with an optional script at $2 for release.
    // TODO: we should pass the result of `$1` on press into the release script as an argument.
    // TODO: we should come up with some "hold" input/script that fires while you're holding a key down.
    Bind(Bind),
    /// Getter/swapper for various settings that are colors.  These can be set via palette index (i64)
    /// or via a color argument (Rgba8).
    ColorSetting(ColorSetting),
    /// Getter/swapper for various settings that are strings.
    StringSetting(StringSetting),
    /// Getter/swapper for various settings that are ints (or booleans).
    // TODO: probably can switch to I32Setting everywhere (e.g., Argument::I32)
    I64Setting(I64Setting),
    /// Commands that don't take any arguments.
    WithoutArguments(ZeroArgumentsFor),
    /// Commands that take an optional i64 argument (but aren't an I64Setting).
    UsingOptionalI64(OptionalI64For),
    /// Commands which take an optional color argument (but aren't a ColorSetting).
    UsingOptionalColor(OptionalColorFor),
    /// Commands that take multiple strings.
    UsingStrings(StringsFor),
    /// Commands that take two integers which will default to 0 if the arguments are Null.
    UsingTwoI64s(TwoI64sFor),

    // TODO: some of these frame manipulation methods can't be used together unless
    //      we maybe handle the effects after every action.
    // TODO: FrameSwap, "fs" with two indices
    // TODO: FrameClear "fc" with index and color, use view.clear_frame
    // TODO: FrameShift, moves the animation over one to start one frame down
    // TODO: FileAppend, keeps the same height but centers the contents in $0 (file name) and adds to animation
    // TODO: WrapHorizontally: moves the contents of a single frame over to the right by $0 amount,
    //          wrapping that many pixels to the left side
    // TODO: WrapVertically: moves the contents of a single frame down by $0 amount,
    //          wrapping that many pixels to the top side

    // TODO: FitPixelWidth, FitPixelHeight; zoom to fit that many pixels within the screen based on available area
    // TODO: convert fg and bg to ColorSetting settings.
    /// Uses $0 for the palette index, and $1 as an optional color to update the palette at that index.
    /// Returns the original color in the palette.
    PaletteColor,
    /// Adds a new color to the palette if present in $0.  Returns the index of this color.
    /// Will check the current palette before adding a new entry, in case this color is already present.
    PaletteAddColor,
    /// Uses $0 for the first color, $1 for the second, and $2 as an optional number of
    /// colors to add, defaulting to 5.  Returns the number of added colors; can be
    /// different than $2 if a color in the gradient was already in the palette.
    PaletteAddGradient,
    /// Sorts the palette roughly by hue and lightness.
    PaletteSort,
    /// Adds colors from the current view into the palette.
    // TODO: we could add an integer argument for the number of colors we'll allow in;
    //      and try to find the most common colors, etc.
    PaletteAddViewColors,
    /// Writes palette colors to a file specified in $0.
    PaletteWriteToFile,
    /// Removes colors specified by arguments if non-null (specified by index or color);
    /// if no arguments are given (or all are null), clears the entire palette.
    /// Returns the number of palette entries deleted.
    PaletteClear,
    // TODO: PaletteRepaint: $0 is palette index, $1 is new color to change *everywhere* in image.
    /// Uses $0 for x, $1 for y, and $2 as an optional color (defaults to foreground color).
    /// E.g., `paint 5 10` to paint the pixel at (5, 10) with the foreground color,
    /// and `paint 11 12 #123456` to paint the pixel at (11, 12) with #123456.
    /// Note that an integer for the color also works as an index to the palette.
    Paint,
    /// Uses $0 for x, $1 for y, and $2 as an optional color (defaults to foreground color).
    /// E.g., `bucket 5 10` to flood fill the pixels starting at (5, 10) with the foreground
    /// color, and `bucket 11 12 #123456` to flood-fill the pixels starting at (11, 12) with
    /// #123456.  Note that an integer for the color also works as an index to the palette.
    Bucket,
    /// Uses ($0, $1) for (x0, y0) and $2, $3 for (x1, y1), and an optional $4 for the color
    /// of the line to draw (defaults to foreground color).  If any of $0-$3 are null, we'll
    /// use the corresponding mouse position (X or Y).
    Line,
    // TODO: Sample, $0 for x and $1 for y, to get the color on the grid.
    /// Returns the current mouse x-position, in pixel coordinates relative to the drawing.
    MouseX,
    /// Returns the current mouse y-position, in pixel coordinates relative to the drawing.
    MouseY,

    // TODO: maybe have a `PreviouslyUsedTool` command to use `session.prev_tool()`
    /// Saves the animation to a file (e.g., for gif, png, etc.).
    Write,
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
            Command::PushPop => write!(f, "push-pop"),
            Command::RunAll => write!(f, "run-all"),
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
            Command::Bind(Bind::Modes) => write!(f, "bind"),
            Command::Bind(Bind::Normal) => write!(f, "bind-normal"),
            Command::Bind(Bind::Visual) => write!(f, "bind-visual"),
            Command::Bind(Bind::Help) => write!(f, "bind-help"),
            Command::ColorSetting(ColorSetting::UiBackground) => write!(f, "ui-bg"),
            Command::ColorSetting(ColorSetting::UiGrid) => write!(f, "grid-color"),
            Command::ColorSetting(ColorSetting::Foreground) => write!(f, "fg"),
            Command::ColorSetting(ColorSetting::Background) => write!(f, "bg"),
            Command::StringSetting(StringSetting::Mode) => write!(f, "mode"),
            Command::StringSetting(StringSetting::Cwd) => write!(f, "cwd"), // Directory
            Command::StringSetting(StringSetting::ConfigDirectory) => write!(f, "config-dir"),
            Command::I64Setting(I64Setting::Debug) => write!(f, "debug"),
            Command::I64Setting(I64Setting::UiAnimate) => write!(f, "ui-a"),
            Command::I64Setting(I64Setting::UiChecker) => write!(f, "checker"),
            Command::I64Setting(I64Setting::UiGrid) => write!(f, "grid"),
            Command::I64Setting(I64Setting::UiScalePercentage) => write!(f, "ui-scale%"),
            Command::I64Setting(I64Setting::UiOffsetX) => write!(f, "ui-x"),
            Command::I64Setting(I64Setting::UiOffsetY) => write!(f, "ui-y"),
            Command::I64Setting(I64Setting::Tool) => write!(f, "t"),
            Command::I64Setting(I64Setting::PaletteHeight) => write!(f, "p-height"),
            Command::I64Setting(I64Setting::UiZoom) => write!(f, "zoom"),
            Command::I64Setting(I64Setting::ViewIndex) => write!(f, "v"),
            Command::I64Setting(I64Setting::CursorXRay) => write!(f, "c-xray"),
            Command::I64Setting(I64Setting::BrushSize) => write!(f, "b-size"),
            Command::I64Setting(I64Setting::BrushErase) => write!(f, "b-erase"),
            Command::I64Setting(I64Setting::BrushMultiFrame) => write!(f, "b-multi"),
            Command::I64Setting(I64Setting::BrushPixelPerfect) => write!(f, "b-perfect"),
            Command::I64Setting(I64Setting::BrushXSymmetry) => write!(f, "b-xsym"),
            Command::I64Setting(I64Setting::BrushYSymmetry) => write!(f, "b-ysym"),
            Command::I64Setting(I64Setting::BrushLineAngle) => write!(f, "b-line"),
            Command::I64Setting(I64Setting::AnimationDelay) => write!(f, "delay"),
            Command::I64Setting(I64Setting::FrameIndex) => write!(f, "f"),
            Command::I64Setting(I64Setting::FrameWidth) => write!(f, "f-width"),
            Command::I64Setting(I64Setting::FrameHeight) => write!(f, "f-height"),
            Command::I64Setting(I64Setting::ImageSplit) => write!(f, "split"),
            Command::I64Setting(I64Setting::History) => write!(f, "history"),
            Command::WithoutArguments(ZeroArgumentsFor::Reset) => write!(f, "reset"),
            Command::WithoutArguments(ZeroArgumentsFor::SelectionExpand) => write!(f, "s-expand"),
            Command::WithoutArguments(ZeroArgumentsFor::SelectionErase) => write!(f, "erase"),
            Command::WithoutArguments(ZeroArgumentsFor::SelectionCopy) => write!(f, "copy"),
            Command::WithoutArguments(ZeroArgumentsFor::SelectionCut) => write!(f, "cut"),
            Command::WithoutArguments(ZeroArgumentsFor::SelectionPaste) => write!(f, "paste"),
            Command::WithoutArguments(ZeroArgumentsFor::SelectionMirrorX) => write!(f, "mirrorx"),
            Command::WithoutArguments(ZeroArgumentsFor::SelectionMirrorY) => write!(f, "mirrory"),
            Command::UsingOptionalI64(OptionalI64For::FrameAdd) => write!(f, "fa"),
            Command::UsingOptionalI64(OptionalI64For::FrameClone) => write!(f, "fc"),
            Command::UsingOptionalI64(OptionalI64For::FrameRemove) => write!(f, "fr"),
            Command::UsingOptionalI64(OptionalI64For::Undo) => write!(f, "undo"),
            Command::UsingOptionalI64(OptionalI64For::Redo) => write!(f, "redo"),
            Command::UsingOptionalColor(OptionalColorFor::SelectionClear) => write!(f, "clear"),
            Command::UsingStrings(StringsFor::Source) => write!(f, "source"),
            Command::UsingStrings(StringsFor::Edit) => write!(f, "e"),
            Command::UsingStrings(StringsFor::Concatenate) => write!(f, "cat"),
            Command::UsingTwoI64s(TwoI64sFor::Pan) => write!(f, "pan"),
            Command::UsingTwoI64s(TwoI64sFor::FrameResize) => write!(f, "f-resize"),
            Command::UsingTwoI64s(TwoI64sFor::SelectionDelta) => write!(f, "s-delta"),
            Command::UsingTwoI64s(TwoI64sFor::SelectionDeltaSymmetric) => write!(f, "s-delta2"),
            Command::UsingTwoI64s(TwoI64sFor::SelectionMove) => write!(f, "s-move"),
            Command::PaletteColor => write!(f, "pc"),
            Command::PaletteAddColor => write!(f, "p-add"),
            Command::PaletteAddGradient => write!(f, "p-gradient"),
            Command::PaletteSort => write!(f, "p-sort"),
            Command::PaletteAddViewColors => write!(f, "p-view"),
            Command::PaletteWriteToFile => write!(f, "p-w"),
            Command::PaletteClear => write!(f, "p-clear"),
            Command::Paint => write!(f, "p"),
            Command::Bucket => write!(f, "b"),
            Command::Line => write!(f, "l"),
            Command::MouseX => write!(f, "mx"),
            Command::MouseY => write!(f, "my"),
            Command::Write => write!(f, "w"),
            Command::Quit(Quit::Safe) => write!(f, "q"),
            Command::Quit(Quit::AllSafe) => write!(f, "qa"),
            Command::Quit(Quit::Forced) => write!(f, "q!"),
            Command::Quit(Quit::AllForced) => write!(f, "qa!"),
            Command::Quit(Quit::AfterWrite) => write!(f, "wq"),
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
            "push-pop" => Ok(Command::PushPop),
            "run-all" => Ok(Command::RunAll),
            "not" => Ok(Command::Not),
            "if" => Ok(Command::If),
            "even" => Ok(Command::Even),
            "odd" => Ok(Command::Odd),
            "+" => Ok(Command::Sum),
            "*" => Ok(Command::Product),
            "set" => Ok(Command::SetVariable),
            "const" => Ok(Command::ConstVariable),
            "alias" => Ok(Command::CreateAlias),
            "bind" => Ok(Command::Bind(Bind::Modes)),
            "bind-normal" => Ok(Command::Bind(Bind::Normal)),
            "bind-visual" => Ok(Command::Bind(Bind::Visual)),
            "bind-help" => Ok(Command::Bind(Bind::Help)),
            "ui-bg" => Ok(Command::ColorSetting(ColorSetting::UiBackground)),
            "grid-color" => Ok(Command::ColorSetting(ColorSetting::UiGrid)),
            "fg" => Ok(Command::ColorSetting(ColorSetting::Foreground)),
            "bg" => Ok(Command::ColorSetting(ColorSetting::Background)),
            "mode" => Ok(Command::StringSetting(StringSetting::Mode)),
            "cwd" => Ok(Command::StringSetting(StringSetting::Cwd)),
            "config-dir" => Ok(Command::StringSetting(StringSetting::ConfigDirectory)),
            "debug" => Ok(Command::I64Setting(I64Setting::Debug)),
            "ui-a" => Ok(Command::I64Setting(I64Setting::UiAnimate)),
            "checker" => Ok(Command::I64Setting(I64Setting::UiChecker)),
            "grid" => Ok(Command::I64Setting(I64Setting::UiGrid)),
            "ui-scale%" => Ok(Command::I64Setting(I64Setting::UiScalePercentage)),
            "ui-x" => Ok(Command::I64Setting(I64Setting::UiOffsetX)),
            "ui-y" => Ok(Command::I64Setting(I64Setting::UiOffsetY)),
            "t" => Ok(Command::I64Setting(I64Setting::Tool)),
            "p-height" => Ok(Command::I64Setting(I64Setting::PaletteHeight)),
            "zoom" => Ok(Command::I64Setting(I64Setting::UiZoom)),
            "v" => Ok(Command::I64Setting(I64Setting::ViewIndex)),
            "c-xray" => Ok(Command::I64Setting(I64Setting::CursorXRay)),
            "b-size" => Ok(Command::I64Setting(I64Setting::BrushSize)),
            "b-erase" => Ok(Command::I64Setting(I64Setting::BrushErase)),
            "b-multi" => Ok(Command::I64Setting(I64Setting::BrushMultiFrame)),
            "b-perfect" => Ok(Command::I64Setting(I64Setting::BrushPixelPerfect)),
            "b-xsym" => Ok(Command::I64Setting(I64Setting::BrushXSymmetry)),
            "b-ysym" => Ok(Command::I64Setting(I64Setting::BrushYSymmetry)),
            "b-line" => Ok(Command::I64Setting(I64Setting::BrushLineAngle)),
            "delay" => Ok(Command::I64Setting(I64Setting::AnimationDelay)),
            "f" => Ok(Command::I64Setting(I64Setting::FrameIndex)),
            "f-width" => Ok(Command::I64Setting(I64Setting::FrameWidth)),
            "f-height" => Ok(Command::I64Setting(I64Setting::FrameHeight)),
            "split" => Ok(Command::I64Setting(I64Setting::ImageSplit)),
            "history" => Ok(Command::I64Setting(I64Setting::History)),
            "reset" => Ok(Command::WithoutArguments(ZeroArgumentsFor::Reset)),
            "s-expand" => Ok(Command::WithoutArguments(ZeroArgumentsFor::SelectionExpand)),
            "erase" => Ok(Command::WithoutArguments(ZeroArgumentsFor::SelectionErase)),
            "copy" => Ok(Command::WithoutArguments(ZeroArgumentsFor::SelectionCopy)),
            "cut" => Ok(Command::WithoutArguments(ZeroArgumentsFor::SelectionCut)),
            "paste" => Ok(Command::WithoutArguments(ZeroArgumentsFor::SelectionPaste)),
            "mirrorx" => Ok(Command::WithoutArguments(
                ZeroArgumentsFor::SelectionMirrorX,
            )),
            "mirrory" => Ok(Command::WithoutArguments(
                ZeroArgumentsFor::SelectionMirrorY,
            )),
            "fa" => Ok(Command::UsingOptionalI64(OptionalI64For::FrameAdd)),
            "fc" => Ok(Command::UsingOptionalI64(OptionalI64For::FrameClone)),
            "fr" => Ok(Command::UsingOptionalI64(OptionalI64For::FrameRemove)),
            "undo" => Ok(Command::UsingOptionalI64(OptionalI64For::Undo)),
            "redo" => Ok(Command::UsingOptionalI64(OptionalI64For::Redo)),
            "clear" => Ok(Command::UsingOptionalColor(
                OptionalColorFor::SelectionClear,
            )),
            "source" => Ok(Command::UsingStrings(StringsFor::Source)),
            "e" => Ok(Command::UsingStrings(StringsFor::Edit)),
            "cat" => Ok(Command::UsingStrings(StringsFor::Concatenate)),
            "pan" => Ok(Command::UsingTwoI64s(TwoI64sFor::Pan)),
            "f-resize" => Ok(Command::UsingTwoI64s(TwoI64sFor::FrameResize)),
            "s-delta" => Ok(Command::UsingTwoI64s(TwoI64sFor::SelectionDelta)),
            "s-delta2" => Ok(Command::UsingTwoI64s(TwoI64sFor::SelectionDeltaSymmetric)),
            "s-move" => Ok(Command::UsingTwoI64s(TwoI64sFor::SelectionMove)),
            "pc" => Ok(Command::PaletteColor),
            "p-add" => Ok(Command::PaletteAddColor),
            "p-gradient" => Ok(Command::PaletteAddGradient),
            "p-sort" => Ok(Command::PaletteSort),
            "p-view" => Ok(Command::PaletteAddViewColors),
            "p-w" => Ok(Command::PaletteWriteToFile),
            "p-clear" => Ok(Command::PaletteClear),
            "p" => Ok(Command::Paint),
            "b" => Ok(Command::Bucket),
            "l" => Ok(Command::Line),
            "mx" => Ok(Command::MouseX),
            "my" => Ok(Command::MouseY),
            "w" => Ok(Command::Write),
            "q" => Ok(Command::Quit(Quit::Safe)),
            "qa" => Ok(Command::Quit(Quit::AllSafe)),
            "q!" => Ok(Command::Quit(Quit::Forced)),
            "qa!" => Ok(Command::Quit(Quit::AllForced)),
            "wq" => Ok(Command::Quit(Quit::AfterWrite)),
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

impl fmt::Display for EmptyCommandParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "EmptyCommandParseError")
    }
}

#[derive(Eq, Hash, PartialEq, Debug, Clone, Copy, EnumIter)]
pub enum Bind {
    /// Standard modes; i.e., everything except Help and Command.
    Modes,
    /// Just the normal mode.
    Normal,
    /// Just the visual mode, technically just Visual::Selecting.
    Visual,
    /// Just the help mode.
    Help,
    // TODO: Command, if we can do it right.
}

#[derive(Eq, Hash, PartialEq, Debug, Clone, Copy, EnumIter)]
pub enum ZeroArgumentsFor {
    // TODO: move PaletteSort here
    // TODO: move PaletteClear here
    // TODO: BrushReset,
    /// Resets settings.
    Reset,
    /// Expands the selection to the encompassing frame(s).
    SelectionExpand,
    /// Erases what's in the selection.
    // TODO: i think we can delete this and just use selection-clear.
    SelectionErase,
    /// Copies what is in the selection to the selection clipboard.
    SelectionCopy,
    /// Copies what is in the selection to the selection clipboard
    /// and erases what was there.
    SelectionCut,
    /// Pastes what is in the selection clipboard.
    SelectionPaste,
    /// Pastes what is in the selection clipboard flipped horizontally.
    SelectionMirrorX,
    /// Pastes what is in the selection clipboard flipped vertically.
    SelectionMirrorY,
    // TODO: SelectionSwap (pastes what was in the selection clipboard and moves what was in the current selection into the clipboard)
}

#[derive(Eq, Hash, PartialEq, Debug, Clone, Copy, EnumIter)]
pub enum OptionalI64For {
    /// Adds a blank frame after Some(i64), otherwise after the current frame.
    FrameAdd,
    /// Clones the frame at Some(i64), otherwise the current frame.
    FrameClone,
    /// Removes/deletes the frame at Some(i64), otherwise the current frame.
    FrameRemove,
    /// Repeats undo the specified number of times, or 1.
    Undo,
    /// Repeats redo the specified number of times, or 1.
    Redo,
    // TODO: Colors, e.g., Red, etc. with 0 to 10
}

#[derive(Eq, Hash, PartialEq, Debug, Clone, Copy, EnumIter)]
pub enum OptionalColorFor {
    /// Uses an optional color at $0 (defaulting to transparent) to
    /// erase everything in the selection and replace all pixels with.
    SelectionClear,
}

#[derive(Eq, Hash, PartialEq, Debug, Clone, Copy, EnumIter)]
pub enum StringsFor {
    /// Path names to source, i.e. for configuration.
    Source,
    /// Path names to edit, i.e. as images.
    Edit,
    /// Path names of images to concatenate horizontally into one new image.
    Concatenate,
    // TODO: `Append` for appending images in the horizontal direction to the current view
    // TODO: `Stack` for concatenating images vertically into one new image
    // TODO: `Layer` for appending images in the vertical direction to the current view
}

#[derive(Eq, Hash, PartialEq, Debug, Clone, Copy, EnumIter)]
pub enum TwoI64sFor {
    /// Distance in X and Y to pan.
    Pan,
    /// Setter for the width x height of each frame, using $0 for width and $1 for height.
    /// If either $0 or $1 is null or 0, it keeps that dimension the same; if both are null,
    /// it crops the frames to content.  Returns the number of pixels in one frame, i.e.,
    /// width * height, from *before* the operation.
    FrameResize,
    /// Expand (or shrink) the selection (i.e., for Visual mode), by deltas based on $0 and $1.
    /// Only moves the right/bottom sides of the selection box.
    SelectionDelta,
    /// Expand (or shrink) the selection in a symmetric way via $0 (width) and $1 (height).
    SelectionDeltaSymmetric,
    /// Move the selection (i.e., for Visual mode).
    SelectionMove,
    // TODO: add SelectionResize that does absolute values
}

/*
/// like TwoI64s but defaulting to mouse coordinates instead of 0 for the I64s.
#[derive(Eq, Hash, PartialEq, Debug, Clone, Copy, EnumIter)]
pub enum CoordinatesFor {
    // TODO: this should have two optional i64 coordinates but default to mouse coordinates.
    // TODO: SelectionCorner - snaps the closest selection corner to coordinates ($0, $1)
}
*/

#[derive(Eq, Hash, PartialEq, Debug, Clone, Copy, EnumIter)]
pub enum Quit {
    /// The default, Quit::Safe, via `quit` or `q`, quits this view if saved.
    Safe,
    /// Quit all views, via `qa`, if safe to do so (work is saved).
    AllSafe,
    /// Force quit this view, via `q!`.
    Forced,
    /// Force quit all views, via `qa!`.
    AllForced,
    /// Writes the view before quitting.
    AfterWrite,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Axis {
    Horizontal,
    Vertical,
}

///////////////////////////////////////////////////////////////////////////////

pub struct CommandLine {
    /// Command auto-complete.
    pub autocomplete: Autocomplete<ScriptCompleter>,
    /// Input cursor position.
    pub cursor: usize,
    /// File extensions supported.
    extensions: Vec<String>,
    /// The history of commands entered.
    pub history: History,
    /// The current input string displayed to the user.
    input: String,
    /// Parser.
    pub parser: Parser<Script>,
}

impl CommandLine {
    const MAX_INPUT: usize = 256;

    pub fn new<P: AsRef<Path>>(cwd: P, history_path: P, extensions: &[&str]) -> Self {
        Self {
            autocomplete: Autocomplete::new(ScriptCompleter::new(cwd, extensions)),
            cursor: 0,
            extensions: extensions.iter().map(|e| (*e).into()).collect(),
            history: History::new(history_path, 1024),
            input: String::with_capacity(Self::MAX_INPUT),
            parser: param::<Script>(),
        }
    }

    pub fn set_cwd(&mut self, path: &Path) {
        let exts: Vec<_> = self.extensions.iter().map(|s| s.as_str()).collect();
        self.autocomplete = Autocomplete::new(ScriptCompleter::new(path, exts.as_slice()));
    }

    pub fn parse(&self, input: &str) -> Result<Script, Error> {
        match self.parser.parse(input) {
            Ok((cmd, _)) => Ok(cmd),
            Err((err, _)) => Err(err),
        }
    }

    pub fn input(&self) -> String {
        self.input.clone()
    }

    pub fn is_empty(&self) -> bool {
        self.input.is_empty()
    }

    pub fn history_prev(&mut self) {
        // TODO: option to avoid using the prefix
        let prefix = self.prefix();

        if let Some(entry) = self.history.prev(&prefix).map(str::to_owned) {
            self.replace(&entry);
        }
    }

    pub fn history_next(&mut self) {
        // TODO: option to avoid using the prefix
        let prefix = self.prefix();

        if let Some(entry) = self.history.next(&prefix).map(str::to_owned) {
            self.replace(&entry);
        } else {
            self.reset();
        }
    }

    pub fn completion_next(&mut self) {
        let prefix = self.prefix();

        if let Some((completion, range)) = self.autocomplete.next(&prefix, self.cursor) {
            // Replace old completion with new one.
            self.cursor = range.start + completion.len();
            self.input.replace_range(range, &completion);
        }
    }

    pub fn cursor_backward(&mut self) -> Option<char> {
        if let Some(c) = self.peek_back() {
            let cursor = self.cursor - c.len_utf8();

            // Don't allow deleting the `:` prefix of the command.
            if c != ':' || cursor > 0 {
                self.cursor = cursor;
                self.autocomplete.invalidate();
                return Some(c);
            }
        }
        None
    }

    pub fn cursor_forward(&mut self) -> Option<char> {
        if let Some(c) = self.input[self.cursor..].chars().next() {
            self.cursor += c.len_utf8();
            self.autocomplete.invalidate();
            Some(c)
        } else {
            None
        }
    }

    pub fn cursor_back(&mut self) {
        if self.cursor > 1 {
            self.cursor = 1;
            self.autocomplete.invalidate();
        }
    }

    pub fn cursor_front(&mut self) {
        self.cursor = self.input.len();
    }

    pub fn putc(&mut self, c: char) {
        if self.input.len() + c.len_utf8() > self.input.capacity() {
            return;
        }
        self.input.insert(self.cursor, c);
        self.cursor += c.len_utf8();
        self.autocomplete.invalidate();
    }

    pub fn puts(&mut self, s: &str) {
        // TODO: Check capacity.
        self.input.push_str(s);
        self.cursor += s.len();
        self.autocomplete.invalidate();
    }

    pub fn delc(&mut self) {
        match self.peek_back() {
            // Don't allow deleting the ':' unless it's the last remaining character.
            Some(c) if self.cursor > 1 || self.input.len() == 1 => {
                self.cursor -= c.len_utf8();
                self.input.remove(self.cursor);
                self.autocomplete.invalidate();
            }
            _ => {}
        }
    }

    pub fn clear(&mut self) {
        self.cursor = 0;
        self.input.clear();
        self.history.reset();
        self.autocomplete.invalidate();
    }

    ////////////////////////////////////////////////////////////////////////////

    fn replace(&mut self, s: &str) {
        // We don't re-assign `input` here, because it
        // has a fixed capacity we want to preserve.
        self.input.clear();
        self.input.push_str(s);
        self.autocomplete.invalidate();
    }

    fn reset(&mut self) {
        self.clear();
        self.putc(':');
    }

    fn prefix(&self) -> String {
        self.input[..self.cursor].to_owned()
    }

    #[cfg(test)]
    fn peek(&self) -> Option<char> {
        self.input[self.cursor..].chars().next()
    }

    fn peek_back(&self) -> Option<char> {
        self.input[..self.cursor].chars().next_back()
    }
}

///////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct ScriptCompleter {
    file_completer: FileCompleter,
}

impl ScriptCompleter {
    fn new<P: AsRef<Path>>(cwd: P, exts: &[&str]) -> Self {
        Self {
            file_completer: FileCompleter::new(cwd, exts),
        }
    }
}

impl autocomplete::Completer for ScriptCompleter {
    type Options = ();

    fn complete(&self, input: &str, _opts: ()) -> Vec<String> {
        // Not sure why we need the map_err, I implemented Display for EmptyCommandParseError
        // but am still getting this compile error if I don't map_err:
        // :::the trait `From<command::EmptyCommandParseError>` is not implemented for `String`
        let p =
            token().try_map(|input| Command::from_str(&input).map_err(|_| "no input".to_string()));

        match p.parse(input) {
            Ok((command, _)) => match command {
                /* TODO: switch to Command:: versions.
                Cmd::ChangeDir(path) | Cmd::WriteFrames(path) => self.complete_path(
                    path.as_ref(),
                    input,
                    FileCompleterOpts { directories: true },
                ),
                Cmd::Source(path) | Cmd::Write(path) => {
                    self.complete_path(path.as_ref(), input, Default::default())
                }
                Cmd::Edit(paths) | Cmd::EditFrames(paths) => {
                    self.complete_path(paths.last(), input, Default::default())
                }
                */
                _ => vec![],
            },
            Err(_) => vec![],
        }
    }
}

impl ScriptCompleter {
    fn complete_path(
        &self,
        path: Option<&String>,
        input: &str,
        opts: FileCompleterOpts,
    ) -> Vec<String> {
        use crate::autocomplete::Completer;

        let empty = "".to_owned();
        let path = path.unwrap_or(&empty);

        // If there's whitespace between the path and the cursor, don't complete the path.
        // Instead, complete as if the input was empty.
        match input.chars().next_back() {
            Some(c) if c.is_whitespace() => self.file_completer.complete("", opts),
            _ => self.file_completer.complete(path, opts),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use strum::IntoEnumIterator;

    #[test]
    fn test_command_parsing() {
        assert_eq!(Command::from_str("?"), Ok(Command::Help));
        assert_eq!(Command::from_str("push-pop"), Ok(Command::PushPop));
        assert_eq!(Command::from_str("run-all"), Ok(Command::RunAll));
        assert_eq!(Command::from_str("not"), Ok(Command::Not));
        assert_eq!(Command::from_str("if"), Ok(Command::If));
        assert_eq!(Command::from_str("even"), Ok(Command::Even));
        assert_eq!(Command::from_str("odd"), Ok(Command::Odd));
        assert_eq!(Command::from_str("+"), Ok(Command::Sum));
        assert_eq!(Command::from_str("*"), Ok(Command::Product));
        assert_eq!(Command::from_str("set"), Ok(Command::SetVariable));
        assert_eq!(Command::from_str("const"), Ok(Command::ConstVariable));
        assert_eq!(Command::from_str("alias"), Ok(Command::CreateAlias));
        assert_eq!(
            Command::from_str("fg"),
            Ok(Command::ColorSetting(ColorSetting::Foreground))
        );
        assert_eq!(
            Command::from_str("bg"),
            Ok(Command::ColorSetting(ColorSetting::Background))
        );
        assert_eq!(Command::from_str("pc"), Ok(Command::PaletteColor));
        assert_eq!(Command::from_str("p-add"), Ok(Command::PaletteAddColor));
        assert_eq!(Command::from_str("p-clear"), Ok(Command::PaletteClear));
        assert_eq!(Command::from_str("p"), Ok(Command::Paint));
        assert_eq!(Command::from_str("w"), Ok(Command::Write));
        assert_eq!(Command::from_str("q"), Ok(Command::Quit(Quit::Safe)));
        assert_eq!(Command::from_str("qa"), Ok(Command::Quit(Quit::AllSafe)));
        assert_eq!(Command::from_str("q!"), Ok(Command::Quit(Quit::Forced)));
        assert_eq!(Command::from_str("qa!"), Ok(Command::Quit(Quit::AllForced)));
        assert_eq!(Command::from_str("wq"), Ok(Command::Quit(Quit::AfterWrite)));
        assert_eq!(
            Command::from_str("gnarly345"),
            Ok(Command::Evaluate("gnarly345".to_string()))
        );
        assert_eq!(Command::from_str(""), Err(EmptyCommandParseError));
    }

    #[test]
    fn test_commands_can_be_printed_and_unprinted() {
        for bind in Bind::iter() {
            let command = Command::Bind(bind);
            let name = format!("{}", command);
            let command_from_name = Command::from_str(&name).unwrap();
            assert_eq!(command_from_name, command);
        }

        for setting in ColorSetting::iter() {
            let command = Command::ColorSetting(setting);
            let name = format!("{}", command);
            let command_from_name = Command::from_str(&name).unwrap();
            assert_eq!(command_from_name, command);
        }

        for setting in StringSetting::iter() {
            let command = Command::StringSetting(setting);
            let name = format!("{}", command);
            let command_from_name = Command::from_str(&name).unwrap();
            assert_eq!(command_from_name, command);
        }

        for setting in I64Setting::iter() {
            let command = Command::I64Setting(setting);
            let name = format!("{}", command);
            let command_from_name = Command::from_str(&name).unwrap();
            assert_eq!(command_from_name, command);
        }

        for what_for in ZeroArgumentsFor::iter() {
            let command = Command::WithoutArguments(what_for);
            let name = format!("{}", command);
            let command_from_name = Command::from_str(&name).unwrap();
            assert_eq!(command_from_name, command);
        }

        for what_for in OptionalI64For::iter() {
            let command = Command::UsingOptionalI64(what_for);
            let name = format!("{}", command);
            let command_from_name = Command::from_str(&name).unwrap();
            assert_eq!(command_from_name, command);
        }

        for what_for in OptionalColorFor::iter() {
            let command = Command::UsingOptionalColor(what_for);
            let name = format!("{}", command);
            let command_from_name = Command::from_str(&name).unwrap();
            assert_eq!(command_from_name, command);
        }

        for what_for in StringsFor::iter() {
            let command = Command::UsingStrings(what_for);
            let name = format!("{}", command);
            let command_from_name = Command::from_str(&name).unwrap();
            assert_eq!(command_from_name, command);
        }

        for what_for in TwoI64sFor::iter() {
            let command = Command::UsingTwoI64s(what_for);
            let name = format!("{}", command);
            let command_from_name = Command::from_str(&name).unwrap();
            assert_eq!(command_from_name, command);
        }
    }

    /* TODO: add back with completions
        #[test]
        fn test_command_line() {
            let tmp = tempfile::tempdir().unwrap();

            fs::create_dir(tmp.path().join("assets")).unwrap();
            for file_name in &["one.png", "two.png", "three.png"] {
                let path = tmp.path().join(file_name);
                File::create(path).unwrap();
            }
            for file_name in &["four.png", "five.png"] {
                let path = tmp.path().join("assets").join(file_name);
                File::create(path).unwrap();
            }

            let mut cli = CommandLine::new(tmp.path(), &tmp.path().join(".history"), &["png"]);

            cli.puts(":e one");
            cli.completion_next();
            assert_eq!(cli.input(), ":e one.png");

            cli.completion_next();
            assert_eq!(cli.input(), ":e one.png");

            cli.clear();
            cli.puts(":e ");
            cli.completion_next();
            assert_eq!(cli.input(), ":e assets");

            cli.completion_next();
            assert_eq!(cli.input(), ":e one.png");

            cli.completion_next();
            assert_eq!(cli.input(), ":e three.png");

            cli.completion_next();
            assert_eq!(cli.input(), ":e two.png");

            cli.completion_next();
            assert_eq!(cli.input(), ":e assets");

            cli.putc('/');
            cli.completion_next();
            assert_eq!(cli.input(), ":e assets/five.png");

            cli.completion_next();
            assert_eq!(cli.input(), ":e assets/four.png");

            cli.completion_next();
            assert_eq!(cli.input(), ":e assets/five.png");

            cli.putc(' ');
            cli.completion_next();
            assert_eq!(cli.input(), ":e assets/five.png assets");
            cli.completion_next();
            assert_eq!(cli.input(), ":e assets/five.png one.png");

            cli.putc(' ');
            cli.putc('t');
            cli.completion_next();
            assert_eq!(cli.input(), ":e assets/five.png one.png three.png");

            cli.completion_next();
            assert_eq!(cli.input(), ":e assets/five.png one.png two.png");

            cli.completion_next();
            assert_eq!(cli.input(), ":e assets/five.png one.png three.png");

            for _ in 0..10 {
                cli.cursor_backward();
            }
            cli.putc(' ');
            cli.putc('o');
            cli.completion_next();
            assert_eq!(cli.input(), ":e assets/five.png one.png one.png three.png");

            cli.clear();
            cli.puts(":e assets");
            cli.completion_next();
            assert_eq!(cli.input(), ":e assets/");

            cli.clear();
            cli.puts(":e asset");

            cli.completion_next();
            assert_eq!(cli.input(), ":e assets/");

            cli.completion_next();
            assert_eq!(cli.input(), ":e assets/five.png");
        }

        #[test]
        fn test_command_line_change_dir() {
            let tmp = tempfile::tempdir().unwrap();

            fs::create_dir(tmp.path().join("assets")).unwrap();
            for file_name in &["four.png", "five.png"] {
                let path = tmp.path().join("assets").join(file_name);
                File::create(path).unwrap();
            }

            let mut cli = CommandLine::new(tmp.path(), Path::new("/dev/null"), &["png"]);

            cli.set_cwd(tmp.path().join("assets/").as_path());
            cli.puts(":e ");

            cli.completion_next();
            assert_eq!(cli.input(), ":e five.png");

            cli.completion_next();
            assert_eq!(cli.input(), ":e four.png");
        }

        #[test]
        fn test_command_line_cd() {
            let tmp = tempfile::tempdir().unwrap();

            fs::create_dir(tmp.path().join("assets")).unwrap();
            fs::create_dir(tmp.path().join("assets").join("1")).unwrap();
            fs::create_dir(tmp.path().join("assets").join("2")).unwrap();
            File::create(tmp.path().join("assets").join("pim.png")).unwrap();

            let mut cli = CommandLine::new(tmp.path(), Path::new("/dev/null"), &["png"]);

            cli.clear();
            cli.puts(":cd assets/");

            cli.completion_next();
            assert_eq!(cli.input(), ":cd assets/1");

            cli.completion_next();
            assert_eq!(cli.input(), ":cd assets/2");

            cli.completion_next();
            assert_eq!(cli.input(), ":cd assets/1");
        }

        #[test]
        fn test_command_line_cursor() {
            let mut cli = CommandLine::new("/dev/null", "/dev/null", &[]);

            cli.puts(":echo");
            cli.delc();
            assert_eq!(cli.input(), ":ech");
            cli.delc();
            assert_eq!(cli.input(), ":ec");
            cli.delc();
            assert_eq!(cli.input(), ":e");
            cli.delc();
            assert_eq!(cli.input(), ":");
            cli.delc();
            assert_eq!(cli.input(), "");

            cli.clear();
            cli.puts(":e");

            assert_eq!(cli.peek(), None);
            cli.cursor_backward();

            assert_eq!(cli.peek(), Some('e'));
            cli.cursor_backward();

            assert_eq!(cli.peek(), Some('e'));
            assert_eq!(cli.peek_back(), Some(':'));

            cli.delc();
            assert_eq!(cli.input(), ":e");

            cli.clear();
            cli.puts(":echo");

            assert_eq!(cli.peek(), None);
            cli.cursor_back();

            assert_eq!(cli.peek(), Some('e'));
            assert_eq!(cli.peek_back(), Some(':'));

            cli.cursor_front();
            assert_eq!(cli.peek(), None);
            assert_eq!(cli.peek_back(), Some('o'));
        }
    */
}
