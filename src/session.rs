#![allow(clippy::needless_collect)]
//! Session
use crate::autocomplete::FileCompleter;
use crate::brush::{self, Brush, BrushState};
use crate::cmd::{self, Cmd, CommandLine, KeyMapping, Op, Value};
use crate::color;
use crate::data;
use crate::event::{Event, TimedEvent};
use crate::execution::{DigestMode, DigestState, Execution};
use crate::flood::FloodFiller;
use crate::hashmap;
use crate::message::*;
use crate::palette::*;
use crate::platform::{self, InputState, Key, KeyboardInput, LogicalSize, ModifiersState};
use crate::script::{
    self, evaluate, Argument, ArgumentResult, Command, Evaluate, Get, Map, OptionalI64For, Quit,
    Script, ScriptRunner, Serialize, StringsFor, TwoI64sFor, Variables, VoidResult,
    ZeroArgumentsFor,
};
use crate::script_runner;
use crate::settings::*;
use crate::util;

use crate::gfx::math::*;
use crate::gfx::rect::Rect;
use crate::gfx::shape2d::{Fill, Rotation, Shape, Stroke};
use crate::gfx::{Lyza, Point, Rgb8, Rgba8, ZDepth};
use crate::view::path;
use crate::view::resource::{EditId, ViewResource};
use crate::view::{
    self, FileStatus, FileStorage, View, ViewCoords, ViewExtent, ViewId, ViewManager, ViewOp,
    ViewState,
};

use arrayvec::ArrayVec;
use claim::assert_ok;

use directories as dirs;
use nonempty::NonEmpty;

use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::fmt;
use std::fs::File;
use std::io;

use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::time;

/// Settings help string.
// TODO: remove after migration to script
pub const SETTINGS: &str = r#"
SETTINGS

debug             on/off             Debug mode
checker           on/off             Alpha checker toggle
scale             1,2,3,4            UI scale
animation         on/off             View animation toggle
animation/delay   1..1000            View animation delay (ms)
background        #000000..#ffffff   Set background appearance to <color>
grid              <d>                If nonzero, the Grid spacing, otherwise hides grid.
grid/color        #000000..#ffffff   Grid color
"#;

#[derive(Copy, Clone, Debug)]
enum InternalCommand {
    StopRecording,
}

/// Session coordinates.
/// Encompasses anything within the window, such as the cursor position.
pub type SessionCoords = Point<Session, f32>;

///////////////////////////////////////////////////////////////////////////////

/// An editing mode the `Session` can be in.
/// Some of these modes are inspired by vi.
#[derive(Eq, PartialEq, Copy, Clone, Debug, Default)]
pub enum Mode {
    /// Allows the user to paint pixels.
    #[default]
    Normal,
    // TODO: rename to "Selection" mode
    /// Allows pixels to be selected, copied and manipulated visually.
    Visual(VisualState),
    /// Allows commands to be run.
    Command,
    /// Activated with the `:help` command.
    Help,
}

impl fmt::Display for Mode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Normal => "normal".fmt(f),
            Self::Visual(VisualState::Selecting { dragging: true }) => "visual (dragging)".fmt(f),
            Self::Visual(VisualState::Selecting { .. }) => "visual".fmt(f),
            Self::Visual(VisualState::Pasting) => "visual (pasting)".fmt(f),
            Self::Command => "command".fmt(f),
            Self::Help => "help".fmt(f),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ModeParseError;

impl FromStr for Mode {
    type Err = ModeParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.trim() {
            "normal" => Ok(Mode::Normal),
            "visual" => Ok(Mode::Visual(VisualState::default())),
            "command" => Ok(Mode::Command),
            "help" => Ok(Mode::Help),
            _ => Err(ModeParseError),
        }
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum VisualState {
    Selecting { dragging: bool },
    Pasting,
}

impl VisualState {
    pub fn selecting() -> Self {
        Self::Selecting { dragging: false }
    }
}

impl Default for VisualState {
    fn default() -> Self {
        Self::selecting()
    }
}

/// A pixel selection within a view.
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub struct Selection(Rect<i32>);

impl Selection {
    /// Create a new selection from a rectangle.
    pub fn new(x1: i32, y1: i32, x2: i32, y2: i32) -> Self {
        Self(Rect::new(x1, y1, x2 - 1, y2 - 1))
    }

    /// Create a new selection from a rectangle.
    pub fn from(r: Rect<i32>) -> Self {
        Self::new(r.x1, r.y1, r.x2, r.y2)
    }

    /// Return the selection bounds as a non-empty rectangle. This function
    /// will never return an empty rectangle.
    pub fn bounds(&self) -> Rect<i32> {
        Rect::new(self.x1, self.y1, self.x2 + 1, self.y2 + 1)
    }

    /// Return the absolute selection.
    pub fn abs(&self) -> Selection {
        Self(self.0.abs())
    }

    /// Translate the selection rectangle.
    pub fn translate(&mut self, x: i32, y: i32) {
        self.0 += Vector2::new(x, y)
    }

    /// Resize the selection by a certain amount.
    pub fn resize(&mut self, x: i32, y: i32) {
        self.0.x2 += x;
        self.0.y2 -= y;
    }
}

impl Deref for Selection {
    type Target = Rect<i32>;

    fn deref(&self) -> &Rect<i32> {
        &self.0
    }
}

/// Session effects. Eg. view creation/destruction.
/// Anything the renderer might want to know.
#[derive(Clone, Debug)]
pub enum Effect {
    /// When the session has been resized.
    SessionResized(LogicalSize),
    /// When the session UI scale has changed.
    SessionScaled(f64),
    /// When a view has been activated.
    ViewActivated(ViewId),
    /// When a view has been added.
    ViewAdded(ViewId),
    /// When a view has been removed.
    ViewRemoved(ViewId),
    /// When a view has been touched (edited).
    ViewTouched(ViewId),
    /// When a view operation has taken place.
    ViewOps(ViewId, Vec<ViewOp>),
    /// When a view requires re-drawing.
    ViewDamaged(ViewId, Option<ViewExtent>),
    /// When the active view is non-permanently painted on.
    ViewPaintDraft(Vec<Shape>),
    /// When the active view is painted on.
    ViewPaintFinal(Vec<Shape>),
    /// The blend mode used for painting has changed.
    ViewBlendingChanged(Blending),
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Blending {
    Constant,
    Alpha,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ExitReason {
    Normal,
    Error(String),
}

impl Default for ExitReason {
    fn default() -> Self {
        Self::Normal
    }
}

/// Session state.
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum State {
    /// The session is initializing.
    Initializing,
    /// The session is running normally.
    Running,
    /// The session is paused. Inputs are not processed.
    Paused,
    /// The session is being shut down.
    Closing(ExitReason),
}

/// An editing tool.
#[derive(PartialEq, Eq, Debug, Copy, Clone, Default)]
pub enum Tool {
    /// The standard drawing tool.
    /// Note that Erase is an option inside Brush.
    #[default]
    Brush,
    /// Used for filling enclosed regions with color.
    FloodFill,
    /// Used to sample colors.
    Sampler,
    /// Used to pan the workspace.
    Pan,
}

impl Tool {
    const COUNT: i64 = 4;

    pub fn from_i64(mut value: i64) -> Tool {
        value = value % Tool::COUNT;
        if value < 0 {
            // Allow for wrap around; -1 should correspond to the last tool
            value += Tool::COUNT;
        }
        match value {
            0 => Tool::Brush,
            1 => Tool::FloodFill,
            2 => Tool::Sampler,
            3 => Tool::Pan,
            _ => panic!("value {} should be in 0..{}", value, Tool::COUNT),
        }
    }
}

///////////////////////////////////////////////////////////////////////////////

/// A generic direction that can be used for things that go backward
/// and forward.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Direction {
    Backward,
    Forward,
}

impl From<Direction> for i32 {
    fn from(dir: Direction) -> i32 {
        match dir {
            Direction::Backward => -1,
            Direction::Forward => 1,
        }
    }
}

///////////////////////////////////////////////////////////////////////////////

/// A session error.
type Error = String;

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Input {
    Key(Key),
    Character(char),
}

impl fmt::Display for Input {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Key(k) => write!(f, "{}", k),
            Self::Character(c) => write!(f, "{}", c),
        }
    }
}

/// A key binding.
#[derive(PartialEq, Clone, Debug)]
pub struct KeyBinding {
    /// The `Mode`s this binding applies to.
    pub modes: Vec<Mode>,
    /// Modifiers which must be held.
    pub modifiers: ModifiersState,
    /// Input expected to trigger the binding.
    pub input: Input,
    /// Whether the key should be pressed or released.
    pub state: InputState,
    /// The `Command` to run when this binding is triggered.
    pub command: Cmd,
    /// Whether this key binding controls a toggle.
    pub is_toggle: bool,
    /// How this key binding should be displayed to the user.
    /// If `None`, then this binding shouldn't be shown to the user.
    pub display: Option<String>,
}

impl KeyBinding {
    fn is_match(
        &self,
        input: Input,
        state: InputState,
        modifiers: ModifiersState,
        mode: Mode,
    ) -> bool {
        match (input, self.input) {
            (Input::Key(key), Input::Key(k)) => {
                key == k
                    && self.state == state
                    && self.modes.contains(&mode)
                    && (self.modifiers == modifiers
                        || state == InputState::Released
                        || key.is_modifier())
            }
            (Input::Character(a), Input::Character(b)) => {
                // Nb. We only check the <ctrl> modifier with characters,
                // because the others (especially <shift>) will most likely
                // input a different character.
                // TODO: we probably should check meta.
                a == b
                    && self.modes.contains(&mode)
                    && self.state == state
                    && self.modifiers.ctrl == modifiers.ctrl
            }
            _ => false,
        }
    }
}

/// Manages a list of key bindings.
#[derive(Debug, Default)]
pub struct KeyBindings {
    elems: Vec<KeyBinding>,
}

impl KeyBindings {
    /// Add a key binding.
    pub fn add(&mut self, binding: KeyBinding) {
        for mode in binding.modes.iter() {
            self.elems
                .retain(|kb| !kb.is_match(binding.input, binding.state, binding.modifiers, *mode));
        }
        self.elems.push(binding);
    }

    pub fn len(&self) -> usize {
        self.elems.len()
    }

    pub fn is_empty(&self) -> bool {
        self.elems.is_empty()
    }

    /// Find a key binding based on some input state.
    pub fn find(
        &self,
        input: Input,
        modifiers: ModifiersState,
        state: InputState,
        mode: Mode,
    ) -> Option<KeyBinding> {
        self.elems
            .iter()
            .rev()
            .cloned()
            .find(|kb| kb.is_match(input, state, modifiers, mode))
    }

    /// Iterate over all key bindings.
    pub fn iter(&self) -> std::slice::Iter<'_, KeyBinding> {
        self.elems.iter()
    }
}

///////////////////////////////////////////////////////////////////////////////

/// A dictionary used to store session settings.
#[derive(Debug)]
pub struct Settings {
    map: HashMap<String, Value>,
}

impl Settings {
    const DEPRECATED: &'static [&'static str] = &["vsync"];

    /// Lookup a setting.
    pub fn get(&self, setting: &str) -> Option<&Value> {
        self.map.get(setting)
    }

    /// Set an existing setting to a new value. Returns `Err` if there is a type
    /// mismatch or the setting isn't found. Otherwise, returns `Ok` with the
    /// old value.
    pub fn set(&mut self, k: &str, v: Value) -> Result<Value, Error> {
        if let Some(current) = self.get(k) {
            if std::mem::discriminant(&v) == std::mem::discriminant(current) {
                return Ok(self.map.insert(k.to_string(), v).unwrap());
            }
            Err(format!(
                "invalid value `{}` for `{}`, expected {}",
                v,
                k,
                current.description()
            ))
        } else {
            Err(format!("no such setting `{}`", k))
        }
    }
}

impl Default for Settings {
    /// The default settings.
    fn default() -> Self {
        Self {
            map: hashmap! {
                "debug" => Value::Bool(false),
                "checker" => Value::U32(0),
                "background" => Value::Rgba8(color::TRANSPARENT),
                "scale%" => Value::U32(100),
                "animation" => Value::Bool(true),
                "animation/delay" => Value::U32(160),

                "grid" => Value::U32(0),
                "grid/color" => Value::Rgba8(color::BLUE),

                "p/height" => Value::U32(Session::PALETTE_HEIGHT)
            },
        }
    }
}

impl std::ops::Index<&str> for Settings {
    type Output = Value;

    fn index(&self, setting: &str) -> &Self::Output {
        self.get(setting)
            .expect(&format!("setting {} should exist", setting))
    }
}

///////////////////////////////////////////////////////////////////////////////

/// The user session.
///
/// Stores all relevant session state.
pub struct Session {
    /// The current session `Mode`.
    pub mode: Mode,
    /// The previous `Mode`.
    pub prev_mode: Option<Mode>,
    /// The current session `State`.
    pub state: State,

    /// The width of the session workspace.
    pub width: f32,
    /// The height of the session workspace.
    pub height: f32,
    /// The current working directory.
    pub cwd: PathBuf,

    /// The cursor coordinates.
    pub cursor: SessionCoords,

    /// The color under the cursor, if any.
    pub hover_color: Option<Rgba8>,
    /// The view under the cursor, if any.
    pub hover_view: Option<ViewId>,

    /// The workspace offset. Views are offset by this vector.
    pub offset: Vector2<f32>,
    /// The help view offset.
    pub help_offset: Vector2<f32>,
    /// The current message displayed to the user.
    pub message: Message,

    /// The session foreground color.
    pub fg: Rgba8,
    /// The session background color.
    pub bg: Rgba8,

    /// The current frame number.
    frame_number: u64,

    /// Directories in which application-specific user configuration is stored.
    proj_dirs: dirs::ProjectDirs,
    /// User directories.
    base_dirs: dirs::BaseDirs,

    /// Whether we should ignore characters received.
    ignore_received_characters: bool,
    /// The set of keys currently pressed.
    keys_pressed: HashSet<platform::Key>,
    /// The list of all active key bindings.
    pub key_bindings: KeyBindings,

    /// Current pixel selection.
    pub selection: Option<Selection>,

    /// The session's current settings.
    pub settings: Settings,
    /// Settings recently changed.
    // TODO: why do we need this?  i think we can remove.
    pub settings_changed: HashSet<String>,

    /// Views loaded in the session.
    pub views: ViewManager<ViewResource>,
    /// Effects produced by the session. Cleared at the beginning of every
    /// update.
    pub effects: Vec<Effect>,
    /// Animation frame time accumulator.
    pub accumulator: time::Duration,

    /// The current state of the command line.
    pub cmdline: CommandLine,
    /// The color palette.
    pub palette: Palette,

    /// Average time it takes for a session update.
    pub avg_time: time::Duration,

    /// The current tool. Only used in `Normal` mode.
    pub tool: Tool,
    /// The previous tool, if any.
    pub prev_tool: Option<Tool>,
    /// The brush tool settings.
    pub brush: Brush,

    /// Input state of the left mouse button.
    lmb_state: InputState,
    /// Input state of the right mouse button
    rmb_state: InputState,

    /// Internal command bus. Used to send internal messages asynchronously.
    /// We do this when we want the renderer to have a chance to run before
    /// the command is processed. For example, when displaying a message before
    /// an expensive process is kicked off.
    queue: Vec<InternalCommand>,

    /// Variables and functions that are created/used on the command line.
    variables: Variables,
}

script_runner! {Session}
impl Session {
    /// Maximum number of views in a session.
    pub const MAX_VIEWS: usize = 64;
    // TODO: read from settings::Settings
    /// Default view width.
    pub const DEFAULT_VIEW_W: u32 = 128;
    /// Default view height.
    pub const DEFAULT_VIEW_H: u32 = 128;

    /// Minimum margin between views, in pixels.
    const VIEW_MARGIN: f32 = 24.;
    /// Size of palette cells, in pixels.
    const PALETTE_CELL_SIZE: f32 = 24.;
    /// Default palette height in cells.
    const PALETTE_HEIGHT: u32 = 16;
    /// Distance to pan when using keyboard.
    const PAN_PIXELS: i64 = 32;
    /// Minimum brush size.
    const MIN_BRUSH_SIZE: usize = 1;
    /// Maximum frame width or height.
    const MAX_FRAME_SIZE: u32 = 4096;
    /// Maximum zoom amount as a multiplier.
    const MAX_ZOOM: u32 = 128;
    /// Zoom levels used when zooming in/out.
    const ZOOM_LEVELS: &'static [u32] =
        &[1, 2, 3, 4, 6, 8, 10, 12, 16, 20, 24, 32, 64, Self::MAX_ZOOM];

    /// Name of pim initialization script.
    const INIT: &'static str = "init.pim";

    /// Create a new un-initialized session.
    pub fn new<P: AsRef<Path>>(
        w: u32,
        h: u32,
        cwd: P,
        proj_dirs: dirs::ProjectDirs,
        base_dirs: dirs::BaseDirs,
    ) -> Self {
        let history_path = proj_dirs.data_dir().join("history");
        let cwd = cwd.as_ref().to_path_buf();

        Self {
            state: State::Initializing,
            width: w as f32,
            height: h as f32,
            cwd: cwd.clone(),
            cursor: SessionCoords::new(0., 0.),
            base_dirs,
            proj_dirs,
            offset: Vector2::zero(),
            help_offset: Vector2::zero(),
            tool: Tool::default(),
            prev_tool: Option::default(),
            lmb_state: InputState::Released,
            rmb_state: InputState::Released,
            hover_color: Option::default(),
            hover_view: Option::default(),
            fg: color::WHITE,
            bg: color::BLACK,
            brush: Brush::default(),
            settings: Settings::default(),
            settings_changed: HashSet::new(),
            views: ViewManager::new(),
            effects: Vec::new(),
            accumulator: time::Duration::from_secs(0),
            palette: Palette::new(Self::PALETTE_CELL_SIZE, Self::PALETTE_HEIGHT as usize),
            key_bindings: KeyBindings::default(),
            keys_pressed: HashSet::new(),
            ignore_received_characters: false,
            cmdline: CommandLine::new(cwd, history_path, path::SUPPORTED_READ_FORMATS),
            mode: Mode::Normal,
            prev_mode: Option::default(),
            selection: Option::default(),
            message: Message::default(),
            avg_time: time::Duration::from_secs(0),
            frame_number: 0,
            queue: Vec::new(),
            variables: Variables::with_built_ins(),
        }
    }

    /// Initialize a session.
    pub fn init(mut self, source: Option<PathBuf>) -> std::io::Result<Self> {
        self.transition(State::Running);
        self.reset()?;

        if let Some(init) = source {
            // The special source '-' is used to skip initialization.
            if init.as_os_str() != "-" {
                self.source_path(&init)?;
            }
        } else {
            let dir = self.proj_dirs.config_dir().to_owned();
            let cfg = dir.join(Self::INIT);

            if cfg.exists() {
                self.source_path(cfg)?;
            }
        }

        self.source_dir(self.cwd.clone()).ok();
        self.cmdline.history.load()?;
        self.message(format!("pim v{}", crate::VERSION), MessageType::Debug);

        Ok(self)
    }

    // Reset to factory defaults.
    pub fn reset(&mut self) -> io::Result<()> {
        self.key_bindings = KeyBindings::default();
        self.settings = Settings::default();
        self.tool = Tool::default();

        // TODO: should this even be done?
        self.source_reader(io::BufReader::new(data::CONFIG), "<init>")
    }

    /// Create a blank view.
    pub fn blank(&mut self, fs: FileStatus, w: u32, h: u32) {
        let frames = vec![vec![Rgba8::TRANSPARENT; w as usize * h as usize]];
        let id = self.add_view(fs, w, h, frames);
        self.organize_views();
        self.edit_view(id);
    }

    pub fn with_blank(mut self, fs: FileStatus, w: u32, h: u32) -> Self {
        self.blank(fs, w, h);

        self
    }

    /// Transition to a new state. Only allows valid state transitions.
    pub fn transition(&mut self, to: State) {
        match (&self.state, &to) {
            (State::Initializing, State::Running)
            | (State::Running, State::Paused)
            | (State::Paused, State::Running)
            | (State::Paused, State::Closing(_))
            | (State::Running, State::Closing(_)) => {
                debug!("state: {:?} -> {:?}", self.state, to);
                self.state = to;
            }
            _ => {}
        }
    }

    /// Update the session by processing new user events and advancing
    /// the internal state.
    pub fn update(
        &mut self,
        events: &mut Vec<Event>,
        exec: &mut Execution,
        delta: time::Duration,
        avg_time: time::Duration,
    ) -> Vec<Effect> {
        self.settings_changed.clear();
        self.avg_time = avg_time;

        if let Tool::Brush = self.tool {
            self.brush.update();
        }

        if let Some(delay) = self.get_delay_if_animating() {
            self.accumulator += delta;
            if self.accumulator >= delay {
                // We only update the active view's animation.
                self.active_view_mut().animation.step();
                self.accumulator = time::Duration::from_secs(0);
            }
        }

        if self.ignore_received_characters {
            self.ignore_received_characters = false;
        }

        // TODO: This whole block needs refactoring..
        if let Execution::Replaying {
            events: recording,
            digest: DigestState { mode, .. },
            result,
            ..
        } = exec
        {
            let mode = *mode;
            let result = result.clone();

            {
                let frame = self.frame_number;
                let end = recording.iter().position(|t| t.frame != frame);

                recording
                    .drain(..end.unwrap_or(recording.len()))
                    .collect::<Vec<TimedEvent>>()
                    .into_iter()
                    .for_each(|t| self.handle_event(t.event, exec));

                let verify_ended = mode == DigestMode::Verify && result.is_done() && end.is_none();
                let replay_ended = mode != DigestMode::Verify && end.is_none();
                let verify_failed = result.is_err();

                // Replay is over.
                if verify_ended || replay_ended || verify_failed {
                    self.release_inputs();
                    self.message("Replay ended", MessageType::Execution);

                    match mode {
                        DigestMode::Verify => {
                            if result.is_ok() {
                                info!("replaying: {}", result.summary());
                                self.quit(ExitReason::Normal);
                            } else {
                                self.quit(ExitReason::Error(result.summary()));
                            }
                        }
                        DigestMode::Record => match exec.finalize_replaying() {
                            Ok(path) => {
                                info!("replaying: digest saved to `{}`", path.display());
                            }
                            Err(e) => {
                                error!("replaying: error saving recording: {}", e);
                            }
                        },
                        DigestMode::Ignore => {}
                    }
                    *exec = Execution::Normal;
                }
            }

            for event in events.drain(..) {
                match event {
                    Event::KeyboardInput(platform::KeyboardInput {
                        key: Some(platform::Key::Escape),
                        ..
                    }) => {
                        self.release_inputs();
                        self.message("Replay ended", MessageType::Execution);

                        *exec = Execution::Normal;
                    }
                    _ => debug!("event (ignored): {:?}", event),
                }
            }
        } else {
            // A common case is that we have multiple `CursorMoved` events
            // in one update. In that case we keep only the last one,
            // since the in-betweens will never be seen.
            if events.len() > 1 && events.iter().all(|e| matches!(e, Event::CursorMoved(_))) {
                events.drain(..events.len() - 1);
            }

            let cmds: Vec<_> = self.queue.drain(..).collect();
            for cmd in cmds.into_iter() {
                self.handle_internal_cmd(cmd, exec);
            }

            for event in events.drain(..) {
                self.handle_event(event, exec);
            }
        }

        if let Tool::Brush = self.tool {
            let brush = &self.brush;
            let output = brush.output(
                Stroke::NONE,
                Fill::Solid(brush.color.into()),
                1.0,
                brush::Align::BottomLeft,
            );
            if !output.is_empty() {
                match brush.state {
                    // If we're erasing, we can't use the staging framebuffer, since we
                    // need to be replacing pixels on the real buffer.
                    _ if brush.is_set(brush::BrushMode::Erase) => {
                        self.effects.extend_from_slice(&[
                            Effect::ViewBlendingChanged(Blending::Constant),
                            Effect::ViewPaintFinal(output),
                        ]);
                    }
                    // As long as we haven't finished drawing, render into the staging buffer.
                    BrushState::DrawStarted(_) | BrushState::Drawing(_) => {
                        self.effects.push(Effect::ViewPaintDraft(output));
                    }
                    // Once we're done drawing, we can render into the real buffer.
                    BrushState::DrawEnded(_) => {
                        self.effects.extend_from_slice(&[
                            Effect::ViewBlendingChanged(Blending::Alpha),
                            Effect::ViewPaintFinal(output),
                        ]);
                    }
                    // If the brush output isn't empty, we can't possibly not
                    // be drawing!
                    BrushState::NotDrawing => unreachable!(),
                }
            }
        }

        if self.views.is_empty() {
            self.quit(ExitReason::Normal);
        } else {
            for v in self.views.iter_mut() {
                if !v.ops.is_empty() {
                    self.effects
                        .push(Effect::ViewOps(v.id, v.ops.drain(..).collect()));
                }
                match v.state {
                    ViewState::Dirty(_) => {}
                    ViewState::Damaged(extent) => {
                        self.effects.push(Effect::ViewDamaged(v.id, extent));
                    }
                    ViewState::Okay => {}
                }
            }
        }

        match exec {
            Execution::Replaying {
                events: recording,
                digest: DigestState { mode, .. },
                ..
            } if *mode == DigestMode::Verify || *mode == DigestMode::Record => {
                // Skip to the next event frame to speed up replay.
                self.frame_number = recording
                    .front()
                    .map(|e| e.frame)
                    .unwrap_or(self.frame_number + 1);
            }
            _ => {
                self.frame_number += 1;
            }
        }

        // Make sure we don't have rounding errors
        debug_assert_eq!(self.offset, self.offset.map(|a| a.floor()));

        // Return and drain accumulated effects
        self.effects.drain(..).collect()
    }

    /// Cleanup to be run at the end of the frame.
    pub fn cleanup(&mut self) {
        for v in self.views.iter_mut() {
            v.okay();
        }
    }

    /// Quit the session.
    pub fn quit(&mut self, r: ExitReason) {
        if self.cmdline.history.save().is_err() {
            error!(
                "Error: couldn't save command history to {}",
                self.cmdline.history.path.display()
            );
        }
        self.transition(State::Closing(r));
    }

    /// Return the session offset as a transformation matrix.
    pub fn transform(&self) -> Matrix4<f32> {
        Matrix4::from_translation(self.offset.extend(0.))
    }

    /// Snap the given session coordinates to the pixel grid.
    /// This only has an effect at zoom levels greater than `1.0`.
    #[allow(dead_code)]
    pub fn snap(&self, p: SessionCoords, offx: f32, offy: f32, zoom: f32) -> SessionCoords {
        SessionCoords::new(
            p.x - ((p.x - offx - self.offset.x) % zoom),
            p.y - ((p.y - offy - self.offset.y) % zoom),
        )
        .floor()
    }

    /// Gets the current animation delay. Returns `None` if animations aren't playing,
    /// or if the current view has only one frame.
    pub fn get_delay_if_animating(&self) -> Option<time::Duration> {
        let view = self.active_view();

        if self.get_i64_setting(I64Setting::UiAnimate) != 0 && view.animation.len() > 1 {
            Some(view.animation.delay)
        } else {
            None
        }
    }

    pub fn animation_delay(&self) -> time::Duration {
        if self.views.len() == 0 {
            // This should only execute at the start of the program when no views are open.
            time::Duration::from_millis(160)
        } else {
            self.active_view().animation.delay
        }
    }

    /// Check whether the session is running.
    pub fn is_running(&self) -> bool {
        self.state == State::Running
    }

    /// Return help string.
    pub fn help(&self) -> Vec<String> {
        self.cmdline
            .commands
            .iter()
            .map(|(_, help, parser)| format!(":{:<42} {}", parser.to_string(), help))
            .collect()
    }

    ////////////////////////////////////////////////////////////////////////////

    /// Pan the view by a relative amount.
    fn pan(&mut self, x: f32, y: f32) {
        match self.mode {
            Mode::Help => {
                self.help_offset.x += x;
                self.help_offset.y += y;

                if self.help_offset.x > 0. {
                    self.help_offset.x = 0.;
                }
                if self.help_offset.y < 0. {
                    self.help_offset.y = 0.;
                }
            }
            _ => {
                self.offset.x += x;
                self.offset.y += y;
            }
        }
        self.cursor_dirty();
    }

    /// Re-compute state related to the cursor position. This is useful
    /// when the cursor hasn't moved relative to the session, but things
    /// within the session have moved relative to the cursor.
    fn cursor_dirty(&mut self) {
        let cursor = self.cursor;
        let palette_hover = self.palette.hover.is_some();

        self.palette.handle_cursor_moved(cursor);
        self.hover_view = None;

        let gained_palette_focus = !palette_hover && self.palette.hover.is_some();

        match &self.tool {
            Tool::Brush if !self.brush.is_drawing() => {
                if gained_palette_focus {
                    self.tool(Tool::Sampler);
                }
            }
            Tool::FloodFill => {
                if gained_palette_focus {
                    self.tool(Tool::Sampler);
                }
            }
            Tool::Sampler if palette_hover && self.palette.hover.is_none() => {
                // Lost palette focus with color sampler.
                self.prev_tool();
            }
            _ => {}
        }

        for v in self.views.iter_mut() {
            let p = cursor - self.offset;
            if v.contains(p) {
                self.hover_view = Some(v.id);
                break;
            }
        }

        self.hover_color = if self.palette.hover.is_some() {
            self.palette.hover
        } else if let Some(v) = self.hover_view {
            let p = self.view_coords(v, cursor).into();
            self.view(v).color_at(p).cloned()
        } else {
            None
        };
    }

    /// Called when settings have been changed.
    // TODO: remove me
    fn setting_changed(&mut self, name: &str, old: Value, new: Value) {
        debug!("set `{}`: {} -> {}", name, old, new);

        self.settings_changed.insert(name.to_owned());

        match name {
            "p/height" => {
                self.palette.height = new.to_u64() as usize;
                self.center_palette();
            }
            "scale%" => {
                let mut new_percentage = new.to_u64();
                if new_percentage < 100 || new_percentage > 400 {
                    self.message(
                        format!(
                            "ui/scale% should be between 100 and 400, got {}",
                            new_percentage
                        ),
                        MessageType::Error,
                    );
                    new_percentage = 100;
                    assert_ok!(self
                        .settings
                        .set("scale%", Value::U32(new_percentage as u32)));
                }
                // TODO: We need to recompute the cursor position here
                // from the window coordinates. Currently, cursor position
                // is stored only in `SessionCoords`, which would have
                // to change.
                self.rescale((old.to_u64() as f64) / 100.0, new_percentage as f64 / 100.0);
            }
            _ => {}
        }
    }

    /// Toggle the session mode.
    fn toggle_mode(&mut self, mode: Mode) {
        if self.mode == mode {
            self.switch_mode(Mode::Normal);
        } else {
            self.switch_mode(mode);
        }
    }

    /// Switch the session mode.
    fn switch_mode(&mut self, mode: Mode) {
        let (old, new) = (self.mode, mode);
        if old == new {
            return;
        }

        match old {
            Mode::Command => {
                // TODO: put this into history
                self.cmdline.clear();
            }
            _ => {}
        }

        match new {
            Mode::Normal => {
                self.selection = None;
            }
            Mode::Command => {
                // When switching to command mode via the keyboard, we simultaneously
                // also receive the character input equivalent of the key pressed.
                // This input, since we are now in command mode, is processed as
                // text input to the command line. To avoid this, we have to ignore
                // all such input until the end of the current update.
                self.ignore_received_characters = true;
                self.cmdline_handle_input(':');
            }
            _ => {}
        }

        self.release_inputs();
        self.prev_mode = Some(self.mode);
        self.mode = new;
    }

    /// Release all keys and mouse buttons.
    fn release_inputs(&mut self) {
        let pressed: Vec<platform::Key> = self.keys_pressed.iter().cloned().collect();
        for k in pressed {
            self.handle_keyboard_input(
                platform::KeyboardInput {
                    key: Some(k),
                    modifiers: ModifiersState::default(),
                    state: InputState::Released,
                },
                &mut Execution::Normal,
            );
        }
        if self.lmb_state == InputState::Pressed {
            self.handle_mouse_input(platform::MouseButton::Left, InputState::Released);
        }
        if self.rmb_state == InputState::Pressed {
            self.handle_mouse_input(platform::MouseButton::Right, InputState::Released);
        }
    }

    fn unimplemented(&mut self) {
        self.message("Error: not yet implemented", MessageType::Error);
    }

    ///////////////////////////////////////////////////////////////////////////////
    /// View functions
    ///////////////////////////////////////////////////////////////////////////////

    /// Get the view with the given id.
    ///
    /// # Panics
    ///
    /// Panics if the view isn't found.
    pub fn view(&self, id: ViewId) -> &View<ViewResource> {
        self.views
            .get(id)
            .expect(&format!("view #{} must exist", id))
    }

    /// Get the view with the given id (mutable).
    ///
    /// # Panics
    ///
    /// Panics if the view isn't found.
    pub fn view_mut(&mut self, id: ViewId) -> &mut View<ViewResource> {
        self.views
            .get_mut(id)
            .expect(&format!("view #{} must exist", id))
    }

    /// Get the currently active view.
    ///
    /// # Panics
    ///
    /// Panics if there is no active view.
    pub fn active_view(&self) -> &View<ViewResource> {
        assert!(
            self.views.active_id != ViewId::default(),
            "fatal: no active view"
        );
        self.view(self.views.active_id)
    }

    /// Get the currently active view (mutable).
    ///
    /// # Panics
    ///
    /// Panics if there is no active view.
    pub fn active_view_mut(&mut self) -> &mut View<ViewResource> {
        assert!(
            self.views.active_id != ViewId::default(),
            "fatal: no active view"
        );
        self.view_mut(self.views.active_id)
    }

    /// Activate a view. This makes the given view the "active" view.
    pub fn activate(&mut self, id: ViewId) {
        if self.views.active_id == id {
            return;
        }
        self.views.activate(id);
        self.effects.push(Effect::ViewActivated(id));
    }

    /// Check whether a view is active.
    pub fn is_active(&self, id: ViewId) -> bool {
        self.views.active_id == id
    }

    /// Convert "logical" window coordinates to session coordinates.
    pub fn window_to_session_coords(&self, position: platform::LogicalPosition) -> SessionCoords {
        let (x, y) = (position.x, position.y);
        let scale = self.get_scale();
        SessionCoords::new(
            (x / scale).floor() as f32,
            self.height - (y / scale).floor() as f32 - 1.,
        )
    }

    /// Convert session coordinates to view coordinates of the given view.
    pub fn view_coords(&self, v: ViewId, p: SessionCoords) -> Point<ViewExtent, f32> {
        let v = self.view(v);
        let SessionCoords { point: mut p, .. } = p;

        p = p - self.offset - v.offset;
        p = p / v.zoom as f32;

        if v.flip_x {
            p.x = v.width() as f32 - p.x;
        }
        if v.flip_y {
            p.y = v.height() as f32 - p.y;
        }

        Point::new(p.x.floor(), p.y.floor())
    }

    /// Convert view coordinates to session coordinates.
    pub fn session_coords(&self, v: ViewId, p: Point<ViewExtent, f32>) -> SessionCoords {
        let v = self.view(v);

        let p = Point2::new(p.x * v.zoom as f32, p.y * v.zoom as f32);
        let p = p + self.offset + v.offset;

        if v.flip_x {
            unimplemented!();
        }
        if v.flip_y {
            unimplemented!();
        }

        SessionCoords::new(p.x, p.y).floor()
    }

    /// Convert session coordinates to view coordinates of the active view.
    pub fn active_view_coords(&self, p: SessionCoords) -> Point<ViewExtent, f32> {
        self.view_coords(self.views.active_id, p)
    }

    /// Convert session coordinates to view coordinates of the active view.
    pub fn get_active_view_mouse_coords(&self) -> Point<ViewExtent, f32> {
        self.view_coords(self.views.active_id, self.cursor)
    }

    /// Check whether a point is inside the selection, if any.
    pub fn is_selected(&self, p: ViewCoords<i32>) -> bool {
        if let Some(s) = self.selection {
            s.abs().bounds().contains(*p)
        } else {
            false
        }
    }

    /// Edit paths, with error handling.
    ///
    /// Loads the given files into the session. Returns an error if one of
    /// the paths couldn't be loaded. If a path points to a directory,
    /// loads all files within that directory.
    ///
    /// If a path doesn't exist, creates a blank view for that path.
    pub fn edit_images<P: AsRef<Path>>(&mut self, paths: &[P]) {
        if paths.is_empty() {
            self.message("include file(s) to edit in the command", MessageType::Error);
            return;
        }
        match self.edit_internal(paths) {
            Ok((success_count, fail_count)) => {
                if success_count + fail_count > 1 {
                    self.message(
                        format!("{} path(s) loaded, {} skipped", success_count, fail_count),
                        MessageType::Info,
                    )
                }
            }
            Err(e) => self.message(format!("Error loading path(s): {}", e), MessageType::Error),
        }
    }

    /// Edit paths.
    ///
    /// Loads the given files into the session. Returns an error if one of
    /// the paths couldn't be loaded. If a path points to a directory,
    /// loads all files within that directory.
    ///
    /// If a path doesn't exist, creates a blank view for that path.
    fn edit_internal<P: AsRef<Path>>(&mut self, paths: &[P]) -> io::Result<(usize, usize)> {
        use std::ffi::OsStr;

        let (mut success_count, mut fail_count) = (0usize, 0usize);

        for path in paths {
            let path = path.as_ref();

            if path.is_dir() {
                for entry in path.read_dir()? {
                    let entry = entry?;
                    let path = entry.path();

                    if path.is_dir() {
                        continue;
                    }

                    if path.file_name() == Some(OsStr::new(".pimrc")) {
                        continue;
                    }

                    if self.load_view(path).is_err() {
                        fail_count += 1;
                        continue;
                    }

                    success_count += 1;
                }
                // TODO: don't source here.
                self.source_dir(path).ok();
            } else {
                if path.exists() {
                    self.load_view(path)?;
                } else if !path.exists() && path.with_extension("png").exists() {
                    self.load_view(path.with_extension("png"))?;
                } else {
                    let (w, h) = if !self.views.is_empty() {
                        let v = self.active_view();
                        (v.width(), v.fh)
                    } else {
                        (Self::DEFAULT_VIEW_W, Self::DEFAULT_VIEW_H)
                    };
                    self.blank(
                        FileStatus::New(FileStorage::Single(path.with_extension("png"))),
                        w,
                        h,
                    );
                }
                success_count += 1;
            }
        }

        if let Some(id) = self.views.last().map(|v| v.id) {
            self.organize_views();
            self.edit_view(id);
        }

        Ok((success_count, fail_count))
    }

    // TODO: it would also be nice to have a `append` command for adding to the current file.
    /// Loads the given paths into the session as frames in a new view.
    pub fn concatenate_images<P: AsRef<Path>>(&mut self, paths: &[P]) {
        if paths.is_empty() {
            self.message(
                "include files to concatenate in the command",
                MessageType::Error,
            );
            return;
        }
        if let Err(e) = self.concatenate_internal(paths) {
            self.message(
                format!("Error concatenating image(s): {}", e),
                MessageType::Error,
            );
        }
    }

    fn concatenate_internal<P: AsRef<Path>>(&mut self, paths: &[P]) -> io::Result<()> {
        let completer = FileCompleter::new(&self.cwd, path::SUPPORTED_READ_FORMATS);
        // TODO: remove, don't source here.
        let mut dirs = Vec::new();

        // TODO: `fn edit` should use the same logic as here.  we should pull out a common method.
        let paths = paths
            .iter()
            .map(|path| {
                let path = path.as_ref();

                if path.is_dir() {
                    // TODO: load all pim-able image files in this directory, sorted:
                    // paths.sort_by(|a, b| a.file_name().cmp(&b.file_name()));
                    dirs.push(path);
                    completer
                        .paths(path)
                        .map(|paths| paths.map(|p| path.join(p)).collect())
                } else if path.exists() {
                    Ok(vec![path.to_path_buf()])
                } else if !path.exists() && path.with_extension("png").exists() {
                    Ok(vec![path.with_extension("png")])
                } else {
                    Ok(vec![])
                }
            })
            // Collect paths and errors.
            .collect::<io::Result<Vec<_>>>()?
            .into_iter()
            .flatten()
            .filter(|p| p.file_name().is_some() && p.file_stem().is_some())
            .collect::<Vec<_>>();

        // Do not sort file names, they were added in a specific order so that's
        // the order they should be concatenated in.

        if paths.is_empty() {
            // If our paths list is empty, don't add another view.
            // This can happen if we opened up an empty directory.
            return Ok(());
        }

        let (fw, fh, frame) = crate::io::concatenate_images(&paths)?;
        self.add_view(
            // Don't save as a range; we want to allow the user to
            // save this as a new file.
            FileStatus::NoFile,
            fw,
            fh,
            vec![frame],
        );

        if let Some(id) = self.views.last().map(|v| v.id) {
            self.organize_views();
            self.edit_view(id);
        }

        Ok(())
    }

    fn append_internal<P: AsRef<Path>>(&mut self, paths: &[P]) -> io::Result<()> {
        let completer = FileCompleter::new(&self.cwd, path::SUPPORTED_READ_FORMATS);
        // TODO: remove, don't source here.
        let mut dirs = Vec::new();

        // TODO: `fn edit` should use the same logic as here.  we should pull out a common method.
        let paths = paths
            .iter()
            .map(|path| {
                let path = path.as_ref();

                if path.is_dir() {
                    // TODO: load all pim-able image files in this directory, sorted:
                    // paths.sort_by(|a, b| a.file_name().cmp(&b.file_name()));
                    dirs.push(path);
                    completer
                        .paths(path)
                        .map(|paths| paths.map(|p| path.join(p)).collect())
                } else if path.exists() {
                    Ok(vec![path.to_path_buf()])
                } else if !path.exists() && path.with_extension("png").exists() {
                    Ok(vec![path.with_extension("png")])
                } else {
                    Ok(vec![])
                }
            })
            // Collect paths and errors.
            .collect::<io::Result<Vec<_>>>()?
            .into_iter()
            .flatten()
            .filter(|p| p.file_name().is_some() && p.file_stem().is_some())
            .collect::<Vec<_>>();

        // Do not sort file names, they were added in a specific order so that's
        // the order they should be concatenated in.

        // If our paths list is empty, return early.
        let paths = if let Some(paths) = NonEmpty::from_slice(paths.as_slice()) {
            paths
        } else {
            return Ok(());
        };

        // Load images and collect errors.
        let mut frames = paths
            .iter()
            .map(crate::io::load_image)
            .collect::<io::Result<Vec<_>>>()?
            .into_iter()
            .peekable();

        // TODO: use the current view frame width as a reference; we should split each loaded
        // image into that width.  alternatively, it might be easier to just convert into one
        // long frame and let the user split into frames after that.
        // Use the first frame as a reference for what size the rest of
        // the frames should be.
        if let Some((fw, fh, _)) = frames.peek() {
            let (fw, fh) = (*fw, *fh);

            if frames.clone().all(|(w, h, _)| w == fw && h == fh) {
                let frames: Vec<_> = frames.map(|(_, _, pixels)| pixels).collect();
                // TODO: don't use Range here.
                self.add_view(FileStatus::Saved(FileStorage::Range(paths)), fw, fh, frames);
            } else {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    format!("frame dimensions must all match {}x{}", fw, fh),
                ));
            }
        }

        if let Some(id) = self.views.last().map(|v| v.id) {
            self.organize_views();
            self.edit_view(id);
        }

        Ok(())
    }

    /// Save the given view to disk with the current file name. Returns
    /// an error if the view has no file name.
    pub fn save_view(&mut self, id: ViewId) -> io::Result<(FileStorage, usize)> {
        let view = self.view_mut(id);

        if let Some(f) = view.file_storage().cloned() {
            view.save_as(&f).map(|w| (f, w))
        } else {
            Err(io::Error::new(io::ErrorKind::Other, "no file name given"))
        }
    }

    /// Private ///////////////////////////////////////////////////////////////////

    /// Export a view in a specific format.
    fn export_as(&mut self, id: ViewId, path: &Path, scale: u32) -> io::Result<()> {
        let ext = get_extension(&path)?;

        let written = match ext {
            "gif" => {
                let palette = self.colors();
                let view = self.view(id);
                // `view.save_gif` would work because View derefs the inner Resource,
                // but `view.resource.save_gif` makes it clear that `view.animation.delay`
                // isn't available in the resource itself, so we need to supply it.
                view.resource
                    .save_gif(path, view.animation.delay, &palette, scale)?
            }
            "svg" => self.view(id).save_svg(path, scale)?,
            "png" => self.view(id).save_png(path, scale)?,
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    format!("`{}` is not a supported export format", ext),
                ));
            }
        };
        self.message(
            format!("\"{}\" {} pixels written", path.display(), written),
            MessageType::Info,
        );
        Ok(())
    }

    /// Load a view into the session.
    fn load_view<P: AsRef<Path>>(&mut self, path: P) -> io::Result<()> {
        let path = path.as_ref();
        let path = view::Path::try_from(path)?;

        debug!("load: {:?}", path);

        if let Some(View { id, .. }) = self
            .views
            .find(|v| v.file_storage().map_or(false, |f| f.contains(&*path)))
        {
            // View is already loaded.
            let id = *id;
            eprint!("centering {}\n", id);
            // TODO: for some reason this doesn't always center.
            self.edit_view(id);
            return Ok(());
        }

        match path.format {
            view::Format::Png => {
                let (width, height, pixels) = crate::io::load_image(&*path)?;

                self.add_view(
                    FileStatus::Saved(FileStorage::Single((*path).into())),
                    width,
                    height,
                    vec![pixels],
                );
                self.message(
                    format!("\"{}\" {} pixels read", path.display(), width * height),
                    MessageType::Info,
                );
            }
            view::Format::Gif => {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "gif files are not supported",
                ));
            }
        }

        Ok(())
    }

    fn add_view(
        &mut self,
        file_status: FileStatus,
        fw: u32,
        fh: u32,
        frames: Vec<Vec<Rgba8>>,
    ) -> ViewId {
        let nframes = frames.len();
        assert!(nframes >= 1);

        // Replace the active view if it's a scratch pad (and hasn't been modified).
        if let Some(v) = self.views.active() {
            let id = v.id;

            if v.file_status == FileStatus::NoFile {
                self.destroy_view(id);
            }
        }

        let pixels = util::stitch_frames(frames, fw as usize, fh as usize, Rgba8::TRANSPARENT);
        let resource = ViewResource::new(pixels, ViewExtent::new(fw, fh, nframes));
        let delay = self.animation_delay();
        let id = self
            .views
            .add(file_status, fw, fh, nframes, delay, resource);

        self.effects.push(Effect::ViewAdded(id));

        id
    }

    /// Destroys the resources associated with a view.
    fn destroy_view(&mut self, id: ViewId) {
        assert!(!self.views.is_empty());

        self.views.remove(id);
        self.effects.push(Effect::ViewRemoved(id));
    }

    /// Quit the view.
    fn quit_view(&mut self, id: ViewId) {
        self.destroy_view(id);

        if !self.views.is_empty() {
            self.organize_views();
            self.center_active_view();
        }
    }

    /// Quit view if it has been saved. Otherwise, display an error.
    fn quit_view_safe(&mut self, id: ViewId) {
        let v = self.view(id);
        if v.file_status.needs_saving() {
            self.message(
                "Error: no write since last change (enter `:q!` to quit without saving)",
                MessageType::Error,
            );
        } else {
            self.quit_view(id);
        }
    }

    fn colors(&self) -> ArrayVec<[Rgba8; 256]> {
        let mut palette = self.palette.colors.clone();

        palette.push(self.fg);
        palette.push(self.bg);
        palette
    }

    /// Start editing the given view.
    fn edit_view(&mut self, id: ViewId) {
        self.activate(id);
        self.center_active_view();
    }

    /// Re-position all views relative to each other so that they don't overlap.
    fn organize_views(&mut self) {
        let mut offset = 0.0;
        for v in self.views.iter_mut().rev() {
            v.offset.y = offset;
            // TODO: We need a way to distinguish view content size with real (rendered) size.
            offset += v.height() as f32 * v.zoom as f32 + Self::VIEW_MARGIN;
        }
        self.cursor_dirty();
    }

    /// Check the current selection and invalidate it if necessary.
    fn check_selection(&mut self) {
        let v = self.active_view();
        let r = v.bounds();
        if let Some(s) = &self.selection {
            if !r.contains(s.min()) && !r.contains(s.max()) {
                self.selection = None;
            }
        }
    }

    fn paste_selection(&mut self) -> Option<Rect<i32>> {
        if let (Mode::Visual(VisualState::Pasting), Some(s)) = (self.mode, self.selection) {
            let bounds = s.abs().bounds();
            self.active_view_mut().paste(bounds);
            Some(bounds)
        } else {
            // TODO: if there is a previous copied selection, we can paste even if we're not in visual mode
            //       e.g., paste to mouse cursor.
            None
        }
    }

    fn yank_selection(&mut self) -> Option<Rect<i32>> {
        // TODO: copy the yanked pixels into their own buffer.
        //       use that buffer when pasting.
        // TODO: longer term, keep track of last Z yanks (e.g., Z = 10, maybe make it a setting),
        //       allow pasting from any of these.
        if let (Mode::Visual(VisualState::Selecting { .. }), Some(s)) = (self.mode, self.selection)
        {
            let v = self.active_view_mut();
            let s = s.abs().bounds();

            if s.intersects(v.layer_bounds()) {
                let s = s.intersection(v.layer_bounds());

                v.yank(s);

                self.selection = Some(Selection::from(s));
                self.switch_mode(Mode::Visual(VisualState::Pasting));

                return Some(s);
            }
        }
        None
    }

    fn undo(&mut self, id: ViewId) {
        self.restore_view_snapshot(id, Direction::Backward);
    }

    fn redo(&mut self, id: ViewId) {
        self.restore_view_snapshot(id, Direction::Forward);
    }

    fn restore_view_snapshot(&mut self, id: ViewId, dir: Direction) {
        self.view_mut(id).restore_snapshot(dir);
        self.organize_views();
        self.cursor_dirty();
    }

    ///////////////////////////////////////////////////////////////////////////
    // Internal command handler
    ///////////////////////////////////////////////////////////////////////////

    fn handle_internal_cmd(&mut self, cmd: InternalCommand, exec: &mut Execution) {
        match cmd {
            InternalCommand::StopRecording => match exec.stop_recording() {
                Ok(path) => {
                    self.message(
                        format!("Recording saved to `{}`", path.display()),
                        MessageType::Execution,
                    );
                    info!("recording: events saved to `{}`", path.display());
                    self.quit(ExitReason::Normal);
                }
                Err(e) => {
                    error!("recording: error stopping: {}", e);
                }
            },
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // Event handlers
    ///////////////////////////////////////////////////////////////////////////

    pub fn handle_event(&mut self, event: Event, exec: &mut Execution) {
        if let Execution::Recording {
            ref mut events,
            start,
            ..
        } = exec
        {
            events.push(TimedEvent::new(
                self.frame_number,
                start.elapsed(),
                event.clone(),
            ));
        }

        match event {
            Event::MouseInput(btn, st) => {
                self.handle_mouse_input(btn, st);
            }
            Event::MouseWheel(delta, modifiers) => {
                self.handle_mouse_wheel(delta, modifiers);
            }
            Event::CursorMoved(position) => {
                let coords = self.window_to_session_coords(position);
                self.handle_cursor_moved(coords);
            }
            Event::KeyboardInput(input) => self.handle_keyboard_input(input, exec),
            Event::ReceivedCharacter(c, mods) => self.handle_received_character(c, mods),
            Event::Paste(p) => self.handle_paste(p),
        }
    }

    pub fn resize(&mut self, size: platform::LogicalSize, scale: f64) {
        let (w, h) = (size.width / scale, size.height / scale);

        self.width = w as f32;
        self.height = h as f32;

        // TODO: Reset session cursor coordinates
        self.center_palette();
        self.center_active_view();
    }

    pub fn rescale(&mut self, old: f64, new: f64) {
        let (w, h) = (self.width as f64 * old, self.height as f64 * old);

        self.resize(platform::LogicalSize::new(w, h), new);
        self.effects.push(Effect::SessionScaled(new));
    }

    pub fn handle_resized(&mut self, size: platform::LogicalSize) {
        self.resize(size, self.get_scale());
        self.effects.push(Effect::SessionResized(size));
    }

    fn get_scale(&self) -> f64 {
        self.get_i64_setting(I64Setting::UiScalePercentage) as f64 / 100.0
    }

    fn handle_mouse_input(&mut self, button: platform::MouseButton, state: platform::InputState) {
        // TODO: add right mouse button
        if button == platform::MouseButton::Left {
            self.lmb_state = state;
        } else if button == platform::MouseButton::Right {
            self.rmb_state = state;
        } else {
            return;
        }

        match state {
            InputState::Pressed => {
                // Click on palette.
                if let Some(color) = self.palette.hover {
                    if self.mode == Mode::Command {
                        // TODO: something more interesting for right-mouse-button
                        self.cmdline.puts(&Rgb8::from(color).to_string());
                    } else {
                        self.pick_color(color, button);
                    }
                    return;
                }

                // Click on a view.
                if let Some(id) = self.hover_view {
                    // Clicking on a view is one way to get out of command mode.
                    if self.mode == Mode::Command {
                        self.cmdline_hide();
                        return;
                    }
                    if self.is_active(id) {
                        let v = self.view(id);
                        let p = self.active_view_coords(self.cursor);

                        let extent = v.extent();

                        match self.mode {
                            Mode::Normal => match self.tool {
                                Tool::Brush => {
                                    let color = if self.brush.is_set(brush::BrushMode::Erase) {
                                        Rgba8::TRANSPARENT
                                    } else {
                                        self.fg_or_bg(button)
                                    };
                                    let previous_out =
                                        self.brush.start_drawing(p.into(), color, extent);
                                    if !previous_out.is_empty() {
                                        self.effects.extend_from_slice(&[
                                            Effect::ViewBlendingChanged(Blending::Alpha),
                                            Effect::ViewPaintFinal(previous_out),
                                        ]);
                                    }
                                }
                                Tool::Sampler => {
                                    self.sample_color(button);
                                }
                                Tool::Pan => {}
                                Tool::FloodFill => {
                                    // Ignore failures here
                                    let _ = self.script_bucket(
                                        p.x as i64,
                                        p.y as i64,
                                        self.fg_or_bg(button),
                                    );
                                }
                            },
                            Mode::Command => {
                                // TODO
                            }
                            Mode::Visual(VisualState::Selecting { ref mut dragging }) => {
                                let p = p.map(|n| n as i32);
                                let unit = Selection::new(p.x, p.y, p.x + 1, p.y + 1);

                                if let Some(s) = &mut self.selection {
                                    if s.abs().bounds().contains(p) {
                                        *dragging = true;
                                    } else {
                                        self.selection = Some(unit);
                                    }
                                } else {
                                    self.selection = Some(unit);
                                }
                            }
                            Mode::Visual(VisualState::Pasting) => {
                                // Re-center the selection in-case we've switched layer.
                                self.center_selection(self.cursor);
                                self.command(Cmd::SelectionPaste);
                            }
                            Mode::Help => {}
                        }
                    } else {
                        self.activate(id);
                        self.center_selection(self.cursor);
                    }
                } else {
                    // Clicking outside a view...
                    match self.mode {
                        Mode::Visual(VisualState::Selecting { ref mut dragging }) => {
                            self.selection = None;
                            *dragging = false;
                        }
                        _ => {}
                    }
                }
            }
            InputState::Released => match self.mode {
                Mode::Visual(VisualState::Selecting { ref mut dragging }) => {
                    *dragging = false;
                }
                Mode::Normal => {
                    if let Tool::Brush = self.tool {
                        match self.brush.state {
                            BrushState::Drawing { .. } | BrushState::DrawStarted { .. } => {
                                self.brush.stop_drawing();
                                // TODO: if the other mouse button is still pressed, resume drawing with it
                                self.active_view_mut().touch();
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            },
            InputState::Repeated => {}
        }
    }

    fn handle_mouse_wheel(
        &mut self,
        delta: platform::LogicalDelta,
        modifiers: platform::ModifiersState,
    ) {
        if modifiers.ctrl {
            // zoom
            if delta.y > 0. {
                if let Some(v) = self.hover_view {
                    self.activate(v);
                }
                self.zoom_in(self.cursor);
            } else if delta.y < 0. {
                self.zoom_out(self.cursor);
            }
            return;
        }
        let axis_pan = (Self::PAN_PIXELS as f32 * delta.y as f32) as f32;
        if modifiers.shift {
            // pan horizontally
            self.pan(axis_pan, 0.0);
        } else {
            // pan vertically
            self.pan(0.0, -axis_pan);
        }
    }

    fn handle_cursor_moved(&mut self, cursor: SessionCoords) {
        if self.cursor == cursor {
            return;
        }

        let prev_cursor = self.cursor;
        let p = self.active_view_coords(cursor);
        let prev_p = self.active_view_coords(prev_cursor);
        let (vw, vh) = self.active_view().size();

        self.cursor = cursor;
        self.cursor_dirty();

        match self.tool {
            Tool::Pan => {
                self.pan(cursor.x - prev_cursor.x, cursor.y - prev_cursor.y);
            }
            Tool::Sampler if self.lmb_state == InputState::Pressed => {
                self.sample_color(platform::MouseButton::Left);
            }
            Tool::Sampler if self.rmb_state == InputState::Pressed => {
                self.sample_color(platform::MouseButton::Right);
            }
            _ => {
                match self.mode {
                    Mode::Normal => match self.tool {
                        Tool::Brush if p != prev_p => match self.brush.state {
                            BrushState::DrawStarted { .. } | BrushState::Drawing { .. } => {
                                let brush = &mut self.brush;
                                let mut p: ViewCoords<i32> = p.into();

                                // TODO: i think this is backwards
                                if brush.is_set(brush::BrushMode::Multi) {
                                    p.clamp(Rect::new(
                                        (brush.size / 2) as i32,
                                        (brush.size / 2) as i32,
                                        vw as i32 - (brush.size / 2) as i32 - 1,
                                        vh as i32 - (brush.size / 2) as i32 - 1,
                                    ));
                                }
                                brush.draw(p);
                            }
                            _ => {}
                        },
                        _ => {}
                    },
                    Mode::Visual(VisualState::Selecting { dragging: false }) => {
                        if self.lmb_state == InputState::Pressed {
                            if let Some(ref mut s) = self.selection {
                                *s = Selection::new(s.x1, s.y1, p.x as i32 + 1, p.y as i32 + 1);
                            }
                        }
                    }
                    Mode::Visual(VisualState::Selecting { dragging: true }) => {
                        let view = self.active_view().layer_bounds();

                        // Resize selection.
                        if self.lmb_state == InputState::Pressed && p != prev_p {
                            if let Some(ref mut s) = self.selection {
                                // TODO: (rgx) Better API.
                                let delta = *p - Vector2::new(prev_p.x, prev_p.y);
                                let delta = Vector2::new(delta.x as i32, delta.y as i32);
                                let t = Selection::from(s.bounds() + delta);

                                if view.intersects(t.abs().bounds()) {
                                    *s = t;
                                }
                            }
                        }
                    }
                    Mode::Visual(VisualState::Pasting) => {
                        self.center_selection(cursor);
                    }
                    _ => {}
                }
            }
        }
    }

    fn handle_paste(&mut self, paste: Option<String>) {
        if let Some(s) = paste {
            self.cmdline.puts(s.as_str())
        }
    }

    fn handle_received_character(&mut self, c: char, mods: ModifiersState) {
        if self.mode == Mode::Command {
            if c.is_control() || self.ignore_received_characters {
                return;
            }
            self.cmdline_handle_input(c);
        } else if let Some(kb) =
            self.key_bindings
                .find(Input::Character(c), mods, InputState::Pressed, self.mode)
        {
            self.command(kb.command);
        }
    }

    fn handle_keyboard_input(&mut self, input: platform::KeyboardInput, exec: &mut Execution) {
        let KeyboardInput {
            state,
            modifiers,
            key,
            ..
        } = input;

        let mut repeat = state == InputState::Repeated;
        let state = if repeat { InputState::Pressed } else { state };

        if let Some(key) = key {
            // While the mouse is down, don't accept keyboard input.
            if self.lmb_state == InputState::Pressed || self.rmb_state == InputState::Pressed {
                return;
            }

            if state == InputState::Pressed {
                repeat = repeat || !self.keys_pressed.insert(key);
            } else if state == InputState::Released {
                if !self.keys_pressed.remove(&key) {
                    return;
                }
            }

            match self.mode {
                Mode::Visual(VisualState::Selecting { .. }) => {
                    if key == platform::Key::Escape && state == InputState::Pressed {
                        self.switch_mode(Mode::Normal);
                        return;
                    }
                }
                Mode::Visual(VisualState::Pasting) => {
                    if key == platform::Key::Escape && state == InputState::Pressed {
                        self.switch_mode(Mode::Visual(VisualState::default()));
                        return;
                    }
                }
                Mode::Command => {
                    if state == InputState::Pressed {
                        match key {
                            platform::Key::Up => {
                                self.cmdline.history_prev();
                            }
                            platform::Key::Down => {
                                self.cmdline.history_next();
                            }
                            platform::Key::Left => {
                                self.cmdline.cursor_backward();
                            }
                            platform::Key::Right => {
                                self.cmdline.cursor_forward();
                            }
                            platform::Key::Tab => {
                                self.cmdline.completion_next();
                            }
                            platform::Key::Backspace => {
                                self.cmdline_handle_backspace();
                            }
                            platform::Key::Return => {
                                self.cmdline_handle_enter();
                            }
                            platform::Key::Escape => {
                                self.cmdline_hide();
                            }
                            platform::Key::Home => {
                                self.cmdline.cursor_back();
                            }
                            platform::Key::End => {
                                self.cmdline.cursor_front();
                            }
                            _ => {}
                        }
                    }
                    return;
                }
                Mode::Normal => {
                    if state == InputState::Pressed {
                        match key {
                            platform::Key::Escape => {
                                self.toggle_mode(Mode::Command);
                            }
                            _ => {}
                        }
                    }
                }
                Mode::Help => {
                    if state == InputState::Pressed && key == platform::Key::Escape {
                        self.switch_mode(Mode::Normal);
                        return;
                    }
                }
            }

            if let Some(kb) = self
                .key_bindings
                .find(Input::Key(key), modifiers, state, self.mode)
            {
                // For toggle-like key bindings, we don't want to run the command
                // on key repeats. For regular key bindings, we run the command
                // depending on if it's supposed to repeat.
                if (repeat && kb.command.repeats() && !kb.is_toggle) || !repeat {
                    self.command(kb.command);
                }
                return;
            }

            if let Execution::Recording { events, .. } = exec {
                if key == platform::Key::End {
                    events.pop(); // Discard this key event.
                    self.message("Saving recording...", MessageType::Execution);
                    self.queue.push(InternalCommand::StopRecording);
                }
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    /// Sourcing
    ///////////////////////////////////////////////////////////////////////////

    /// Source a pim script at the given path. Returns an error if the path
    /// does not exist or the script couldn't be sourced.
    fn source_path<P: AsRef<Path>>(&mut self, path: P) -> io::Result<()> {
        let path = path.as_ref();
        debug!("source: {}", path.display());

        // TODO: if `path` is a directory, `source_dir`.
        File::open(path)
            .or_else(|_| File::open(self.proj_dirs.config_dir().join(path)))
            .and_then(|f| self.source_reader(io::BufReader::new(f), path))
            .map_err(|e| {
                io::Error::new(
                    e.kind(),
                    format!("error sourcing {}: {}", path.display(), e),
                )
            })
    }

    /// Source a directory which contains a `.pimrc` script. Returns an
    /// error if the script wasn't found or couldn't be sourced.
    fn source_dir<P: AsRef<Path>>(&mut self, dir: P) -> io::Result<()> {
        self.source_path(dir.as_ref().join(".pimrc"))
    }

    /// Source a script from an [`io::BufRead`].
    fn source_reader<P: AsRef<Path>, R: io::BufRead>(&mut self, r: R, _path: P) -> io::Result<()> {
        for (i, line) in r.lines().enumerate() {
            let line = line?;

            if line.starts_with(cmd::COMMENT) {
                continue;
            }
            match self.cmdline.parse(&format!(":{}", line)) {
                Err(e) => {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        format!("{} on line {}", e, i + 1),
                    ))
                }
                Ok(cmd) => {
                    let result = self.command(cmd);
                    if result != "" {
                        self.message(
                            format!("Unexpected result in source: {}", result),
                            MessageType::Warning,
                        );
                    }
                }
            }
        }
        Ok(())
    }

    ///////////////////////////////////////////////////////////////////////////
    /// Centering
    ///////////////////////////////////////////////////////////////////////////

    /// Center the palette in the workspace.
    fn center_palette(&mut self) {
        let h = self.palette.height;
        let n = usize::min(self.palette.size(), h) as f32;
        let p = &mut self.palette;

        p.x = 0.;
        p.y = self.height / 2. - n * p.cellsize / 2.;
    }

    /// Vertically the active view in the workspace.
    fn center_active_view_v(&mut self) {
        if let Some(v) = self.views.active() {
            let zoom = v.zoom as f32;
            self.offset.y =
                // TODO: This should center based on the total view height, not the frame height.
                (self.height / 2. - v.fh as f32 / 2. * zoom - v.offset.y).floor();
            self.cursor_dirty();
        }
    }

    /// Horizontally center the active view in the workspace.
    fn center_active_view_h(&mut self) {
        if let Some(v) = self.views.active() {
            let zoom = v.zoom as f32;
            self.offset.x = (self.width / 2. - v.width() as f32 * zoom / 2. - v.offset.x).floor();
            self.cursor_dirty();
        }
    }

    /// Center the active view in the workspace.
    fn center_active_view(&mut self) {
        self.center_active_view_v();
        self.center_active_view_h();
    }

    /// Center the given frame of the active view in the workspace.
    fn center_active_view_frame(&mut self, frame: usize) {
        self.center_active_view_v();

        if let Some(v) = self.views.active() {
            let zoom = v.zoom as f32;
            let offset = (frame as u32 * v.fw) as f32 * zoom;

            self.offset.x = self.width / 2. - offset - v.offset.x - v.fw as f32 / 2. * zoom;
            self.offset.x = self.offset.x.floor();

            self.cursor_dirty();
        }
    }

    /// The session center.
    fn center(&self) -> SessionCoords {
        SessionCoords::new(self.width / 2., self.height / 2.)
    }

    /// Center the selection to the given session coordinates.
    fn center_selection(&mut self, p: SessionCoords) {
        let c = self.active_view_coords(p);
        if let Some(ref mut s) = self.selection {
            let r = s.abs().bounds();
            let (w, h) = (r.width(), r.height());
            let (x, y) = (c.x as i32 - w / 2, c.y as i32 - h / 2);
            *s = Selection::new(x, y, x + w, y + h);
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    /// Zoom functions
    ///////////////////////////////////////////////////////////////////////////
    /// Returns the center for a zoom operation.
    pub fn get_zoom_center(&self) -> SessionCoords {
        if let Some(s) = self.selection {
            let v = self.active_view();
            let coords = s.bounds().center().map(|n| n as f32);
            self.session_coords(v.id, coords.into())
        } else if self.hover_view.is_some() {
            self.cursor
        } else {
            self.session_coords(self.views.active_id, self.active_view().center())
        }
    }

    /// Gets Some index of the level bigger than or equal to the passed-in zoom,
    /// or None if no higher level.
    fn get_zoom_index(&self, zoom: u32) -> usize {
        let lvls = Self::ZOOM_LEVELS;
        for (i, z) in lvls.iter().enumerate() {
            if zoom <= *z {
                return i;
            }
        }
        Self::ZOOM_LEVELS.len()
    }

    /// Zoom the active view in.
    fn zoom_in(&mut self, center: SessionCoords) {
        let zoom = self.active_view().zoom;
        let index = self.get_zoom_index(zoom);
        if let Some(z) = Self::ZOOM_LEVELS.get(index + 1) {
            self.zoom(*z, center);
        } else {
            self.message("Maximum zoom level reached", MessageType::Hint);
        }
    }

    /// Zoom the active view out.
    fn zoom_out(&mut self, center: SessionCoords) {
        let zoom = self.active_view().zoom;
        let index = self.get_zoom_index(zoom);
        if index == 0 {
            self.message("Minimum zoom level reached", MessageType::Hint);
        } else if let Some(z) = Self::ZOOM_LEVELS.get(index - 1) {
            self.zoom(*z, center);
        } else {
            unreachable!();
        }
    }

    /// Set the active view zoom. Takes a center to zoom to.
    fn zoom(&mut self, z: u32, center: SessionCoords) {
        let px = center.x - self.offset.x;
        let py = center.y - self.offset.y;

        let zprev = self.active_view().zoom;
        let zdiff = z as f32 / zprev as f32;

        let nx = (px * zdiff).floor();
        let ny = (py * zdiff).floor();

        let mut offset = Vector2::new(center.x - nx, center.y - ny);

        let v = self.active_view_mut();

        let vx = v.offset.x;
        let vy = v.offset.y;

        v.zoom = z;

        let dx = v.offset.x - (vx * zdiff);
        let dy = v.offset.y - (vy * zdiff);

        offset.x -= dx;
        offset.y -= dy;

        self.offset = offset.map(f32::floor);
        self.organize_views();
    }

    ///////////////////////////////////////////////////////////////////////////
    /// Commands
    ///////////////////////////////////////////////////////////////////////////

    /// Process a command.
    fn command(&mut self, cmd: Cmd) -> String {
        debug!("command: {:?}", cmd);

        let mut result: String = "".to_string();
        match cmd {
            Cmd::Mode(m) => {
                self.toggle_mode(m);
            }
            Cmd::Quit => {
                self.quit_view_safe(self.views.active_id);
            }
            Cmd::QuitAll => {
                // TODO (rust)
                let ids: Vec<ViewId> = self.views.ids().collect();
                for id in ids {
                    self.quit_view_safe(id);
                }
            }
            Cmd::SwapColors => {
                std::mem::swap(&mut self.fg, &mut self.bg);
            }
            Cmd::BrushSet(mode) => {
                self.brush.set(mode, 1);
            }
            Cmd::BrushUnset(mode) => {
                self.brush.set(mode, 0);
            }
            // NOTE: not implemented in script.rs;
            // if you want to toggle, do `brush/abc (not brush/abc)`
            Cmd::BrushToggle(mode) => {
                self.brush.toggle(mode);
            }
            Cmd::Brush => {
                self.unimplemented();
            }
            Cmd::BrushSize(op) => {
                let b = &mut self.brush;
                match op {
                    Op::Incr => {
                        b.size += 1;
                        b.size += b.size % 2;
                    }
                    Op::Decr => {
                        b.size -= 1;
                        b.size -= b.size % 2;
                    }
                    Op::Set(s) => {
                        b.size = s as usize;
                    }
                }
                if b.size < Self::MIN_BRUSH_SIZE {
                    b.size = Self::MIN_BRUSH_SIZE;
                }
            }
            Cmd::FrameResize(fw, fh) => match self.resize_frames(fw as i64, fh as i64) {
                Err(error) => self.message(format!("Error: {}", error), MessageType::Error),
                _ => {}
            },
            Cmd::FrameCurrent => {
                result = self.current_frame().to_string();
            }
            Cmd::FramePrev => {
                let frame = self.current_frame();
                let max_frames = self.active_view().animation.len();
                // Use wrap around (-1 == max_frames - 1) mod max_frames:
                let frame = (frame + max_frames - 1) % max_frames;
                result = frame.to_string();
                self.center_active_view_frame(frame);
            }
            Cmd::FrameNext => {
                let frame = self.current_frame();
                let max_frames = self.active_view().animation.len();
                // Use wrap around:
                let frame = (frame + 1) % max_frames;
                result = frame.to_string();
                self.center_active_view_frame(frame);
            }
            Cmd::ForceQuit => self.quit_view(self.views.active_id),
            Cmd::ForceQuitAll => self.quit(ExitReason::Normal),
            Cmd::Echo(ref v) => {
                let result = match v {
                    Value::U32(u) => {
                        let i = *u as usize;
                        if i >= self.palette.size() {
                            Err(format!("invalid palette index: {}", i))
                        } else {
                            let lyza = Lyza::from(self.palette.colors[i]);
                            Ok(Value::Str(format!("{:?}", lyza)))
                        }
                    }
                    Value::U32Tuple(u1, u2) => {
                        let i1 = *u1 as usize;
                        let i2 = *u2 as usize;
                        if i1 >= self.palette.size() || i2 >= self.palette.size() {
                            Err(format!("one or two invalid palette indices: {} {}", i1, i2))
                        } else {
                            let lyza1 = Lyza::from(self.palette.colors[i1]);
                            let lyza2 = Lyza::from(self.palette.colors[i2]);
                            Ok(Value::Str(format!(
                                "{:?} {:?} {:?}",
                                lyza1,
                                lyza1.compare(&lyza2),
                                lyza2
                            )))
                        }
                    }
                    Value::Str(s) => Ok(Value::Str(s.clone())),
                    Value::Ident(s) => match s.as_str() {
                        "config/dir" => Ok(Value::Str(format!(
                            "{}",
                            self.proj_dirs.config_dir().display()
                        ))),
                        "s/cwd" | "cwd" => Ok(Value::Str(self.cwd.display().to_string())),
                        "s/offset" => Ok(Value::F32Tuple(self.offset.x, self.offset.y)),
                        "v/offset" => {
                            let v = self.active_view();
                            Ok(Value::F32Tuple(v.offset.x, v.offset.y))
                        }
                        "v/zoom" => Ok(Value::U32(self.active_view().zoom as u32)),
                        _ => match self.settings.get(s) {
                            None => Err(format!("Error: {} is undefined", s)),
                            Some(result) => Ok(Value::Str(format!("{} = {}", v.clone(), result))),
                        },
                    },
                    _ => Err(format!("Error: argument cannot be echoed")),
                };
                match result {
                    Ok(v) => self.message(v, MessageType::Echo),
                    Err(e) => self.message(e, MessageType::Error),
                }
            }
            Cmd::PaletteAdd(rgba) => {
                self.palette.add(rgba);
                self.center_palette();
            }
            Cmd::PaletteClear => {
                self.palette.clear();
            }
            Cmd::PaletteGradient(colorstart, colorend, steps) => {
                self.palette.gradient(colorstart, colorend, steps);
                self.center_palette();
            }
            Cmd::PaletteSort => {
                self.palette.sort();
            }
            Cmd::PaletteSample => {
                self.add_view_colors();
                self.command(Cmd::PaletteSort);
            }
            Cmd::PaletteWrite(path) => match self.palette.write(path) {
                Ok(text) => self.message(text, MessageType::Info),
                Err(err) => self.message(err, MessageType::Error),
            },
            Cmd::Zoom(op) => {
                let center = self.get_zoom_center();

                match op {
                    Op::Incr => {
                        self.zoom_in(center);
                    }
                    Op::Decr => {
                        self.zoom_out(center);
                    }
                    Op::Set(zoom) => {
                        let z = zoom as u32;
                        if z <= 0 || z > Self::MAX_ZOOM {
                            self.message("Error: invalid zoom level", MessageType::Error);
                        } else {
                            self.zoom(z, center);
                        }
                    }
                }
            }
            Cmd::Reset => {
                if let Err(e) = self.reset() {
                    self.message(format!("Error: {}", e), MessageType::Error);
                } else {
                    self.message("Settings reset to default values", MessageType::Okay);
                }
            }
            // These Fill commands are purposely left out of script.rs.
            // They clear the entire view (all frames).
            Cmd::Fill(None) => {
                let bg = self.bg;
                self.active_view_mut().clear(bg);
            }
            Cmd::Fill(Some(color)) => {
                self.active_view_mut().clear(color);
            }
            Cmd::Pan(x, y) => {
                self.pan(
                    -(x * Self::PAN_PIXELS as i32) as f32,
                    -(y * Self::PAN_PIXELS as i32) as f32,
                );
            }
            Cmd::ViewNext => {
                if let Some(id) = self.views.after(self.views.active_id) {
                    self.edit_view(id);
                }
            }
            Cmd::ViewPrev => {
                if let Some(id) = self.views.before(self.views.active_id) {
                    self.edit_view(id);
                }
            }
            // This is intentionally ignored in script.rs
            Cmd::ViewCenter => {
                self.center_active_view();
            }
            Cmd::FrameAdd => self.add_frame(None).expect("no err"),
            Cmd::FrameClone(index) => self.clone_frame(Some(index as i64)).expect("no err"),
            Cmd::FrameRemove => self.remove_frame(None).expect("no err"),
            Cmd::Slice(None) => {
                let v = self.active_view_mut();
                v.slice(1);
            }
            Cmd::Slice(Some(nframes)) => {
                let v = self.active_view_mut();
                if !v.slice(nframes) {
                    self.message(
                        format!("Error: slice: view width is not divisible by {}", nframes),
                        MessageType::Error,
                    );
                }
            }
            Cmd::Set(ref k, ref v) => {
                if Settings::DEPRECATED.contains(&k.as_str()) {
                    self.message(
                        format!("Warning: the setting `{}` has been deprecated", k),
                        MessageType::Warning,
                    );
                    return result;
                }
                match self.settings.set(k, v.clone()) {
                    Err(e) => {
                        self.message(format!("Error: {}", e), MessageType::Error);
                    }
                    Ok(old) => {
                        if old != *v {
                            self.setting_changed(k, old, v.clone());
                        }
                    }
                }
            }
            Cmd::Toggle(ref k) => match self.settings.get(k) {
                Some(Value::Bool(b)) => {
                    self.command(Cmd::Set(k.clone(), Value::Bool(!b)));
                }
                Some(_) => {
                    self.message(format!("Error: can't toggle `{}`", k), MessageType::Error);
                }
                None => {
                    self.message(
                        format!("Error: no such setting `{}`", k),
                        MessageType::Error,
                    );
                }
            },
            Cmd::Noop => {
                // Nothing happening!
            }
            Cmd::ChangeDir(dir) => {
                let home = self.base_dirs.home_dir().to_path_buf();
                let path = dir.map(|s| s.into()).unwrap_or(home);

                match std::env::set_current_dir(&path) {
                    Ok(()) => {
                        self.cwd = path.clone();
                        self.cmdline.set_cwd(path.as_path());
                    }
                    Err(e) => self.message(format!("Error: {}: {:?}", e, path), MessageType::Error),
                }
            }
            Cmd::Source(Some(ref path)) => {
                if let Err(ref e) = self.source_path(path) {
                    self.message(
                        format!("Error sourcing `{}`: {}", path, e),
                        MessageType::Error,
                    );
                }
            }
            Cmd::Source(None) => {
                self.message(
                    format!("Error: source command requires a path"),
                    MessageType::Error,
                );
            }
            Cmd::Edit(ref paths) => {
                self.edit_images(paths);
            }
            Cmd::EditFrames(ref paths) => {
                self.concatenate_images(paths);
            }
            Cmd::Export(scale, path) => {
                let view = self.active_view();
                let id = view.id;
                let scale = scale.unwrap_or(view.zoom as u32);

                if let Err(e) = self.export_as(id, Path::new(&path), scale) {
                    self.message(format!("Error: {}", e), MessageType::Error);
                }
            }
            Cmd::Write(None) => match self.save_view(self.views.active_id) {
                Ok((storage, written)) => self.message(
                    format!("\"{}\" {} pixels written", storage, written),
                    MessageType::Info,
                ),
                Err(err) => self.message(format!("Error: {}", err), MessageType::Error),
            },
            Cmd::Write(Some(ref path)) => {
                match self.active_view_mut().save_as(&Path::new(path).into()) {
                    Ok(written) => self.message(
                        format!("\"{}\" {} pixels written", path, written),
                        MessageType::Info,
                    ),
                    Err(err) => self.message(format!("Error: {}", err), MessageType::Error),
                }
            }
            Cmd::WriteFrames(None) => {
                self.command(Cmd::WriteFrames(Some(".".to_owned())));
            }
            Cmd::WriteFrames(Some(ref dir)) => {
                // We won't port this to script.rs for now; i don't see a good use case for editing purposes.
                // one can always use ImageMagick to split into frames.
                let path = Path::new(dir);

                std::fs::create_dir_all(path).ok();

                let paths: Vec<_> = (0..self.active_view().animation.len())
                    .map(|i| path.join(format!("{:03}.png", i)))
                    .collect();
                let paths = NonEmpty::from_slice(paths.as_slice())
                    .expect("views always have at least one frame");

                let view = self.active_view_mut();
                let fs = FileStorage::Range(paths);

                match view.save_as(&fs) {
                    Ok(written) => self.message(
                        format!("{} {} pixels written", fs, written),
                        MessageType::Info,
                    ),
                    Err(e) => self.message(format!("Error: {}", e), MessageType::Error),
                }
            }
            Cmd::Map(map) => {
                let KeyMapping {
                    modifiers,
                    input,
                    press,
                    release,
                    modes,
                } = *map;

                self.key_bindings.add(KeyBinding {
                    input,
                    modes: modes.clone(),
                    command: press,
                    state: InputState::Pressed,
                    modifiers,
                    is_toggle: release.is_some(),
                    display: Some(format!("{}", input)),
                });
                if let Some(cmd) = release {
                    self.key_bindings.add(KeyBinding {
                        input,
                        modes,
                        command: cmd,
                        state: InputState::Released,
                        modifiers,
                        is_toggle: true,
                        display: None,
                    });
                }
            }
            // Not represented in script.rs; just restart.
            Cmd::MapClear => {
                self.key_bindings = KeyBindings::default();
            }
            Cmd::Undo => {
                self.undo(self.views.active_id);
            }
            Cmd::Redo => {
                self.redo(self.views.active_id);
            }
            Cmd::Tool(t) => {
                self.tool(t);
            }
            Cmd::ToolPrev => {
                self.prev_tool();
            }
            Cmd::Crop(_) => {
                self.unimplemented();
            }
            Cmd::SelectionMove(x, y) => {
                if let Some(ref mut s) = self.selection {
                    s.translate(x, y);
                }
            }
            Cmd::SelectionResize(x, y) => {
                if let Some(ref mut s) = self.selection {
                    s.resize(x, y);
                }
            }
            Cmd::SelectionExpand => self.expand_selection(),
            Cmd::SelectionOffset(mut x, mut y) => {
                if let Some(s) = &mut self.selection {
                    let r = s.abs().bounds();
                    if r.width() <= 2 && x < 0 {
                        x = 0;
                    }
                    if r.height() <= 2 && y < 0 {
                        y = 0;
                    }
                    *s = Selection::from(s.bounds().expand(x, y, x, y));
                } else if let Some(id) = self.hover_view {
                    if id == self.views.active_id {
                        let p = self.active_view_coords(self.cursor).map(|n| n as i32);
                        self.selection = Some(Selection::new(p.x, p.y, p.x + 1, p.y + 1));
                    }
                }
            }
            // Not ported to script.rs; just use `s-move fw` or `s-move -fw`
            Cmd::SelectionJump(dir) => {
                // TODO: Test this across layers.
                let v = self.active_view();
                let r = v.bounds();
                let fw = v.extent().fw as i32;
                if let Some(s) = &mut self.selection {
                    let mut t = *s;
                    t.translate(fw * i32::from(dir), 0);

                    if r.intersects(t.abs().bounds()) {
                        *s = t;
                    }
                }
            }
            Cmd::SelectionPaste => {
                if let (Mode::Visual(VisualState::Pasting), Some(s)) = (self.mode, self.selection) {
                    self.active_view_mut().paste(s.abs().bounds());
                } else {
                    // TODO: Enter paste mode?
                }
            }
            // TODO: Continue here!
            Cmd::SelectionYank => {
                self.yank_selection();
            }
            Cmd::SelectionFlip(dir) => {
                if let (Mode::Visual(VisualState::Selecting { .. }), Some(s)) =
                    (self.mode, self.selection)
                {
                    let v = self.active_view_mut();
                    let s = s.abs().bounds();

                    if s.intersects(v.layer_bounds()) {
                        let s = s.intersection(v.layer_bounds());

                        // The flip operation works by copying the flipped image into
                        // the paste buffer, and pasting.
                        v.flip(s, dir);
                        v.paste(s);

                        self.selection = Some(Selection::from(s));
                        self.switch_mode(Mode::Visual(VisualState::Pasting));
                    }
                    // Note that the effects generated here will be processed *before* the
                    // view operations.
                    self.command(Cmd::SelectionErase);
                    self.command(Cmd::Mode(Mode::Visual(VisualState::Selecting {
                        dragging: false,
                    })));
                }
            }
            Cmd::SelectionCut => {
                // To mimick the behavior of `vi`, we yank the selection
                // before deleting it.
                if self.yank_selection().is_some() {
                    self.command(Cmd::SelectionErase);
                }
            }
            Cmd::SelectionFill(color) => {
                if let Some(s) = self.selection {
                    self.effects
                        .push(Effect::ViewPaintFinal(vec![Shape::Rectangle(
                            s.abs().bounds().map(|n| n as f32),
                            ZDepth::default(),
                            Rotation::ZERO,
                            Stroke::NONE,
                            Fill::Solid(color.unwrap_or(self.fg).into()),
                        )]));
                    self.active_view_mut().touch();
                }
            }
            Cmd::SelectionErase => {
                if let Some(s) = self.selection {
                    self.effects.extend_from_slice(&[
                        Effect::ViewBlendingChanged(Blending::Constant),
                        Effect::ViewPaintFinal(vec![Shape::Rectangle(
                            s.abs().bounds().map(|n| n as f32),
                            ZDepth::default(),
                            Rotation::ZERO,
                            Stroke::NONE,
                            Fill::Solid(Rgba8::TRANSPARENT.into()),
                        )]),
                    ]);
                    self.active_view_mut().touch();
                }
            }
            // DONE - script.rs already has these.
            Cmd::PaintLine(rgba, x1, y1, x2, y2) => {
                let mut stroke = vec![];
                Brush::line(Point2::new(x1, y1), Point2::new(x2, y2), &mut stroke);
                for pt in stroke {
                    self.active_view_mut().paint_color(rgba, pt.x, pt.y);
                }
            }
            Cmd::PaintColor(rgba, x, y) => {
                self.active_view_mut().paint_color(rgba, x, y);
            }
            Cmd::PaintForeground(x, y) => {
                let fg = self.fg;
                self.active_view_mut().paint_color(fg, x, y);
            }
            Cmd::PaintBackground(x, y) => {
                let bg = self.bg;
                self.active_view_mut().paint_color(bg, x, y);
            }
            Cmd::PaintPalette(i, x, y) => {
                let c = self.palette.colors.to_vec();
                let v = self.active_view_mut();

                if let Some(color) = c.get(i) {
                    v.paint_color(*color, x, y);
                }
            }
            Cmd::WriteQuit => match self.save_view(self.views.active_id) {
                Ok(_) => self.quit_view(self.views.active_id),
                Err(e) => self.message(e, MessageType::Error),
            },
        };
        result
    }

    pub fn current_frame(&self) -> usize {
        let view = self.active_view();
        let extent = view.extent();
        let center = self.active_view_coords(self.center());

        if center.x >= 0. {
            extent.to_frame(center.into()).min(view.animation.len() - 1)
        } else {
            0
        }
    }

    fn cmdline_hide(&mut self) {
        self.switch_mode(self.prev_mode.unwrap_or(Mode::Normal));
    }

    fn cmdline_handle_backspace(&mut self) {
        self.cmdline.delc();

        if self.cmdline.is_empty() {
            self.cmdline_hide();
        }
    }

    fn cmdline_handle_enter(&mut self) {
        let input = self.cmdline.input();
        // Always hide the command line before executing the command,
        // because commands will often require being in a specific mode, e.g.
        // visual mode for commands that run on selections.
        self.cmdline_hide();

        if input.is_empty() {
            return;
        }
        // Always add the command, even if it's bad; maybe the user forgot
        // a letter/syntax but they can go back and edit it rather than
        // lose it completely.
        self.cmdline.history.add(&input);

        match self.cmdline.parse(&input) {
            Err(e) => self.message(format!("Error: {}", e), MessageType::Error),
            Ok(cmd) => {
                let result = self.command(cmd);
                if result != "" {
                    self.message(result, MessageType::Info);
                }
            }
        }
    }

    fn cmdline_handle_input(&mut self, c: char) {
        self.cmdline.putc(c);
        self.message_clear();
    }

    fn tool(&mut self, t: Tool) {
        if t == self.tool {
            return;
        }
        self.prev_tool = Some(self.tool);
        self.tool = t;
    }

    fn prev_tool(&mut self) {
        self.tool = self.prev_tool.unwrap_or_default();
    }

    ///////////////////////////////////////////////////////////////////////////
    /// Color functions
    ///////////////////////////////////////////////////////////////////////////

    fn fg_or_bg(&self, button: platform::MouseButton) -> Rgba8 {
        if button == platform::MouseButton::Left {
            self.fg
        } else {
            self.bg
        }
    }

    /// Picks the given color as foreground or background color, depending on mouse button.
    fn pick_color(&mut self, color: Rgba8, button: platform::MouseButton) {
        if color.a == 0x0 {
            return;
        }
        if button == platform::MouseButton::Left {
            self.fg = color;
        } else {
            self.bg = color;
        }
    }

    fn sample_color(&mut self, button: platform::MouseButton) {
        if let Some(color) = self.hover_color {
            self.pick_color(color, button);
        }
    }

    fn begin_script_command(&mut self, _command: &Command) {}
    fn end_script_command(&mut self, _command: &Command) {}

    fn script_evaluate(
        &mut self,
        script_stack: &Vec<&Script>,
        evaluate_this: Evaluate,
    ) -> ArgumentResult {
        script::evaluate(self, script_stack, evaluate_this)
    }

    pub fn script_paint(&mut self, x: i64, y: i64, color: Rgba8) -> ArgumentResult {
        self.active_view_mut()
            .paint_color(color, x as i32, y as i32);

        // TODO: there's probably something better to return here, e.g., the pixel
        // color that was under the cursor.
        Ok(Argument::Color(color))
    }

    pub fn script_bucket(&mut self, x: i64, y: i64, color: Rgba8) -> ArgumentResult {
        let start_time = time::Instant::now();
        let maybe_filler = FloodFiller::new(
            self.active_view(),
            ViewCoords::new(x as f32, y as f32),
            color,
        );
        match maybe_filler {
            None => Err(format!(
                "invalid starting point for bucket fill: ({}, {})",
                x, y
            )),
            Some(filler) => {
                let target_color = filler.target_color();
                if let Some(shapes) = filler.run() {
                    self.effects.push(Effect::ViewPaintFinal(shapes));
                    self.active_view_mut().touch();
                }
                debug!("flood fill in: {:?}", start_time.elapsed());
                Ok(Argument::Color(target_color))
            }
        }
    }

    pub fn script_line(&mut self, x0: i64, y0: i64, x1: i64, y1: i64, color: Rgba8) -> VoidResult {
        let mut stroke = vec![];
        Brush::line(
            Point2::new(x0 as i32, y0 as i32),
            Point2::new(x1 as i32, y1 as i32),
            &mut stroke,
        );
        for point in stroke {
            self.active_view_mut().paint_color(color, point.x, point.y);
        }
        Ok(())
    }

    pub fn add_view_colors(&mut self) {
        let v = self.active_view();
        let (_, pixels) = self
            .views
            .get(v.id)
            .expect(&format!("view #{} must exist", v.id))
            .layer
            .current_snapshot();

        for pixel in pixels.iter().cloned() {
            if pixel != Rgba8::TRANSPARENT {
                self.palette.add(pixel);
            }
        }
        self.center_palette();
    }

    pub fn get_color_setting(&self, setting: ColorSetting) -> Rgba8 {
        match setting {
            ColorSetting::UiBackground => self.settings["background"].to_rgba8(),
            ColorSetting::UiGrid => self.settings["grid/color"].to_rgba8(),
            ColorSetting::Foreground => self.fg,
            ColorSetting::Background => self.bg,
        }
    }

    pub fn set_color_setting(&mut self, setting: ColorSetting, color: Rgba8) -> VoidResult {
        match setting {
            ColorSetting::UiBackground => {
                self.settings.set("background", Value::Rgba8(color))?;
            }
            ColorSetting::UiGrid => {
                self.settings.set("grid/color", Value::Rgba8(color))?;
            }
            ColorSetting::Foreground => {
                self.fg = color;
            }
            ColorSetting::Background => {
                self.bg = color;
            }
        }
        Ok(())
    }

    pub fn get_string_setting(&self, setting: StringSetting) -> String {
        match setting {
            StringSetting::Mode => self.mode.to_string(),
            StringSetting::Cwd => {
                std::env::current_dir().map_or("".to_string(), |cwd| cwd.display().to_string())
            }
            StringSetting::ConfigDirectory => self.proj_dirs.config_dir().display().to_string(),
        }
    }

    pub fn set_string_setting(&mut self, setting: StringSetting, value: String) -> VoidResult {
        match setting {
            StringSetting::Mode => {
                let mode =
                    Mode::from_str(&value).map_err(|_| format!("invalid mode: `{}`", value))?;
                self.switch_mode(mode);
            }
            StringSetting::Cwd => {
                let path = Path::new(&value).to_path_buf();
                if std::env::set_current_dir(&path).is_err() {
                    return Err(format!("could not change directory to `{}`", value));
                }
                self.cwd = path.clone();
                self.cmdline.set_cwd(path.as_path());
            }
            StringSetting::ConfigDirectory => {
                // TODO: implement something here, maybe read a config file from new directory
                return Err("cannot set config directory yet".to_string());
            }
        }
        Ok(())
    }

    pub fn get_i64_setting(&self, setting: I64Setting) -> i64 {
        match setting {
            I64Setting::Debug => self.settings["debug"].is_set() as i64,
            I64Setting::UiAnimate => self.settings["animation"].is_set() as i64,
            I64Setting::UiChecker => self.settings["checker"].to_u64() as i64,
            I64Setting::UiGrid => self.settings["grid"].to_u64() as i64,
            I64Setting::UiScalePercentage => self.settings["scale%"].to_u64() as i64,
            I64Setting::UiOffsetX => self.offset.x as i64,
            I64Setting::UiOffsetY => self.offset.y as i64,
            I64Setting::Tool => self.tool as i64,
            I64Setting::PaletteHeight => self.palette.height as i64,
            I64Setting::UiZoom => self.active_view().zoom as i64,
            I64Setting::ViewIndex => self.views.active_id.0 as i64,
            I64Setting::CursorXRay => self.brush.is_set(brush::BrushMode::XRay) as i64,
            I64Setting::BrushSize => self.brush.size as i64,
            I64Setting::BrushErase => self.brush.is_set(brush::BrushMode::Erase) as i64,
            I64Setting::BrushMultiFrame => self.brush.is_set(brush::BrushMode::Multi) as i64,
            I64Setting::BrushPixelPerfect => self.brush.is_set(brush::BrushMode::Perfect) as i64,
            I64Setting::BrushXSymmetry => self.brush.is_set(brush::BrushMode::XSym) as i64,
            I64Setting::BrushYSymmetry => self.brush.is_set(brush::BrushMode::YSym) as i64,
            I64Setting::BrushLineAngle => {
                if let Some(brush::BrushMode::Line(current_line)) = self.brush.line_mode() {
                    current_line.unwrap_or(0) as i64
                } else {
                    0
                }
            }
            I64Setting::AnimationDelay => self.animation_delay().as_millis() as i64,
            I64Setting::FrameIndex => self.current_frame() as i64,
            I64Setting::FrameWidth => self.active_view().fw as i64,
            I64Setting::FrameHeight => self.active_view().fh as i64,
            I64Setting::ImageSplit => self.active_view().animation.len() as i64,
            I64Setting::History => self.active_view().current_edit() as i64,
        }
    }

    pub fn set_i64_setting(
        &mut self,
        setting: I64Setting,
        old_value: i64,
        new_value: i64,
    ) -> VoidResult {
        match setting {
            I64Setting::Debug => {
                self.settings.set("debug", Value::Bool(new_value != 0))?;
            }
            I64Setting::UiAnimate => {
                self.settings
                    .set("animation", Value::Bool(new_value != 0))?;
            }
            I64Setting::UiChecker => {
                if new_value >= 0 {
                    self.settings.set("checker", Value::U32(new_value as u32))?;
                } else {
                    return Err(format!("invalid value for checkerboard: {}", new_value));
                }
            }
            I64Setting::UiGrid => {
                if new_value >= 0 {
                    self.settings.set("grid", Value::U32(new_value as u32))?;
                } else {
                    return Err(format!("invalid value for grid: {}", new_value));
                }
            }
            I64Setting::UiScalePercentage => {
                if new_value < 100 || new_value > 400 {
                    return Err(format!(
                        "ui scale % should be between 100 and 400, got {}",
                        new_value
                    ));
                }
                self.settings.set("scale%", Value::U32(new_value as u32))?;
                // TODO: We need to recompute the cursor position here
                // from the window coordinates. Currently, cursor position
                // is stored only in `SessionCoords`, which would have
                // to change.
                // TODO: would be nice not to need to pass in old_value here
                self.rescale((old_value as f64) / 100.0, (new_value as f64) / 100.0);
            }
            I64Setting::UiOffsetX => {
                self.offset.x = new_value as f32;
            }
            I64Setting::UiOffsetY => {
                self.offset.y = new_value as f32;
            }
            I64Setting::Tool => {
                self.tool = Tool::from_i64(new_value);
            }
            I64Setting::PaletteHeight => {
                if new_value > 0 {
                    self.palette.height = new_value as usize;
                    self.center_palette();
                } else {
                    return Err(format!("invalid palette height: {}", new_value));
                }
            }
            I64Setting::UiZoom => {
                let valid_zoom = if new_value <= 0 {
                    self.message("Minimum zoom level reached", MessageType::Hint);
                    1
                } else if new_value > Self::MAX_ZOOM as i64 {
                    self.message("Maximum zoom level reached", MessageType::Hint);
                    Self::MAX_ZOOM
                } else {
                    new_value as u32
                };
                self.zoom(valid_zoom, self.get_zoom_center());
            }
            I64Setting::ViewIndex => {
                assert!(old_value != new_value);
                let maybe_id = if new_value < old_value {
                    // We want to solve for this type of situation:
                    // if we are at view-index 5, and we want to decrement to
                    // view-index 4 but that is missing;
                    // in that case, find the one before 4.
                    self.views
                        .get(ViewId(new_value as i32))
                        .map(|v| v.id)
                        .or_else(|| self.views.before(ViewId(new_value as i32)))
                } else {
                    // We want to solve for this type of situation:
                    // if we are at view-index 5, and we want to increment to
                    // view-index 6 but that is missing;
                    // in that case, find the one after 6.
                    self.views
                        .get(ViewId(new_value as i32))
                        .map(|v| v.id)
                        .or_else(|| self.views.after(ViewId(new_value as i32)))
                };
                if let Some(id) = maybe_id {
                    self.edit_view(id);
                } else {
                    self.message(format!("no such view: {}", new_value), MessageType::Error);
                }
            }
            I64Setting::CursorXRay => self.brush.set(brush::BrushMode::XRay, new_value),
            I64Setting::BrushSize => {
                if new_value < 1 || new_value > 1000 {
                    return Err(format!("brush size of {} is invalid", new_value));
                }
                self.brush.size = new_value as usize
            }
            I64Setting::BrushErase => self.brush.set(brush::BrushMode::Erase, new_value),
            I64Setting::BrushMultiFrame => self.brush.set(brush::BrushMode::Multi, new_value),
            I64Setting::BrushPixelPerfect => self.brush.set(brush::BrushMode::Perfect, new_value),
            I64Setting::BrushXSymmetry => self.brush.set(brush::BrushMode::XSym, new_value),
            I64Setting::BrushYSymmetry => self.brush.set(brush::BrushMode::YSym, new_value),
            I64Setting::BrushLineAngle => {
                let snap = new_value as u32;
                if snap == 0 {
                    // TODO: clean up brush line mode, any line will do to remove:
                    self.brush.set(brush::BrushMode::Line(None), 0);
                } else if snap < 360 {
                    self.brush.set(brush::BrushMode::Line(Some(snap)), 0);
                } else {
                    return Err(format!(
                        "brush line snap angle should be between 0 and 360, got {}",
                        new_value
                    ));
                }
            }
            I64Setting::AnimationDelay => {
                if new_value <= 0 {
                    return Err(format!("invalid delay: {}", new_value));
                }
                if self.views.len() == 0 {
                    // TODO: we could have a `session.animation_delay` variable for this,
                    //       but i don't know if we actually run into this situation at all.
                    return Err("session hasn't been populated with a view yet".to_string());
                }
                self.active_view_mut().animation.delay =
                    time::Duration::from_millis(new_value as u64);
            }
            I64Setting::FrameIndex => {
                let max_frames = self.active_view().animation.len();
                let new_frame = new_value.rem_euclid(max_frames as i64);
                self.center_active_view_frame(new_frame as usize);
            }
            I64Setting::FrameWidth => {
                self.resize_frames(new_value, self.active_view().fh as i64)?;
            }
            I64Setting::FrameHeight => {
                self.resize_frames(self.active_view().fw as i64, new_value)?;
            }
            I64Setting::ImageSplit => {
                if !self.active_view_mut().slice(new_value as usize) {
                    return Err(format!(
                        "split: view width is not divisible by {}",
                        new_value
                    ));
                }
            }
            I64Setting::History => {
                self.active_view_mut().restore(new_value as EditId);
            }
        }
        Ok(())
    }

    fn add_frame(&mut self, index: Option<i64>) -> VoidResult {
        match self.frame_index(index, "add") {
            Ok(index) => self.active_view_mut().add_frame_after(index),
            // TODO: return the error here and make the script fire off the message.
            Err(e) => self.message(e, MessageType::Error),
        }
        Ok(())
    }

    fn clone_frame(&mut self, index: Option<i64>) -> VoidResult {
        match self.frame_index(index, "clone") {
            Ok(index) => self.active_view_mut().clone_frame(index),
            // TODO: return the error here and make the script fire off the message.
            Err(e) => self.message(e, MessageType::Error),
        }
        Ok(())
    }

    fn remove_frame(&mut self, index: Option<i64>) -> VoidResult {
        match self.frame_index(index, "remove") {
            Ok(index) => self.active_view_mut().remove_frame(index),
            // TODO: return the error here and make the script fire off the message.
            Err(e) => self.message(e, MessageType::Error),
        }
        Ok(())
    }

    fn frame_index(&self, index: Option<i64>, what_for: &str) -> Result<usize, String> {
        let view = self.active_view();
        let max_frames = view.animation.len();
        let index = match index {
            None => self.current_frame(),
            Some(index) => {
                if index < 0 {
                    // This will convert to a large number if we're still negative after adding
                    // index because we convert to usize.
                    (max_frames as i64 + index) as usize
                } else {
                    index as usize
                }
            }
        };
        if index < max_frames {
            Ok(index)
        } else {
            Err(format!(
                "Error: {} index must be in the range [{}, {}]",
                what_for,
                0,
                max_frames - 1
            ))
        }
    }

    fn resize_frames(&mut self, width: i64, height: i64) -> Result<(), String> {
        if width <= 0 || height <= 0 {
            return Err(format!(
                "cannot set frame dimensions to ({}, {})",
                width, height
            ));
        }
        if width > Self::MAX_FRAME_SIZE.into() || height > Self::MAX_FRAME_SIZE.into() {
            return Err(format!(
                "({}, {}) is larger than the maximum frame size ({}, {})",
                width,
                height,
                Self::MAX_FRAME_SIZE,
                Self::MAX_FRAME_SIZE,
            ));
        }

        let v = self.active_view_mut();
        v.resize_frames(width as u32, height as u32);

        self.check_selection();
        self.organize_views();
        Ok(())
    }

    pub fn script_zero(&mut self, for_what: ZeroArgumentsFor) -> ArgumentResult {
        match for_what {
            ZeroArgumentsFor::Reset => {
                if let Err(e) = self.reset() {
                    self.message(format!("Error: {}", e), MessageType::Error);
                    return Err(format!("{}", e));
                }
                self.message("Settings reset to default values", MessageType::Okay);
            }
            ZeroArgumentsFor::SelectionExpand => {
                self.expand_selection();
            }
            ZeroArgumentsFor::SelectionPaste => {
                self.paste_selection();
            }
        }
        Ok(Argument::Null)
    }

    fn expand_selection(&mut self) {
        let v = self.active_view();
        let (fw, fh) = (v.fw as i32, v.fh as i32);
        let (vw, vh) = (v.width() as i32, v.fh as i32);

        if let Some(ref mut selection) = self.selection {
            let r = Rect::origin(vw, vh);
            let s = selection.bounds();
            let min = s.min();
            let max = s.max();

            // If the selection is within the view rectangle, expand it,
            // otherwise do nothing.
            if r.contains(min) && r.contains(max.map(|n| n - 1)) {
                let x1 = if min.x % fw == 0 {
                    min.x - fw
                } else {
                    min.x - min.x % fw
                };
                let x2 = max.x + (fw - max.x % fw);
                let y2 = fh;

                *selection = Selection::from(Rect::new(x1, 0, x2, y2).intersection(r));
            }
        } else {
            self.selection = Some(Selection::new(0, 0, fw, fh));
        }
    }

    pub fn script_optional_i64(
        &mut self,
        for_what: OptionalI64For,
        optional_i64: Option<i64>,
    ) -> VoidResult {
        match for_what {
            OptionalI64For::FrameAdd => self.add_frame(optional_i64)?,
            OptionalI64For::FrameClone => self.clone_frame(optional_i64)?,
            OptionalI64For::FrameRemove => self.remove_frame(optional_i64)?,
            OptionalI64For::Undo => {
                // TODO: return the current history ID before the undo.
                // We'll need to return an `ArgumentResult` here instead of VoidResult.
                let amount = get_undo_or_redo_amount(optional_i64, "undo")?;
                for _i in 0..amount {
                    // TODO: this should probably return an Err if we try to undo too much
                    self.undo(self.views.active_id);
                }
            }
            OptionalI64For::Redo => {
                // TODO: return the current history ID before the redo.
                let amount = get_undo_or_redo_amount(optional_i64, "redo")?;
                for _i in 0..amount {
                    // TODO: this should probably return an Err if we try to redo too much
                    self.redo(self.views.active_id);
                }
            }
        }
        Ok(())
    }

    pub fn script_strings(&mut self, for_what: StringsFor, strings: Vec<String>) -> VoidResult {
        match for_what {
            StringsFor::Source => {
                for string in strings {
                    self.source_path(string).map_err(|e| e.to_string())?;
                }
            }
            StringsFor::Edit => {
                self.edit_images(&strings);
            }
            StringsFor::Concatenate => {
                self.concatenate_images(&strings);
            }
        }
        Ok(())
    }

    pub fn script_two_i64s(&mut self, for_what: TwoI64sFor, x: i64, y: i64) -> ArgumentResult {
        match for_what {
            TwoI64sFor::Pan => {
                // TODO: add I64Setting for PanPixelsX/Y sensitivity instead of PAN_PIXELS
                self.pan(
                    (-Self::PAN_PIXELS * x) as f32,
                    (-Self::PAN_PIXELS * y) as f32,
                );
            }
            TwoI64sFor::FrameResize => {
                let old_width = self.get_i64_setting(I64Setting::FrameWidth);
                let old_height = self.get_i64_setting(I64Setting::FrameHeight);
                if x > 0 || y > 0 {
                    self.resize_frames(
                        if x > 0 { x } else { old_width },
                        if y > 0 { y } else { old_height },
                    )?;
                } else {
                    // TODO: Crop to content
                    return Err(format!(
                        "{} without arguments is not yet implemented",
                        Command::UsingTwoI64s(for_what)
                    ));
                }
                return Ok(Argument::I64(old_width * old_height));
            }
            TwoI64sFor::SelectionDelta => {
                if let Some(ref mut s) = self.selection {
                    s.resize(x as i32, y as i32);
                }
            }
            TwoI64sFor::SelectionDeltaSymmetric => {
                if let Some(s) = &mut self.selection {
                    let mut x = x as i32;
                    let mut y = y as i32;
                    let r = s.abs().bounds();
                    if r.width() <= 2 && x < 0 {
                        x = 0;
                    }
                    if r.height() <= 2 && y < 0 {
                        y = 0;
                    }
                    *s = Selection::from(s.bounds().expand(x, y, x, y));
                }
            }
            TwoI64sFor::SelectionMove => {
                if let Some(ref mut s) = self.selection {
                    s.translate(x as i32, y as i32);
                    // TODO: wrap around if s.0.x >= width of drawing
                    // TODO: wrap around if s.0.y >= height of drawing
                }
            }
        }
        Ok(Argument::Null)
    }

    pub fn map_input(
        &mut self,
        modes: Vec<Mode>,
        input: script::Input,
        script: Script,
    ) -> VoidResult {
        // TODO: need to add self.key_bindings with the Script.
        Ok(())
    }

    fn script_write(&mut self, arg0: Option<String>, arg1: Option<i64>) -> VoidResult {
        // TODO: clean up `export_as` and `save_view` logic
        let scale = arg1.unwrap_or(1) as u32;
        if scale < 1 || scale > 16 {
            return Err(format!(
                "invalid scale for writing to file: {}",
                arg1.unwrap()
            ));
        }
        let arg0 = arg0.as_ref().map(|p| Path::new(p));
        let rewrite_save_file = scale == 1
            && arg0.as_ref().map_or(true, |path| {
                get_extension(&path).map_or(false, |ext| ext == "png")
            });
        match arg0 {
            None => match self.save_view(self.views.active_id) {
                Ok((storage, written)) => self.message(
                    format!("\"{}\" {} pixels written", storage, written),
                    MessageType::Info,
                ),
                Err(err) => self.message(format!("Error: {}", err), MessageType::Error),
            },
            Some(path) if rewrite_save_file => match self.active_view_mut().save_as(&path.into()) {
                Ok(written) => self.message(
                    format!("\"{}\" {} pixels written", path.display(), written),
                    MessageType::Info,
                ),
                Err(err) => self.message(format!("Error: {}", err), MessageType::Error),
            },
            Some(path) => {
                if let Err(err) = self.export_as(self.active_view().id, path, scale) {
                    self.message(format!("Error: {}", err), MessageType::Error);
                }
                // TODO: probably should have an export message otherwise.
            }
        }
        // TODO: clean up self.message above, do it when hearing back from evaluations in script.rs.
        Ok(())
    }

    pub fn script_quit(&mut self, quit: Quit) {
        match quit {
            Quit::Safe => self.quit_view_safe(self.views.active_id),
            Quit::AllSafe => {
                let ids: Vec<ViewId> = self.views.ids().collect();
                for id in ids {
                    self.quit_view_safe(id);
                }
            }
            Quit::Forced => self.quit_view(self.views.active_id),
            Quit::AllForced => self.quit(ExitReason::Normal),
            Quit::AfterWrite => match self.save_view(self.views.active_id) {
                Ok(_) => self.quit_view(self.views.active_id),
                Err(e) => self.message(e, MessageType::Error),
            },
        }
    }
}

fn get_extension<'a>(path: &'a Path) -> Result<&'a str, io::Error> {
    let ext = path
        .extension()
        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "file path requires an extension"))?;
    ext.to_str()
        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "file extension is not valid unicode"))
}

fn get_undo_or_redo_amount(optional_i64: Option<i64>, for_what: &str) -> Result<usize, String> {
    match optional_i64 {
        Some(value) if value < 0 => {
            return Err(format!("cannot {} in reverse: {}", for_what, value));
        }
        Some(value) => Ok(value as usize),
        None => Ok(1),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_key_bindings() {
        let mut kbs = KeyBindings::default();
        let state = InputState::Pressed;
        let modifiers = Default::default();

        let kb1 = KeyBinding {
            modes: vec![Mode::Normal],
            input: Input::Key(platform::Key::A),
            command: Cmd::Noop,
            is_toggle: false,
            display: None,
            modifiers,
            state,
        };
        let kb2 = KeyBinding {
            modes: vec![Mode::Command],
            ..kb1.clone()
        };

        kbs.add(kb1);
        kbs.add(kb2.clone());

        assert_eq!(
            kbs.len(),
            2,
            "identical bindings for different modes can co-exist"
        );

        let kb3 = KeyBinding {
            command: Cmd::Quit,
            ..kb2.clone()
        };
        kbs.add(kb3.clone());

        assert_eq!(kbs.len(), 2, "bindings can be overwritten");
        assert_eq!(
            kbs.find(kb2.input, kb2.modifiers, kb2.state, kb2.modes[0]),
            Some(kb3),
            "bindings can be overwritten"
        );
    }

    #[test]
    fn test_key_bindings_modifier() {
        let kb = KeyBinding {
            modes: vec![Mode::Normal],
            input: Input::Key(platform::Key::Control),
            command: Cmd::Noop,
            is_toggle: false,
            display: None,
            modifiers: Default::default(),
            state: InputState::Pressed,
        };

        let mut kbs = KeyBindings::default();
        kbs.add(kb.clone());

        assert_eq!(
            kbs.find(
                Input::Key(platform::Key::Control),
                ModifiersState {
                    ctrl: true,
                    alt: false,
                    shift: false,
                    meta: false
                },
                InputState::Pressed,
                Mode::Normal
            ),
            Some(kb.clone())
        );

        assert_eq!(
            kbs.find(
                Input::Key(platform::Key::Control),
                ModifiersState::default(),
                InputState::Pressed,
                Mode::Normal
            ),
            Some(kb)
        );
    }
}
