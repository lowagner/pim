use strum_macros::EnumIter;

// TODO: move this file to another (e.g., like command.rs).
// TODO: move other things like UsingOptionalI64 there too.

#[derive(Eq, Hash, PartialEq, Debug, Clone, Copy, EnumIter)]
pub enum StringSetting {
    /// Current mode (e.g., normal, command, etc.).
    Mode,
    /// Current working directory
    Cwd,
    /// Configuration directory, e.g., /home/.config/pim
    ConfigDirectory,
}

#[derive(Eq, Hash, PartialEq, Debug, Clone, Copy, EnumIter)]
pub enum I64Setting {
    // TODO: Pan sensitivity.  add PanPixelsX and PanPixelsY, and add a built-in command
    //       to set both via a single `pan-sensitivity Z` script `(run-all (pansx $0) (pansy $0))`
    /// Whether in debug mode or not.
    Debug,
    /// Animate the frames in the UI; boolean (0 or 1).
    UiAnimate,
    /// Whether to show a checkerboard for transparent colors.
    UiChecker,
    /// Whether we show a grid for drawing, if nonzero, and if so, what size.
    UiGrid,
    /// The interface scale (e.g., for palette boxes and command line), as a percentage (100 = 1x).
    UiScalePercentage,
    /// Offset for the UI, x coordinate.
    UiOffsetX,
    /// Offset for the UI, y coordinate.
    UiOffsetY,
    /// The current tool (see session::Tool).
    Tool,
    /// Height of the palette, as the number of colors it supports vertically.
    PaletteHeight,
    /// Zoom for the UI, roughly 1 to 128.
    UiZoom,
    /// Index for the current view.
    ViewIndex,
    // TODO: ViewAnimationDelay -- delay in ms for the current animation frame
    /// X-Ray mode to show the color of the pixel below your cursor; boolean (0 or 1)
    CursorXRay,
    // TODO: BrushOn as an integer, stacked so that multiple inputs can request brush down.
    /// Size of the brush, in pixels.
    BrushSize,
    /// Erase pixels with the brush; boolean (0 or 1).
    BrushErase,
    /// Draw on all frames at once; boolean (0 or 1).
    BrushMultiFrame,
    /// Pixel-perfect drawing mode; boolean (0 or 1).
    BrushPixelPerfect,
    /// Draw with X-Symmetry; boolean (0 or 1).
    BrushXSymmetry,
    /// Draw with Y-Symmetry; boolean (0 or 1).
    BrushYSymmetry,
    /// Confine line angles to multiples of this value, or 0 for no snapping.
    BrushLineAngle,
    /// The delay (in milliseconds) for the current view's animation frame.
    AnimationDelay,
    /// The current frame index.
    FrameIndex,
    /// The width of each frame in the animation, in pixels.
    FrameWidth,
    /// The height of each frame in the animation, in pixels.
    FrameHeight,
    /// The number of frames that the image divides into for animation.
    /// Changing this does *not* affect the width of the image, and
    /// requires that the desired number of frames divides the image width.
    ImageSplit,
    // TODO: ImageLayer - do layers as vertical, unless rx has a better idea
    // we can split vertically, etc.
    /// The current view's history ID.
    History,
    /// Whether we're in normal mode or not.  0 = no, 1 = yes.
    /// If setting to 1, will bring to normal mode, otherwise will bring to command mode
    /// if the old value is 1.
    NormalMode,
}

#[derive(Eq, Hash, PartialEq, Debug, Clone, Copy, EnumIter)]
pub enum ColorSetting {
    /// Background color for the UI.
    UiBackground,
    /// If visible, what the grid line color is.
    UiGrid,
    /// Foreground color for drawing (left click/left mouse button).
    Foreground,
    /// Background color for drawing (right click/right mouse button).
    Background,
}
