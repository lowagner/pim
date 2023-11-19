use strum_macros::EnumIter;

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
    /// Height of the palette, as the number of colors it supports vertically.
    PaletteHeight,
    /// Zoom for the UI, roughly 1 to 128.
    UiZoom,
    /// Index for the current view.
    ViewIndex,
    // TODO: ViewAnimationDelay -- delay in ms for the current animation frame
    /// X-Ray mode to show the color of the pixel below your cursor; boolean (0 or 1)
    CursorXRay,
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
