//! Data included in the `pim` binary.

/// Initial (default) configuration for pim.
pub const CONFIG: &[u8] = include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/config/init.pim"));

/// Cursor sprites.
pub const CURSORS: &[u8] =
    include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/assets/cursors.png"));

/// Glyphs used for font rendering.
pub const GLYPHS: &[u8] = include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/assets/glyphs.png"));

/// Static assets in use by the editor.
pub struct Assets<'a> {
    pub glyphs: &'a [u8],
}

impl<'a> Assets<'a> {
    pub fn new(glyphs: &'a [u8]) -> Self {
        Self { glyphs }
    }
}
