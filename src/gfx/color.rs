use std::fmt;
use std::str::FromStr;

///////////////////////////////////////////////////////////////////////////
// Rgba8
///////////////////////////////////////////////////////////////////////////

/// RGBA color with 8-bit channels.
#[repr(C)]
#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Debug, Default)]
pub struct Rgba8 {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}

impl Rgba8 {
    pub const TRANSPARENT: Self = Self {
        r: 0,
        g: 0,
        b: 0,
        a: 0,
    };
    pub const WHITE: Self = Self {
        r: 0xff,
        g: 0xff,
        b: 0xff,
        a: 0xff,
    };
    pub const BLACK: Self = Self {
        r: 0,
        g: 0,
        b: 0,
        a: 0xff,
    };
    pub const RED: Self = Self {
        r: 0xff,
        g: 0,
        b: 0,
        a: 0xff,
    };
    pub const GREEN: Self = Self {
        r: 0,
        g: 0xff,
        b: 0,
        a: 0xff,
    };
    pub const BLUE: Self = Self {
        r: 0,
        g: 0,
        b: 0xff,
        a: 0xff,
    };

    /// Create a new [`Rgba8`] color from individual channels.
    pub const fn new(r: u8, g: u8, b: u8, a: u8) -> Self {
        Self { r, g, b, a }
    }

    /// Invert the color.
    pub fn invert(self) -> Self {
        Self::new(0xff - self.r, 0xff - self.g, 0xff - self.b, self.a)
    }

    /// Return the color with a changed alpha.
    ///
    /// ```
    /// use pim::gfx::color::Rgba8;
    ///
    /// let c = Rgba8::WHITE;
    /// assert_eq!(c.alpha(0x88), Rgba8::new(c.r, c.g, c.b, 0x88))
    /// ```
    pub fn alpha(self, a: u8) -> Self {
        Self::new(self.r, self.g, self.b, a)
    }

    /// Given a byte slice, returns a slice of [`Rgba8`] values.
    pub fn align<'a, S: 'a, T: AsRef<[S]> + ?Sized>(bytes: &'a T) -> &'a [Rgba8] {
        let bytes = bytes.as_ref();
        let (head, body, tail) = unsafe { bytes.align_to::<Rgba8>() };

        if !(head.is_empty() && tail.is_empty()) {
            panic!("Rgba8::align: input is not a valid Rgba8 buffer");
        }
        body
    }
}

/// ```
/// use pim::gfx::color::Rgba8;
///
/// assert_eq!(format!("{}", Rgba8::new(0xff, 0x0, 0xa, 0xff)), "#ff000a");
/// ```
impl fmt::Display for Rgba8 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{:02x}{:02x}{:02x}", self.r, self.g, self.b)?;
        if self.a != 0xff {
            write!(f, "{:02x}", self.a)?;
        }
        Ok(())
    }
}

/// ```
/// use pim::gfx::color::{Rgba8, Rgba};
///
/// assert_eq!(Rgba8::from(Rgba::RED), Rgba8::RED);
/// ```
impl From<Rgba> for Rgba8 {
    fn from(rgba: Rgba) -> Self {
        Self {
            r: (rgba.r * 255.0).round() as u8,
            g: (rgba.g * 255.0).round() as u8,
            b: (rgba.b * 255.0).round() as u8,
            a: (rgba.a * 255.0).round() as u8,
        }
    }
}

impl From<u32> for Rgba8 {
    fn from(rgba: u32) -> Self {
        unsafe { std::mem::transmute(rgba) }
    }
}

impl FromStr for Rgba8 {
    type Err = String;

    /// Parses a color code of the form `#ffffff` or `#abc` into an
    /// instance of `Rgba8`.  Can have optional alpha hex digit(s)
    /// at the end, e.g., `#123456ef` with `0xef` being the alpha,
    /// or `#abc3` with `0x3` being the alpha.
    fn from_str(hex_code: &str) -> Result<Self, String> {
        if !hex_code.starts_with('#') {
            return Err(format!("color should start with #, got `{}`", hex_code));
        }
        // TODO: improve error messages, e.g., "bad r in color `{}`"
        match hex_code.len() {
            9 => {
                let r: u8 =
                    u8::from_str_radix(&hex_code[1..3], 16).map_err(|_| "bad r".to_string())?;
                let g: u8 =
                    u8::from_str_radix(&hex_code[3..5], 16).map_err(|_| "bad g".to_string())?;
                let b: u8 =
                    u8::from_str_radix(&hex_code[5..7], 16).map_err(|_| "bad b".to_string())?;
                let a: u8 =
                    u8::from_str_radix(&hex_code[7..9], 16).map_err(|_| "bad a".to_string())?;

                Ok(Rgba8 { r, g, b, a })
            }
            7 => {
                let r: u8 =
                    u8::from_str_radix(&hex_code[1..3], 16).map_err(|_| "bad r".to_string())?;
                let g: u8 =
                    u8::from_str_radix(&hex_code[3..5], 16).map_err(|_| "bad g".to_string())?;
                let b: u8 =
                    u8::from_str_radix(&hex_code[5..7], 16).map_err(|_| "bad b".to_string())?;
                let a: u8 = 0xff;

                Ok(Rgba8 { r, g, b, a })
            }
            5 => {
                // Multiplying by 17 since 15 is the largest hex digit you can get (with `f`),
                // and 15 * 17 = 255 would therefore be the max color value.
                let r: u8 =
                    u8::from_str_radix(&hex_code[1..2], 16).map_err(|_| "bad r".to_string())? * 17;
                let g: u8 =
                    u8::from_str_radix(&hex_code[2..3], 16).map_err(|_| "bad g".to_string())? * 17;
                let b: u8 =
                    u8::from_str_radix(&hex_code[3..4], 16).map_err(|_| "bad b".to_string())? * 17;
                let a: u8 =
                    u8::from_str_radix(&hex_code[4..5], 16).map_err(|_| "bad a".to_string())? * 17;

                Ok(Rgba8 { r, g, b, a })
            }
            4 => {
                // Multiplying by 17 since 15 is the largest hex digit you can get (with `f`),
                // and 15 * 17 = 255 would therefore be the max color value.
                let r: u8 =
                    u8::from_str_radix(&hex_code[1..2], 16).map_err(|_| "bad r".to_string())? * 17;
                let g: u8 =
                    u8::from_str_radix(&hex_code[2..3], 16).map_err(|_| "bad g".to_string())? * 17;
                let b: u8 =
                    u8::from_str_radix(&hex_code[3..4], 16).map_err(|_| "bad b".to_string())? * 17;
                let a: u8 = 0xff;

                Ok(Rgba8 { r, g, b, a })
            }
            _ => Err(format!("malformed color: `{}`", hex_code)),
        }
    }
}

//////////////////////////////////////////////////////////////////////////////
// Rgb8
//////////////////////////////////////////////////////////////////////////////

/// An RGB 8-bit color. Used when the alpha value isn't used.
#[repr(C)]
#[derive(Copy, Clone)]
pub struct Rgb8 {
    r: u8,
    g: u8,
    b: u8,
}

impl From<Rgba8> for Rgb8 {
    fn from(rgba: Rgba8) -> Self {
        Self {
            r: rgba.r,
            g: rgba.g,
            b: rgba.b,
        }
    }
}

impl fmt::Display for Rgb8 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{:02X}{:02X}{:02X}", self.r, self.g, self.b)
    }
}

//////////////////////////////////////////////////////////////////////////////
// Rgba
//////////////////////////////////////////////////////////////////////////////

/// A normalized RGBA color.
#[repr(C)]
#[derive(Copy, Clone, PartialEq, Debug, Default)]
pub struct Rgba {
    pub r: f32,
    pub g: f32,
    pub b: f32,
    pub a: f32,
}

// TODO: what is this used for?  switch to OkLab color instead.
impl Rgba {
    pub const RED: Self = Rgba::new(1.0, 0.0, 0.0, 1.0);
    pub const GREEN: Self = Rgba::new(0.0, 1.0, 0.0, 1.0);
    pub const BLUE: Self = Rgba::new(0.0, 0.0, 1.0, 1.0);
    pub const WHITE: Self = Rgba::new(1.0, 1.0, 1.0, 1.0);
    pub const BLACK: Self = Rgba::new(0.0, 0.0, 0.0, 1.0);
    pub const TRANSPARENT: Self = Rgba::new(0.0, 0.0, 0.0, 0.0);

    /// Create a new `Rgba` color.
    pub const fn new(r: f32, g: f32, b: f32, a: f32) -> Self {
        Self { r, g, b, a }
    }

    /// Invert the color.
    pub fn invert(self) -> Self {
        Self::new(1.0 - self.r, 1.0 - self.g, 1.0 - self.b, self.a)
    }
}

impl From<Rgba8> for Rgba {
    fn from(rgba8: Rgba8) -> Self {
        Self {
            r: (rgba8.r as f32 / 255.0),
            g: (rgba8.g as f32 / 255.0),
            b: (rgba8.b as f32 / 255.0),
            a: (rgba8.a as f32 / 255.0),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_rgba8_short_parsing() {
        assert_eq!(
            Rgba8::from_str("#abc"),
            Ok(Rgba8 {
                r: 0xa * 17,
                g: 0xb * 17,
                b: 0xc * 17,
                a: 0xff,
            })
        );

        assert_eq!(
            Rgba8::from_str("#123d"),
            Ok(Rgba8 {
                r: 0x1 * 17,
                g: 0x2 * 17,
                b: 0x3 * 17,
                a: 0xd * 17,
            })
        );
    }

    #[test]
    fn test_rgba8_long_parsing() {
        assert_eq!(
            Rgba8::from_str("#123456"),
            Ok(Rgba8 {
                r: 0x12,
                g: 0x34,
                b: 0x56,
                a: 0xff,
            })
        );

        assert_eq!(
            Rgba8::from_str("#fedcba98"),
            Ok(Rgba8 {
                r: 0xfe,
                g: 0xdc,
                b: 0xba,
                a: 0x98,
            })
        );
    }

    #[test]
    fn test_rgba8_invalid_strings() {
        assert_eq!(
            Rgba8::from_str("abcdef"),
            Err("color should start with #, got `abcdef`".to_string())
        );

        // TODO: make this gray
        assert_eq!(
            Rgba8::from_str("#3"),
            Err("malformed color: `#3`".to_string())
        );

        // TODO: make this double-digit gray
        assert_eq!(
            Rgba8::from_str("#ab"),
            Err("malformed color: `#ab`".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#123de"),
            Err("malformed color: `#123de`".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#123def3"),
            Err("malformed color: `#123def3`".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#123def359"),
            Err("malformed color: `#123def359`".to_string())
        );
    }

    #[test]
    fn test_rgba8_invalid_digits() {
        assert_eq!(
            Rgba8::from_str("#p23"), // short, r bad
            Err("bad r".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#p234"), // short, r bad, with alpha
            Err("bad r".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#p12345"), // long, r bad
            Err("bad r".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#p1234567"), // long, r bad, with alpha
            Err("bad r".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#1p3"), // short, g bad
            Err("bad g".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#1p34"), // short, g bad, with alpha
            Err("bad g".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#12p345"), // long, g bad
            Err("bad g".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#12p34567"), // long, g bad, with alpha
            Err("bad g".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#12p"), // short, b bad
            Err("bad b".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#13p4"), // short, b bad, with alpha
            Err("bad b".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#1234p5"), // long, b bad
            Err("bad b".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#1234p567"), // long, b bad, with alpha
            Err("bad b".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#123q"), // short, bad alpha
            Err("bad a".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#123456p7"), // long, bad alpha
            Err("bad a".to_string())
        );
    }
}
