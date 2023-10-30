use std::fmt;
use std::str::FromStr;

use libm;
use num;

///////////////////////////////////////////////////////////////////////////
// Rgba8
///////////////////////////////////////////////////////////////////////////

/// RGBA color with 8-bit channels.
#[repr(C)]
#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Debug, Default, Hash)]
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
    /// or `#abc3` with `0x3` being the alpha.  We also allow gray
    /// shorthand with `#31` being `#313131` and `#a` being `#aaa`.
    fn from_str(hex_code: &str) -> Result<Self, String> {
        if !hex_code.starts_with('#') {
            return Err(format!("color should start with #, got `{}`", hex_code));
        }
        match hex_code.len() {
            9 => {
                let r = hex_u8(&hex_code[1..3], "r")?;
                let g = hex_u8(&hex_code[3..5], "g")?;
                let b = hex_u8(&hex_code[5..7], "b")?;
                let a = hex_u8(&hex_code[7..9], "a")?;

                Ok(Rgba8 { r, g, b, a })
            }
            7 => {
                let r = hex_u8(&hex_code[1..3], "r")?;
                let g = hex_u8(&hex_code[3..5], "g")?;
                let b = hex_u8(&hex_code[5..7], "b")?;
                let a: u8 = 0xff;

                Ok(Rgba8 { r, g, b, a })
            }
            5 => {
                // Multiplying by 17 since 15 is the largest hex digit you can get (with `f`),
                // and 15 * 17 = 255 would therefore be the max color value.
                let r = hex_u8(&hex_code[1..2], "r")? * 17;
                let g = hex_u8(&hex_code[2..3], "g")? * 17;
                let b = hex_u8(&hex_code[3..4], "b")? * 17;
                let a = hex_u8(&hex_code[4..5], "a")? * 17;

                Ok(Rgba8 { r, g, b, a })
            }
            4 => {
                // Multiplying by 17 since 15 is the largest hex digit you can get (with `f`),
                // and 15 * 17 = 255 would therefore be the max color value.
                let r = hex_u8(&hex_code[1..2], "r")? * 17;
                let g = hex_u8(&hex_code[2..3], "g")? * 17;
                let b = hex_u8(&hex_code[3..4], "b")? * 17;
                let a: u8 = 0xff;

                Ok(Rgba8 { r, g, b, a })
            }
            3 => {
                let gray = hex_u8(&hex_code[1..3], "gray")?;
                Ok(Rgba8 {
                    r: gray,
                    g: gray,
                    b: gray,
                    a: 0xff,
                })
            }
            2 => {
                let gray = hex_u8(&hex_code[1..2], "gray")? * 17;
                Ok(Rgba8 {
                    r: gray,
                    g: gray,
                    b: gray,
                    a: 0xff,
                })
            }
            _ => Err(format!("malformed color: `{}`", hex_code)),
        }
    }
}

fn hex_u8(hex: &str, context: &str) -> Result<u8, String> {
    u8::from_str_radix(hex, 16).map_err(|_| format!("invalid {}: `{}`", context, hex))
}

//////////////////////////////////////////////////////////////////////////////
// Rgb8
//////////////////////////////////////////////////////////////////////////////

// TODO: what is this used for? let's delete it if possible.
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

/// Following OkLab but swapping order of the hue-related fields (b <-> y, a <-> z),
/// and normalizing the fields a bit differently (to get things approximately
/// into the 0 to 1 or -1 to 1 range).
/// See https://bottosson.github.io/posts/oklab/
#[derive(Copy, Clone, PartialEq, Debug, Default)]
pub struct Lyza {
    /// Lightness, 0 to 1.
    pub l: f32,
    /// Yellow minus blue, roughly -1 to 1.
    /// More precisely, -1.0 to 0.6374, but we'll clamp on the way back to Rgba8.
    pub y: f32,
    /// Red minus green, roughly -1 to 1.  Pronounce z as "zed" to rhyme with red.
    /// More precisely, -0.7508 to 0.8867, but we'll clamp on the way back to Rgba8.
    pub z: f32,
    /// Alpha, 0 to 1.
    pub a: f32,
}

impl Lyza {
    /// How bright the color is (white is 1, black is 0).
    pub fn lightness(&self) -> f32 {
        self.l
    }

    /// Returns the "chroma" or how thick the color is.
    /// Roughly from 0 to 1, but can go a little above 1.
    /// Highest value is magenta #ff00ff with 1.0351907.
    pub fn saturation(&self) -> f32 {
        let chroma2 = self.y.powf(2.0) + self.z.powf(2.0);
        chroma2.powf(0.5)
    }

    /// Returns the hue, from 0 (red + to orange) to 1 (red - to purple).
    pub fn hue(&self) -> f32 {
        let normalized_angle = libm::atan2f(self.y, self.z) / (2.0 * std::f32::consts::PI);
        if normalized_angle < 0.0 {
            normalized_angle + 1.0
        } else {
            normalized_angle
        }
    }

    // TODO: add set_hue, set_saturation, set_lightness
}

impl From<Rgba8> for Lyza {
    fn from(c: Rgba8) -> Self {
        let r = c.r as f32 / 255.0;
        let g = c.g as f32 / 255.0;
        let b = c.b as f32 / 255.0;
        let a = c.a as f32 / 255.0;

        let o3: f32 = 0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b;
        let m3: f32 = 0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b;
        let s3: f32 = 0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b;

        let o = o3.powf(1.0 / 3.0);
        let m = m3.powf(1.0 / 3.0);
        let s = s3.powf(1.0 / 3.0);

        // OkLab extrema values for (r, g, b) -- per scripts/oklab.py:
        // l min/max: (0.0, 0.0, 0.0) -> 0.0, (1.0, 1.0, 1.0) -> 0.9999999934735462
        // y min/max: (0.0, 0.0, 1.0) -> -0.3115281476783751, (1.0, 1.0, 0.0) -> 0.19856975465179516
        // z min/max: (0.0, 1.0, 0.0) -> -0.23388757418790818, (1.0, 0.0, 0.7411764705882353) -> 0.27621675349252356
        let oklab_l = 0.2104542553 * o + 0.7936177850 * m - 0.0040720468 * s;
        let oklab_y = 0.0259040371 * o + 0.7827717662 * m - 0.8086757660 * s;
        let oklab_z = 1.9779984951 * o - 2.4285922050 * m + 0.4505937099 * s;
        Self {
            l: oklab_l * 1.000000006526, // * 1.0 / 0.999999993474,
            y: oklab_y * 3.209982813603, // * 1.0 / 0.311528147678,
            // We could normalize by dividing by 0.276216753493, but we should
            // normalize y and z with the same coefficient so that chroma/saturation
            // properties stay the same with OkLab.
            z: oklab_z * 3.209982813603,
            a,
        }
    }
}

impl From<Lyza> for Rgba8 {
    fn from(c: Lyza) -> Self {
        // Get pre-normalization out of the way:
        let oklab_l = c.l * 0.999999993474; // * 1.0 / 1.000000006526
        let oklab_y = c.y * 0.311528147678; // * 1.0 / 3.209982813603
        let oklab_z = c.z * 0.311528147678;

        let o = oklab_l + 0.3963377774 * oklab_z + 0.2158037573 * oklab_y;
        let m = oklab_l - 0.1055613458 * oklab_z - 0.0638541728 * oklab_y;
        let s = oklab_l - 0.0894841775 * oklab_z - 1.2914855480 * oklab_y;

        let o3 = o * o * o;
        let m3 = m * m * m;
        let s3 = s * s * s;

        let r = 4.0767416621 * o3 - 3.3077115913 * m3 + 0.2309699292 * s3;
        let g = -1.2684380046 * o3 + 2.6097574011 * m3 - 0.3413193965 * s3;
        let b = -0.0041960863 * o3 - 0.7034186147 * m3 + 1.7076147010 * s3;
        let a = c.a;

        Self {
            r: num::clamp((r * 255.0).round(), 0.0, 255.0) as u8,
            g: num::clamp((g * 255.0).round(), 0.0, 255.0) as u8,
            b: num::clamp((b * 255.0).round(), 0.0, 255.0) as u8,
            a: num::clamp((a * 255.0).round(), 0.0, 255.0) as u8,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_rgba8_gray_parsing() {
        assert_eq!(
            Rgba8::from_str("#ab"),
            Ok(Rgba8 {
                r: 0xab,
                g: 0xab,
                b: 0xab,
                a: 0xff,
            })
        );

        assert_eq!(
            Rgba8::from_str("#a"),
            Ok(Rgba8 {
                r: 0xa * 17,
                g: 0xa * 17,
                b: 0xa * 17,
                a: 0xff,
            })
        );
    }

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
            Rgba8::from_str("#p"), // short gray
            Err("invalid gray: `p`".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#1p"), // long gray
            Err("invalid gray: `1p`".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#p23"), // short, r bad
            Err("invalid r: `p`".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#q234"), // short, r bad, with alpha
            Err("invalid r: `q`".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#p12345"), // long, r bad
            Err("invalid r: `p1`".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#1p234567"), // long, r bad, with alpha
            Err("invalid r: `1p`".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#1p3"), // short, g bad
            Err("invalid g: `p`".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#1z34"), // short, g bad, with alpha
            Err("invalid g: `z`".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#12p345"), // long, g bad
            Err("invalid g: `p3`".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#123y4567"), // long, g bad, with alpha
            Err("invalid g: `3y`".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#12s"), // short, b bad
            Err("invalid b: `s`".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#13g4"), // short, b bad, with alpha
            Err("invalid b: `g`".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#1234p5"), // long, b bad
            Err("invalid b: `p5`".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#12345g67"), // long, b bad, with alpha
            Err("invalid b: `5g`".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#123q"), // short, bad alpha
            Err("invalid a: `q`".to_string())
        );

        assert_eq!(
            Rgba8::from_str("#1234563g"), // long, bad alpha
            Err("invalid a: `3g`".to_string())
        );
    }

    #[test]
    fn test_lyza_converts_correctly() {
        let black = Lyza::from(Rgba8 {
            r: 0,
            g: 0,
            b: 0,
            a: 0,
        });
        assert_eq!(
            black,
            Lyza {
                l: 0.0,
                y: 0.0,
                z: 0.0,
                a: 0.0
            }
        );
        assert_eq!(black.hue(), 0.0);
        assert_eq!(black.saturation(), 0.0);
        assert_eq!(
            Rgba8::from(black),
            Rgba8 {
                r: 0,
                g: 0,
                b: 0,
                a: 0
            }
        );

        let white = Lyza::from(Rgba8 {
            r: 255,
            g: 255,
            b: 255,
            a: 255,
        });
        assert_eq!(
            white,
            Lyza {
                l: 1.0,
                y: 1.9132989e-7, // smol
                z: 0.0,
                a: 1.0
            }
        );
        assert_eq!(white.hue(), 0.25); // doesn't really matter
        assert_eq!(white.saturation(), 1.9132989e-7);
        assert_eq!(
            Rgba8::from(white),
            Rgba8 {
                r: 255,
                g: 255,
                b: 255,
                a: 255
            }
        );

        let red = Lyza::from(Rgba8 {
            r: 255,
            g: 0,
            b: 0,
            a: 255,
        });
        assert_eq!(
            red,
            Lyza {
                l: 0.6279554,
                y: 0.40396446,
                z: 0.72180617,
                a: 1.0
            }
        );
        assert_eq!(red.hue(), 0.08120527);
        assert_eq!(red.saturation(), 0.82715863);
        assert_eq!(
            Rgba8::from(red),
            Rgba8 {
                r: 255,
                g: 0,
                b: 0,
                a: 255
            }
        );

        let yellow = Lyza::from(Rgba8 {
            r: 255,
            g: 255,
            b: 0,
            a: 128,
        });
        assert_eq!(
            yellow,
            Lyza {
                l: 0.9679827,
                y: 0.6374054,
                z: -0.22909354,
                a: 0.5019608
            }
        );
        assert_eq!(yellow.hue(), 0.30491453);
        assert_eq!(yellow.saturation(), 0.67732525);
        assert_eq!(
            Rgba8::from(yellow),
            Rgba8 {
                r: 255,
                g: 255,
                b: 0,
                a: 128
            }
        );

        let green = Lyza::from(Rgba8 {
            r: 0,
            g: 255,
            b: 0,
            a: 50,
        });
        assert_eq!(
            green,
            Lyza {
                l: 0.8664396,
                y: 0.5761871,
                z: -0.75077456,
                a: 0.19607843
            }
        );
        assert_eq!(green.hue(), 0.3958203);
        assert_eq!(green.saturation(), 0.94639);
        assert_eq!(
            Rgba8::from(green),
            Rgba8 {
                r: 0,
                g: 255,
                b: 0,
                a: 50
            }
        );

        let cyan = Lyza::from(Rgba8 {
            r: 0,
            g: 255,
            b: 255,
            a: 100,
        });
        assert_eq!(
            cyan,
            Lyza {
                l: 0.90539926,
                y: -0.12646733,
                z: -0.47971234,
                a: 0.39215687
            }
        );
        assert_eq!(cyan.hue(), 0.5410248);
        assert_eq!(cyan.saturation(), 0.49610272);
        assert_eq!(
            Rgba8::from(cyan),
            Rgba8 {
                r: 0,
                g: 255,
                b: 255,
                a: 100
            }
        );

        let blue = Lyza::from(Rgba8 {
            r: 0,
            g: 0,
            b: 255,
            a: 255,
        });
        assert_eq!(
            blue,
            Lyza {
                l: 0.4520137,
                y: -1.0,
                z: -0.10418649,
                a: 1.0
            }
        );
        assert_eq!(blue.hue(), 0.73347783);
        assert_eq!(blue.saturation(), 1.0054128);
        assert_eq!(
            Rgba8::from(blue),
            Rgba8 {
                r: 0,
                g: 0,
                b: 255,
                a: 255
            }
        );

        let magenta = Lyza::from(Rgba8 {
            r: 255,
            g: 0,
            b: 255,
            a: 0,
        });
        assert_eq!(
            magenta,
            Lyza {
                l: 0.7016738,
                y: -0.5429883,
                z: 0.8813532,
                a: 0.0
            }
        );
        assert_eq!(magenta.hue(), 0.9121206);
        assert_eq!(magenta.saturation(), 1.0351907);
        assert_eq!(
            Rgba8::from(magenta),
            Rgba8 {
                r: 255,
                g: 0,
                b: 255,
                a: 0
            }
        );
    }

    #[test]
    fn test_lyza_converts_out_of_bounds_correctly() {
        assert_eq!(
            Rgba8::from(Lyza {
                l: 1.5,
                y: -2.0,
                z: 1.3,
                a: 4.5
            }),
            Rgba8 {
                r: 255,
                g: 67,
                b: 255,
                a: 255
            }
        );
        assert_eq!(
            Rgba8::from(Lyza {
                l: -1.0,
                y: 2.0,
                z: -1.0,
                a: -0.5
            }),
            Rgba8 {
                r: 0,
                g: 122,
                b: 0,
                a: 0
            }
        );
        assert_eq!(
            Rgba8::from(Lyza {
                l: 0.5,
                y: -2.0,
                z: -1.0,
                a: 0.51
            }),
            Rgba8 {
                r: 0,
                g: 0,
                b: 255,
                a: 130
            }
        );
        assert_eq!(
            Rgba8::from(Lyza {
                l: 0.9,
                y: -0.0,
                z: -3.0,
                a: 0.0
            }),
            Rgba8 {
                r: 0,
                g: 255,
                b: 236,
                a: 0
            }
        );
    }
}
