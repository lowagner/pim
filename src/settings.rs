use claim::assert_ok;
use directories;
use strum_macros::EnumIter;

use std::collections::HashMap;
use std::io;

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
    /// Animate the frames in the UI; boolean (0 or 1).
    UiAnimate,
    /// The interface scale (e.g., for palette boxes and command line), as a percentage (100 = 1x).
    UiScalePercentage,
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
    /// The current frame index.
    FrameIndex,
    /// The width of each frame in the animation, in pixels.
    FrameWidth,
    /// The height of each frame in the animation, in pixels.
    FrameHeight,
}

pub struct Settings {
    i64_map: HashMap<I64Setting, i64>,
    string_map: HashMap<StringSetting, String>,
}

impl Settings {
    pub fn new() -> Self {
        // Default settings!
        let mut i64_map = HashMap::new();
        i64_map.insert(I64Setting::UiAnimate, 1);
        i64_map.insert(I64Setting::UiScalePercentage, 100);
        i64_map.insert(I64Setting::CursorXRay, 1);
        i64_map.insert(I64Setting::BrushSize, 1);
        i64_map.insert(I64Setting::BrushErase, 0);
        i64_map.insert(I64Setting::BrushMultiFrame, 1);
        i64_map.insert(I64Setting::BrushPixelPerfect, 1);
        i64_map.insert(I64Setting::BrushXSymmetry, 0);
        i64_map.insert(I64Setting::BrushYSymmetry, 0);
        i64_map.insert(I64Setting::BrushLineAngle, 0);
        i64_map.insert(I64Setting::FrameIndex, 0);
        i64_map.insert(I64Setting::FrameWidth, 128);
        i64_map.insert(I64Setting::FrameHeight, 128);

        let mut string_map = HashMap::new();
        string_map.insert(StringSetting::Mode, "normal".to_string());
        string_map.insert(
            StringSetting::Cwd,
            std::env::current_dir().map_or("".to_string(), |cwd| cwd.display().to_string()),
        );
        let project_dirs = assert_ok!(directories::ProjectDirs::from("com", "patchsoul", "pim")
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "config directory not found")));
        string_map.insert(
            StringSetting::ConfigDirectory,
            project_dirs.config_dir().display().to_string(),
        );

        Self {
            i64_map,
            string_map,
        }
    }

    pub fn get_i64(&self, setting: I64Setting) -> i64 {
        *self.i64_map.get(&setting).unwrap()
    }

    // Returns the old value while setting a new one.
    pub fn set_i64(&mut self, setting: I64Setting, value: i64) -> i64 {
        self.i64_map.insert(setting, value).unwrap()
    }

    pub fn get_string(&self, setting: StringSetting) -> String {
        self.string_map.get(&setting).unwrap().clone()
    }

    // Returns the old value while setting a new one.
    pub fn set_string(&mut self, setting: StringSetting, value: String) -> String {
        self.string_map.insert(setting, value).unwrap()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use strum::IntoEnumIterator;

    #[test]
    fn test_adds_all_string_settings() {
        let settings = Settings::new();
        for string_setting in StringSetting::iter() {
            eprint!("checking string setting {:?}\n", string_setting);
            // This will panic if string_setting is not set in the default hash map.
            settings.get_string(string_setting);
        }
    }

    #[test]
    fn test_adds_all_i64_settings() {
        let settings = Settings::new();
        for i64_setting in I64Setting::iter() {
            eprint!("checking i64 setting {:?}\n", i64_setting);
            // This will panic if i64_setting is not set in the default hash map.
            settings.get_i64(i64_setting);
        }
    }
}
