use crate::session::SessionCoords;

use crate::gfx::{Lyza, Rgba8};
use arrayvec::ArrayVec;

use std::fs::File;
use std::io::Write;

pub struct Palette {
    pub colors: ArrayVec<[Rgba8; 256]>,
    pub hover: Option<Rgba8>,
    pub cellsize: f32,
    pub height: usize,
    pub x: f32,
    pub y: f32,
}

impl Palette {
    pub fn new(cellsize: f32, height: usize) -> Self {
        Self {
            colors: ArrayVec::new(),
            hover: None,
            cellsize,
            height,
            x: 0.,
            y: 0.,
        }
    }

    /// Returns the palette color index, and avoids adding a new color
    /// if already present in the palette.
    pub fn add(&mut self, color: Rgba8) -> usize {
        for i in 0..self.colors.len() {
            if self.colors[i] == color {
                return i;
            }
        }
        self.colors.push(color);
        self.colors.len() - 1
    }

    pub fn gradient(&mut self, color_start: Rgba8, color_end: Rgba8, number: usize) -> usize {
        if number <= 1 {
            // No changes, not sure if we should add color_start or color_end.
            // TODO: We could average them for number == 1.
            return 0;
        }
        let start = Lyza::from(color_start);
        let end = Lyza::from(color_end);
        let starting_count = self.colors.len();

        for i in 0..number {
            let t = (i as f32) / (number as f32 - 1.0);
            let color = Rgba8::from(Lyza {
                l: (1.0 - t) * start.l + t * end.l,
                y: (1.0 - t) * start.y + t * end.y,
                z: (1.0 - t) * start.z + t * end.z,
                a: (1.0 - t) * start.a + t * end.a,
            });
            self.add(color);
        }
        let ending_count = self.colors.len();
        ending_count - starting_count
    }

    pub fn clear(&mut self) {
        self.colors.clear();
    }

    pub fn size(&self) -> usize {
        self.colors.len()
    }

    pub fn handle_cursor_moved(&mut self, p: SessionCoords) {
        let (x, y) = (p.x, p.y);
        let relative_x = x as i32 - self.x as i32;
        let relative_y = y as i32 - self.y as i32;
        let cell_size_in_pixels = self.cellsize as i32;
        let color_count = self.size() as i32;
        let cells_per_column = self.height as i32;
        let column_count = (color_count as f32 / cells_per_column as f32).ceil() as i32;

        let width_in_pixels = column_count * cell_size_in_pixels;
        let height_in_pixels = i32::min(color_count, cells_per_column) * cell_size_in_pixels;

        if relative_x >= width_in_pixels
            || relative_y >= height_in_pixels
            || relative_x < 0
            || relative_y < 0
        {
            self.hover = None;
            return;
        }

        // Convert the cursor position into which cell the cursor is in.
        let cell_x = relative_x / cell_size_in_pixels;
        let cell_y = relative_y / cell_size_in_pixels;

        // Larger y is going up, but we index the palette going down.
        let index = (cells_per_column - cell_y - 1) + cell_x * cells_per_column;

        self.hover = if index < color_count {
            Some(self.colors[index as usize])
        } else {
            None
        };
    }

    pub fn sort(&mut self) {
        self.colors
            .sort_by(|a, b| Lyza::from(*a).compare(&Lyza::from(*b)))
    }

    pub fn write(&self, path: String) -> Result<String, String> {
        let mut f = File::create(&path)
            .map_err(|err| format!("Error creating palette file `{}`: {}", path, err))?;
        for color in self.colors.iter() {
            // TODO: use `p-add` after we switch to script.rs from cmd.rs
            writeln!(f, "{}", color)
                .map_err(|err| format!("Error writing to palette file `{}`: {}", path, err))?;
        }
        Ok(format!(
            "palette written to {} ({} colors)",
            path,
            self.colors.len()
        ))
    }
}
