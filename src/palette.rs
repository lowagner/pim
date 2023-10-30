use crate::session::SessionCoords;

use crate::gfx::{Rgba8, Lyza};
use arrayvec::ArrayVec;

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

    pub fn gradient(&mut self, color_start: Rgba8, color_end: Rgba8, number: usize) {
        if number <= 1 { return; }
        let start = Lyza::from(color_start);
        let end = Lyza::from(color_end);

        for i in 0..number {
            let t = (i as f32) / (number as f32 - 1.0);
            let color = Rgba8::from(Lyza {
                l: (1.0 - t) * start.l + t * end.l,
                y: (1.0 - t) * start.y + t * end.y,
                z: (1.0 - t) * start.z + t * end.z,
                a: (1.0 - t) * start.a + t * end.a,
            });
            self.colors.push(color);
        }
    }

    pub fn clear(&mut self) {
        self.colors.clear();
    }

    pub fn size(&self) -> usize {
        self.colors.len()
    }

    pub fn handle_cursor_moved(&mut self, p: SessionCoords) {
        let (x, y) = (p.x, p.y);
        let mut x = x as i32 - self.x as i32;
        let mut y = y as i32 - self.y as i32;
        let cellsize = self.cellsize as i32;
        let size = self.size() as i32;
        let height = self.height as i32;
        let columns = (self.size() as f32 / self.height as f32).ceil() as i32;

        let width = if size > height {
            cellsize * columns
        } else {
            cellsize
        };
        let height = i32::min(size, height) * cellsize;

        if x >= width || y >= height || x < 0 || y < 0 {
            self.hover = None;
            return;
        }

        x /= cellsize;
        y /= cellsize;

        let index = y + x * (height / cellsize);

        self.hover = if index < size {
            // We index from the back because the palette is reversed
            // before it is displayed, due to the Y axis pointing up,
            // where as the palette is created starting at the top
            // and going down.
            Some(self.colors[self.size() - index as usize - 1])
        } else {
            None
        };
    }
}
