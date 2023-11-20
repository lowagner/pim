use crate::image;
use crate::view::ViewExtent;

use crate::gfx::color::Rgba8;

use std::io;
use std::path::Path;

#[allow(dead_code)]
#[derive(Debug)]
pub struct Manifest {
    pub extent: ViewExtent,
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Archive {
    pub layers: Vec<Vec<Vec<Rgba8>>>,
    pub manifest: Manifest,
}

pub fn load_image<P: AsRef<Path>>(path: P) -> io::Result<(u32, u32, Vec<Rgba8>)> {
    let (buffer, width, height) = image::load(path)?;
    let pixels = Rgba8::align(&buffer);

    // TODO: (perf) Avoid the copy?

    Ok((width, height, pixels.into()))
}

pub fn concatenate_images<P: AsRef<Path>>(paths: &[P]) -> io::Result<(u32, u32, Vec<Rgba8>)> {
    if paths.is_empty() {
        return Ok((0, 0, vec![]));
    }
    let (first_width, first_height, pixels) = load_image(&paths[0])?;
    let mut total_width = first_width;
    let mut images: Vec<(u32, Vec<Rgba8>)> = vec![(first_width, pixels)];
    for i in 1..paths.len() {
        let (width, height, pixels) = load_image(&paths[i])?;
        if height != first_height {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "not all images have the same height",
            ));
        }
        images.push((width, pixels));
        total_width += width;
    }
    let total_height = first_height;

    let mut pixels = vec![];
    pixels.reserve_exact(total_width as usize * total_height as usize);
    for y in 0..total_height {
        for image in &images {
            let (width, image_pixels) = image;
            let width = *width;
            let start = (y * width) as usize;
            // TODO: Rust why this not work
            // pixels.extend(image_pixels[start..start + width as usize]);
            for x in 0..width {
                pixels.push(image_pixels[start + x as usize]);
            }
        }
    }

    Ok((total_width, total_height, pixels))
}
