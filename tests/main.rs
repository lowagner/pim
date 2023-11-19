use pim::execution::{DigestMode, ExecutionMode};
use std::env;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use serde_derive::Deserialize;

#[macro_use]
extern crate lazy_static;

#[derive(Deserialize)]
struct Config {
    window: WindowConfig,
    assets: AssetConfig,
}

#[derive(Deserialize)]
struct WindowConfig {
    width: u32,
    height: u32,
}

#[derive(Deserialize)]
struct AssetConfig {
    glyphs: PathBuf,
}

lazy_static! {
    /// This mutex is here to prevent certain tests from running
    /// in parallel. This is due to the fact that we spawn windows
    /// and graphics contexts which are not thread-safe.
    pub static ref MUTEX: Mutex<()> = Mutex::new(());
}

#[test]
fn views() {
    test("views");
}

////////////////////////////////////////////////////////////////////////////////

fn test(name: &str) {
    if let Err(e) = run(name) {
        panic!("test '{}' failed with: {}", name, e);
    }
}

fn run(name: &str) -> io::Result<()> {
    // We allow tests to create these temporary files,
    // so make sure it's not there when a test is run.
    fs::remove_file("/tmp/pim.png").ok();

    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join(name);
    let cfg: Config = {
        let path = path.join(name).with_extension("toml");
        let cfg = fs::read_to_string(&path)
            .map_err(|e| io::Error::new(e.kind(), format!("{}: {}", path.display(), e)))?;
        toml::from_str(&cfg)?
    };
    let glyphs = fs::read(Path::new(env!("CARGO_MANIFEST_DIR")).join(&cfg.assets.glyphs))
        .map_err(|e| io::Error::new(e.kind(), format!("{}: {}", path.display(), e)))?;

    let glyphs = glyphs.as_slice();

    let options = pim::Options {
        resizable: false,
        headless: true,
        source: Some(path.join(name).with_extension("pim")),
        width: cfg.window.width,
        height: cfg.window.height,
        exec: ExecutionMode::Replay(path.clone(), DigestMode::Verify),
        glyphs,
        debug: false,
    };

    {
        let _guard = MUTEX.lock();
        pim::init::<&str>(&[], options)
    }
}
