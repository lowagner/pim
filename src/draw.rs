use crate::brush::{Align, Brush, BrushMode};
use crate::color;
use crate::execution::Execution;
use crate::font::{TextAlign, TextBatch};
use crate::platform;
use crate::session::{Mode, Select, Session, Tool};
use crate::settings::*;
use crate::sprite;
use crate::view::{View, ViewCoords};

use crate::gfx::math::{Matrix4, Vector2};
use crate::gfx::rect::Rect;
use crate::gfx::shape2d::{Fill, Line, Rotation, Shape, Stroke};
use crate::gfx::Geometry;
use crate::gfx::Rgba;
use crate::gfx::{shape2d, sprite2d};
use crate::gfx::{Repeat, Rgb8, Rgba8, ZDepth};

use std::time;

pub const CHECKER_LAYER: ZDepth = ZDepth(-0.9);
pub const VIEW_LAYER: ZDepth = ZDepth(-0.7);
pub const BRUSH_LAYER: ZDepth = ZDepth(-0.6);
pub const GRID_LAYER: ZDepth = ZDepth(-0.5);
pub const UI_LAYER: ZDepth = ZDepth(-0.4);
pub const TEXT_LAYER: ZDepth = ZDepth(-0.3);
pub const PALETTE_LAYER: ZDepth = ZDepth(-0.2);
pub const HELP_LAYER: ZDepth = ZDepth(-0.1);
pub const CURSOR_LAYER: ZDepth = ZDepth(0.0);
pub const XRAY_RADIUS: f32 = 3.0;
pub const XRAY_MIN_ZOOM: f32 = 16.0;

pub const GLYPH_WIDTH: f32 = 8.;
pub const GLYPH_HEIGHT: f32 = 14.;

#[rustfmt::skip]
pub const CHECKER: [u8; 16] = [
    0x55, 0x55, 0x55, 0xff,
    0x66, 0x66, 0x66, 0xff,
    0x66, 0x66, 0x66, 0xff,
    0x55, 0x55, 0x55, 0xff,
];
const LINE_HEIGHT: f32 = GLYPH_HEIGHT + 4.;
const MARGIN: f32 = 10.;

pub mod cursors {
    use super::*;

    pub struct Cursor {
        pub(super) rect: Rect<f32>,
        pub(super) offset: Vector2<f32>,
        pub(super) invert: bool,
    }

    impl Cursor {
        const fn new(rect: Rect<f32>, off_x: f32, off_y: f32, invert: bool) -> Self {
            Self {
                rect,
                offset: Vector2::new(off_x, off_y),
                invert,
            }
        }
    }

    const SAMPLER: Cursor = Cursor::new(Rect::new(0., 0., 16., 16.), 1., 1., false);
    const CROSSHAIR: Cursor = Cursor::new(Rect::new(16., 0., 32., 16.), -8., -8., true);
    const OMNI: Cursor = Cursor::new(Rect::new(32., 0., 48., 16.), -8., -8., false);
    const PAN: Cursor = Cursor::new(Rect::new(48., 0., 64., 16.), -8., -8., false);
    const ERASE: Cursor = Cursor::new(Rect::new(64., 0., 80., 16.), -8., -8., true);
    // TODO: fix flood cursor offset, doesn't seem to hit the pixel where the bucket spill is located.
    const FLOOD: Cursor = Cursor::new(Rect::new(80., 0., 96., 16.), -8., -8., false);

    pub fn info(t: &Tool, b: &Brush, m: Mode, in_view: bool, in_selection: bool) -> Option<Cursor> {
        match (m, t) {
            (Mode::Help, Tool::Pan) => {}
            (Mode::Help, Tool::Brush) => {}
            (Mode::Help, _) => return None,
            _ => {}
        }
        let cursor = match t {
            Tool::Sampler => self::SAMPLER,
            Tool::Pan => self::PAN,
            Tool::FloodFill => self::FLOOD,

            Tool::Brush => match m {
                Mode::Select(_) if in_selection && in_view => self::OMNI,
                Mode::Select(Select::Dragging) if in_selection => self::OMNI,
                _ => {
                    if b.effectively_erases() {
                        self::ERASE
                    } else {
                        self::CROSSHAIR
                    }
                }
            },
        };
        Some(cursor)
    }
}

mod checker {
    use crate::gfx::rect::Rect;

    pub fn rect() -> Rect<f32> {
        Rect::origin(2., 2.)
    }
}

pub struct Context {
    pub ui_batch: shape2d::Batch,
    pub text_batch: TextBatch,
    pub overlay_batch: TextBatch,
    pub cursor_sprite: sprite::Sprite,
    pub tool_batch: sprite2d::Batch,
    pub paste_batch: sprite2d::Batch,
    pub checker_batch: sprite2d::Batch,
}

impl Context {
    pub fn draw(
        &mut self,
        session: &Session,
        avg_frametime: &time::Duration,
        execution: &Execution,
    ) {
        // TODO: allow layers in the row direction.
        // TODO: with layers, hover animation vertically in the row/layer you're editing.
        // TODO: allow an optional animation overlay in the upper right corner.
        // TODO: allow showing other open files at the same time in separate rows.
        // TODO: allow a "keyboard" mode where you can select an entire frame to copy from
        // one animation to another.
        self::draw_brush(session, &session.brush, &mut self.ui_batch);
        self::draw_paste(session, &mut self.paste_batch);
        self::draw_grid(session, &mut self.ui_batch);
        self::draw_ui(session, &mut self.ui_batch, &mut self.text_batch);
        self::draw_status(session, &mut self.ui_batch, &mut self.text_batch);
        self::draw_switcher(session, &mut self.ui_batch, &mut self.text_batch);
        self::draw_meta(session, &mut self.ui_batch, &mut self.text_batch);
        self::draw_overlay(session, avg_frametime, &mut self.overlay_batch, execution);
        self::draw_palette(session, &mut self.ui_batch);
        self::draw_cursor(session, &mut self.cursor_sprite, &mut self.tool_batch);
        self::draw_checker(session, &mut self.checker_batch);
    }

    pub fn clear(&mut self) {
        self.ui_batch.clear();
        self.text_batch.clear();
        self.overlay_batch.clear();
        self.cursor_sprite.clear();
        self.tool_batch.clear();
        self.paste_batch.clear();
        self.checker_batch.clear();
    }
}

// TODO: rename this, as there are a lot of UI-related draw methods that aren't in this method,
//       e.g., draw_palette is separate.
fn draw_ui(session: &Session, canvas: &mut shape2d::Batch, text: &mut TextBatch) {
    let view = session.active_view();
    let zoom = view.zoom as f32;

    if let Some(selection) = session.selection {
        let fill = match session.mode {
            Mode::Select(Select::Selecting) => color::RED.alpha(0x55),
            Mode::Select(Select::Dragging) => color::YELLOW.alpha(0x55),
            // TODO: Handle different modes differently.
            _ => Rgba8::TRANSPARENT,
        };
        let stroke = color::RED;

        // TODO: when super zoomed-in, the red outline that boxes pixels
        //       doesn't seem to be lined up correctly.
        let r = selection.abs().bounds();
        let offset = session.offset + view.offset;

        {
            // Selection dimensions.
            let s = selection;
            let t = format!("{}x{}", r.width(), r.height());
            let (x, align) = if s.x2 > s.x1 {
                ((s.x2 + 1) as f32 * zoom, TextAlign::Right)
            } else {
                ((s.x2 as f32) * zoom, TextAlign::Left)
            };
            let y = if s.y2 >= s.y1 {
                (s.y2 + 1) as f32 * zoom + 1.
            } else {
                (s.y2) as f32 * zoom - self::LINE_HEIGHT + 1.
            };
            text.add(
                &t,
                x + offset.x,
                y + offset.y,
                self::TEXT_LAYER,
                stroke,
                align,
            );
        }

        let t = Matrix4::from_translation(offset.extend(0.)) * Matrix4::from_scale(zoom);

        // Selection stroke.
        canvas.add(Shape::Rectangle(
            r.map(|n| n as f32).transform(t),
            self::UI_LAYER,
            Rotation::ZERO,
            Stroke::new(1., stroke.into()),
            Fill::Empty,
        ));
        // Selection fill.
        if r.intersects(view.layer_bounds()) {
            canvas.add(Shape::Rectangle(
                r.intersection(view.layer_bounds())
                    .map(|n| n as f32)
                    .transform(t),
                self::UI_LAYER,
                Rotation::ZERO,
                Stroke::NONE,
                Fill::Solid(fill.into()),
            ));
        }
    }

    for v in session.views.iter() {
        let zoom = v.zoom as f32;
        let offset = v.offset + session.offset;

        let (border_color, current_view_frame) = if session.is_active(v.id) {
            let color = match session.mode {
                // TODO: (rgx) Use `Rgba8::alpha`.
                Mode::Select(_) => Rgba8::new(color::RED.r, color::RED.g, color::RED.b, 0xdd),
                _ => color::WHITE,
            };
            (color.into(), session.current_frame())
        } else if session.hover_view == Some(v.id) {
            (Rgba::new(0.7, 0.7, 0.7, 1.0), usize::MAX)
        } else {
            (Rgba::new(0.5, 0.5, 0.5, 1.0), usize::MAX)
        };

        // View border
        let r = v.rect();
        canvas.add(Shape::Rectangle(
            Rect::new(r.x1 - 1., r.y1 - 1., r.x2 + 1., r.y2 + 1.) + session.offset,
            self::UI_LAYER,
            Rotation::ZERO,
            Stroke::new(1.0, border_color),
            Fill::Empty,
        ));

        // Current frame border
        if current_view_frame < usize::MAX {
            let color = Rgba::new(1.0, 1.0, 1.0, 0.3);
            let x1 = current_view_frame as f32 * zoom * v.fw as f32 + offset.x;
            let rect_height = 12.0 + (zoom - 6.0).min(8.0);
            let rect_below = Rect::new(
                x1 - 1.0,
                offset.y,
                x1 + zoom * v.fw as f32
                    + (current_view_frame + 1 == v.animation.len()) as usize as f32,
                offset.y - rect_height,
            );
            let rect_above = rect_below + Vector2::new(0.0, rect_height + zoom * v.fh as f32);
            // TODO: there's a hard-to-repro bug where these selection elements obscure the text.
            //       for now, just moving the text below them.
            canvas.add(Shape::Rectangle(
                rect_above,
                self::UI_LAYER,
                Rotation::ZERO,
                Stroke::NONE,
                Fill::Solid(color),
            ));
            canvas.add(Shape::Rectangle(
                rect_below,
                self::UI_LAYER,
                Rotation::ZERO,
                Stroke::NONE,
                Fill::Solid(color),
            ));
            text.add(
                &format!(
                    "{}x{} f{}/{}",
                    v.fw,
                    v.fh,
                    current_view_frame,
                    v.animation.len()
                ),
                // Have text track along with the focused frame,
                // so we can see it at all times.
                rect_below.x1,
                rect_below.y2 - self::LINE_HEIGHT,
                self::TEXT_LAYER,
                color::GREY,
                TextAlign::Left,
            );
        }

        // Frame lines
        for n in 1..v.animation.len() {
            let n = n as f32;
            let x = n * zoom * v.fw as f32 + offset.x;
            canvas.add(Shape::Line(
                Line::new([x, offset.y], [x, zoom * (v.fh as usize) as f32 + offset.y]),
                self::UI_LAYER,
                Rotation::ZERO,
                Stroke::new(1.0, Rgba::new(1., 1., 1., 0.6)),
            ));
        }
    }
}

fn draw_status(session: &Session, _canvas: &mut shape2d::Batch, text: &mut TextBatch) {
    let view = session.active_view();
    let zoom = view.zoom as f32;

    // Active view status
    text.add(
        &view.status(),
        MARGIN,
        MARGIN + self::LINE_HEIGHT,
        self::TEXT_LAYER,
        Rgba8::WHITE,
        TextAlign::Left,
    );

    // Session status
    text.add(
        &format!("{:>5}%", (zoom * 100.) as u32),
        session.width - MARGIN,
        MARGIN + self::LINE_HEIGHT,
        self::TEXT_LAYER,
        Rgba8::WHITE,
        TextAlign::Right,
    );

    if session.width >= 600. {
        let cursor = session.view_coords(view.id, session.cursor);
        let hover_color = session
            .hover_color
            .map_or(String::new(), |c| Rgb8::from(c).to_string());
        text.add(
            &format!("{:>4},{:<4} {}", cursor.x, cursor.y, hover_color),
            (session.width * 0.5).floor(),
            MARGIN + self::LINE_HEIGHT,
            self::TEXT_LAYER,
            Rgba8::WHITE,
            TextAlign::Left,
        );
    }
}

fn draw_switcher(session: &Session, canvas: &mut shape2d::Batch, _text: &mut TextBatch) {
    if session.width >= 400. {
        // Foreground color (session.fg, left click)
        canvas.add(Shape::Rectangle(
            Rect::origin(11., 11.).with_origin(
                (session.width * 0.4).floor(),
                self::LINE_HEIGHT + self::MARGIN + 2.,
            ),
            self::UI_LAYER,
            Rotation::ZERO,
            Stroke::new(1.0, Rgba::WHITE),
            Fill::Solid(session.fg.into()),
        ));
        // Background color (session.bg, right click)
        canvas.add(Shape::Rectangle(
            Rect::origin(11., 11.).with_origin(
                (session.width * 0.4).floor() + 25.,
                self::LINE_HEIGHT + self::MARGIN + 2.,
            ),
            self::UI_LAYER,
            Rotation::ZERO,
            Stroke::new(1.0, Rgba::WHITE),
            Fill::Solid(session.bg.into()),
        ));
    }
}

fn draw_meta(session: &Session, canvas: &mut shape2d::Batch, text: &mut TextBatch) {
    // Command-line & message
    if session.mode == Mode::Command {
        let s = format!("{}", &session.cmdline.input());
        text.add(
            &s,
            MARGIN,
            MARGIN,
            self::TEXT_LAYER,
            // TODO: create a session.text_color option that updates when session.settings.ui_background changes.
            //       replace all `text Rgba8::WHITE` instances.
            Rgba8::WHITE,
            TextAlign::Left,
        );
        text.glyph(
            96,
            MARGIN + session.cmdline.cursor as f32 * self::GLYPH_WIDTH,
            MARGIN,
            self::TEXT_LAYER,
            // TODO: shift from session.text_color
            Rgba8::RED,
        );
    } else if !session.message.is_execution() && !session.message.is_debug() {
        let s = format!("{}", &session.message);
        text.add(
            &s,
            MARGIN,
            MARGIN,
            self::TEXT_LAYER,
            session.message.color(),
            TextAlign::Left,
        );
    }

    if session.get_i64_setting(I64Setting::Debug) != 0 {
        canvas.add(Shape::Line(
            Line::new(
                [session.width / 2., 0.],
                [session.width / 2., session.height],
            ),
            self::UI_LAYER,
            Rotation::ZERO,
            Stroke::new(1.0, color::RED.into()),
        ));
        canvas.add(Shape::Line(
            Line::new(
                [0., session.height / 2.],
                [session.width, session.height / 2.],
            ),
            self::UI_LAYER,
            Rotation::ZERO,
            Stroke::new(1.0, color::RED.into()),
        ));
    }
}

fn draw_overlay(
    session: &Session,
    avg_frametime: &time::Duration,
    text: &mut TextBatch,
    exec: &Execution,
) {
    let debug = session.get_i64_setting(I64Setting::Debug) != 0;

    match exec {
        Execution::Recording { path, .. } => {
            text.add(
                &format!("* recording: {} (<End> to stop)", path.display()),
                MARGIN * 2.,
                session.height - self::LINE_HEIGHT - MARGIN,
                ZDepth::ZERO,
                color::RED,
                TextAlign::Left,
            );
        }
        Execution::Replaying { events, path, .. } => {
            if let Some(event) = events.front() {
                text.add(
                    &format!(
                        "> replaying: {}: {:32} (<Esc> to stop)",
                        path.display(),
                        String::from(event.clone()),
                    ),
                    MARGIN * 2.,
                    session.height - self::LINE_HEIGHT - MARGIN,
                    ZDepth::ZERO,
                    color::LIGHT_GREEN,
                    TextAlign::Left,
                );
            }
        }
        Execution::Normal => {}
    }

    if debug {
        let mem = crate::ALLOCATOR.allocated();

        // Frame-time
        let txt = &format!(
            "{:3.2}ms {:3.2}ms {}MB {}KB {}",
            avg_frametime.as_micros() as f64 / 1000.,
            session.avg_time.as_micros() as f64 / 1000.,
            mem / (1024 * 1024),
            mem / 1024 % (1024),
            session.mode,
        );
        text.add(
            txt,
            session.width - MARGIN,
            session.height - MARGIN - self::LINE_HEIGHT,
            ZDepth::ZERO,
            Rgba8::WHITE,
            TextAlign::Right,
        );
    }

    if session.message.is_execution() || (session.message.is_debug() && debug) {
        text.add(
            &format!("{}", session.message),
            MARGIN,
            MARGIN,
            ZDepth::ZERO,
            session.message.color(),
            TextAlign::Left,
        );
    }
}

fn draw_palette(session: &Session, batch: &mut shape2d::Batch) {
    let p = &session.palette;
    let height = p.height;
    for (i, color) in p.colors.iter().cloned().enumerate() {
        let x = (i / height) as f32 * p.cellsize;
        let y = p.y - (i % height) as f32 * p.cellsize;

        let mut stroke = shape2d::Stroke::NONE;
        if let (Tool::Sampler, Some(c)) = (&session.tool, p.hover) {
            if c == color {
                stroke = shape2d::Stroke::new(1., Rgba::WHITE);
            }
        }

        batch.add(Shape::Rectangle(
            Rect::new(x, y - p.cellsize, x + p.cellsize, y),
            self::PALETTE_LAYER,
            Rotation::ZERO,
            stroke,
            shape2d::Fill::Solid(color.into()),
        ));
    }
}

fn draw_checker(session: &Session, batch: &mut sprite2d::Batch) {
    let checker_side = session.get_i64_setting(I64Setting::UiChecker) as f32;
    if checker_side <= 0.0 {
        return;
    }
    for v in session.views.iter() {
        // Number of times to repeat in the Y direction:
        let ry = v.height() as f32 / checker_side;
        let ratio = v.width() as f32 / v.height() as f32;
        // Number of times to repeat in the X direction:
        let rx = ry * ratio;

        batch.add(
            checker::rect(),
            v.rect() + session.offset,
            self::CHECKER_LAYER,
            Rgba::TRANSPARENT,
            1.,
            Repeat::new(rx, ry),
        );
    }
}

fn draw_grid(session: &Session, batch: &mut shape2d::Batch) {
    let g = session.get_i64_setting(I64Setting::UiGrid);
    if g <= 0 {
        return;
    }

    let g = g as u32;
    let color = session.get_color_setting(ColorSetting::UiGrid).alpha(0xcc);

    let v = session.active_view();
    let t = session.offset + v.offset;
    let w = v.width();
    let h = v.height();
    let m = Matrix4::from_translation(t.extend(0.)) * Matrix4::from_scale(v.zoom as f32);

    // Grid columns.
    for x in (0..).step_by(g as usize).skip(1).take_while(|x| *x < w) {
        let h = h as f32;
        let x = x as f32;

        batch.add(Shape::Line(
            Line::new([x, 0.], [x, h]).transform(m),
            self::GRID_LAYER,
            Rotation::ZERO,
            Stroke::new(1., color.into()),
        ));
    }
    // Grid rows.
    for y in (0..).step_by(g as usize).skip(1).take_while(|y| *y < h) {
        let w = w as f32;
        let y = y as f32;

        batch.add(Shape::Line(
            Line::new([0., y], [w, y]).transform(m),
            self::GRID_LAYER,
            Rotation::ZERO,
            Stroke::new(1., color.into()),
        ));
    }

    // Draw center lines.
    if w % g == 0 && h % g == 0 {
        let (w, h) = (w as f32, h as f32);
        let stroke = Stroke::new(1., color.alpha(0xee).into());

        batch.add(Shape::Line(
            Line::new([0., h / 2.], [w, h / 2.]).transform(m),
            self::GRID_LAYER,
            Rotation::ZERO,
            stroke,
        ));
        batch.add(Shape::Line(
            Line::new([w / 2., 0.], [w / 2., h]).transform(m),
            self::GRID_LAYER,
            Rotation::ZERO,
            stroke,
        ));
    }
}

fn draw_cursor(session: &Session, inverted: &mut sprite::Sprite, batch: &mut sprite2d::Batch) {
    let v = session.active_view();
    let c = session.cursor;

    if let Some(cursors::Cursor {
        rect,
        offset,
        invert,
    }) = cursors::info(
        &session.tool,
        &session.brush,
        session.mode,
        v.contains(c - session.offset),
        session.is_selected(session.view_coords(v.id, c).into()),
    ) {
        let dst = rect.with_origin(c.x, c.y) + offset;
        let zdepth = self::CURSOR_LAYER;

        if invert {
            inverted.set(rect, dst, zdepth);
        } else {
            batch.add(rect, dst, zdepth, Rgba::TRANSPARENT, 1., Repeat::default());
        }
    }
}

fn draw_brush(session: &Session, brush: &Brush, shapes: &mut shape2d::Batch) {
    if session.palette.hover.is_some() {
        return;
    }
    let v = session.active_view();
    let c = session.cursor;
    let z = v.zoom as f32;

    match session.mode {
        Mode::Select(Select::Selecting) | Mode::Select(Select::Dragging) => {
            if session.is_selected(session.view_coords(v.id, c).into()) {
                return;
            }

            if v.contains(c - session.offset) {
                let c = session.snap(c, v.offset.x, v.offset.y, z);
                shapes.add(Shape::Rectangle(
                    Rect::new(c.x, c.y, c.x + z, c.y + z),
                    self::UI_LAYER,
                    Rotation::ZERO,
                    Stroke::new(1.0, color::RED.into()),
                    Fill::Empty,
                ));
            }
        }
        Mode::Normal => {
            if let Tool::Brush = session.tool {
                let view_coords = session.active_view_coords(c);

                // Draw enabled brush
                if v.contains(c - session.offset) {
                    let (stroke, fill) = if brush.effectively_erases() {
                        // When erasing, we draw a stroke that is the inverse of the underlying
                        // color at the cursor. Note that this isn't perfect, since it uses
                        // the current snapshot to get the color, so it may be incorrect
                        // while erasing over previously erased pixels in the same stroke.
                        // To make this 100% correct, we have to read the underlying color
                        // from the view's staging buffer.
                        if let Some(color) = v.color_at(view_coords.into()).cloned().map(Rgba::from)
                        {
                            (
                                Stroke::new(
                                    1.0,
                                    Rgba::new(1. - color.r, 1. - color.g, 1. - color.b, 1.0),
                                ),
                                Fill::Empty,
                            )
                        } else {
                            (Stroke::new(1.0, Rgba::WHITE), Fill::Empty)
                        }
                    } else {
                        (Stroke::NONE, Fill::Solid(brush.color.into()))
                    };

                    for p in brush.expand(view_coords.into(), v.extent()) {
                        shapes.add(brush.shape(
                            *session.session_coords(v.id, p.into()),
                            self::BRUSH_LAYER,
                            stroke,
                            fill,
                            z,
                            Align::BottomLeft,
                        ));
                    }

                    // X-Ray brush mode.
                    if brush.is_set(BrushMode::XRay) && brush.size == 1 && z >= self::XRAY_MIN_ZOOM
                    {
                        let p: ViewCoords<u32> = view_coords.into();

                        if let Some(xray) = v.color_at(p).cloned() {
                            if xray != session.fg {
                                let center = *session.session_coords(v.id, view_coords)
                                    + Vector2::new(z / 2., z / 2.);

                                shapes.add(
                                    Shape::circle(center, self::XRAY_RADIUS, 16)
                                        .zdepth(self::BRUSH_LAYER)
                                        .fill(Fill::Solid(xray.alpha(0xff).into())),
                                );
                            }
                        }
                    }
                // Draw disabled brush
                } else {
                    let color = if brush.effectively_erases() {
                        color::GREY
                    } else {
                        session.fg
                    };
                    shapes.add(brush.shape(
                        *c,
                        self::UI_LAYER,
                        Stroke::new(1.0, color.into()),
                        Fill::Empty,
                        z,
                        Align::Center,
                    ));
                }
            }
        }
        _ => {}
    }
}

fn draw_paste(session: &Session, batch: &mut sprite2d::Batch) {
    if let (Mode::Select(Select::Pasting), Some(s)) = (session.mode, session.selection) {
        batch.add(
            Rect::origin(batch.w as f32, batch.h as f32),
            Rect::new(s.x1 as f32, s.y1 as f32, s.x2 as f32 + 1., s.y2 as f32 + 1.),
            ZDepth::default(),
            Rgba::TRANSPARENT,
            0.9,
            Repeat::default(),
        );
    }
}

pub fn draw_view_animation<R>(v: &View<R>) -> sprite2d::Batch {
    sprite2d::Batch::singleton(
        v.width(),
        v.fh,
        v.animation.val().map(|e| e as f32),
        Rect::new(-(v.fw as f32), 0.0, 0.0, v.fh as f32) * v.zoom as f32,
        self::VIEW_LAYER,
        Rgba::TRANSPARENT,
        1.,
        Repeat::default(),
    )
}

pub fn draw_view_composites<R>(session: &Session, v: &View<R>) -> sprite2d::Batch {
    let mut batch = sprite2d::Batch::new(v.width(), v.fh);

    for frame in v.animation.frames.iter() {
        let frame = frame.map(|e| e as f32);
        batch.add(
            frame,
            (frame - Vector2::new(0., v.fh as f32)) * v.zoom as f32 + (session.offset + v.offset),
            self::VIEW_LAYER,
            Rgba::TRANSPARENT,
            1.,
            Repeat::default(),
        )
    }

    batch
}

pub fn draw_help(session: &Session, text: &mut TextBatch, shape: &mut shape2d::Batch) {
    shape.add(Shape::Rectangle(
        Rect::origin(session.width, session.height),
        ZDepth(0.0),
        Rotation::ZERO,
        Stroke::new(1., color::RED.into()),
        Fill::Empty,
    ));
    shape.add(Shape::Rectangle(
        Rect::origin(session.width, session.height),
        self::HELP_LAYER,
        Rotation::ZERO,
        Stroke::NONE,
        Fill::Solid(Rgba::BLACK),
    ));

    let column_offset = self::GLYPH_WIDTH * 24.;
    let left_margin = self::MARGIN * 2.;

    text.add(
        &format!(
            "pim v{}: help ({} to exit)",
            crate::VERSION,
            platform::Key::Escape,
        ),
        left_margin,
        session.height - self::MARGIN - self::LINE_HEIGHT,
        self::HELP_LAYER,
        color::LIGHT_GREY,
        TextAlign::Left,
    );

    /* TODO: bring back help for key bindings
    let (normal_kbs, visual_kbs): (
        Vec<(&String, &session::KeyBinding)>,
        Vec<(&String, &session::KeyBinding)>,
    ) = session
        .key_bindings
        .iter()
        .filter_map(|kb| kb.display.as_ref().map(|d| (d, kb)))
        .partition(|(_, kb)| kb.modes.contains(&Mode::Normal));

    let mut line = (0..(session.height as usize - self::LINE_HEIGHT as usize * 4))
        .rev()
        .step_by(self::LINE_HEIGHT as usize);

    for (display, kb) in normal_kbs.iter() {
        if let Some(y) = line.next() {
            text.add(
                display,
                left_margin,
                y as f32,
                self::HELP_LAYER,
                color::RED,
                TextAlign::Left,
            );
            text.add(
                &format!("{}", kb.script),
                left_margin + column_offset,
                y as f32,
                self::HELP_LAYER,
                color::LIGHT_GREY,
                TextAlign::Left,
            );
        }
    }

    if let Some(y) = line.nth(1) {
        text.add(
            "VISUAL MODE",
            left_margin,
            y as f32,
            self::HELP_LAYER,
            color::RED,
            TextAlign::Left,
        );
    }
    line.next();

    for (display, kb) in visual_kbs.iter() {
        if let Some(y) = line.next() {
            text.add(
                display,
                left_margin,
                y as f32,
                self::HELP_LAYER,
                color::RED,
                TextAlign::Left,
            );
            text.add(
                &format!("{}", kb.script),
                left_margin + column_offset,
                y as f32,
                self::HELP_LAYER,
                color::LIGHT_GREY,
                TextAlign::Left,
            );
        }
    }
    */
    /* // TODO: add settings info here
    for l in session::SETTINGS.lines() {
        if let Some(y) = line.next() {
            text.add(
                l,
                left_margin,
                y as f32,
                self::HELP_LAYER,
                color::RED,
                TextAlign::Left,
            );
        }
    }
    */

    for (i, l) in session.help().iter().enumerate() {
        let y = session.height - (i + 4) as f32 * self::LINE_HEIGHT;

        text.add(
            l,
            left_margin + column_offset * 3. + 64.,
            y,
            self::HELP_LAYER,
            color::LIGHT_GREEN,
            TextAlign::Left,
        );
    }

    text.offset(session.help_offset.x, session.help_offset.y);
}
