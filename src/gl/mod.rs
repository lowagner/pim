use crate::command::Axis;
use crate::draw;
use crate::execution::Execution;
use crate::font::TextBatch;
use crate::platform::{self, LogicalSize};
use crate::renderer;
use crate::session::{self, Blending, Effect, Selection, Session};
use crate::settings::*;
use crate::sprite;
use crate::util;
use crate::view::resource::ViewResource;
use crate::view::{View, ViewId, ViewOp, ViewState};
use crate::{data, data::Assets, image};

use crate::gfx::rect::{ensure_within, Rect};
use crate::gfx::{shape2d, sprite2d, Origin, Rgba, Rgba8, ZDepth};
use crate::gfx::{Matrix4, Repeat, Vector3};

use luminance::context::GraphicsContext;
use luminance::depth_test::DepthComparison;
use luminance::framebuffer::Framebuffer;
use luminance::pipeline::{PipelineState, TextureBinding};
use luminance::pixel;
use luminance::render_state::RenderState;
use luminance::shader::{Program, Uniform};
use luminance::tess::{Mode, Tess, TessBuilder};
use luminance::texture::{Dim2, GenMipmaps, MagFilter, MinFilter, Sampler, Texture, Wrap};
use luminance::{
    blending::{self, Equation, Factor},
    pipeline::PipelineError,
};

use luminance_derive::{Semantics, UniformInterface, Vertex};
use luminance_gl::gl33;

use std::collections::BTreeMap;
use std::error::Error;
use std::fmt;
use std::io;
use std::mem;
use std::time;

type Backend = gl33::GL33;
type M44 = [[f32; 4]; 4];

const SAMPLER: Sampler = Sampler {
    wrap_r: Wrap::Repeat,
    wrap_s: Wrap::Repeat,
    wrap_t: Wrap::Repeat,
    min_filter: MinFilter::Nearest,
    mag_filter: MagFilter::Nearest,
    depth_comparison: None,
};

#[derive(UniformInterface)]
struct Sprite2dInterface {
    tex: Uniform<TextureBinding<Dim2, pixel::NormUnsigned>>,
    ortho: Uniform<M44>,
    transform: Uniform<M44>,
}

#[derive(Copy, Clone, Debug, Semantics)]
pub enum VertexSemantics {
    #[sem(name = "position", repr = "[f32; 3]", wrapper = "VertexPosition")]
    Position,
    #[sem(name = "uv", repr = "[f32; 2]", wrapper = "VertexUv")]
    Uv,
    #[sem(name = "color", repr = "[u8; 4]", wrapper = "VertexColor")]
    Color,
    #[sem(name = "opacity", repr = "[f32; 1]", wrapper = "VertexOpacity")]
    Opacity,
    #[sem(name = "angle", repr = "[f32; 1]", wrapper = "VertexAngle")]
    Angle,
    #[sem(name = "center", repr = "[f32; 2]", wrapper = "VertexCenter")]
    Center,
}

#[repr(C)]
#[derive(Copy, Clone, Vertex)]
#[vertex(sem = "VertexSemantics")]
#[rustfmt::skip]
struct Sprite2dVertex {
    #[allow(dead_code)] position: VertexPosition,
    #[allow(dead_code)] uv: VertexUv,
    #[vertex(normalized = "true")]
    #[allow(dead_code)] color: VertexColor,
    #[allow(dead_code)] opacity: VertexOpacity,
}

////////////////////////////////////////////////////////////

#[derive(UniformInterface)]
struct Shape2dInterface {
    ortho: Uniform<M44>,
    transform: Uniform<M44>,
}

#[repr(C)]
#[derive(Copy, Clone, Vertex)]
#[vertex(sem = "VertexSemantics")]
#[rustfmt::skip]
struct Shape2dVertex {
    #[allow(dead_code)] position: VertexPosition,
    #[allow(dead_code)] angle: VertexAngle,
    #[allow(dead_code)] center: VertexCenter,
    #[vertex(normalized = "true")]
    #[allow(dead_code)] color: VertexColor,
}

#[derive(UniformInterface)]
struct Cursor2dInterface {
    cursor: Uniform<TextureBinding<Dim2, pixel::NormUnsigned>>,
    framebuffer: Uniform<TextureBinding<Dim2, pixel::NormUnsigned>>,
    ortho: Uniform<M44>,
    scale: Uniform<f32>,
}

#[repr(C)]
#[derive(Copy, Clone, Vertex)]
#[vertex(sem = "VertexSemantics")]
#[rustfmt::skip]
struct Cursor2dVertex {
    #[allow(dead_code)] position: VertexPosition,
    #[allow(dead_code)] uv: VertexUv,
}

#[derive(UniformInterface)]
struct Screen2dInterface {
    framebuffer: Uniform<TextureBinding<Dim2, pixel::NormUnsigned>>,
}

pub type SrgbaTexture = Texture<Backend, Dim2, pixel::SRGBA8UI>;

pub struct Renderer {
    pub win_size: LogicalSize,

    ctx: Context,
    draw_ctx: draw::Context,
    scale_factor: f64,
    scale: f64,
    present_fb: Framebuffer<Backend, Dim2, (), ()>,
    screen_fb: Framebuffer<Backend, Dim2, pixel::SRGBA8UI, pixel::Depth32F>,
    render_st: RenderState,
    pipeline_st: PipelineState,
    blending: Blending,

    staging_batch: shape2d::Batch,
    final_batch: shape2d::Batch,

    font: SrgbaTexture,
    cursors: SrgbaTexture,
    checker: SrgbaTexture,
    paste: SrgbaTexture,
    paste_outputs: Vec<Tess<Backend, Sprite2dVertex>>,

    sprite2d: Program<Backend, VertexSemantics, (), Sprite2dInterface>,
    shape2d: Program<Backend, VertexSemantics, (), Shape2dInterface>,
    cursor2d: Program<Backend, VertexSemantics, (), Cursor2dInterface>,
    screen2d: Program<Backend, VertexSemantics, (), Screen2dInterface>,

    view_data: BTreeMap<ViewId, ViewData>,
}

struct LayerData {
    fb: Framebuffer<Backend, Dim2, pixel::SRGBA8UI, pixel::Depth32F>,
    tess: Tess<Backend, Sprite2dVertex>,
}

impl LayerData {
    fn new(w: u32, h: u32, pixels: Option<&[Rgba8]>, ctx: &mut Context) -> Self {
        let batch = sprite2d::Batch::singleton(
            w,
            h,
            Rect::origin(w as f32, h as f32),
            Rect::origin(w as f32, h as f32),
            ZDepth::default(),
            Rgba::TRANSPARENT,
            1.,
            Repeat::default(),
        );

        let verts: Vec<Sprite2dVertex> = batch
            .vertices()
            .iter()
            .map(|v| unsafe { mem::transmute(*v) })
            .collect();
        let tess = TessBuilder::new(ctx)
            .set_vertices(verts)
            .set_mode(Mode::Triangle)
            .build()
            .unwrap();

        let mut fb: Framebuffer<Backend, Dim2, pixel::SRGBA8UI, pixel::Depth32F> =
            Framebuffer::new(ctx, [w, h], 0, self::SAMPLER).unwrap();

        fb.color_slot().clear(GenMipmaps::No, (0, 0, 0, 0)).unwrap();

        if let Some(pixels) = pixels {
            let aligned = util::align_u8(pixels);
            fb.color_slot().upload_raw(GenMipmaps::No, aligned).unwrap();
        }

        Self { fb, tess }
    }

    fn clear(&mut self) -> Result<(), RendererError> {
        self.fb
            .color_slot()
            .clear(GenMipmaps::No, (0, 0, 0, 0))
            .map_err(RendererError::Texture)
    }

    fn upload_part(
        &mut self,
        offset: [u32; 2],
        size: [u32; 2],
        texels: &[u8],
    ) -> Result<(), RendererError> {
        self.fb
            .color_slot()
            .upload_part_raw(GenMipmaps::No, offset, size, texels)
            .map_err(RendererError::Texture)
    }

    fn upload(&mut self, texels: &[u8]) -> Result<(), RendererError> {
        self.fb
            .color_slot()
            .upload_raw(GenMipmaps::No, texels)
            .map_err(RendererError::Texture)
    }

    fn pixels(&mut self) -> Vec<Rgba8> {
        let texels = self
            .fb
            .color_slot()
            .get_raw_texels()
            .expect("getting raw texels never fails");
        Rgba8::align(&texels).to_vec()
    }
}

struct ViewData {
    layer: LayerData,
    staging_fb: Framebuffer<Backend, Dim2, pixel::SRGBA8UI, pixel::Depth32F>,
    anim_tess: Option<Tess<Backend, Sprite2dVertex>>,
    layer_tess: Option<Tess<Backend, Sprite2dVertex>>,
}

impl ViewData {
    fn new(w: u32, h: u32, pixels: Option<&[Rgba8]>, ctx: &mut Context) -> Self {
        let mut staging_fb: Framebuffer<Backend, Dim2, pixel::SRGBA8UI, pixel::Depth32F> =
            Framebuffer::new(ctx, [w, h], 0, self::SAMPLER).unwrap();

        staging_fb
            .color_slot()
            .clear(GenMipmaps::No, (0, 0, 0, 0))
            .unwrap();

        Self {
            layer: LayerData::new(w, h, pixels, ctx),
            staging_fb,
            anim_tess: None,
            layer_tess: None,
        }
    }
}

struct Context {
    ctx: Backend,
}

unsafe impl GraphicsContext for Context {
    type Backend = self::Backend;

    fn backend(&mut self) -> &mut Self::Backend {
        &mut self.ctx
    }
}

impl Context {
    fn program<T>(&mut self, vert: &str, frag: &str) -> Program<Backend, VertexSemantics, (), T>
    where
        T: luminance::shader::UniformInterface<Backend>,
    {
        self.new_shader_program()
            .from_strings(vert, None, None, frag)
            .unwrap()
            .ignore_warnings()
    }

    fn tessellation<T, S>(&mut self, verts: &[T]) -> Tess<Backend, S>
    where
        S: luminance::vertex::Vertex + Sized,
    {
        let (head, body, tail) = unsafe { verts.align_to::<S>() };

        assert!(head.is_empty());
        assert!(tail.is_empty());

        TessBuilder::new(self)
            .set_vertices(body)
            .set_mode(Mode::Triangle)
            .build()
            .unwrap()
    }
}

#[derive(Debug)]
pub enum RendererError {
    Initialization,
    Texture(luminance::texture::TextureError),
    Framebuffer(luminance::framebuffer::FramebufferError),
    Pipeline(luminance::pipeline::PipelineError),
    State(luminance_gl::gl33::StateQueryError),
    InvalidArgument(String),
}

impl From<luminance::pipeline::PipelineError> for RendererError {
    fn from(other: luminance::pipeline::PipelineError) -> Self {
        Self::Pipeline(other)
    }
}

impl From<RendererError> for io::Error {
    fn from(err: RendererError) -> io::Error {
        io::Error::new(io::ErrorKind::Other, err)
    }
}

impl fmt::Display for RendererError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Self::Initialization => write!(f, "initialization error"),
            Self::Texture(e) => write!(f, "texture error: {}", e),
            Self::Framebuffer(e) => write!(f, "framebuffer error: {}", e),
            Self::Pipeline(e) => write!(f, "pipeline error: {}", e),
            Self::State(e) => write!(f, "state error: {}", e),
            Self::InvalidArgument(e) => write!(f, "invalid argument: {}", e),
        }
    }
}

impl Error for RendererError {
    fn description(&self) -> &str {
        "Renderer error"
    }

    fn cause(&self) -> Option<&dyn Error> {
        None
    }
}

impl<'a> renderer::Renderer<'a> for Renderer {
    type Error = RendererError;

    fn new(
        win: &mut platform::backend::Window,
        win_size: LogicalSize,
        scale_factor: f64,
        assets: Assets<'a>,
    ) -> io::Result<Self> {
        use RendererError as Error;

        gl::load_with(|s| win.get_proc_address(s) as *const _);

        let ctx = Backend::new().map_err(Error::State)?;
        let mut ctx = Context { ctx };

        let (font_img, font_w, font_h) = image::read(assets.glyphs)?;
        let (cursors_img, cursors_w, cursors_h) = image::read(data::CURSORS)?;
        let (checker_w, checker_h) = (2, 2);
        let (paste_w, paste_h) = (8, 8);

        let mut font =
            Texture::new(&mut ctx, [font_w, font_h], 0, self::SAMPLER).map_err(Error::Texture)?;
        let mut cursors = Texture::new(&mut ctx, [cursors_w, cursors_h], 0, self::SAMPLER)
            .map_err(Error::Texture)?;
        let paste =
            Texture::new(&mut ctx, [paste_w, paste_h], 0, self::SAMPLER).map_err(Error::Texture)?;
        let mut checker = Texture::new(&mut ctx, [checker_w, checker_h], 0, self::SAMPLER)
            .map_err(Error::Texture)?;

        font.upload_raw(GenMipmaps::No, &font_img)
            .map_err(Error::Texture)?;
        cursors
            .upload_raw(GenMipmaps::No, &cursors_img)
            .map_err(Error::Texture)?;
        checker
            .upload_raw(GenMipmaps::No, &draw::CHECKER)
            .map_err(Error::Texture)?;

        let sprite2d = ctx.program::<Sprite2dInterface>(
            include_str!("data/sprite.vert"),
            include_str!("data/sprite.frag"),
        );
        let shape2d = ctx.program::<Shape2dInterface>(
            include_str!("data/shape.vert"),
            include_str!("data/shape.frag"),
        );
        let cursor2d = ctx.program::<Cursor2dInterface>(
            include_str!("data/cursor.vert"),
            include_str!("data/cursor.frag"),
        );
        let screen2d = ctx.program::<Screen2dInterface>(
            include_str!("data/screen.vert"),
            include_str!("data/screen.frag"),
        );

        let physical = win_size.to_physical(scale_factor);
        let present_fb =
            Framebuffer::back_buffer(&mut ctx, [physical.width as u32, physical.height as u32])
                .map_err(Error::Framebuffer)?;
        let screen_fb = Framebuffer::new(
            &mut ctx,
            [win_size.width as u32, win_size.height as u32],
            0,
            self::SAMPLER,
        )
        .map_err(Error::Framebuffer)?;

        let render_st = RenderState::default()
            .set_blending(blending::Blending {
                equation: Equation::Additive,
                src: Factor::SrcAlpha,
                dst: Factor::SrcAlphaComplement,
            })
            .set_depth_test(Some(DepthComparison::LessOrEqual));
        let pipeline_st = PipelineState::default()
            .set_clear_color([0., 0., 0., 0.])
            .enable_srgb(true)
            .enable_clear_depth(true)
            .enable_clear_color(true);

        let draw_ctx = draw::Context {
            ui_batch: shape2d::Batch::new(),
            text_batch: self::text_batch(font.size()),
            overlay_batch: self::text_batch(font.size()),
            cursor_sprite: sprite::Sprite::new(cursors_w, cursors_h),
            tool_batch: sprite2d::Batch::new(cursors_w, cursors_h),
            paste_batch: sprite2d::Batch::new(paste_w, paste_h),
            checker_batch: sprite2d::Batch::new(checker_w, checker_h),
        };

        Ok(Renderer {
            ctx,
            draw_ctx,
            win_size,
            scale_factor,
            scale: 1.0,
            blending: Blending::Alpha,
            present_fb,
            screen_fb,
            render_st,
            pipeline_st,
            sprite2d,
            shape2d,
            cursor2d,
            screen2d,
            font,
            cursors,
            checker,
            paste,
            paste_outputs: Vec::new(),
            staging_batch: shape2d::Batch::new(),
            final_batch: shape2d::Batch::new(),
            view_data: BTreeMap::new(),
        })
    }

    fn init(&mut self, effects: Vec<Effect>, session: &mut Session) {
        self.handle_effects(effects, session).unwrap();
    }

    fn frame(
        &mut self,
        session: &mut Session,
        execution: &mut Execution,
        effects: Vec<session::Effect>,
        avg_frametime: &time::Duration,
    ) -> Result<(), RendererError> {
        if session.state != session::State::Running {
            return Ok(());
        }
        self.staging_batch.clear();
        self.final_batch.clear();

        self.handle_effects(effects, session).unwrap();
        self.update_view_animations(session);
        self.update_view_composites(session);

        let [screen_w, screen_h] = self.screen_fb.size();
        let ortho: M44 = Matrix4::ortho(screen_w, screen_h, Origin::TopLeft).into();
        let identity: M44 = Matrix4::identity().into();

        let Self {
            draw_ctx,
            font,
            cursors,
            checker,
            sprite2d,
            shape2d,
            cursor2d,
            screen2d,
            scale_factor,
            present_fb,
            blending,
            screen_fb,
            render_st,
            pipeline_st,
            paste,
            paste_outputs,
            view_data,
            ..
        } = self;

        draw_ctx.clear();
        draw_ctx.draw(session, avg_frametime, execution);

        let text_tess = self
            .ctx
            .tessellation::<_, Sprite2dVertex>(&draw_ctx.text_batch.vertices());
        let overlay_tess = self
            .ctx
            .tessellation::<_, Sprite2dVertex>(&draw_ctx.overlay_batch.vertices());
        let ui_tess = self
            .ctx
            .tessellation::<_, Shape2dVertex>(&draw_ctx.ui_batch.vertices());
        let tool_tess = self
            .ctx
            .tessellation::<_, Sprite2dVertex>(&draw_ctx.tool_batch.vertices());
        let cursor_tess = self
            .ctx
            .tessellation::<_, Cursor2dVertex>(&draw_ctx.cursor_sprite.vertices());
        let checker_tess = self
            .ctx
            .tessellation::<_, Sprite2dVertex>(&draw_ctx.checker_batch.vertices());
        let screen_tess = TessBuilder::<Backend, ()>::new(&mut self.ctx)
            .set_vertex_nb(6)
            .set_mode(Mode::Triangle)
            .build()
            .unwrap();

        let paste_tess = if draw_ctx.paste_batch.is_empty() {
            None
        } else {
            Some(
                self.ctx
                    .tessellation::<_, Sprite2dVertex>(&draw_ctx.paste_batch.vertices()),
            )
        };
        let staging_tess = if self.staging_batch.is_empty() {
            None
        } else {
            Some(
                self.ctx
                    .tessellation::<_, Shape2dVertex>(&self.staging_batch.vertices()),
            )
        };
        let final_tess = if self.final_batch.is_empty() {
            None
        } else {
            Some(
                self.ctx
                    .tessellation::<_, Shape2dVertex>(&self.final_batch.vertices()),
            )
        };

        let help_tess = if session.mode == session::Mode::Help {
            let mut win = shape2d::Batch::new();
            let mut text = self::text_batch(font.size());
            draw::draw_help(session, &mut text, &mut win);

            let win_tess = self
                .ctx
                .tessellation::<_, Shape2dVertex>(win.vertices().as_slice());
            let text_tess = self
                .ctx
                .tessellation::<_, Sprite2dVertex>(text.vertices().as_slice());
            Some((win_tess, text_tess))
        } else {
            None
        };

        let v = session
            .views
            .active()
            .expect("there must always be an active view");
        let v_data = view_data.get(&v.id).unwrap();
        let l_data = &v_data.layer;
        let view_ortho = Matrix4::ortho(v.width(), v.fh, Origin::TopLeft);

        let mut builder = self.ctx.new_pipeline_gate();

        // Render to view staging buffer.
        builder.pipeline::<PipelineError, _, _, _, _>(
            &v_data.staging_fb,
            pipeline_st,
            |pipeline, mut shd_gate| {
                // Render staged brush strokes.
                if let Some(tess) = staging_tess {
                    shd_gate.shade(shape2d, |mut iface, uni, mut rdr_gate| {
                        iface.set(&uni.ortho, view_ortho.into());
                        iface.set(&uni.transform, identity);

                        rdr_gate.render(render_st, |mut tess_gate| tess_gate.render(&tess))
                    })?;
                }
                // Render staging paste buffer.
                if let Some(tess) = paste_tess {
                    let bound_paste = pipeline
                        .bind_texture(paste)
                        .expect("binding textures never fails. qed.");
                    shd_gate.shade(sprite2d, |mut iface, uni, mut rdr_gate| {
                        iface.set(&uni.ortho, view_ortho.into());
                        iface.set(&uni.transform, identity);
                        iface.set(&uni.tex, bound_paste.binding());

                        rdr_gate.render(render_st, |mut tess_gate| tess_gate.render(&tess))
                    })?;
                }
                Ok(())
            },
        );

        // Render to view final buffer.
        builder.pipeline::<PipelineError, _, _, _, _>(
            &l_data.fb,
            &pipeline_st.clone().enable_clear_color(false),
            |pipeline, mut shd_gate| {
                let bound_paste = pipeline
                    .bind_texture(paste)
                    .expect("binding textures never fails. qed.");

                // Render final brush strokes.
                if let Some(tess) = final_tess {
                    shd_gate.shade(shape2d, |mut iface, uni, mut rdr_gate| {
                        iface.set(&uni.ortho, view_ortho.into());
                        iface.set(&uni.transform, identity);

                        let render_st = if blending == &Blending::Constant {
                            render_st.clone().set_blending(blending::Blending {
                                equation: Equation::Additive,
                                src: Factor::One,
                                dst: Factor::Zero,
                            })
                        } else {
                            render_st.clone()
                        };

                        rdr_gate.render(&render_st, |mut tess_gate| tess_gate.render(&tess))
                    })?;
                }
                if !paste_outputs.is_empty() {
                    shd_gate.shade(sprite2d, |mut iface, uni, mut rdr_gate| {
                        iface.set(&uni.ortho, view_ortho.into());
                        iface.set(&uni.transform, identity);
                        iface.set(&uni.tex, bound_paste.binding());

                        for out in paste_outputs.drain(..) {
                            rdr_gate.render(render_st, |mut tess_gate| tess_gate.render(&out))?;
                        }
                        Ok(())
                    })?;
                }
                Ok(())
            },
        );

        // Render to screen framebuffer.
        let bg = Rgba::from(session.get_color_setting(ColorSetting::UiBackground));
        let screen_st = &pipeline_st
            .clone()
            .set_clear_color([bg.r, bg.g, bg.b, bg.a]);
        builder.pipeline::<PipelineError, _, _, _, _>(
            screen_fb,
            screen_st,
            |pipeline, mut shd_gate| {
                // Draw view checkers to screen framebuffer.
                let checker_side = session.get_i64_setting(I64Setting::UiChecker);
                if checker_side > 0 {
                    shd_gate.shade(sprite2d, |mut iface, uni, mut rdr_gate| {
                        let bound_checker = pipeline
                            .bind_texture(checker)
                            .expect("binding textures never fails");

                        iface.set(&uni.ortho, ortho);
                        iface.set(&uni.transform, identity);
                        iface.set(&uni.tex, bound_checker.binding());

                        rdr_gate.render(render_st, |mut tess_gate| tess_gate.render(&checker_tess))
                    })?;
                }

                for (id, v) in view_data.iter_mut() {
                    if let Some(view) = session.views.get(*id) {
                        let transform = Matrix4::from_translation(
                            (session.offset + view.offset).extend(*draw::VIEW_LAYER),
                        ) * Matrix4::from_nonuniform_scale(
                            view.zoom as f32,
                            view.zoom as f32,
                            1.0,
                        );

                        // Render views.
                        shd_gate.shade(sprite2d, |mut iface, uni, mut rdr_gate| {
                            let bound_view = pipeline
                                .bind_texture(v.layer.fb.color_slot())
                                .expect("binding textures never fails");

                            iface.set(&uni.ortho, ortho);
                            iface.set(&uni.transform, transform.into());
                            iface.set(&uni.tex, bound_view.binding());

                            rdr_gate.render(render_st, |mut tess_gate| {
                                tess_gate.render(&v.layer.tess)
                            })?;

                            // TODO: We only need to render this on the active view.
                            let staging_texture = v.staging_fb.color_slot();
                            let bound_view_staging = pipeline
                                .bind_texture(staging_texture)
                                .expect("binding textures never fails");

                            iface.set(&uni.tex, bound_view_staging.binding());
                            rdr_gate.render(render_st, |mut tess_gate| {
                                tess_gate.render(&v.layer.tess)
                            })?;

                            Ok(())
                        })?;
                    }
                }

                // Render UI.
                shd_gate.shade(shape2d, |mut iface, uni, mut rdr_gate| {
                    iface.set(&uni.ortho, ortho);
                    iface.set(&uni.transform, identity);

                    rdr_gate.render(render_st, |mut tess_gate| tess_gate.render(&ui_tess))
                })?;

                // Render text, tool & view animations.
                shd_gate.shade(sprite2d, |mut iface, uni, mut rdr_gate| {
                    iface.set(&uni.ortho, ortho);
                    iface.set(&uni.transform, identity);

                    // Render view animations, but only for the active view.
                    let view = session.active_view();
                    if session.get_i64_setting(I64Setting::UiAnimate) != 0
                        && view.animation.len() > 1
                    {
                        if let Some(v) = view_data.get_mut(&view.id) {
                            if let Some(tess) = &v.anim_tess {
                                let bound_layer = pipeline
                                    .bind_texture(v.layer.fb.color_slot())
                                    .expect("binding textures never fails");
                                let zoom = view.zoom as f32;
                                let anim_width = zoom * view.fw as f32;
                                let anim_height = zoom * view.height() as f32;
                                let anim_y = session.offset.y + view.offset.y;
                                /* TODO: for some reason this code doesn't work well in certain zooms?
                                let anim_y = if anim_height
                                    > session.height - 2.0 * session.palette.cellsize
                                {
                                    // Show to the right of the current view:
                                    session.offset.y
                                } else {
                                    // Show at the top of the screen.
                                    session.height - session.palette.cellsize - anim_height
                                };
                                */
                                // TODO: we could actually determine this correctly.
                                let rough_palette_width = 2.0 * session.palette.cellsize;
                                // TODO: this heuristic isn't great but it's ok for
                                //       switching between left and right.  improve:
                                let anim_x = if view.offset.x * zoom
                                    > view.fw as f32 + rough_palette_width
                                {
                                    // Show on the left, give room for 32 palette colors
                                    rough_palette_width + anim_width
                                } else {
                                    // Show to the right
                                    session.width - session.palette.cellsize
                                };
                                let t =
                                    Matrix4::from_translation(Vector3::new(anim_x, anim_y, 0.0));

                                // Render layer animation.
                                // TODO: add a session.ui-bg square behind the animation in case it overlaps
                                //       with later frames.
                                iface.set(&uni.tex, bound_layer.binding());
                                iface.set(&uni.transform, t.into());
                                rdr_gate
                                    .render(render_st, |mut tess_gate| tess_gate.render(tess))?;
                            }
                        }
                    }

                    {
                        let bound_font = pipeline
                            .bind_texture(font)
                            .expect("binding textures never fails");
                        iface.set(&uni.tex, bound_font.binding());
                        iface.set(&uni.transform, identity);

                        // Render text.
                        rdr_gate.render(render_st, |mut tess_gate| tess_gate.render(&text_tess))?;
                    }
                    {
                        let bound_tool = pipeline
                            .bind_texture(cursors)
                            .expect("binding textures never fails");
                        iface.set(&uni.tex, bound_tool.binding());

                        // Render tool.
                        rdr_gate.render(render_st, |mut tess_gate| tess_gate.render(&tool_tess))?;
                    }
                    Ok(())
                })?;

                // Render help.
                if let Some((win_tess, text_tess)) = help_tess {
                    shd_gate.shade(shape2d, |_iface, _uni, mut rdr_gate| {
                        rdr_gate.render(render_st, |mut tess_gate| tess_gate.render(&win_tess))
                    })?;
                    shd_gate.shade(sprite2d, |mut iface, uni, mut rdr_gate| {
                        let bound_font = pipeline
                            .bind_texture(font)
                            .expect("binding textures never fails");

                        iface.set(&uni.tex, bound_font.binding());
                        iface.set(&uni.ortho, ortho);
                        iface.set(&uni.transform, identity);

                        rdr_gate.render(render_st, |mut tess_gate| tess_gate.render(&text_tess))
                    })?;
                }
                Ok(())
            },
        );

        // Render to back buffer.
        builder.pipeline::<PipelineError, _, _, _, _>(
            present_fb,
            pipeline_st,
            |pipeline, mut shd_gate| {
                // Render screen framebuffer.
                let bound_screen = pipeline
                    .bind_texture(screen_fb.color_slot())
                    .expect("binding textures never fails");
                shd_gate.shade(screen2d, |mut iface, uni, mut rdr_gate| {
                    iface.set(&uni.framebuffer, bound_screen.binding());

                    rdr_gate.render(render_st, |mut tess_gate| tess_gate.render(&screen_tess))
                })?;

                if session.get_i64_setting(I64Setting::Debug) != 0 || !execution.is_normal() {
                    let bound_font = pipeline
                        .bind_texture(font)
                        .expect("binding textures never fails");

                    shd_gate.shade(sprite2d, |mut iface, uni, mut rdr_gate| {
                        iface.set(&uni.tex, bound_font.binding());
                        iface.set(
                            &uni.ortho,
                            Matrix4::ortho(screen_w, screen_h, Origin::BottomLeft).into(),
                        );

                        rdr_gate.render(render_st, |mut tess_gate| tess_gate.render(&overlay_tess))
                    })?;
                }

                // Render cursor.
                let bound_cursors = pipeline
                    .bind_texture(cursors)
                    .expect("binding textures never fails");
                shd_gate.shade(cursor2d, |mut iface, uni, mut rdr_gate| {
                    let ui_scale =
                        session.get_i64_setting(I64Setting::UiScalePercentage) as f64 / 100.0;
                    let pixel_ratio = platform::pixel_ratio(*scale_factor);

                    iface.set(&uni.cursor, bound_cursors.binding());
                    iface.set(&uni.framebuffer, bound_screen.binding());
                    iface.set(&uni.ortho, ortho);
                    iface.set(&uni.scale, (ui_scale * pixel_ratio) as f32);

                    rdr_gate.render(render_st, |mut tess_gate| tess_gate.render(&cursor_tess))
                })
            },
        );

        // If active view is dirty, record a snapshot of it.
        if v.is_dirty() {
            // FIXME: This is ugly.
            let id = v.id;
            let state = v.state;
            let is_resized = v.is_resized();
            let extent = v.extent();

            if let Some(vr) = session.views.get_mut(id) {
                let v_data = view_data.get_mut(&id).unwrap();

                match state {
                    ViewState::Dirty(_) if is_resized => {
                        vr.record_view_resized(v_data.layer.pixels(), extent);
                    }
                    ViewState::Dirty(_) => {
                        vr.record_view_painted(v_data.layer.pixels());
                    }
                    ViewState::Okay | ViewState::Damaged(_) => {}
                }
            }
        }

        if !execution.is_normal() {
            let texels = screen_fb
                .color_slot()
                .get_raw_texels()
                .expect("binding textures never fails");
            let texels = Rgba8::align(&texels);

            execution.record(texels).ok();
        }

        Ok(())
    }

    fn handle_scale_factor_changed(&mut self, scale_factor: f64) {
        self.scale_factor = scale_factor;
        self.handle_resized(self.win_size);
    }
}

impl Renderer {
    pub fn handle_resized(&mut self, size: platform::LogicalSize) {
        let physical = size.to_physical(self.scale_factor);

        self.present_fb = Framebuffer::back_buffer(
            &mut self.ctx,
            [physical.width as u32, physical.height as u32],
        )
        .expect("binding textures never fails");

        self.win_size = size;
        self.handle_session_scale_changed(self.scale);
    }

    pub fn handle_session_scale_changed(&mut self, scale: f64) {
        self.scale = scale;
        self.screen_fb = Framebuffer::new(
            &mut self.ctx,
            [
                (self.win_size.width / scale) as u32,
                (self.win_size.height / scale) as u32,
            ],
            0,
            self::SAMPLER,
        )
        .unwrap();
    }

    fn handle_effects(
        &mut self,
        mut effects: Vec<Effect>,
        session: &mut Session,
    ) -> Result<(), RendererError> {
        for eff in effects.drain(..) {
            match eff {
                Effect::SessionResized(size) => {
                    self.handle_resized(size);
                }
                Effect::SessionScaled(scale) => {
                    self.handle_session_scale_changed(scale);
                }
                Effect::ViewActivated(_) => {}
                Effect::ViewAdded(id) => {
                    // FIXME: This should be done when the view is added in the ViewManager.
                    if let Some((s, pixels)) = session.views.get_snapshot_safe(id) {
                        let (w, h) = (s.width(), s.height());

                        self.view_data
                            .insert(id, ViewData::new(w, h, Some(pixels), &mut self.ctx));
                    }
                }
                Effect::ViewRemoved(id) => {
                    self.view_data.remove(&id);
                }
                Effect::ViewOps(id, ops) => {
                    self.handle_view_ops(session.view(id), &ops)?;
                }
                Effect::ViewDamaged(id, Some(extent)) => {
                    self.handle_view_resized(session.view(id), extent.width(), extent.height())?;
                }
                Effect::ViewDamaged(id, None) => {
                    self.handle_view_damaged(session.view(id))?;
                }
                Effect::ViewBlendingChanged(blending) => {
                    self.blending = blending;
                }
                Effect::ViewPaintDraft(shapes) => {
                    shapes.into_iter().for_each(|s| self.staging_batch.add(s));
                }
                Effect::ViewPaintFinal(shapes) => {
                    shapes.into_iter().for_each(|s| self.final_batch.add(s));
                }
                Effect::ViewTouched(_) => {}
            }
        }
        Ok(())
    }

    fn handle_view_ops(
        &mut self,
        v: &View<ViewResource>,
        ops: &[ViewOp],
    ) -> Result<(), RendererError> {
        use RendererError as Error;

        for op in ops {
            match op {
                ViewOp::Resize(w, h) => {
                    self.resize_view(v, *w, *h)?;
                }
                ViewOp::Clear(color) => {
                    let view = self
                        .view_data
                        .get_mut(&v.id)
                        .expect("views must have associated view data");

                    view.layer
                        .fb
                        .color_slot()
                        .clear(GenMipmaps::No, (color.r, color.g, color.b, color.a))
                        .map_err(Error::Texture)?;
                }
                ViewOp::ClearRect(color, rect) => {
                    self.clear_rect(v, *color, *rect);
                }
                ViewOp::Blit(src, dst) => {
                    assert!(src.width() == dst.width());
                    assert!(src.height() == dst.height());

                    let view = self
                        .view_data
                        .get_mut(&v.id)
                        .expect("views must have associated view data");

                    let (_, texels) = v.layer.get_snapshot_rect(&src.map(|n| n as i32)).unwrap(); // TODO: Handle this nicely?
                    let texels = util::align_u8(&texels);

                    view.layer
                        .fb
                        .color_slot()
                        .upload_part_raw(
                            GenMipmaps::No,
                            [dst.x1, dst.y1],
                            [src.width(), src.height()],
                            texels,
                        )
                        .map_err(Error::Texture)?;
                }
                ViewOp::Flip(src, dir) => {
                    let (_, mut pixels) = v.layer.get_snapshot_rect(&src.map(|n| n)).unwrap();
                    let (w, h) = (src.width() as u32, src.height() as u32);
                    let [paste_w, paste_h] = self.paste.size();

                    if paste_w != w || paste_h != h {
                        self.paste = Texture::new(&mut self.ctx, [w, h], 0, self::SAMPLER)
                            .map_err(Error::Texture)?;
                    }

                    match dir {
                        Axis::Vertical => {
                            let len = pixels.len();

                            let (front, back) = pixels.split_at_mut(len / 2);
                            for (front_row, back_row) in front
                                .chunks_exact_mut(w as usize)
                                .zip(back.rchunks_exact_mut(w as usize))
                            {
                                front_row.swap_with_slice(back_row);
                            }
                        }
                        Axis::Horizontal => {
                            pixels
                                .chunks_exact_mut(w as usize)
                                .for_each(|row| row.reverse());
                        }
                    }

                    let body = util::align_u8(&pixels);

                    self.paste
                        .upload_raw(GenMipmaps::No, body)
                        .map_err(Error::Texture)?;
                }
                ViewOp::Copy(src) => {
                    let (_, pixels) = v.layer.get_snapshot_rect(src).unwrap();
                    let (w, h) = (src.width() as u32, src.height() as u32);
                    let [paste_w, paste_h] = self.paste.size();

                    if paste_w != w || paste_h != h {
                        self.paste = Texture::new(&mut self.ctx, [w, h], 0, self::SAMPLER)
                            .map_err(Error::Texture)?;
                    }
                    let body = util::align_u8(&pixels);

                    self.paste
                        .upload_raw(GenMipmaps::No, body)
                        .map_err(Error::Texture)?;
                }
                ViewOp::Paste(dst) => {
                    let [paste_w, paste_h] = self.paste.size();

                    let batch = sprite2d::Batch::singleton(
                        paste_w,
                        paste_h,
                        Rect::origin(paste_w as f32, paste_h as f32),
                        dst.map(|n| n as f32),
                        ZDepth::default(),
                        Rgba::TRANSPARENT,
                        1.,
                        Repeat::default(),
                    );

                    self.paste_outputs.push(
                        self.ctx
                            .tessellation::<_, Sprite2dVertex>(batch.vertices().as_slice()),
                    );
                }
                ViewOp::SwapCut(mut paste_rect, mut cut_rect) => {
                    // TODO: ensure that paste_rect has the same size as existing pixels.
                    let (_, pixels) = v
                        .layer
                        .get_snapshot_rect(&cut_rect.map(|n| n as i32))
                        .unwrap();
                    let (w, h) = (cut_rect.width() as u32, cut_rect.height() as u32);

                    let mut swap_paste = Texture::new(&mut self.ctx, [w, h], 0, self::SAMPLER)
                        .map_err(Error::Texture)?;
                    swap_paste
                        .upload_raw(GenMipmaps::No, util::align_u8(&pixels))
                        .map_err(Error::Texture)?;

                    let (y1, y2) = (cut_rect.y1, cut_rect.y2);
                    // source y is swapped in textures.  (y up is positive)
                    cut_rect.y1 = v.fh - y2;
                    cut_rect.y2 = v.fh - y1;
                    self.clear_rect(v, Rgba8::TRANSPARENT, cut_rect);

                    let (y1, y2) = (paste_rect.y1, paste_rect.y2);
                    // source y is swapped in textures.  (y up is positive)
                    paste_rect.y1 = v.fh as i32 - y2;
                    paste_rect.y2 = v.fh as i32 - y1;
                    // Need to paste from `self.paste` now and not async in `self.paste_outputs`.
                    std::mem::swap(&mut self.paste, &mut swap_paste);
                    self.paste_now(swap_paste, v, paste_rect);
                }
                ViewOp::SetPixel(rgba, x, y) => {
                    let fb = &mut self
                        .view_data
                        .get_mut(&v.id)
                        .expect("views must have associated view data")
                        .layer
                        .fb;
                    let texels = &[*rgba];
                    let texels = util::align_u8(texels);
                    fb.color_slot()
                        .upload_part_raw(GenMipmaps::No, [*x as u32, *y as u32], [1, 1], texels)
                        .map_err(Error::Texture)?;
                }
            }
        }
        Ok(())
    }

    fn clear_rect(
        &mut self,
        v: &View<ViewResource>,
        color: Rgba8,
        rect: Rect<u32>,
    ) -> Result<(), RendererError> {
        let view = self
            .view_data
            .get_mut(&v.id)
            .expect("views must have associated view data");

        // TODO: this seems inefficient but haven't figured out the luminance
        //      API to see how to clear a texture in a given region.
        let texels =
            vec![(color.r, color.g, color.b, color.a); (rect.width() * rect.height()) as usize];
        let texels = util::align_u8(&texels);

        view.layer
            .fb
            .color_slot()
            .upload_part_raw(
                GenMipmaps::No,
                [rect.x1, rect.y1],
                [rect.width(), rect.height()],
                texels,
            )
            .map_err(RendererError::Texture)?;
        Ok(())
    }

    fn paste_now(
        &mut self,
        to_paste: SrgbaTexture,
        v: &View<ViewResource>,
        paste_rect: Rect<i32>,
    ) -> Result<(), RendererError> {
        let [paste_w, paste_h] = to_paste.size();
        let src_rect = Rect::origin(paste_w as i32, paste_h as i32);
        let width = v.width();
        let (src_rect, paste_rect) = ensure_within(width, v.fh, src_rect, paste_rect);
        // The src_rect was (0,0) to (width, fh) so we can convert to usize:
        let src_rect = src_rect.map(|n| n as usize);
        let src_texels = to_paste.get_raw_texels().map_err(RendererError::Texture)?;
        let mut texel_vec = Vec::with_capacity(4 * src_rect.area());

        for y in src_rect.y1..src_rect.y2 {
            let row_offset = y * paste_w as usize;
            let z1 = row_offset + src_rect.x1;
            let z2 = row_offset + src_rect.x2;
            // Raw texels don't take into account each channel, so multiply
            // by the channel count here (RGBA = 4)
            texel_vec.extend_from_slice(&src_texels[4 * z1 as usize..4 * z2 as usize]);
        }
        let src_texels = util::align_u8(&texel_vec);

        self.view_data
            .get_mut(&v.id)
            .expect("views must have associated view data")
            .layer
            .fb
            .color_slot()
            .upload_part_raw(
                GenMipmaps::No,
                [paste_rect.x1, paste_rect.y1],
                [paste_rect.width(), paste_rect.height()],
                src_texels,
            )
            .map_err(RendererError::Texture)
    }

    fn handle_view_damaged(&mut self, view: &View<ViewResource>) -> Result<(), RendererError> {
        let layer = &mut self
            .view_data
            .get_mut(&view.id)
            .expect("views must have associated view data")
            .layer;

        let (_, pixels) = view.layer.current_snapshot();

        layer.clear()?;
        layer.upload(util::align_u8(pixels))?;

        Ok(())
    }

    fn handle_view_resized(
        &mut self,
        view: &View<ViewResource>,
        vw: u32,
        vh: u32,
    ) -> Result<(), RendererError> {
        self.resize_view(view, vw, vh)
    }

    fn resize_view(
        &mut self,
        view: &View<ViewResource>,
        vw: u32,
        vh: u32,
    ) -> Result<(), RendererError> {
        // View size changed. Re-create view resources.
        let (ew, eh) = {
            let extent = view.resource.extent;
            (extent.width(), extent.height())
        };

        // Ensure not to transfer more data than can fit in the view buffer.
        let tw = u32::min(ew, vw);
        let th = u32::min(eh, vh);

        let mut view_data = ViewData::new(vw, vh, None, &mut self.ctx);
        let trect = Rect::origin(tw as i32, th as i32);
        // The following sequence of commands will try to copy a rect that isn't contained
        // in the snapshot, hence we must skip the uploading in that case:
        //
        //     :f/add
        //     :f/remove
        //     :undo
        //
        if let Some((_, texels)) = view.layer.get_snapshot_rect(&trect) {
            let texels = util::align_u8(&texels);
            let l = &mut view_data.layer;

            l.upload_part([0, vh - th], [tw, th], texels)?;
        }

        self.view_data.insert(view.id, view_data);

        Ok(())
    }

    fn update_view_animations(&mut self, s: &Session) {
        if s.get_i64_setting(I64Setting::UiAnimate) == 0 {
            return;
        }
        // TODO: Does this need to run if the view has only one frame?
        // Only update animation for the active view.
        let v = s.active_view();
        // FIXME: When `v.animation.val()` doesn't change, we don't need
        // to re-create the buffer.
        let batch = draw::draw_view_animation(v);

        if let Some(vd) = self.view_data.get_mut(&v.id) {
            vd.anim_tess = Some(
                self.ctx
                    .tessellation::<_, Sprite2dVertex>(batch.vertices().as_slice()),
            );
        }
    }

    fn update_view_composites(&mut self, s: &Session) {
        for v in s.views.iter() {
            let batch = draw::draw_view_composites(s, v);

            if let Some(vd) = self.view_data.get_mut(&v.id) {
                vd.layer_tess = Some(
                    self.ctx
                        .tessellation::<_, Sprite2dVertex>(batch.vertices().as_slice()),
                );
            }
        }
    }
}

fn text_batch([w, h]: [u32; 2]) -> TextBatch {
    TextBatch::new(w, h, draw::GLYPH_WIDTH, draw::GLYPH_HEIGHT)
}
