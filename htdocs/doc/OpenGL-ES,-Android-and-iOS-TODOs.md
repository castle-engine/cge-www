Here we list the features not (yet!) implemented in [Castle Game Engine](https://castle-engine.sourceforge.io/) for OpenGLES (which is used by default on Android and iOS, although can be tested on other platforms too), but already implemented for OpenGL (used by default on desktops).

Note that, except issues listed below, **everything else that works on desktops (Linux, Windows, MacOSX) should also work flawlessly on OpenGLES (Android, iOS)**! See [the engine features](https://castle-engine.sourceforge.io/features.php) :)

**Contributions to fix/implement missing features below are very welcome!** We tried to give below a detailed overview of what is needed for a particular feature, to make it easier to jump in and help :)

OpenGLES renderer
====

1. (*Easy*) **Shadow volumes**. Initializing stencil buffer is not a problem on OpenGLES. We need to 
    1. fix [castleshapeinternalrendershadowvolumes.pas](https://github.com/castle-engine/castle-engine/blob/master/src/x3d/opengl/castleshapeinternalrendershadowvolumes.pas) to use VBOs and shaders (right now it uses old fixed-function for rendering). It should be changed to use VBOs and `simplest.vs` / `simplest.fs` shaders, I advice looking at [castlesceneinternalocclusion.pas](https://github.com/castle-engine/castle-engine/blob/master/src/x3d/opengl/castlesceneinternalocclusion.pas) that does this correctly now.
    2. later, you may also fix TGLRenderToTexture to honour PackedStencilDepth for OpenGLES (to be able to use FBO with stencil, for screen effects and cubemap mirrors).

    In progress now: https://github.com/castle-engine/castle-engine/pull/312

1. (*Very easy*) **3D textures**. They are part of OpenGLES 3.0 standard, but are also available on some OpenGLES 2.0 devices through extension. The existing OpenGL code for them should work, just some constants need to be adjusted and tested. A demos of 3D textures can be found in [demo-models/texturing_advanced](https://github.com/castle-engine/demo-models/tree/master/texturing_advanced).

1. (*Very easy*) **Occlusion query**. The implementation in [castlesceneinternalocclusion.pas](https://github.com/castle-engine/castle-engine/blob/master/src/x3d/opengl/castlesceneinternalocclusion.pas) is mostly valid for OpenGLES already: it uses VBOs and shaders for rendering. However, it needs fixing to use triangles instead of quads (OpenGLES does not support GL_QUAD primitive). Also, we should use [EXT_occlusion_query_boolean](https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_occlusion_query_boolean.txt) that has a chance to exist on OpenGLES.

1. (*Medium*) **Wireframe rendering**. Since `glPolygonMode` is not available at all on OpenGLES, we need to be able to generate wireframe geometry arrays on CPU, adding a "wireframe" option to `CastleArraysGenerator` that will generate a different (line) primitive for TGeometryArrays. Then the renderer can use such TGeometryArrays for OpenGLES.

    This is started now, by having <code>Shape.shading = "WIREFRAME"</code> option, see [Shape.shading field](https://castle-engine.sourceforge.io/x3d_implementation_shape_extensions.php#section_ext_shading). We need to 
    * make it available for *all* geometry shapes
    * make this feature more flexible, so that the renderer can switch between wireframe/non-wireframe rendering without any cost.
    * And then we can implement Attribute.WireframeEffect on OpenGLES correctly,
    * and allow to set Wireframe on specific viewports (so that some viewports may view wireframe, independent of others) on OpenGLES.

Other missing features on Android / iOS
====

* On-screen keyboard on iOS is not available yet. On Android it's partially working since CGE 6.5 (we can show the keyboard, but we don't process keys yet).

* TGLImage.GetContents is not available on OpenGLES, as it doesn't support glGetTexImage. Fixing it is a matter of implementing alternative way that draws texture to FBO and grabs using SaveScreenGL (using glReadPixels internally).

    See https://discord.com/channels/389676745957310465/389676745957310467/851790467766026280 , 

    """"
    It is however possible to implement this operation on mobile, just a little bit differently. One has to use TGLRenderToTexture to render quad with a texture (making sure the framebuffer size, quad size, and texture size match, and  without filtering) and then use glReadPixels to get the pixels from FBO. This will work on mobile (and also on desktops, although it will be less optimal alternative to glGetTexImage).
    """