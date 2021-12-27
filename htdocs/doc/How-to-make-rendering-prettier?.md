This page is the opposite of the manual page [How to optimize your games](https://castle-engine.io/manual_optimization.php) :)

* Use glTF, which by default does _PBR (Physical-Based Rendering)_. You can export it from [Blender](https://castle-engine.io/creating_data_blender.php) out-of-the-box.

    Some information about PBR, and how it is done in X3D in a way compatible with glTF, is available here: https://github.com/michaliskambi/x3d-tests/wiki/Include-PBR-%28PhysicalMaterial-and-related-concepts%29-in-the-official-X3D-specification .

* Use bump mapping. In Blender, just set a normalmap texture (texture that affects normal vectors) and export to glTF, it just works.

    Internally: use bump mapping by `Material.normalTexture` or `PhysicalMaterial.normalTexture` X3D fields. Deprecated: it is also possible by `CommonSurfaceShader.normalMap` or `Appearance.normalMap`.

    Deprecated: See [ComonSurfaceShader](http://castle-engine.io/x3d_implementation_texturing_extensions.php#section_ext_common_surface_shader) documentation. Our [Blender exporter](http://castle-engine.io/creating_data_blender.php) can export X3D or castle-anim-frames files with the `CommonSurfaceShader` node set up.

* Use `Material` (non-PBR) or `PhysicalMaterial` (PBR) in [X3D version 4](https://github.com/michaliskambi/x3d-tests/wiki/X3D-version-4:-New-features-of-materials,-lights-and-textures) that adds various optional texture slots to affect various additional material parameters (e.g. specular texture in case of Phong, or metallic-roughness texture in case of PBR). 

* Set shading to Phong. You can [set shading per-shape](http://castle-engine.io/x3d_implementation_shape_extensions.php#section_ext_shading), but usually it's easier to just set `Scene.Attributes.PhongShading := true` for the whole scene (TCastleScene instance).

    Note that Phong shading is activated automatically (for a particular shape) when you use PBR, bump mapping or shadow maps or some other engine effect. So you may not need to actually request it explicitly.

* Use post-processing, called "screen effects" in our engine: https://castle-engine.io/x3d_extensions_screen_effects.php

* Use engine effects like [mirrors](https://castle-engine.io/x3d_extensions_mirror_plane.php) and [shadows](https://castle-engine.io/x3d_extensions_shadow_maps.php).

* Use animation blending when appropriate. To do this, set `DefaultAnimationTransition` to non-zero on your scene, or use `PlayAnimation` with `TPlayAnimationParameters.TransitionDuration` set as non-zero.

* Use [gamma correction](https://castle-engine.io/manual_gamma_correction.php). It is automatically used with  `PhysicalMaterial`, so it is automatically used with glTF models using PBR. You can set `GammaCorrection := gcAlways` to just use it always (also with Phong lighting or unlit materials).

    This makes calculations more correct with respect to reality. This should be turned "on" from the begging of working on your graphic assets -- as it necessarily affects to colors of everything.

* Also try and use one of the [tone mapping](https://castle-engine.io/apidoc-unstable/html/CastleRendererBaseTypes.html#ToneMapping) options if you feel they improve your colors.

* You can increase the number of allowed lights per shape using <code>Scene.Attributes.MaxLightsPerShape</code>. <a href="https://castle-engine.io/view3dscene.php">view3dscene</a> has a menu item to experiment with it (<em>"View -> Max Lights Per Shape..."</em>). A test model is in <a href="https://github.com/castle-engine/demo-models/tree/master/gltf/multiple_animated_lights">demo-models: gltf/multiple_animated_lights</a>.