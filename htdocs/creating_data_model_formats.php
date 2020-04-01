<?php
require_once 'castle_engine_functions.php';
creating_data_header('Supported model formats');

$toc = new TableOfContents(
  array(
    new TocItem('Best formats to use', 'best'),
      new TocItem('glTF 2.0', 'gltf', 1),
      new TocItem('Spine JSON', 'spine', 1),
      new TocItem('X3D and VRML', 'x3d', 1),
      new TocItem('Castle Animation Frames (castle-anim-frames) format', 'castle_anim_frames', 1),
    new TocItem('Other formats you can use', 'other'),
      new TocItem('Animation through a series of static models', 'animation_counter', 1),
      new TocItem('Collada', 'collada', 1),
      new TocItem('OpenInventor', 'open_inventor', 1),
      new TocItem('3DS', '3ds', 1),
      new TocItem('MD3', 'md3', 1),
      new TocItem('Wavefront OBJ', 'wavefront_obj', 1),
      new TocItem('STL', 'stl', 1),
      new TocItem('Videoscape GEO', '', 1),
  )
);
?>

<p>The following 3D and 2D model formats are supported by <i>Castle Game Engine</i>.
They can be loaded e.g. <a href="manual_load_3d.php">using
the TCastleScene.Load</a> method.
You can also <a href="view3dscene.php">open them in view3dscene</a>.

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

<p>An efficient, modern format, developed by Khronos. Resources:

<ul>
  <li><p><a href="https://www.khronos.org/gltf">glTF overview</a>,
  <li><p><a href="https://github.com/KhronosGroup/glTF">glTF specification and extensions</a>,
  <li><p><a href="https://github.com/KhronosGroup/glTF-Sample-Models">glTF sample models</a> (open them with <a href="http://michalis.ii.uni.wroc.pl/view3dscene-snapshots/">view3dscene from snapshots right now</a>),
  <li><p><a href="https://www.blender.org/">Blender</a> includes a full-featured glTF exporter (see <a href="https://docs.blender.org/manual/en/dev/addons/io_gltf2.html">manual about using Blender glTF exporter</a>).

<?php /* Now we support skinning, which means below problem can be workarounded using skin.

    <p>Note that (unfortunatately) it seems not possible to export a single animation like "walk" that animates transformations of multiple Blender objects. That's a consequence of how the "actions" in Blender work ("action" is a set of animation curves, and using the same action for multiple objects forces them to share the animation curves &mdash; which may not be what you want). You can workaround it by <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/animations/simultaneous_animations_one_scene">running muliple animations simultaneously from CGE</a>, but this is less comfortable than calling <code>Scene.PlayAnimation('walk')</code>.
    */ ?>
</ul>

<p>Supported features of glTF models in our engine:

<ul>
  <li><p>Meshes (polygons, lines), transformation hierarchy.
  <li><p>Materials (with physically-based or unlit shading), alpha mode, double-sidedness, per-vertex colors.
  <li><p>Texturing (for base color, normal maps, emissive, material-roughness).
  <li><p>Cameras (perspective and orthogonal).
  <li><p>Animating transformations (position, rotation, scale) and using skin ("armature" in Blender). They can be played using <a href="manual_scene.php#section_play_animation">standard CGE <code>PlayAnimation</code> method</a> (or <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/animations/simultaneous_animations_one_scene">other means, e.g. to play multiple animations from one model simultaneously</a>).
  <li><p>Both <code>.glb</code> and <code>.gltf</code> extensions are supported. Textures can be provided in separate files or embedded inside the GLTF stream.
  <li><p>It is integrated in our engine as X3D nodes graph. This means that you can include a glTF model inside larger X3D file using the <code>Inline</code> node, you can modify the glTF scene graph at runtime (e.g. modify material color, show/hide something etc.) and you can serialize the scene graph to an X3D file.
  <li><p>We use <a href="https://github.com/BeRo1985/pasgltf/">PasGLTF</a>, a great open-source library for reading glTF by <a href="https://www.patreon.com/bero">Benjamin "Bero" Rosseaux</a>.
</ul>

<p>TODO: Main missing feature is morph targets.

<?php echo $toc->html_section(); ?>

<a href="http://esotericsoftware.com/">Spine</a> is a powerful program
for 2D game skeletal animations.
<a href="creating_data_dragon_bones.php">Dragon Bones</a> can also export
to this format.
</p>

<p><a href="https://github.com/castle-engine/castle-engine/wiki/Spine">We have a big support for Spine JSON features</a>,
and our friendly game studio <a href="http://cat-astrophe-games.com/">Cat-astrophe Games</a>
is using Spine for all 2D games.

<?php echo $toc->html_section(); ?>

<p><?php echo a_href_page('X3D and VRML', 'vrml_x3d'); ?> is a flexible
and powerful format for 3D models and a basis of our scene graph.

<p>The following extensions are recognized:

<ul>
  <li><p>X3D in classic and XML encoding:
    <code>.x3d</code>, <code>.x3dz</code>, <code>.x3d.gz</code>,
    <code>.x3dv</code>, <code>.x3dvz</code>, <code>.x3dv.gz</code>

  <li><p>VRML 1.0 and 2.0:
    <code>.wrl</code>, <code>.wrz</code> and <code>.wrl.gz</code>.
</ul>

<!-- p>Almost complete VRML&nbsp;1.0 support is done.
VRML&nbsp;2.0 (aka VRML&nbsp;97) and X3D support is also quite advanced,
a lot of nodes and features are implemented (including
advanced texturing and GLSL shaders,
<code>PROTO</code> and <code>EXTERNPROTO</code> support,
events mechanism with routes, sensors and interpolators).
-->

<!-- Among things that work are embedded textures,
semi-transparent materials and semi-transparent textures,
automatic normals smoothing (based on <code>creaseAngle</code>),
triangulating non-convex faces, understanding camera nodes,
WWWInline handling, text rendering and more. -->

<p>We have a great support for X3D and VRML,
and handle rendering, animation, interaction, scripts, shaders
and more features of these formats.
An entire section of this website,
<?php echo a_href_page('Scene Graph (X3D)', 'vrml_x3d'); ?>,
documents all the features we support (from the X3D and VRML standard,
and many of our own extensions).

<!-- <p><i>If your authoring software -->
<!-- can export to X3D, this is the format you should probably use.</i> -->

<?php echo $toc->html_section(); ?>

<p><?php echo a_href_page('Castle Animation Frames
  (castle-anim-frames) format', 'castle_animation_frames'); ?></b>,
 formerly known as <code>kanim</code>.

<p>This is a simple format for animations, used
to <a href="creating_data_blender.php">export animated models from Blender</a>.
It supports animating of <i>everything</i> in Blender (transformations, skin,
shape keys, materials, particles, physics...), although it is somewhat
memory-hungry.

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

<p><b>Animation through a series of static models like <code>xxx000.obj</code>,
<code>xxx001.obj</code>, <code>xxx002.obj</code>...</b>.

<p>This is useful if you have files named like:

<pre>
xxx001.obj
xxx002.obj
xxx003.obj
...
</pre>

<p>You can load such animation from a sequence of files using the URL <code>xxx@counter(3).obj</code>. It's exactly how you can load <a href="x3d_implementation_texturing_extensions.php#section_ext_movie_from_image_sequence">a movie from a sequence of images</a>. See <a href="https://github.com/castle-engine/demo-models/tree/master/blender/skinned_animation/wavefront_obj_animation">demo-models/.../wavefront_obj_animation</a> for an example, you would load them by <code>skinned_anim_@counter(6).obj</code>.

<p>It's not an efficient way to store the animation in a file (or to load it). It is much better to export to X3D or glTF animation, if you can. But it may be useful when you have no other option to export.

<p>Blender exporter to <i>Wavefront OBJ</i> with <i>"Animation"</i> checkbox generates such animation. Hint: uncheck <i>"Write Normals"</i> and check <i>"Keep Vertex Order"</i> in the exporting dialog box, to make sure models are "structurally equal" which allows CGE to merge the animation nicely (even produce more intermediate animation frames, if needed). This also means that CGE will recalculate normals by itself (unfortunately this isn't always desired, because Blender doesn't pass the creaseAngle information to CGE, so CGE cannot smooth the normals as in Blender -- it doesn't know when it should).

<p>The animation is played and smoothed following the same logic as <a href="castle_animation_frames.php">castle-anim-frames</a> format. In a way, using URL <code>xxx@counter(4).obj</code> is just a shortcut for creating a <code>xxx.castle-anim-frames</code> file that would list the appropriate static frames using XML elements.

<?php echo $toc->html_section(); ?>

<p><b><a href="http://www.khronos.org/collada/">Collada</a></b>
(<code>.dae</code> extension).
We support a lot of Collada features &mdash; geometry with materials,
textures, cameras, lights. Tested on many Collada examples,
like <a href="http://collada.org/owl/">Collada Test Model Bank</a>
and Collada models exported from various <a href="http://www.blender.org/">Blender</a>
versions.
All modern Collada versions (1.3, 1.4, 1.5) are handled.

<p>Animations in Collada files are not handled yet.
If you need animations, we advise to use glTF instead.
glTF is newer and has similar feature set.
<a href="https://github.com/KhronosGroup/COLLADA2GLTF">Khronos provides a converter
from Collada to glTF</a>.

<?php echo $toc->html_section(); ?>

<p><a href="http://oss.sgi.com/projects/inventor/"><b>OpenInventor</b></a>
1.0 ASCII files (<code>.iv</code> extension) are handled.
Inventor 1.0 and VRML 1.0 are very similar
formats, we also handle some additional Inventor-specific nodes.

<?php echo $toc->html_section(); ?>

<p><b>3d Studio 3DS format</b>. We support most important things:
meshes, cameras, materials, textures.

<?php echo $toc->html_section(); ?>

<p><b>MD3</b>. This is the format used for models
in Quake 3 and derivatives (<a href="http://tremulous.net/">Tremulous</a>
etc.). Almost everything useful from MD3 file is supported:
geometry with texture (coordinates, and texture filename from
associated <code>xxx_default.skin</code> file), <i>animation is also read
and played</i>.</p>

<?php echo $toc->html_section(); ?>

<p><b>Wavefront OBJ files</b>. We support most important things:
geometry (with texture coords, normal vectors), materials
(colors, opacity, texture filenames).</p>

<?php echo $toc->html_section(); ?>

<p><b>STL (Standard Triangle Language, aka STereoLithography)</b>.
<a href="https://en.wikipedia.org/wiki/STL_%28file_format%29">STL</a> is a simple
popular 3D format used in 3D printing.
We support both ASCII and binary formats.</p>

<?php echo $toc->html_section(); ?>

<p><b><a href="http://local.wasp.uwa.edu.au/~pbourke/dataformats/geo/">Videoscape GEO</a></b>
(<code>.geo</code> extension).
Very basic support for this very old 3D format.

<?php
creating_data_footer();
?>
