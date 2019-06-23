<?php
require_once 'castle_engine_functions.php';
creating_data_header('Supported model formats');

$toc = new TableOfContents(
  array(
    new TocItem('Best formats to use', 'best'),
      new TocItem('X3D and VRML', 'x3d', 1),
      new TocItem('glTF 2.0', 'gltf', 1),
      new TocItem('Castle Animation Frames (castle-anim-frames) format', 'castle_anim_frames', 1),
      new TocItem('Spine JSON', 'spine', 1),
    new TocItem('Other formats you can use', 'other'),
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

<p>An efficient, modern format, developed by Khronos. Resources:

<ul>
  <li><p><a href="https://www.khronos.org/gltf">glTF overview</a>,
  <li><p><a href="https://github.com/KhronosGroup/glTF">glTF specification and extensions</a>,
  <li><p><a href="https://github.com/KhronosGroup/glTF-Sample-Models">glTF sample models</a> (open them with <a href="http://michalis.ii.uni.wroc.pl/view3dscene-snapshots/">view3dscene from snapshots right now</a>),
  <li><p>In Blender: <a href="https://github.com/KhronosGroup/glTF-Blender-IO">Blender glTF exporter and importer</a> is packaged inside <a href="https://builder.blender.org/download/">Blender 2.80</a> out-of-the-box. See <a href="https://docs.blender.org/manual/en/dev/addons/io_gltf2.html">manual about using Blender glTF exporter</a>.

    <p>Note that (unfortunatately) it seems not possible to export a single animation like "walk" that animates transformations of multiple Blender objects. That's a consequence of how the "actions" in Blender work ("action" is a set of animation curves, and using the same action for multiple objects forces them to share the animation curves &mdash; which may not be what you want). You can workaround it by <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/animations/simultaneous_animations_one_scene">running muliple animations simultaneously from CGE</a>, but this is less comfortable than calling <code>Scene.PlayAnimation('walk')</code>.
</ul>

<p>Supported features of glTF models in our engine:

<ul>
  <li><p>Meshes (polygons, lines), transformation hierarchy.
  <li><p>Materials using Phong shading, alpha mode, double-sidedness, per-vertex colors.
  <li><p>Texturing for base color, normal maps, emissive.
  <li><p>Cameras (perspective and orthogonal).
  <li><p>Animations of transformations (position, rotation, scale), that can be played using <a href="manual_scene.php#section_play_animation">standard CGE <code>PlayAnimation</code> method</a> (or <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/animations/simultaneous_animations_one_scene">other means, e.g. to play multiple animations from one model simultaneously</a>).
  <li><p>Both <code>.glb</code> and <code>.gltf</code> extensions are supported. Textures can be provided in separate files or embedded inside the GLTF stream.
  <li><p>It is integrated in our engine as X3D nodes graph. This means that you can include a glTF model inside larger X3D file using the <code>Inline</code> node, you can modify the glTF scene graph at runtime (e.g. modify material color, show/hide something etc.) and you can serialize the scene graph to an X3D file.
  <li><p>We use <a href="https://github.com/BeRo1985/pasgltf/">PasGLTF</a>, a great open-source library for reading glTF by <a href="https://www.patreon.com/bero">Benjamin "Bero" Rosseaux</a>.
</ul>

<p>Main missing features are PBR (Physical-Based Rendered materials, <a href="https://github.com/michaliskambi/x3d-tests/wiki/Include-PBR-%28PhysicalMaterial-and-related-concepts%29-in-the-official-X3D-specification">we are working on it</a>), morph targets and skinned animation.

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

<a href="http://esotericsoftware.com/">Spine</a> is a powerful program
for 2D game skeletal animations.
<a href="creating_data_dragon_bones.php">Dragon Bones</a> can also export
to this format.
</p>

<p><a href="https://github.com/castle-engine/castle-engine/wiki/Spine">We have a big support for Spine JSON features</a>,
and our friendly game studio <a href="http://cat-astrophe-games.com/">Cat-astrophe Games</a>
is using Spine for all 2D games.

<?php echo $toc->html_section(); ?>

<ul>
  <li><p><b><a href="http://www.khronos.org/collada/">Collada</a></b>
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

  <li><p><a href="http://oss.sgi.com/projects/inventor/"><b>OpenInventor</b></a>
    1.0 ASCII files (<code>.iv</code> extension) are handled.
    Inventor 1.0 and VRML 1.0 are very similar
    formats, we also handle some additional Inventor-specific nodes.

  <li><p><b>3d Studio 3DS format</b>. We support most important things:
    meshes, cameras, materials, textures.

  <li><p><b>MD3</b>. This is the format used for models
    in Quake 3 and derivatives (<a href="http://tremulous.net/">Tremulous</a>
    etc.). Almost everything useful from MD3 file is supported:
    geometry with texture (coordinates, and texture filename from
    associated <code>xxx_default.skin</code> file), <i>animation is also read
    and played</i>.</p>

  <li><p><b>Wavefront OBJ files</b>. We support most important things:
    geometry (with texture coords, normal vectors), materials
    (colors, opacity, texture filenames).</p>

  <li><p><b>STL (Standard Triangle Language, aka STereoLithography)</b>.
    <a href="https://en.wikipedia.org/wiki/STL_%28file_format%29">STL</a> is a simple
    popular 3D format used in 3D printing.
    We support both ASCII and binary formats.</p>

  <li><p><b><a href="http://local.wasp.uwa.edu.au/~pbourke/dataformats/geo/">Videoscape
    GEO</a></b> (<code>.geo</code> extension).
    Very basic support for this very old 3D format.
</ul>


<?php
creating_data_footer();
?>
