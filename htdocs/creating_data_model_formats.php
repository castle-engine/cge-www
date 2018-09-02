<?php
require_once 'castle_engine_functions.php';
creating_data_header('Supported model formats');

$toc = new TableOfContents(
  array(
    new TocItem('Best formats to use', 'best'),
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

<ul>
  <li><p><b><?php echo a_href_page('X3D and VRML', 'vrml_x3d'); ?></b>.

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

    <p><i>If your authoring software
    can export to X3D, this is the format you should probably use.</i>

  <li><p><b><?php echo a_href_page('Castle Animation Frames
    (castle-anim-frames) format', 'castle_animation_frames'); ?></b>,
    formerly known as <code>kanim</code>.

    <p><i>This is a simple format for animations, used
    to <a href="creating_data_blender.php">export animated models from Blender</a>.</i>

  <li><p><b>Spine JSON animations</b>.
    <a href="http://esotericsoftware.com/">Spine</a> is a powerful program
    for 2D game skeletal animations.
    <a href="creating_data_dragon_bones.php">Dragon Bones</a> can also export
    to this format.
    </p>

    <p><a href="https://github.com/castle-engine/castle-engine/wiki/Spine">We have a big support for Spine JSON features</a>,
    and our friendly game studio <a href="http://cat-astrophe-games.com/">Cat-astrophe Games</a>
    is using Spine for all 2D games.

  <li><p><b>Not yet, but coming soon: glTF 2.0</b>.

    <p>Great format from Khronos. See <a href="planned_features.php">planned features</a>
    for a lot of details.
</ul>

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

    <p><i>Animations in Collada files are not handled yet.</i>

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
