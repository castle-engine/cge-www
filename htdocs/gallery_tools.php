<?php
require_once 'castle_engine_functions.php';
castle_header('Tools using Castle Game Engine', array(
  'path' => array('gallery'),
));

echo pretty_heading($page_title);
?>

<p><i>Want your project listed here?
<a href="talk.php">Tell us about it!</a>
We love to see how you use CGE.</i>

<div class="row">
  <?php gallery_link('view3dscene',
    'VRML / X3D browser, and a viewer for other 3D model formats
    (Collada, 3DS, MD3, Wavefront OBJ, Spine...).
    Explore the virtual world, with collision-checking, gravity, interactive animations, shadows, mirrors, shaders and more.
    Convert various models to VRML/X3D.',
    'view3dscene_outlines.png',
    'view3dscene'); ?>

  <?php gallery_link('Convert everything to X3D',
    'Convert onine glTF, OBJ, STL, Collada, 3DS (and other model formats handled by Castle Game Engine) to X3D. This is an online version of our <a href="view3dscene.php"><code>tovrmlx3d</code> converter (distributed with view3dscene)</a>.',
    'convert-to-x3d.png',
    'convert'); ?>

  <?php gallery_link('castle-view-image (formerly glViewImage)',
    'Image viewer, handles many image formats (including some exotic ones: DDS, RGBE).',
    "castle-view-image_dds.png", 'castle-view-image'); ?>

  <?php gallery_link("Curves Tool",
    "Design a 2D curve, save it as XML, and use it in your own programs, e.g. to move something (camera, another object) along a smooth designed trajectory.",
    'castle_curves.png',
    "https://github.com/castle-engine/castle-engine/wiki/Curves-tool"); ?>

  <?php gallery_link('glplotter',
    'Plotting graphs of mathematical functions.',
    "glplotter_screen_demo_1.png", 'glplotter'); ?>

  <?php gallery_link("rayhunter",
    "Command-line simple ray-tracer (classic deterministic ray-tracer and basic Monte Carlo path tracer).<br/>Handles VRML/X3D and other 3D model formats.<br/>" .
    a_href_page("See also it's gallery.","raytr_gallery"),
    'rayhunter_graz_demo.png',
    "rayhunter"); ?>
</div>

<p>More:

<ul>
  <?php gallery_link_noimage("glinformation",
    'Output information about OpenGL installed on your system.',
    "https://github.com/castle-engine/glinformation");
  ?>
</ul>

<?php castle_footer(); ?>
