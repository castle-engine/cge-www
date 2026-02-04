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
  <?php gallery_link('Room Arranger',
    'Design your room, office, house and preview them in 3D. Using our engine through the <a href=""https://github.com/castle-engine/castle-engine/tree/master/src/deprecated_library">library API</a>.',
    'room_arranger_scan.png',
    'https://www.roomarranger.com/'); ?>

  <?php gallery_link('Castle Model Viewer (formerly view3dscene)',
    'Viewer for 3D and 2D models like glTF, X3D, VRML, IFC, Collada, 3DS, MD3, Wavefront OBJ, Spine and <a href="creating_data_model_formats.php">many others supported by Castle Game Engine</a>.
    Explore the virtual world, with collision-checking, gravity, interactive animations, shadows, mirrors, shaders and more.
    Can also convert various models to X3D.',
    'view3dscene_outlines.png',
    'doc/castle-model-viewer'); ?>

  <?php gallery_link('Castle Model Viewer Mobile',
    'A mobile version (for <a href="https://play.google.com/store/apps/details?id=io.castleengine.castle.model.viewer.mobile">Android</a> and <a href="https://apps.apple.com/app/id6752208775">iOS</a>) of our viewer for 3D and 2D models like glTF, X3D, VRML, IFC, and more.',
    'castle-model-viewer-mobile-feature.png',
    'doc/castle-model-viewer-mobile'); ?>

<?php gallery_link('Castle Model Converter (formerly tovrmlx3d)',
    'Command-line converter from <a href="creating_data_model_formats.php">all model formats supported by Castle Game Engine (like glTF)</a> to X3D. Can also act as validator.',
    'castle-model-converter.png',
    'doc/castle-model-converter'); ?>

  <?php gallery_link('Online Model Converter',
    'Convert 3D and 2D models. Can convert glTF, OBJ, STL, Collada, 3DS (and other model formats handled by Castle Game Engine) to X3D. This is an online version of our <a href="castle-model-converter">Castle Model Converter</a>.',
    'online-model-converter.png',
    'convert'); ?>

  <?php gallery_link('Castle Image Viewer',
    'Image viewer, handles many common image formats (PNG, JPG...) and some special formats used in game development (DDS, KTX, RGBE)',
    'castle-image-viewer_alpha.png',
    'doc/castle-image-viewer'); ?>

  <?php gallery_link("Curves Tool",
    "Design a 2D curve, save it as XML, and use it in your own programs, e.g. to move something (camera, another object) along a smooth designed trajectory.",
    'castle_curves.png',
    "doc/curves_tool"); ?>

  <?php gallery_link('glplotter',
    'Plotting graphs of mathematical functions.',
    "glplotter_screen_demo_1.png", 'doc/glplotter'); ?>

  <?php gallery_link("rayhunter",
    "Command-line simple ray-tracer (classic deterministic ray-tracer and basic Monte Carlo path tracer).<br/>Handles VRML/X3D and other 3D model formats.<br/>" .
    a_href_page("See also it's gallery.","doc/raytr_gallery"),
    'rayhunter_graz_demo.png',
    "doc/rayhunter"); ?>
</div>

<p>More:

<ul>
  <?php gallery_link_noimage("glinformation",
    'Output information about OpenGL installed on your system.',
    "https://github.com/castle-engine/glinformation");
  ?>
</ul>

<?php castle_footer(); ?>
