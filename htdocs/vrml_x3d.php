<?php
  require_once 'vrmlengine_functions.php';
  require_once 'vrml_implementation_common.php';
  vrmlx3d_header('What is VRML / X3D');

  echo vrmlengine_thumbs(array(
    array('filename' => 'sunny_street_tree_hard.png', 'titlealt' => 'Close up shadows on the tree. Notice that leaves (modeled by alpha-test texture) also cast correct shadows.'),
//    array('filename' => 'castle_screen_3.png', 'titlealt' => 'Werewolves with shadows'),
//    array('filename' => 'rendered_texture_with_background.png', 'titlealt' => 'RenderedTexture with background and mirrors thrown in'),
    array('filename' => 'water_reflections.png', 'titlealt' => 'Water reflections by optimized GeneratedCubeMapTexture'),
//    array('filename' => 'tex3d_smoke.png', 'titlealt' => 'Fog from 3D noise'),
    array('filename' => 'rendered_texture_mirror_2.png', 'titlealt' => 'Mirrors by RenderedTexture, by Victor Amat'),
  ));

  echo pretty_heading($page_title);
?>

<p>Simply put, X3D (and it's older version, VRML) is a file format for 3D models.
You will find that virtually any 3D modelling program can export to it,
for exampe <a href="http://www.blender.org/">Blender</a> includes
VRML and X3D exporters (and <?php echo
a_href_page('we also have our own customized exporter', 'blender'); ?>).</p>

<p>To start the fun, just create some VRML/X3D models
(or download them from the Internet, or grab our
<?php echo a_href_page('VRML/X3D demo models', 'demo_models'); ?>)
and open them with our
<?php echo a_href_page('view3dscene', 'view3dscene'); ?>.</p>

<p>As a 3D file format, VRML/X3D is quite unique because it's designed
to describe <i>virtual 3D worlds</i>, not just static scenes.
So you can do animations, interactive behaviors (e.g. open the door
when user presses a handle), and scripting right inside the VRML/X3D file.
Many advanced graphic effects are also possible, like making
mirrors by generated (cube map or flat) textures, using GLSL shaders
and much more.</p>

<p>The <b><a href="http://web3d.org/x3d/specifications/">specifications
for X3D (and older VRML) are available here</a></b>.
This is your ultimate resource to learn what you can do with VRML/X3D.
The older versions were called VRML (VRML 1.0, then VRML 2.0 also
known as VRML 97). Newer versions are called X3D (3.x).
I collectively call them all <i>VRML/X3D</i> because our engine handles
all versions of them. You probably want to use the newest one,
X3D, whenever possible.</p>

<p>Pages in this section describe our handling of VRML/X3D:
<?php echo a_href_page('how we handle the stuff in official VRML/X3D specification', 'vrml_implementation_status'); ?> and
<?php echo a_href_page('what new features we add to VRML/X3D', 'kambi_vrml_extensions'); ?>.

<?php vrmlx3d_footer(); ?>
