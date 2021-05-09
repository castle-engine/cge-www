<?php
require_once 'castle_engine_functions.php';
castle_header('Additional Components', array(
  'path' => array('gallery'),
));

echo pretty_heading($page_title);
?>

<p>Additional components listed here are built on top of <i>Castle Game Engine</i>.
You can use them in your own projects, to enhance what you can do with CGE.

<p><i>Want your project listed here?
<a href="talk.php">Tell us about it!</a>
We love to see how you use CGE.</i>

<div class="row">
  <?php gallery_link("cge-2d-particle-emitter",
    '2D particle system using "71 Squared" format (.pex files). Particles are calculated on CPU or with GPU acceleration (transform feedback).
     <p>See also <a href="https://forum.castle-engine.io/t/cge-2d-particle-emitter-now-supports-gpu-based-particle/">forum thread</a>.',
    'cge_2d_particles.png',
    'https://github.com/Kagamma/cge-2d-particle-emitter');
  ?>

  <?php gallery_link("cge-3d-particle-emitter",
    '3D particle system, with an editor also implemented in CGE. Particles are calculated with GPU acceleration (transform feedback).
     <p>See also <a href="https://forum.castle-engine.io/t/3d-particle-system/">forum thread</a>.',
    'cge_3d_particles_edit_fire.png',
    'https://github.com/Kagamma/cge-3d-particle-emitter');
  ?>

  <?php gallery_link("Database aware CGE UI",
    'Implementation of database-aware TCastleDBEdit component. See the <a href="https://github.com/castle-engine/castle-db-aware-controls/">GitHub code</a> and <a href="https://castle-engine.io/wp/2018/12/24/database-aware-demo-using-castle-game-engine-ui/">this news post</a> for documentation.',
    'database_aware.png',
    'https://github.com/castle-engine/castle-db-aware-controls');
  ?>
</div>

<p>More:

<ul>
  <?php gallery_link_noimage("wrltodxf",
    'Convert a VRML, X3D or other 3D model to DXF file.',
    'https://github.com/rweinzierl10/wrltodxf');
  ?>
</ul>

<?php castle_footer(); ?>
