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
  <?php gallery_link("Particle Emitter",
    'Particle system for 3D and 2D that supports editing right inside CGE editor. Particles are calculated with GPU acceleration (transform feedback).
     <p>See forum threads: <a href="https://forum.castle-engine.io/t/3d-particle-system/">1</a>, <a href="https://forum.castle-engine.io/t/3d-particle-system-now-with-full-support-for-2d-and-castle-editor/">2</a>. See <a href="https://castle-engine.io/wp/2021/08/20/particle-system-updates-with-editing-integrated-inside-cge-editor-and-improvements-to-building-editor-with-custom-components/">news</a>.
     <p>By <i>Trung Le (Kagamma)</i>.',
    //'cge_3d_particles_edit_fire.png',
    'cge_3d_particles_editor.png',
    'https://github.com/Kagamma/cge-3d-particle-emitter');
  ?>

  <?php gallery_link("Effekseer Integration",
    'Integration with <a href="https://effekseer.github.io/en/">Effekseer</a>, a full-featured open-source particle effect creation tool.
     <p>See <a href="https://castle-engine.io/wp/2021/08/22/effekseer-particle-effect-creation-and-runtime-integration/">news</a>.
     <p>By <i>Trung Le (Kagamma)</i>.',
    'cge_effekseer.png',
    'https://github.com/Kagamma/cge-effekseer');
  ?>

  <?php gallery_link("2D water effect",
    'A beautiful water for 2D games. The water is represented as a volume that you can easily move and scale around. It supports reflection and refraction.
     <p>See <a href="https://castle-engine.io/wp/2021/08/21/water-effect-ready-to-use-in-2d-games-like-a-platformer-demo/">news</a>.
     <p>By <i>Trung Le (Kagamma)</i>.',
    'cge_2d_water.png',
    'https://github.com/Kagamma/cge-2d-water-effect');
  ?>

  <?php gallery_link("cge-2d-particle-emitter",
    '2D particle system using "71 Squared" format (.pex files). Particles are calculated on CPU or with GPU acceleration (transform feedback).
     <p>This is <i>deprecated</i> now in favor of <a href="https://github.com/Kagamma/cge-3d-particle-emitter">cge-3d-particle-emitter</a>.
     <p>See also <a href="https://forum.castle-engine.io/t/cge-2d-particle-emitter-now-supports-gpu-based-particle/">forum thread</a>.
     <p>By <i>Trung Le (Kagamma)</i>.',
    'cge_2d_particles.png',
    'https://github.com/Kagamma/cge-2d-particle-emitter');
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
