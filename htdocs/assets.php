<?php
require_once 'castle_engine_functions.php';
castle_header('Assets (3D and 2D Graphics, Sound)', array(
  'path' => array('gallery'),
));

echo pretty_heading($page_title);
?>

<p>Completely subjective list of places with good quality assets (graphics, sounds) in formats useful with <i>Castle Game Engine</i>. See and post on the <code>#assets</code> channel of <a href="talk.php">our Discord</a> for more.

<div class="row">
  <?php gallery_link('Quaternius',
    'Many 3D model packs, on a public domain license.', // https://www.patreon.com/quaternius
    'assets_quaternius.png',
    'https://quaternius.com/');
  ?>

  <?php gallery_link('OpenGameArt.org',
    'Game art (3D, 2D, sounds) uploaded by various artists on clear open-source licenses.
    <!-- Commented out, this did not catch on.
    <p><i>Hint:</i> Do you want to share an asset you made and tested specifically for CGE game? Upload it to <a href="https://opengameart.org/art-search-advanced?keys=&title=%5Bcge%5D">OpenGameArt.org with <code>[CGE]</code> in the name</a>.
    -->',
    'assets_opengameart.png',
    'https://opengameart.org/');
  ?>

  <?php gallery_link('Sketchfab',
    '3D models, in glTF format, uploaded by various artists. Some free (on clear licenses, like <i>Creative Commons</i>), some paid. <p>Note: you can also <a href="https://castle-engine.io/sketchfab">import from Sketchfab directly in CGE editor</a>!',
    'assets_sketchfab.png',
    'https://sketchfab.com/features/gltf');
  ?>

  <?php gallery_link('Kenney',
    'Sprites, 3D models, sound effects on a public domain ("do what you want") license.',
    'assets_kenney.png',
    'https://kenney.nl/');
  ?>

  <?php gallery_link('Blend Swap',
    'Models made using <a href="blender">Blender</a> (can be easily exported e.g. to <a href="gltf">glTF</a> to use in our engine). On various <i>Creative Commons</i> licenses.',
    'assets_blendswap.png',
    'https://blendswap.com/');
  ?>

</div>

<?php castle_footer(); ?>
