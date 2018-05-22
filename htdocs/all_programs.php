<?php
require_once 'castle_engine_functions.php';
castle_header('Games and Tools Developed Using Castle Game Engine');

/* Link to program page.
   Image used here for $image_name must have "program_link_size"
   generated, so make sure it's listed in images/Makefile in PROGRAM_LINK_SIZE.
   $page_name may be a complete URL, or a page name for a_href_page. */
function program_image_link($title, $subtitle, $image_name, $page_name)
{
  $s = '<div class="col-sm-4"><div class="program_image_link">';
  $s .= '<p>' . a_href_page(
    "<img src=\"images/program_link_size/$image_name\" alt=\"$title\" />", $page_name) .
    '</p>';
  $s .= '<p class="program_image_link_title">' .
    a_href_page("<b>$title</b>", $page_name) . '</p>' .
    '<p>' . $subtitle . '</p>' .
    '</div></div>';

  echo $s;
}

/* Link to program that doesn't have nice screenshot. */
function program_link($title, $subtitle, $page_name)
{
  $s = '<li><p class="program_image_link_title">' .
    a_href_page("<b>$title</b>", $page_name) . '</p>' .
    '<p>' . $subtitle . '</p>' .
    '</li>';
  echo $s;
}

echo pretty_heading($page_title);

$toc = new TableOfContents(
  array(
    new TocItem('Games', 'games'),
    new TocItem('Tools and other applications', 'tools'),
  ));

?>

<p><b>Do you want to see your project listed here?</b>
Sure! Just <a href="talk.php">tell us about it (using Discord, forum, GitHub issue, ...)</a>.

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<div class="row">
  <?php program_image_link('Connect4',
    'Fun free game for Android. You can plan against a computer, or against a friend over the network. Shows various engine features like localization and 2D scalable UI. <a href="https://castle-engine.io/wp/2018/04/11/connect4-new-android-game-using-castle-game-engine/">By Benedikt Magnus and LanIstAn.</a>',
    'connect4.png',
    'https://play.google.com/store/apps/details?id=de.benediktmagnus.viergewinnt.app');
  ?>

  <?php program_image_link('The Unholy Society',
    'Game inspired by action movies and comic books such as <i>Preacher</i> and <i>Constantine</i>. Developed using Spine and Castle Game Engine. <i>Coming to Steam in 2018</i>, for Windows, Linux and macOS.',
    'unholy_society_in_game.jpg',
    'https://store.steampowered.com/app/746750/The_Unholy_Society/');
  ?>

  <?php program_image_link('Wyrd Forest',
    'A demo done for <a href="https://www.patreon.com/castleengine">Castle Game Engine supporters on Patreon</a>, featuring terrain editor and planting trees and destructible shooting targets with physics. The <a href="https://github.com/castle-engine/wyrd-forest">source code and binaries are on GitHub</a>.',
    'wyrd_forest_screen_0-2.png',
    'https://www.patreon.com/posts/wyrd-forest-demo-15811244');
  ?>

  <?php program_image_link('Mazer',
    'A maze generation algorithm and a simple game behind that. By Eugene Loza.',
    'mazer.jpg',
    'https://decoherence.itch.io/mazer'); ?>

  <?php program_image_link('Fire Madness',
    'Top-down bullet hell shooter inspired by CrossFire game. By Eugene Loza. <a href="https://github.com/eugeneloza/FireMadness">The source code is available on GitHub</a>. <!--a href="http://opengameart.org/forumtopic/fire-madness-top-down-shooter-bullet-hell">See also the opengameart thread</a-->',
    'fire_madness.png',
    'https://decoherence.itch.io/fire-maddness'); ?>

  <?php program_image_link('Escape from the Universe',
    'Action shooter in the outer space with an incredible randomized storyline. For mobile devices (Android, iOS).',
    'escape_universe.png',
    'https://cat-astrophe-games.com/escape-universe/'); ?>

  <?php program_image_link('Dragon Squash',
    'Android game where you defend beautiful animated castles. For Android, with Google Games integration and dragons.',
    'dragon_squash_screen_1.png', 'https://play.google.com/store/apps/details?id=net.sourceforge.castleengine.dragonsquash'); ?>

  <?php program_image_link('Dragon Spine',
    'Fly your dragon in a dark comic setting. Demo of <i>Spine animations</i> and <i>Google Games achievements</i> inside Castle Game Engine on Android. Available for free from Google Play, the source code is inside the engine examples (<a href="https://github.com/castle-engine/castle-engine/tree/master/examples/2d_dragon_spine_game">2d_dragon_spine_game</a>).',
    'castle_spine_screen_3.png', 'https://play.google.com/store/apps/details?id=net.sourceforge.castleengine.castlespine'); ?>

  <?php program_image_link('Hydra Battles',
    'Isometric RTS game for 2 players, with some twists. <a href="https://github.com/castle-engine/hydra-battles">Source code on GitHub</a>.',
    "hydra_battles_screen_best.png", 'http://michaliskambi.itch.io/hydra-battles'); ?>

  <?php program_image_link('Mountains Of Fire',
    '3D game with split-screen view where human and worm cooperate to survive. For single player or 2 players.',
    "mountains_of_fire_screen_1.png", 'mountains_of_fire'); ?>

  <?php program_image_link('Little Things',
    'A pretty 3D game-like experience where you swim towards a sound. Developed during 48h gamejam.',
    'little_things_screen_7.png',
    'https://github.com/castle-engine/little-things');
  ?>

  <?php program_image_link('Darkest Before the Dawn',
    'Scary 3D game, for Android and standalone.',
    "darkest_before_dawn_2.png", 'darkest_before_dawn'); ?>

  <?php program_image_link('&quot;The Castle&quot;',
    'First-person perspective game, in a dark fantasy setting.',
    "castle_screen_demo_1.png", 'castle'); ?>

  <?php program_image_link('lets_take_a_walk',
    "Small 3D toy, demonstrating rendering integrated with 3D sound using Castle Game Engine. The source code is now available in <a href=\"https://github.com/castle-engine/castle-engine/tree/master/examples/3d_sound_game\">examples/3d_sound_game/</a> in engine sources.",
    'lets_take_a_walk_screen_demo.png',
    'index'); ?>

  <?php program_image_link('malfunction',
    'Small 3D space-shooter. One of the first games made using (an ancient version of) Castle Game Engine, with VRML models.',
    'malfunction_screen_demo.png',
    'malfunction'); ?>

   <?php program_image_link('kambi_lines',
     'Arrange colored balls in lines. Quickly.',
     'kambi_lines_screen_demo.png', 'kambi_lines'); ?>
</div>

<p>See also:

<ul>
  <?php program_link("Decoherence game collection on itch.io",
    'Various games by Eugene Loza, some using Castle Game Engine.',
    "https://decoherence.itch.io/");
  ?>

  <?php program_link("Michalis game collection on itch.io",
    'Various games by Michalis Kamburelis, all using Castle Game Engine. You can <a href="all_programs_sources.php">download sources of all these programs here</a>. They are also available on GitHub, <a href="https://github.com/castle-engine/">as part of GitHub Castle Game Engine organization</a>.',
    "http://michaliskambi.itch.io/");
  ?>
</ul>

<?php echo $toc->html_section(); ?>

<div class="row">
  <?php program_image_link('view3dscene',
    'VRML / X3D browser, and a viewer for other 3D model formats
    (Collada, 3DS, MD3, Wavefront OBJ, Spine...).
    Explore the virtual world, with collision-checking, gravity, interactive animations, shadows, mirrors, shaders and more.
    Convert various models to VRML/X3D.',
    //"view3dscene_2.0.0_screen_demo.png",
    'castle_sunset.png',
    'view3dscene'); ?>

  <?php program_image_link('glViewImage',
    'Image viewer, handles many image formats (including some exotic ones: DDS, RGBE).',
    "glviewimage_dds.png", 'glviewimage'); ?>

  <?php program_image_link("Curves Tool",
    "Design a 2D curve, save it as XML, and use it in your own programs, e.g. to move something (camera, another object) along a smooth designed trajectory.",
    'castle_curves.png',
    "https://github.com/castle-engine/castle-engine/wiki/Curves-tool"); ?>

  <?php program_image_link('glplotter',
    'Plotting graphs (e.g. of functions).',
    "glplotter_screen_demo_1.png", 'glplotter_and_gen_function'); ?>

  <?php program_image_link("rayhunter",
    "Command-line simple ray-tracer (classic deterministic ray-tracer and basic Monte Carlo path tracer).<br/>Handles VRML/X3D and other 3D model formats.<br/>" .
    a_href_page("See also it's gallery.","raytr_gallery"),
    'rayhunter_graz_demo.png',
    "rayhunter"); ?>
</div>

<ul>
  <?php program_link("glinformation",
    'Output information about OpenGL installed on your system.',
    "glinformation");
  ?>

  <?php program_link("wrltodxf",
    'Convert a VRML, X3D or other 3D model to DXF file.',
    'https://github.com/rweinzierl10/wrltodxf');
  ?>
</ul>

<?php castle_footer(); ?>
