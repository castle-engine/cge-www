<?php
require_once 'castle_engine_functions.php';
castle_header('Games using Castle Game Engine');

echo pretty_heading($page_title);
?>

<p><i>Want your project listed here?
<a href="talk.php">Tell us about it!</a>
We love to see how you use CGE.</i>

<div class="row">

  <?php gallery_link('DSDrive',
    'A simple racing game developed by DidiSoft. Two game modes, three different racetracks, car customization, weather effects, different views. Source code available at the game page.',
    'dsdrive.png',
    'https://didisoft.itch.io/dsdrive');
  ?>

  <?php gallery_link('Lynch',
    'Walk around a mansion and discover the secret of the statues. Short gamejam game by Michalis. Source code is <a href="https://github.com/castle-engine/lynch">available on GitHub</a>.',
    'lynch_editor_2.png',
    'https://cat-astrophe-games.itch.io/lynch');
  ?>

  <?php gallery_link('Foxy Misfortunes',
    'Top-down shooter with a funny NSFW theme. Download it for Windows, Linux and Android. Source code is <a href="https://gitlab.com/EugeneLoza/foxy-misfortunes">available on GitLab</a>.

    <p>By Eugene Loza.',
    'foxy-misfortunes.png',
    'https://decoherence.itch.io/foxy-misfortunes');
  ?>

  <?php gallery_link('Platformer',
    'Platformer game demo. A completely playable game, sources available in CGE as <code>examples/platformer/</code>.

    <p>By Andrzej Kilijański.',
    'platformer.png',
    'https://castle-engine.itch.io/platformer');
  ?>

  <?php gallery_link('Kryftolike',
    'A roguelite-like hide-and seek game.
    Hide from monsters and seek treasure in an infinite procedurally-generated map.
    Danger awaits around every corner!
    For Android, Windows and Linux.

    <p>Full <a href="https://gitlab.com/EugeneLoza/kryftolike">source code</a> available too. By Eugene Loza.',
    'kryftolike.png',
    'https://decoherence.itch.io/kryftolike');
  ?>

  <?php gallery_link('The Unholy Society',
    'Game inspired by action movies and comic books such as <i>Preacher</i> and <i>Constantine</i>. Developed using <i>Castle Game Engine</i> by <a href="https://cat-astrophe-games.com/">Cat-astrophe Games</a>, most animations done in <a href="https://castle-engine.io/spine">Spine</a>. Released on <a href="https://www.nintendo.com/games/detail/the-unholy-society-switch/">Nintendo Switch</a>, <a href="https://store.steampowered.com/app/746750/The_Unholy_Society/">Steam (Windows, Linux)</a>, <a href="https://apps.apple.com/app/id1562725324">iOS</a> and <a href="https://play.google.com/store/apps/details?id=games.catastrophe.adventure.point.click.unholy.society">Android</a> (mobile versions come with free demo).',
    'unholy_society_in_game.jpg',
    'https://unholy-society.com/');
  ?>

  <?php gallery_link('Bumpcars-2019',
    'Race against the clock to complete the three tracks: park, city and beach. By Rafael Campos. The game is a submission for the gamejam <a href="https://itch.io/jam/50-assembly-language">&quot;I\'m using a lot of assembly language&quot;</a>, and includes a large assembler code which main objective is the subdivision of 3D triangles.',
    'bumpcars.jpg',
    'https://juegosenlazaruscr.itch.io/bumpcars-2019'); ?>

  <?php gallery_link('Bricks Color Pick',
    'New approach to the classic arkanoid games, with a twist: there is no paddle. Free game for Android (on Google Play). By Digital Karabela – <a href="https://digitalkarabela.com/category/gamedev/castle-game-engine/">Andrzej Kilijański</a>.',
    'bricks_color_pick.jpg',
    'https://play.google.com/store/apps/details?id=com.digitalkarabela.cge.colorpick'); ?>

  <?php gallery_link('Swappy Jigsaw',
    'Jigsaw puzzle game, where you have to unravel an image split into many tiles. With <a href="https://gitlab.com/EugeneLoza/swappy-jigsaw">source code</a>. By Eugene Loza.',
    'swappy_jigsaw.png',
    'https://decoherence.itch.io/swappy-jigsaw'); ?>

  <?php gallery_link('Escape from the Universe',
    'Action shooter in the outer space with an incredible randomized storyline. New version is already released on the Nintendo Switch platform. Originally, the game was available for mobile devices (Android and iOS).',
    'escape_universe.png',
    'https://cat-astrophe-games.com/escape-universe/'); ?>

  <?php gallery_link('Connect4',
    'Fun free game for Android. You can plan against a computer, or against a friend over the network. Shows various engine features like localization and 2D scalable UI. <a href="https://castle-engine.io/wp/2018/04/11/connect4-new-android-game-using-castle-game-engine/">By Benedikt Magnus and LanIstAn.</a>',
    'connect4.png',
    'https://play.google.com/store/apps/details?id=de.benediktmagnus.viergewinnt.app');
  ?>

  <?php gallery_link('Wyrd Forest',
    'A demo done for <a href="https://www.patreon.com/castleengine">Castle Game Engine supporters on Patreon</a>, featuring terrain editor and planting trees and destructible shooting targets with physics. The <a href="https://github.com/castle-engine/wyrd-forest">source code and binaries are on GitHub</a>.',
    'wyrd_forest_screen_0-2.png',
    'https://www.patreon.com/posts/wyrd-forest-demo-15811244');
  ?>

  <?php gallery_link('Mazer',
    'A maze generation algorithm and a simple game behind that. By Eugene Loza.',
    'mazer.jpg',
    'https://decoherence.itch.io/mazer'); ?>

  <?php gallery_link('Fire Madness',
    'Top-down bullet hell shooter inspired by CrossFire game. By Eugene Loza. <a href="https://github.com/eugeneloza/FireMadness">The source code is available on GitHub</a>. <!--a href="http://opengameart.org/forumtopic/fire-madness-top-down-shooter-bullet-hell">See also the opengameart thread</a-->',
    'fire_madness.png',
    'https://decoherence.itch.io/fire-maddness'); ?>

  <?php gallery_link('Dragon Squash',
    'Android game where you defend beautiful animated castles. For Android, with Google Games integration and dragons.',
    'dragon_squash_screen_1.png', 'https://play.google.com/store/apps/details?id=net.sourceforge.castleengine.dragonsquash'); ?>

  <?php gallery_link('Dragon Spine',
    'Fly your dragon in a dark comic setting. Demo of <i>Spine animations</i> and <i>Google Games achievements</i> inside Castle Game Engine on Android. Available for free from Google Play, the source code is inside the engine examples (<a href="https://github.com/castle-engine/castle-engine/tree/master/examples/2d_dragon_spine_game">2d_dragon_spine_game</a>).',
    'castle_spine_screen_3.png', 'https://play.google.com/store/apps/details?id=net.sourceforge.castleengine.castlespine'); ?>

  <?php gallery_link('Hydra Battles',
    'Isometric RTS game for 2 players, with some twists. <a href="https://github.com/castle-engine/hydra-battles">Source code on GitHub</a>.',
    "hydra_battles_screen_best.png", 'http://michaliskambi.itch.io/hydra-battles'); ?>

  <?php gallery_link('Mountains Of Fire',
    '3D game with split-screen view where human and worm cooperate to survive. For single player or 2 players.',
    "mountains_of_fire_screen_1.png", 'mountains_of_fire'); ?>

  <?php gallery_link('Little Things',
    'A pretty 3D game-like experience where you swim towards a sound. Developed during 48h gamejam.',
    'little_things_screen_7.png',
    'https://github.com/castle-engine/little-things');
  ?>

  <?php gallery_link('Darkest Before the Dawn',
    'Scary 3D game, for Android and standalone.',
    "darkest_before_dawn_2.png", 'darkest_before_dawn'); ?>

  <?php gallery_link('&quot;The Castle&quot;',
    'First-person perspective game, in a dark fantasy setting.',
    "castle_screen_demo_1.png",
    'castle'); ?>

  <?php gallery_link('lets_take_a_walk',
    "Small 3D toy, demonstrating rendering integrated with 3D sound using Castle Game Engine. The source code is now available in <a href=\"https://github.com/castle-engine/castle-engine/tree/master/examples/audio/game_3d_sound\">examples/audio/game_3d_sound/</a> in engine sources.",
    'lets_take_a_walk_screen_demo.png',
    'https://github.com/castle-engine/castle-engine/tree/master/examples/audio/game_3d_sound'); ?>

  <?php gallery_link('malfunction',
    'Small 3D space-shooter. One of the first games made using (an ancient version of) Castle Game Engine, with VRML models.',
    'malfunction_screen_demo.png',
    'malfunction'); ?>

   <?php gallery_link('kambi_lines',
     'Arrange colored balls in lines. Quickly.',
     'kambi_lines_screen_demo.png', 'kambi_lines'); ?>
</div>

<p>More:

<ul>
  <?php gallery_link_noimage("Decoherence game collection on itch.io",
    'Various games by Eugene Loza, some using Castle Game Engine.',
    "https://decoherence.itch.io/");
  ?>

  <?php gallery_link_noimage("Michalis game collection on itch.io",
    'Various games by Michalis Kamburelis, all using Castle Game Engine. You can <a href="all_programs_sources.php">download sources of all these programs here</a>. They are also available on GitHub, <a href="https://github.com/castle-engine/">as part of GitHub Castle Game Engine organization</a>.',
    "http://michaliskambi.itch.io/");
  ?>
</ul>

<?php castle_footer(); ?>
