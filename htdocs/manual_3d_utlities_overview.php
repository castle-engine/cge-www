<?php
require_once 'castle_engine_functions.php';
manual_header('Overview');
?>

<p>Units described in this section of the manual are optional,
and independent from the rest of the engine.
They implement a <i>common logic typical to 3D games</i>.

<p>The units described here are in the CGE source code subdirectory <a href="https://github.com/castle-engine/castle-engine/tree/master/src/game">src/game/</a>,
they are clearly separated from the rest of CGE code.
Using these units is <i>optional</i>. You can implement a perfect 3D game
without them (you only need to use <a href="manual_scene.php">scenes</a>,
and the scene manager already gives you flexible camera and navigation features)
and have full flexibility of a general 3D engine.
Using these units makes sense if your game concepts fit within typical concepts
of the <i>3D first-person shooter games</i>, described below.

<p>The features provided are:

<ul>
  <li><p><?php api_link('CastlePlayer unit', 'CastlePlayer.html'); ?>:
    Single player management.
    Player has hit points, may collect items, may use weapons (short-range,
    immediate shooting,
    missile shooting), may be swimming, may have footsteps sound
    depending on the terrain.

  <li><p><?php api_link('CastleCreatures unit', 'CastleCreatures.html'); ?>:
    Creatures with various AI (<i>artificial intelligence</i>).
    AI ranges from smart walk-attack-flee, to a trivial "missile" (fly along a direction,
    explode on hit) AI.

  <li><p><?php api_link('CastleItems unit', 'CastleItems.html'); ?>:
    Items that are pickable, may lie on the level, may be in player's inventory,
    have icons.

  <li><p><?php api_link('CastleLevels unit', 'CastleLevels.html'); ?>:
    Levels allow to load a 3D scene with placeholders to define creatures and items,
    to define water volume, to define move limit for player etc.

  <li><p>And utilities for the above units:
    <?php api_link('CastleResources', 'CastleResources.html'); ?>,
    <?php api_link('CastleGameNotifications', 'CastleGameNotifications.html'); ?>,
    <?php api_link('CastleDebugTransform', 'CastleDebugTransform.html'); ?>,
</ul>

<?php
manual_footer();
?>
