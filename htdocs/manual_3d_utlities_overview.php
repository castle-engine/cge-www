<?php
require_once 'castle_engine_functions.php';
castle_header('Overview');
?>

<p><b>Units described here are deprecated.</b>

<ul>
  <li>
    <p>Most of their features can be achieved easily
     (and with much more flexibility) by using <a href="viewport_and_scenes">viewport and scenes</a> directly.
     See <a href="planned_features.php">our plans</a> for detailed reasoning.

  <li>
    <p>In particular, using <a href="viewport_and_scenes">viewport and scenes</a> directly
    allows to design your levels using our <a href="manual_editor.php">visual editor</a>,
    and benefit from all the features we have (composing level from a number of scenes and primitives,
    assigning skybox)
    and will have (placing lights, editing materials) in the CGE editor.

  <li>
    <p>The most prominent reason to keep still using these units is that <code>CastleCreatures</code>
    gives you <i>"ready creatures AI (artificial intelligence)"</i>.
    We don't have an equivalent of if <i>yet</i> in non-deprecated units... but we work on it.
    We will expose flexible creature AI as <code>TCastleBehavior</code> descendant class
    called <code>TCastleMoveAttack</code>.</b>
</ul>

<p>Units described in this section
implement a <i>common logic typical to 3D games</i>.

<p>Using these units is <i>optional</i>. You can implement a perfect 3D game
without them (just use <a href="viewport_and_scenes">viewport and scenes</a>,
the viewport already gives you flexible camera and navigation features)
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
    <?php api_link('CastleGameNotifications', 'CastleGameNotifications.html'); ?>.
</ul>

<?php
castle_footer();
?>
