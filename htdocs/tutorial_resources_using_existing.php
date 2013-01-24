<?php
  require_once 'tutorial_common.php';
  tutorial_header('Using existing creatures / items classes');
?>

<p>For starters, you can just use the existing creature classes on
CastleCreatures unit.

<p>To get walk-attack-state intelligence, use the
TWalkAttackCreatureResource class. Such creature tracks the player
(remembers last seen player 3D position, walks/flies to it, possibly
through sectors/waypoints — so it can pass through narrow doors in a
labyrinth or walk over a narrow bridge), attacks the player from the
right distance (this can be either a melee attack, or shooting a
missile — which adds a missile to the 3D world), eventually runs from
the player (when he's too close and/or our health is low). For now,
the player is always the main enemy, and there's no "creature vs
creature" fighting, although it can be added easily in the future.

<p>For basic usage, there's no need to even derive new classes. All you
need to do is to create a directory holding creature 3D data and
resource.xml describing it (for example, see various subdirectories of
castle/data/creatures/ in SVN).

<p>There are a lot of settings to achieve particular behavior,
e.g. cowardly/brave, offensive/defensive, melee/ranged, etc.

<p>There is also a "missile" intelligence (TMissileCreatureResource),
that causes the creature to blindly go in given direction (possibly
with gravity and/or homing (close-on-target) features). On impact,
missile may explode, hurting player and/or other creatures. Any kind
of missile, like arrow or lighting bolt, is represented as such
"missile creature", that flies independently of the shooter.

<p>And there is a TStillCreatureResource that just stands still (it can
still show some looping animation, but there is no fancy logic behind
it).

<?php
  tutorial_footer();
?>
