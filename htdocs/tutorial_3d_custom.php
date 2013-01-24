<?php
require_once 'castle_engine_functions.php';
tutorial_header('Define other 3D objects');
?>

<p>Our creature and items classes in
 <?php api_link('CastleCreatures', 'CastleCreatures.html'); ?> and
 <?php api_link('CastleItems', 'CastleItems.html'); ?>
 units are not magic, they do not get any special treatment from the
scene manager. Instead, they just extend the base 3D classes in
<?php api_link('Castle3D', 'Castle3D.html'); ?> unit.
There are base classes to represent 3D things, that
can move, can collide with other 3D stuff and so on. If you find that
our creatures/items design is not enough for you, or maybe you want to
have in your 3D world something that doesn't really fit the
creature/item definition, you can always derive new classes from the
<?php api_link('Castle3D', 'Castle3D.html'); ?>
 classes. Like <?php api_link('T3DAlive', 'Castle3D.T3DAlive.html'); ?>,
 <?php api_link('T3DOrient', 'Castle3D.T3DOrient.html'); ?>,
 <?php api_link('T3DTransform', 'Castle3D.T3DTransform.html'); ?>.

<p>Note that every <?php api_link('T3D', 'Castle3D.T3D.html'); ?> object knows
it's <?php api_link('World', 'Castle3D.T3D.html#World'); ?>
 so it knows how to move and collide
within the 3D world. That's how AI is implemented.
See <?php api_link('T3D.Move', 'Castle3D.T3D.html#Move'); ?>,
 <?php api_link('T3D.MoveAllowed', 'Castle3D.T3D.html#MoveAllowed'); ?>,
 <?php api_link('T3D.Height', 'Castle3D.T3D.html#Height'); ?> and
 <?php api_link('T3D.LineOfSight', 'Castle3D.T3D.html#LineOfSight'); ?> methods.

<?php
tutorial_footer();
?>
