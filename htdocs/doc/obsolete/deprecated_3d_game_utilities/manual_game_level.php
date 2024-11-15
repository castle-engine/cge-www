<?php
require_once 'castle_engine_functions.php';
castle_header('Loading game level');

echo castle_thumbs(array(
  array('filename' => 'fps_game_screen_18.png', 'titlealt' => 'FPS game demo'),
  array('filename' => 'fps_game_blender.png', 'titlealt' => 'FPS game demo - design of level in Blender'),
));
?>

<p>We can load a game level using the
<?php echo cgeRef('TCastleScene'); ?>
 class, described in the previous chapters. Same goes for the game creatures,
and actually for everything else you need to display in the game.
 <?php echo cgeRef('TCastleScene'); ?>
 is really versatile:)

<p>We also have a comfortable way to manage typical levels in a 3D game.
To load a level create the
<?php echo removedCgeRef('TLevel'); ?> instance and call the
<?php echo removedCgeRef('TLevel.Load'); ?> method. Under the hood,
<?php echo removedCgeRef('TLevel.Load'); ?>
 uses <?php echo cgeRef('TCastleScene'); ?>,
 adding it to the <?php echo cgeRef('TCastleViewport.Items'); ?>, and setting
as
<?php echo removedCgeRef('Viewport.Items.MainScene'); ?>.
It also does something extra: it detects a "placeholders" in your 3D model,
which should be replaced with creatures and items.</p>

<p>It is also integrated with <?php echo removedCgeRef('CastleLevels'); ?> definition of what a "level"
is: namely, it's a <code>level.xml</code> file referencing a 3D model and
describing some additional level properties.
For each level, you create a file named <code>level.xml</code>,
with contents like this:</p>

<?php echo xml_full_highlight(
'<?xml version="1.0"?>
<level
  name="pits"
  type="Level"
  scene="pits.x3dv"
  title="The Pits of Azeroth"
  placeholders="blender"
/>'); ?>

<p>See <?php echo a_href_page('creating levels',
'creating_data_levels'); ?> for more details about what you can define
using <code>level.xml</code> files. In short, the most important attributes are:</p>

<ul>
  <li><code>"name"</code> is a unique internal name for the level, it will come in handy soon.
  <li><code>"type"</code> identifies the ObjectPascal class handling the level logic. Type <code>"Level"</code> is always available, for advanced games you can also create and use your own level logic types.
  <li><code>"scene"</code> is an URL or a filename of the 3D model containing the level. Usually it's a simple filename, but you could as well use here an <code>http:</code> URL to <?php echo a_href_page('download the actual 3D data from the Internet', 'manual_network'); ?>.
  <li><code>"title"</code> is just used for some logging and messages. Depending on your game, you may also find this title useful for other purposes, e.g. show the user a list of levels available to start. But this is up to you. The engine itself doesn't really make much use of the title. There are other optional properties, like "demo" or "number" that you may use.
  <li><code>"placeholders"</code> says how to detect placeholders (for creatures and other things) in your 3D level model. This is optional, but highly useful. It should reflect the modeler/exporter used to create this level 3D model.
</ul>

<p>Traditionally, I place each level in a subdirectory of <code>data/levels/</code>,
along with it's associated 3D models. So I have a structure like this:</p>

<pre>data/
  levels/
    pits/
      level.xml
      pits.x3dv
    swamp/
      level.xml
      swamp.x3dv
    mountains/
      level.xml
      mountains.x3dv
    ...</pre>

<p>Although this convention proved comfortable, it's not forced at
all. The 3D model files may live in other directories than <code>level.xml</code>
files, and <code>"scene"</code> field is actually a relative URL to get them.
And <code>level.xml</code> files may be scattered
throughout your directory as you wish. And what is "data directory" is
configurable (by <?php echo removedCgeRef('Levels.LoadFromFiles'); ?> optional parameter, by default
we use <a href="data">data directory</a>). It's only important that the XML files are named
<code>level.xml</code>.</p>

<p>You can now start a game by scanning all levels information from
<code>level.xml</code> files, and then loading your desired level by referring to
it's <code>"name"</code> field:</p>

<?php echo pascal_highlight(
'uses ..., CastleLevels;

...
Levels.LoadFromFiles;

Level := TLevel.Create(Application);
Level.Viewport := Viewport;
Level.Load(\'pits\'); // refer to name="pits" in level.xml
// the last line is a shortcut for
// Level.Load(Levels.FindName(\'pits\'));'); ?>

<p>The <?php echo removedCgeRef('TLevel.Load'); ?> will clear all <code>Viewport.Items</code> (except <code>Level.Player</code>,
more about this in a second). Then it will load new 3D model with a
level (adding it to <code>Viewport.Items</code> and setting as
<code>Viewport.Items.MainScene</code>, just as we did manually in previous chapter),
and do some more interesting stuff that we'll learn later.</p>

<p>The important thing here is that (except the "name" of the 1st
level) levels are not hardcoded in any way in your program. This makes
game data nice for people preparing new content: they can just drop
new subdirectory into the <code>data/levels/</code>, and if it will contain proper
<code>level.xml</code> file, it will be automatically recognized. A similar scheme
will be used for creatures/items in the following chapter. Your game
data is immediately friendly to MODders.</p>

<?php
castle_footer();
?>
