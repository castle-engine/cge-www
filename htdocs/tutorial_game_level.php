<?php
require_once 'castle_engine_functions.php';
tutorial_header('Loading game level');
?>

<p>Instead of loading level like above, there is a slightly more
comfortable and feature-rich way to load a level for a game: call the
<?php api_link('TGameSceneManager.LoadLevel', 'CastleLevels.TGameSceneManager.html#LoadLevel'); ?> method. Under the hood,
<?php api_link('TGameSceneManager.LoadLevel', 'CastleLevels.TGameSceneManager.html#LoadLevel'); ?> does eberything mentioned above &mdash; the
new 3D model is loaded, and added to the SceneManager.Items, and set
as SceneManager.MainScene. But it also does some additional work, like
detecting a "placeholders" in your 3D model, which should be replaced
with creatures (from CastleCreatures unit) and items (from CastleItems
unit).</p>

<p>It is also integrated with CastleLevels definition of what a "level"
is: namely, it's a level.xml file referencing a 3D model and
describing some additional level properties.</p>

<p>The TGameSceneManager class allows you to define levels by using
simple XML files. For each level, you create a file named "level.xml",
with contents like this:</p>

<?php echo xml_highlight(
'<?xml version="1.0"?>
<level
  name="pits"
  type="Level"
  scene="pits.x3dv"
  title="The Pits of Azeroth"
  placeholders="blender"
/>'); ?>

<ul>
  <li>"name" is a unique internal name for the level, it will come in handy soon.
  <li>"type" identifies the ObjectPascal class handling the level logic &mdash; type "Level" is always available, for advanced games you can also create and use your own level logic types.
  <li>"scene" is a filename of the 3D model containing the level.
  <li>"title" is just used for some logging and messages. Depending on your game, you may also find this title useful for other purposes, e.g. show the user a list of levels available to start. But this is up to you. The engine itself doesn't really make much use of the title. There are other optional properties, like "demo" or "number" that you may use.
  <li>"placeholders" says how to detect placeholders (for creatures and other things) in your 3D level model. This is optional, but highly useful.
</ul>

<p>See <?php echo a_href_page('creating levels',
'creating_data_levels'); ?> for more details about what you can define
using <tt>level.xml</tt> files.</p>

<p>Traditionally, I place each level in a subdirectory of <tt>data/levels/</tt>,
along with it's associated 3D models. So I have a structure like this:</p>

<pre class="sourcecode">data/
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
all. The 3D models files may live in other directories than level.xml
files, as "scene" field may contain a relative filename (in future, it
should be possible to use there any absolute or relative URL; for now,
only filenames are allowed). And level.xml files may be scattered
throughout your directory as you wish. And what is "data directory" is
configurable (by Levels.LoadFromFiles optional parameter, by default
we use ProgramDataPath). It's only important that the XML files have
names "level.xml".</p>

<p>You can now start a game by scanning all levels information from
level.xml files, and then loading your desired level by referring to
it's "name" field:</p>

<?php echo pascal_highlight(
'uses ..., CastleLevels;

...
Levels.LoadFromFiles;
SceneManager.LoadLevel(\'pits\'); // refer to name="pits" in level.xml
// the 2nd line is a shortcut for
// SceneManager.LoadLevel(Levels.FindName(\'pits\'));'); ?>

<p>The <?php api_link('TGameSceneManager.LoadLevel', 'CastleLevels.TGameSceneManager.html#LoadLevel'); ?> will clear all SceneManager.Items (except Player,
more about this in a second). Then it will load new 3D model with a
level (adding it to SceneManager.Items and setting as
SceneManager.MainScene, just as we did manually in previous chapter),
and do some more interesting stuff that we'll learn later.</p>

<p>The important thing here is that (except the "name" of the 1st
level) levels are not hardcoded in any way in your program. This makes
game data nice for people preparing new content: they can just drop
new subdirectory into the data/levels/, and if it will contain proper
level.xml file, it will be automatically recognized. A similar scheme
will be used for creatures/items in the following chapter. Your game
data is immediately friendly to MODders.</p>

<p>Note that you have to initialize OpenGL context before
calling <?php api_link('TGameSceneManager.LoadLevel', 'CastleLevels.TGameSceneManager.html#LoadLevel'); ?>.
That's because loading level wants to prepare resources for OpenGL rendering.

<ol>
  <li><p>If you use
    <?php api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html'); ?>,
    make sure that
    <?php api_link('Open method', 'CastleWindow.TCastleWindowBase.html#Open'); ?>
    was already called.
    By the way, you can also initialize progress interface, to see nice progress
    bar when loading. Like this:

    <?php echo pascal_highlight(
'uses ..., CastleProgress, CastleWindowProgress;

...
Window.Open; // this goes before preparing level

{ initialize progress bar to use our window }
Progress.UserInterface := WindowProgressInterface;
WindowProgressInterface.Window := Window;

Levels.LoadFromFiles;
SceneManager.LoadLevel(\'pits\');

Application.Run; // this goes after preparing level (and everything else)'); ?>

  <li><p>If you use Lazarus
    <?php api_link('TCastleControl', 'CastleControl.TCastleControl.html'); ?>,
    make sure you call
    <?php api_link('TGameSceneManager.LoadLevel', 'CastleLevels.TGameSceneManager.html#LoadLevel'); ?>
    from
    <?php api_link('OnGLContextOpen event', 'CastleControl.TCastleControlBase.html#OnGLContextOpen'); ?>
    event (or something that occurs even later).
</ol>

<?php
tutorial_footer();
?>
