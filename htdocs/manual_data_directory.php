<?php
require_once 'castle_engine_functions.php';
castle_header('Data directory');
?>

<p>The <code>data</code> subdirectory of your <i>Castle Game Engine</i>
project is somewhat special. For typical games (that are distributed with
read-only data files) it is recommended to put all your data inside the
<code>data</code> subdirectory, and load them using a special URL
protocol <code>castle-data:/xxx</code>.

<!--
Loading from
<code>castle-data:/images/my_image.png</code> is equivalent to loading from
<code>ApplicationData('images/my_image.png')</code>.
We advise to use only <code>castle-data:/xxx</code> for code targeting the new CGE
versions &mdash; it is simpler, can be saved to files better (useful when saving CGE editor designs,
X3D models with URLs etc.),
and it works better in some corner-cases (e.g. <?php api_link('FindFiles', 'CastleFindFiles.html#FindFiles'); ?> will work
on Android, thanks to an internal information stored in the data on Android).
-->

<p>Advantages:

<ol>
  <li><p>This directory is automatically correctly packaged by the
    <a href="https://castle-engine.io/build_tool">CGE build tool</a>
    and editor.
    E.g. it will be correctly added to the Android <code>apk</code> file,
    iOS or Nintendo Switch application.

  <li><p>It is detected in a smart way. E.g. it allows to place your data files
    in a system-wide location on Unix.

  <li><p>It can be customized using the
    <?php api_link('ApplicationDataOverride', 'CastleFilesUtils.html#ApplicationDataOverride'); ?>
    global variable.
</ol>

<p>Note that you do not have to place your files inside the <code>data</code>
subdirectory, or use the <?php api_link('ApplicationData', 'CastleFilesUtils.html#ApplicationData'); ?> function
or <code>castle-data:/xxx</code> URLs,
 if you don't want to.
You can always load a file from any filename
or URL, so you can open any file on disk etc.
However, using <code>data</code> subdirectory is adviced for typical cross-platform games.
This way the build tool will automatically package your game correctly.

<p>Example things to put in the <code>data</code> subdirectory:

<ul>
  <li><p>Game 3D and 2D models,
    loaded e.g. by
    <?php echo pascal_highlight('MyCastleSceneInstance.Load(\'castle-data:/my_model.x3d\')'); ?>

    <p>See <a href="https://castle-engine.io/manual_load_3d.php">loading 3D models</a>.

  <li><p>2D images, loaded e.g. by
    <?php echo pascal_highlight('MyImage := TDrawableImage.Create(\'castle-data:/my_image.png\')'); ?>

    <p>See <a href="https://castle-engine.io/manual_2d_ui_custom_drawn.php">loading images</a>.

  <li><p>Sounds, loaded e.g. by
    <?php echo pascal_highlight('MySoundBuffer := SoundEngine.LoadBuffer(\'castle-data:/my_sound.wav\')'); ?>

    <p>See <a href="https://castle-engine.io/manual_sound.php">loading sounds</a>.

  <li><p>... and really anything else you plan to load during the game.
    Your custom files can be loaded using
    <?php echo pascal_highlight('MyStream := Download(\'castle-data:/my_binary_file\')'); ?>
    or
    <?php echo pascal_highlight('MyTextReader := TTextReader.Create(\'castle-data:/my_text_file.txt\')'); ?>

    <p>See <a href="https://castle-engine.io/manual_network.php">loading from URLs</a>
    and the <?php api_link('CastleDownload', 'CastleDownload.html'); ?> unit.
</ul>

<p>Note that the data contents should be treated as read-only in cross-platform
applications. In some cases it may be writeable (e.g. on desktop, when the application
files are owned by the current user) but on some platforms it may be read-only
(e.g. in case of Android, it resides in a special area which cannot be modified by code).

<?php
castle_footer();
?>
