<?php
require_once 'castle_engine_functions.php';
manual_header('Data directory');
?>

<p>The <code>data</code> subdirectory of your <i>Castle Game Engine</i>
project is somewhat special. For typical games (that are distributed with
read-only data files) it is recommended to put all your data inside the
<code>data</code> subdirectory, and load them using the
<?php api_link('ApplicationData', 'CastleFilesUtils.html#ApplicationData'); ?>
 function.

<p>Advantages:

<ol>
  <li><p>This directory is automatically correctly packaged by the
    <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">CGE build tool</a>
    and upcoming editor.
    E.g. it will be correctly added to the Android <code>apk</code> file.

  <li><p>It is detected in a smart way. E.g. it allows to place your data files
    in a system-wide location on Unix.

  <li><p>It can be customized using the
    <?php api_link('ApplicationDataOverride', 'CastleFilesUtils.html#ApplicationDataOverride'); ?>
    global variable.
</ol>

<p>Note that you do not have to place your files inside the <code>data</code>
subdirectory, or use the <?php api_link('ApplicationData', 'CastleFilesUtils.html#ApplicationData'); ?> function,
 if you don't want to.
You can always load a file from any filename
or URL, so you can open any file on disk etc.
However, using <code>data</code> subdirectory is adviced for typical cross-platform games.
This way the build tool will automatically package your game correctly.

<p>Example things to put in the <code>data</code> subdirectory:

<ul>
  <li><p>Game 3D and 2D models,
    loaded e.g. by <code>TCastleScene.Load(ApplicationData('my_model.x3d'))</code>.
    See <a href="https://castle-engine.io/manual_load_3d.php">loading 3D models</a>.

  <li><p>2D images, loaded e.g. by <code>TGLImage.Create(ApplicationData('my_image.png'))</code>.
    See <a href="https://castle-engine.io/manual_2d_ui_custom_drawn.php">loading images</a>.

  <li><p>Sounds, loaded e.g. by <code>SoundEngine.LoadBuffer(ApplicationData('my_sound.wav'))</code>.
    See <a href="https://castle-engine.io/manual_sound.php">loading sounds</a>.

  <li><p>... and really anything else you plan to load during the game.
    Your custom files can be loaded using <code>Download(ApplicationData('my_binary_file'))</code>
    or <code>TTextReader.Create(ApplicationData('my_text_file.txt'))</code>.
    See <a href="https://castle-engine.io/manual_network.php">loading from URLs</a>
    and the <?php api_link('CastleDownload', 'CastleDownload.html'); ?> unit.
</ul>

<?php
manual_footer();
?>
