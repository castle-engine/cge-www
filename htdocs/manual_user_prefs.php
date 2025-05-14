<?php
require_once 'castle_engine_functions.php';
castle_header('Persistent data (user preferences, savegames)');

$toc = new TableOfContents(
  array(
    new TocItem('Storing user preferences locally (UserConfig)', 'user_config'),
    new TocItem('Storing user preferences in the cloud', 'cloud'),
    new TocItem('Storing more data using <code>castle-config:/</code> URLs', 'castle_config'),
  )
);
?>

<?php echo $toc->html_toc(); ?>
<?php echo $toc->html_section(); ?>

<p>To manage persistent data, like user preferences
or a simple <i>save game</i> values,
use <?php echo cgeRef('CastleConfig'); ?> unit
with a <?php echo cgeRef('UserConfig'); ?> singleton inside. A simple example:</p>

<?php echo pascal_highlight_file('code-samples/user_prefs_demo.lpr'); ?>

<p>To load and save config values, you should use <code>GetValue</code>
and <code>SetValue</code> (or <code>SetDeleteValue</code>) methods.
See the <a href="http://wiki.freepascal.org/xmlconf"><code>TXMLConfig</code>
class documentation</a>. These provide basic means to load/save
integers, booleans and strings in a simple XML format.

<p>We extend the standard <code>TXMLConfig</code> with more
methods:
<ul>
  <li>to load/save more types (floats, vectors, colors, URLs),
  <li>to load/save from an URL (not just a filename),
  <li>to encrypt/decrypt contents, which may be useful as a simple protection
    against cheaters (if you want this, just set the simple <code>BlowFishKeyPhrase</code> property).
  <li>to add "loader" functions (<code>GetValue</code>, <code>GetVector3</code> etc.)
    that require the presence of given attribute in the XML file.
    They raise an exception when the attribute is missing or invalid.
    This is useful if you want to somewhat validate the XML file
    by the way (for example when it's a game model file that must contain given
    variables).
</ul>

<p>See the <?php echo cgeRef('TCastleConfig'); ?>
 for a documentation of our extensions.

<p>While you can load and save the config data at any time,
you can also register your own load and save listeners using
the
<?php echo cgeRef('TCastleConfig.AddLoadListener'); ?>,
<?php echo cgeRef('TCastleConfig.AddSaveListener'); ?>
 mechanism. This sometimes allows to decentralize your code better.

<?php echo $toc->html_section(); ?>

<p>On Android, our engine allows to easily upload and download
the savegames using the <a href="https://developers.google.com/games/services/common/concepts/savedgames">Google Play Games "Saved Games"</a>
feature. To use this feature:

<ol>
  <li><p>Turn on the <a href="https://github.com/castle-engine/castle-engine/blob/master/tools/build-tool/data/android/integrated-services/google_play_games/README.adoc">Google Play Games integration</a>
    for your project.

  <li><p>Create and initialize the
    <?php echo cgeRef('TGameService'); ?>
    instance in your code. Be sure to pass parameter <code>SaveGames</code>
    as <code>true</code> to the <code>TGameService.Initialize</code>
    call.

  <li><p>Connect player to the Google Play Games at runtime,
    using <?php echo cgeRef('TGameService.RequestSignedIn'); ?> method,
    and / or passing <code>AutoStartSignInFlow</code> as <code>true</code>
    to the <code>TGameService.Initialize</code> call.

    <p>You can wait for the sign-in to happen by the
    <?php echo cgeRef('TGameService.OnSignedInChanged'); ?>
    event, or just observe the
    <?php echo cgeRef('TGameService.SignedIn'); ?>
    property.

  <li><p>Then load and save games using the
    <?php echo cgeRef('TGameService.SaveGameLoad'); ?> and
    <?php echo cgeRef('TGameService.SaveGameSave'); ?>
    methods. They represent the "savegame contents" as a simple string,
    and you can use the <code>UserConfig.SaveToString</code>
    and <code>UserConfig.LoadFromString</code> methods
    to trivially upload / download the <code>UserConfig</code>
    contents to the cloud!

  <li><p>If you want to allow user to choose a "slot" where to save the game,
    or from which to load the game, you can use a ready dialog by calling
    <?php echo cgeRef('TGameService.ShowSaveGames'); ?>.
</ol>

<?php echo $toc->html_section(); ?>

<p>You're not limited to storing the data using <?php echo cgeRef('UserConfig'); ?>.

<p>You can create any files and organize them in any subdirectory hierarchy by saving and loading using <a href="url#castle-config">castle-config:/</a> URLs. Follow the <a href="url">URLs, loading (downloading) and saving resources</a> documentation for more information.

<?php
castle_footer();
?>
