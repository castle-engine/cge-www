<?php
require_once 'castle_engine_functions.php';
castle_header('Persistent data (user preferences, savegames)');

$toc = new TableOfContents(
  array(
    new TocItem('Storing user preferences locally (UserConfig)', 'user_config'),
    new TocItem('Storing user preferences in the cloud', 'cloud'),
  )
);
?>

<?php echo $toc->html_toc(); ?>
<?php echo $toc->html_section(); ?>

<p>To manage persistent data, like user preferences
or a simple <i>save game</i> values,
use <?php echo cgeRef('CastleConfig'); ?> unit
with a <code>UserConfig</code> singleton inside. A simple example:</p>

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

<p>Some engine components provide ready methods to load / save
their configuration into a
<?php echo cgeRef('TCastleConfig'); ?> instance
(for example into the <code>UserConfig</code>). These include:

<ul>
  <li><?php echo cgeRef('SoundEngine'); ?>
    &mdash; can load/save sound enabled state, sound volume and other parameters.
    See
    <?php echo cgeRef('TSoundEngine.LoadFromConfig'); ?>,
    <?php echo cgeRef('TSoundEngine.SaveToConfig'); ?>.

  <li><?php echo cgeRef('InputsAll'); ?>
    &mdash; input shortcuts (named key and mouse shortcuts) customizations. See
    <?php echo cgeRef('TInputShortcutList.LoadFromConfig'); ?>,
    <?php echo cgeRef('TInputShortcutList.SaveToConfig'); ?>.
</ul>

<p>Note that the engine does <b>not</b> automatically
call the load / save methods mentioned above. We used to call them automatically
(in engine version &lt;= 5.2.0), but this automatization was more trouble
than gain. <small>(It meant that <code>UserConfig.Load</code> could, often by surprise
to the developer, override the sound parameters set by
<code>SoundEngine.ParseParameters</code> or explicit
<code>SoundEngine.Enabled := false</code> code.)</small>
So you're supposed to call them yourself (see example above) if you want to save
these values as user preferences.

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

<?php
castle_footer();
?>
