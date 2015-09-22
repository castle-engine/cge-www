<?php
require_once 'castle_engine_functions.php';
tutorial_header('Persistent data (user preferences, savegames)');
?>

<p>To manage persistent data, like user preferences
or a simple <i>save game</i> values,
use <?php api_link('CastleConfig', 'CastleConfig.html'); ?>.
Some engine components automatically use it
to load and save user preferences (like whether the sound is enabled),
you can also use it for your own custom parameters. A simple example:</p>

<?php echo pascal_highlight(
'uses SysUtils, CastleWindow, CastleConfig;

var
  Window: TCastleWindow;
  MyParameter: string;

function MyGetApplicationName: string;
begin
  Result := \'my_game_name\';
end;

begin
  { make sure application name is correct by setting OnGetApplicationName,
    this is used by Config.Load to determine config file location. }
  OnGetApplicationName := @MyGetApplicationName;

  { load config from file }
  Config.Load;

  { load your own data like this: }
  MyParameter := Config.GetValue(\'my_parameter\', \'default_value\');

  { ... do the main part of your program }
  Window := TCastleWindow.Create(Application);
  Window.OpenAndRun;

  { save your own data like this: }
  Config.SetDeleteValue(\'my_parameter\', MyParameter, \'default_value\');

  { save config to file }
  Config.Save;
end.'); ?>

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
</ul>

<p>See the <?php api_link('TCastleConfig', 'CastleXMLConfig.TCastleConfig.html'); ?>
 for a documentation of our extensions.

<p>While you can load and save the config data at any time,
you can also register
 http://wiki.freepascal.org/xmlconf

    Of course you can also use this for your game specific purposes,
    as Config is just standard FPC TXMLConfig class (with some extensions,
    see CastleXMLConfig unit).

<?php
tutorial_footer();
?>
