<?php
require_once 'castle_engine_functions.php';
tutorial_header('Sound');
?>

<p>As with many other operations, you can add and control sounds to your
game either by Pascal code, or by editing your data files. This gives
flexibility both to a programmer and the content designer. It's your
choice which approach you use &mdash; usually it's better to keep as
much as possible in data files, and use code only when necessary for
non-trivial situations.</p>

<h2>Loading and playing sound inside VRML/X3D</h2>

<p>First, get a sample sound file and place it within your game data.
You can find some sample files inside <tt>examples/fps_game/data/sounds/</tt>,
or in <?php echo a_href_page('our demo VRML/X3D models', 'demo_models'); ?>
 (subdirectory <tt>sound/</tt>),
or on websites like <a href="http://opengameart.org/">OpenGameArt.org</a>.
</p>

<p>To add a looping sound to your VRML/X3D file, just open xxx.x3dv and
paste there this:</p>

<pre class="sourcecode">Sound {
  source AudioClip { url "sample.wav" loop TRUE }
}</pre>

<p>Remember that URL "sample.wav" is specified relative to the location
of your xxx.x3dv file. In the simplest case, just place both xxx.x3dv
and sample.wav in the same directory, and you're fine.</p>

<h2>Loading and playing sound inside ObjectPascal code</h2>

<p>To play a sound from a code, add this code:</p>

<?php echo pascal_highlight(
'var
  Buffer: TSoundBuffer;
  ...
  Buffer := SoundEngine.LoadBuffer(\'sample.wav\');
  SoundEngine.PlaySound(Buffer, ...); // see PlaySound reference for parameters'); ?>

<p>You can free the buffer once the sound has stopped. It's not important
for simple programs, as we will take care to always free it before
closing OpenAL context.</p>

<h2>Sounds repository</h2>

<p>Larger games may find it comfortable to define a repository of sounds.
You do it by creating an XML file, for example named <tt>sounds.xml</tt>,
looking like this:</p>

<?php echo xml_highlight(
'<?xml version="1.0"?>
<sounds>
  <sound name="sample" file_name="sample.wav" />
  <!-- Actually, you can omit the file_name, it\'s the same
    as name with .wav extension.
    Also, you can add a lot of interesting attributes here, like
    default_importance, gain, min_gain, max_gain &mdash; see TODO. -->
</sounds>'); ?>

<p>See
<?php echo a_href_page('creating game sounds guide', 'creating_data_sound'); ?>
 for detailed specification about sound XML files.
See <?php api_link('TRepoSoundEngine', 'CastleSoundEngine.TRepoSoundEngine.html'); ?> docs
 and <tt>fps_game</tt> for example.
You have to initialize the sound repository inside your game code like this:</p>

<?php echo pascal_highlight(
'SoundEngine.SoundsFileName := ProgramDataPath + \'data\' +
  PathDelim + \'sounds\' + PathDelim + \'index.xml\';'); ?>

<p>Ater this, you can refer to your sound names from files like
<tt>resource.xml</tt> (for creatures/items sounds)
or <tt>material_properties.xml</tt>  (for footsteps)
or <tt>level.xml</tt> (for level music).
See <?php echo a_href_page('creating game data guide', 'creating_data_intro'); ?>
 for reference of these files.</p>

<p>You can also play named sounds from ObjectPascal code:</p>

<?php echo pascal_highlight(
'var
  SoundType: TSoundType;
  ...
  SoundType := SoundEngine.SoundFromName(\'sample\');
  SoundEngine.Sound3D(SoundType, Vector3Single(1, 2, 3), false { looping });
  SoundEngine.Sound(SoundType, false { looping }); // non-3D sound'); ?>

<p>The SoundEngine.Sound3D and SoundEngine.Sound are a little simpler to
use than SoundEngine.PlaySound, they have fewer parameters. That is
because the default sound properties (it's individual gain, importance
(priority), actual filename and other stuff) is already recorded in
the data/sounds/index.xml file. That's one advantage of using the
sounds repository: all your sounds properties are centrally stored in
the data/sounds/index.xml file.</p>

<!--
<p>You can also refer to your sound names from VRML/X3D AudioClip node,
using the "sounds-repository" protocol:</p>

<pre class="sourcecode">Sound {
  source AudioClip { url "sounds-repository:sample" loop TRUE }
}</pre>
-->

<!--
Mentioned above already?

<h2>Level music</h2>

There is a special comfortable way to enable looping music on a level,
if you use <tt>level.xml</tt> file with TGameSceneManager.LoadLevel. Simply add
<tt>music_sound="xxx"</tt> attribute to the root element of your
<tt>level.xml</tt> file, where <tt>xxx</tt> refers to a sound name
defined in <tt>data/sounds/index.xml</tt>.
-->

<h2>More</h2>

<p>For more advanced uses, you can use the return value of PlaySound or
Sound or Sound3D: it's either nil (if no OpenAL resources available to
play this sound, and it's priority doesn't allow overriding other
sounds) or it's a TSound instance. If you have TSound instance, you
can use it's TSound.OnRelease event to be notified when source stops
playing. You can also use other TSound methods, e.g. update
TSound.Position, TSound.Gain and such. You can stop playing the sound
by TSound.Release.</p>

<h2>Predefined sounds</h2>

<p>Some engine components already define some sound names. To make them
defined, just use the appropriate names in your
<tt>data/sounds/index.xml</tt> file described above.

<p>See "Common sounds" section in CastleSoundEngine unit sources for a
current list of predefined sound names.

<?php
tutorial_footer();
?>
