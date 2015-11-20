<?php
require_once 'castle_engine_functions.php';
tutorial_header('Sound');
?>

<p>As with many other operations, you can add and control sounds to your
game either by ObjectPascal code, or by editing your data files. This gives
flexibility both to a programmer and the content designer. It's your
choice which approach you use &mdash; usually it's better to keep as
much as possible in data files, and use ObjectPascal only when necessary for
non-trivial situations.</p>

<h2>Loading and playing sound inside VRML/X3D</h2>

<p>First, get a sample sound file and place it within your game data.
You can find some sample files inside <code>examples/fps_game/data/sounds/</code>,
or in <?php echo a_href_page('our demo VRML/X3D models', 'demo_models'); ?>
 (subdirectory <code>sound/</code>),
or on websites like <a href="http://opengameart.org/">OpenGameArt.org</a>.
</p>

<p>To add a looping sound to your VRML/X3D file in classic encoding
(<code>xxx.x3dv</code> files) add this:</p>

<pre>Sound {
  source AudioClip { url "sample.wav" loop TRUE }
}</pre>

<p>Remember that URL <code>"sample.wav"</code> is specified relative to the location
of your <code>xxx.x3dv</code> file. In the simplest case, just place both <code>xxx.x3dv</code>
and <code>sample.wav</code> in the same directory, and you're fine.</p>

<h2>Loading and playing sound inside ObjectPascal code</h2>

<p>To play a sound using direct code, do this:</p>

<?php echo pascal_highlight(
'// add this to your uses clause:
uses ..., CastleSoundEngine;

// use this at initialization:
var
  Buffer: TSoundBuffer;
procedure LoadSoundBuffers;
begin
  Buffer := SoundEngine.LoadBuffer(ApplicationData(\'sample.wav\'));
end;

// during the game, call this to play a sound:
SoundEngine.PlaySound(Buffer);'); ?>

<p>See
<?php api_link('SoundEngine.PlaySound', 'CastleSoundEngine.TSoundEngine.html#PlaySound'); ?>
 docs for the description of parameters.

<p>You can free the buffer once the sound has stopped. It's not important
for simple programs, as we will take care to always free it before
closing OpenAL context.</p>

<h2>Sounds repository</h2>

<p>Larger games may find it comfortable to define a repository of sounds.
You do it by creating an XML file, for example named <code>sounds.xml</code>,
looking like this:</p>

<?php echo xml_highlight(
'<?xml version="1.0"?>
<sounds>
  <sound name="sample" url="sample.wav" />
  <!-- Actually, you can omit the url, by default it\'s the same
    as sound name with .wav extension. -->
</sounds>'); ?>

<p>See
<?php echo a_href_page('creating game sounds guide', 'creating_data_sound'); ?>
 for detailed specification about sound XML files.
See <?php api_link('TRepoSoundEngine', 'CastleSoundEngine.TRepoSoundEngine.html'); ?> docs
 and <code>examples/fps_game/</code> for example.
You have to initialize the sound repository inside your game code like this:</p>

<?php echo pascal_highlight(
'SoundEngine.RepositoryURL := ApplicationData(\'sounds/index.xml\');'); ?>

<p>After this, you can refer to your sound names from files like
<code>resource.xml</code> (for creatures/items sounds)
or <code>material_properties.xml</code>  (for footsteps)
or <code>level.xml</code> (for level music).
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

<p>The
<?php api_link('SoundEngine.Sound3D', 'CastleSoundEngine.TRepoSoundEngine.html#Sound3D'); ?>
 and
<?php api_link('SoundEngine.Sound', 'CastleSoundEngine.TRepoSoundEngine.html#Sound'); ?>
 are a little easier to use than
<?php api_link('SoundEngine.PlaySound', 'CastleSoundEngine.TSoundEngine.html#PlaySound'); ?>,
 they have fewer parameters. That is
because the default sound properties (it's individual gain, importance
(priority), URL and other stuff) is already recorded in
the sounds XML file. That's one advantage of using the
sounds repository: all your sounds properties are centrally stored in
the sounds XML file.</p>

<!--
<p>You can also refer to your sound names from VRML/X3D AudioClip node,
using the "sounds-repository" protocol:</p>

<pre>Sound {
  source AudioClip { url "sounds-repository:sample" loop TRUE }
}</pre>
-->

<!--
Mentioned above already?

<h2>Level music</h2>

There is a special comfortable way to enable looping music on a level,
if you use <code>level.xml</code> file with TGameSceneManager.LoadLevel. Simply add
<code>music_sound="xxx"</code> attribute to the root element of your
<code>level.xml</code> file, where <code>xxx</code> refers to a sound name
defined in <code>data/sounds/index.xml</code>.
-->

<h2>More</h2>

<p>For more advanced uses, you can use the return value of
<?php api_link('SoundEngine.PlaySound', 'CastleSoundEngine.TSoundEngine.html#PlaySound'); ?>,
<?php api_link('SoundEngine.Sound3D', 'CastleSoundEngine.TRepoSoundEngine.html#Sound3D'); ?> or
<?php api_link('SoundEngine.Sound', 'CastleSoundEngine.TRepoSoundEngine.html#Sound'); ?>.
It's either <code>nil</code> (if no OpenAL resources were available to
play this sound, and it's priority doesn't allow overriding other
sounds) or it's a
<?php api_link('TSound', 'CastleSoundAllocator.TSound.html'); ?>
 instance. If you have
 <?php api_link('TSound', 'CastleSoundAllocator.TSound.html'); ?>
 instance, you can save it to a variable and use for various purposes.
 For example you can update sound parameters during the game,
 e.g. changing
 <?php api_link('TSound.Position', 'CastleSoundAllocator.TSound.html#Position'); ?>,
 <?php api_link('TSound.Gain', 'CastleSoundAllocator.TSound.html#Gain'); ?>
 and such.
 You can use it's <?php api_link('TSound.OnRelease', 'CastleSoundAllocator.TSound.html#OnRelease'); ?>
 event to be notified when source stops
playing. You can stop playing the sound
by <?php api_link('TSound.Release', 'CastleSoundAllocator.TSound.html#Release'); ?>.</p>

<h2>Predefined sounds</h2>

<p>Some engine components already define some sound names. To make them
defined, just use the appropriate names in your sounds XML file described above.
They will be automatically found and played by engine components.

<p>See <i>"Common sounds"</i> section in
<?php api_link('CastleSoundEngine', 'CastleSoundEngine.html'); ?>
 unit sources for a current list of predefined sound names.

<?php
tutorial_footer();
?>
