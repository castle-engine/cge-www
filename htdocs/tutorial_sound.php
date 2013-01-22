<?php
  require_once 'tutorial_common.php';
  tutorial_header('Sound');
?>

As with many other operations, you can add and control sounds to your game either by Pascal code, or by editing your data files. This gives flexibility both to a programmer and the content designer. It's your choice which approach you use &mdash; usually it's better to keep as much as possible in data files, and use code only when necessary for non-trivial situations.

--- Loading and playing sound inside VRML/X3D

First, get a sample sound file and place it within your game data. You can use this sample.wav file.

To add a looping sound to your VRML/X3D file, just open xxx.x3dv and paste there this:

[[
Sound {
  source AudioClip { url "sample.wav" loop TRUE }
}
]]

Remember that URL "sample.wav" is specified relative to the location of your xxx.x3dv file. In the simplest case, just place both xxx.x3dv and sample.wav in the same directory, and you're fine.

--- Loading and playing sound inside ObjectPascal code

To play a sound from a code, add this code:

[[
var
  Buffer: TSoundBuffer;
...
Buffer := SoundEngine.LoadBuffer('sample.wav');
SoundEngine.PlaySound(Buffer, ...); // see PlaySound reference for parameters
]]

You can free the buffer once the sound has stopped. It's not important for simple programs, as we will take care to always free it before closing OpenAL context.

--- Sounds repository

Larger games may find it comfortable to define a repository of sounds in an XML file, conventionally named data/sounds/index.xml. See TXmlSoundEngine docs and castle1 game data for example. Assuming your <tt>data/sounds/index.xml</tt> looks like this:

[[
<?xml version="1.0"?>
<sounds>
  <sound name="sample" file_name="sample.wav" />
  <!-- Actually, you can omit the file_name, it's the same
    as name with .wav extension.
    Also, you can add a lot of interesting attributes here, like
    default_importance, gain, min_gain, max_gain --- see TODO. -->
</sounds>
]]

Then you can initialize it inside your game code like this:

[[
SoundEngine.SoundsFileName := ProgramDataPath + 'data' +
  PathDelim + 'sounds' + PathDelim + 'index.xml';
]]

Ater this, you can refer to your sound names from files like resource.xml, TODO: add example. TODO: move section about "sounds repo" up, to the beginning, before other ways to get sound.

This allows to play sounds like this from ObjectPascal code:

[[
var
  SoundType: TSoundType;
...
SoundType := SoundEngine.SoundFromName('sample');
SoundEngine.Sound3D(SoundType, Vector3Single(1, 2, 3), false { looping });
SoundEngine.Sound(SoundType, false { looping }); // non-3D sound
]]

The SoundEngine.Sound3D and SoundEngine.Sound are a little simpler to use than SoundEngine.PlaySound, they have fewer parameters. That is because the default sound properties (it's individual gain, importance (priority), actual filename and other stuff) is already recorded in the data/sounds/index.xml file. That's one advantage of using the sounds repository: all your sounds properties are centrally stored in the data/sounds/index.xml file.

You can also refer to your sound names from VRML/X3D AudioClip node, using the "sounds-repository" protocol:

[[
Sound {
  source AudioClip { url "sounds-repository:sample" loop TRUE }
}
]]

--- Level music

There is a special comfortable way to enable looping music on a level, if you use level.xml file with TGameSceneManager.LoadLevel. Simply add <tt>music_sound="xxx"</tt> attribute to the root element of your <tt>level.xml</tt> file, where <tt>xxx</tt> refers to a sound name defined in <tt>data/sounds/index.xml</tt>.

--- More

For more advanced uses, you can use the return value of PlaySound or Sound or Sound3D: it's either nil (if no OpenAL resources available to play this sound, and it's priority doesn't allow overriding other sounds) or it's a TSound instance. If you have TSound instance, you can use it's TSound.OnRelease event to be notified when source stops playing. You can also use other TSound methods, e.g. update TSound.Position, TSound.Gain and such. You can stop playing the sound by TSound.Release.

--- Predefined sounds

Various engine components already want to use some sounds. To make it actually happen, you need to create file like <tt>data/sounds/index.xml</tt> described above, and define there sounds with a predefined names.

See "Common sounds" section in CastleSoundEngine unit sources for a current list of predefined sound names.

<?php
  tutorial_footer();
?>
