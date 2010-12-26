<?php
  require_once 'vrml_implementation_common.php';
  x3d_status_header('Sound', 'sound',
    'This component provides sound support.
    <tt>AudioClip</tt> defines a sound file and allows to start and stop playback.
    <tt>Sound</tt> positions the sound in 3D world and configures sound
    parameters. Sounds may be spatialized (3D) or not.');

  echo vrmlengine_thumbs(array(
  array('filename' => 'sound.png', 'titlealt' => 'Sound demo (from Kambi VRML test suite)'),
));
?>

<p><?php echo x3d_node_link('Sound'); ?> defines a sound in the 3D world.
It contains (in the "<tt>source</tt>" field) an <tt>AudioClip</tt> node.
Supported fields:</p>

<ul>
  <li>intensity</li>
  <li>location (correctly transformed by Sound transformation; animating either location field and the transformation around it also works perfectly)</li>
  <li>priority (we have a smart sounds allocator, so it actually matters when you have many sounds playing at once)</li>
  <li>source (to indicate AudioClip node)</li>
  <li>spatialize. Note that multi-channel (e.g. stereo) sounds are never spatialized.</li>
</ul>

<p>TODO:</p>

<ul>
  <li>All sound nodes are treated like they would be in an active graph part.
    In other words, a Sound node plays the same, regardless if it's an
    active or inactive child of LOD / Switch child nodes.</li>
  <li>The ellipsoids defining sound range don't matter.
    So Sound.direction, min/maxBack/Front are ignored.
    Sound is always spatialized following the default OpenAL settings.
    Beware of placing too many simultaneously playing sounds in your 3D world.

    <p>This will be partially fixed for 3.8.0 release, such that minFront
    and maxFront will matter.</li>
  <li>Sounds are not sorted exactly like the specification says.
    In short, we only look at your given "priority" field,
    disregarding current sound distance to player and such.</li>
</ul>

<p><?php echo x3d_node_link('AudioClip'); ?> defines the sound file to be played and basic playback
properties. It also provides events to start/stop playing of the sound.
Supported fields:</p>

<ul>
  <li>url (allowed sound file formats are OggVorbis and (uncompressed) WAV).</li>
  <li>duration_changed</li>
  <li>loop (we loop without any glitches between)</li>
  <li>description (is simply ignored, this is valid behavior)</li>
  <li>pitch (correctly changes both the sound pitch and speed of playing)</li>
  <li>all time-dependent fields (start/stop/pause/resumeTime, elapsedTime, isActive, isPaused).

    <p>TODO: But we don't really control our position (offset) within the
    sound. When we detect that we should be playing, but we're not &mdash;
    we just start playing, always from the beginning.</p>
</ul>

<p>TODO: There's no streaming implemented yet. So too many and too long
music files in one world may cause large memory consumption,
and at the first play of a long sound there may be a noticeable loading delay.</p>

<p>TODO: <tt>Sound</tt> and <tt>AudioClip</tt> nodes should only be
instantiated once in the scene (don't USE it many times).
The specification doesn't say what to do on multiple instantiated AudioClips,
and I see no sensible interpretation. Playing all instances would be useless,
as all audio clips have to start/stop playing at the same time
(startTime and such is defined at AudioClip), so you would hear the same sound
from various points of 3D space.</p>

<p>TODO: Only <tt>AudioClip</tt> works as sound source for now.
You cannot use <tt>MovieTexture</tt> as sound source.</p>

<?php
  x3d_status_footer();
?>
