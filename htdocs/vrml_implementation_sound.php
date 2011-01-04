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

<p><b>Work in progress: the sound support is not released yet.
You have to use <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/">nightly
builds</a> to test features described here.</b></p>

<p><?php echo x3d_node_link('Sound'); ?> defines a sound in the 3D world.
It contains (in the "<tt>source</tt>" field) an <tt>AudioClip</tt> node.
Supported fields:</p>

<ul>
  <li>intensity</li>
  <li>location (correctly transformed by Sound transformation; animating either location field and the transformation around it also works perfectly)</li>
  <li>priority (we have a smart sounds allocator, so it actually matters when you have many sounds playing at once)</li>
  <li>source (to indicate AudioClip node)</li>
  <li>spatialize. Note that multi-channel (e.g. stereo) sounds are never spatialized.</li>
  <li>minFront and maxFront values are handled.</li>
</ul>

<p>TODO:</p>

<ul>
  <li><p>Our sound attenuation model is a little simpler than VRML/X3D requirements.
    We have an inner sphere, of radius minFront, where sound gain is at maximum.
    And we have an outer sphere, of radius maxFront, where sound gain drops
    to zero. Between them sound gain drops linearly.</p>

    <p>Contrast this with VRML/X3D spec, that requires two ellipsoids
    (not just spheres). In our implementation, the sounds at your back
    are attenuated just the same as the front sounds.
    We simply ignore direction, minBack and maxBack fields.</p></li>

  <li><p>All sound nodes are treated like they would be in an active graph part.
    In other words, a Sound node plays the same, regardless if it's an
    active or inactive child of LOD / Switch child nodes.</li>

  <li><p>Sounds are not sorted exactly like the specification says.
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

<li><i>Note about multiple instances:</i> VRML/X3D define the play/stop events
at the <tt>AudioClip</tt> node (not at higher-level <tt>Sound</tt> node,
which would be more useful IMO). This means that <tt>USE</tt>ing many times
the same <tt>AudioClip</tt> or <tt>Sound</tt> nodes doesn't make much sense.
You can only achieve the same sound, playing simultaneously the same thing,
from multiple 3D sources. Since such simultaneous sounds are pretty useless,
we don't even implement them (so don't reUSE <tt>AudioClip</tt> or <tt>Sound</tt> nodes,
or an arbitrary one will be playing).
If you want to use the same sound file many times, you will usually
want to just add many <tt>AudioClip</tt> nodes, and set their <tt>url</tt>
field to the same value. Our implementation is optimized for this case,
we have an internal cache that will actually load the sound file only once,
even when it's referenced by many <tt>AudioClip.url</tt> values.</p>

<p>TODO: There's no streaming implemented yet. So too many and too long
music files in one world may cause large memory consumption,
and at the first play of a long sound there may be a noticeable loading delay.</p>

<p>TODO: Only <tt>AudioClip</tt> works as sound source for now.
You cannot use <tt>MovieTexture</tt> as sound source.</p>

<?php
  x3d_status_footer();
?>
