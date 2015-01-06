<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Sound', 'sound',
    'This component provides sound support.
    <code>AudioClip</code> defines a sound file and allows to start and stop playback.
    <code>Sound</code> positions the sound in 3D world and configures sound
    parameters. Sounds may be spatialized (3D) or not.');

  echo castle_thumbs(array(
    array('filename' => 'sound.png', 'titlealt' => 'Sound demo'),
  ));

  $toc = new TableOfContents(
    array(
      new TocItem('Demos', 'demos'),
      new TocItem('Supported nodes', 'support'),
      new TocItem('DEF / USE on sounds', 'def_use'),
    ));
  $toc->echo_numbers = true;
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>For demos and tests of these features,
see the <code>sound</code> subdirectory inside <?php
echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>.</p>

<?php echo $toc->html_section(); ?>

<p>There are two nodes dealing with sound:</p>

<ol>
  <li><code>AudioClip</code> node is a buffer for sound data.
    Basically, it's a wrapper around a sound data coming from
    .wav, .ogg and such files.
  <li><code>Sound</code> node is a 3D sound, that has a 3D position,
    can move (by animating <code>Sound.location</code> and the parent transformation).
    Naturally <code>Sound</code> references <code>AudioClip</code> by <code>Sound.source</code> field.
</ol>

<p><?php echo x3d_node_link('Sound'); ?> node (3D sound)
supported fields / events:</p>

<ul>
  <li>intensity</li>
  <li>location (correctly transformed by <code>Sound</code> transformation; animating the location field or the transformation of the <code>Sound</code> node works perfectly)</li>
  <li>priority (we have a smart sounds allocator, and the priority really matters when you have many sounds playing at once)</li>
  <li>source (to indicate AudioClip node)</li>
  <li>spatialize. Note that <b>multi-channel (e.g. stereo) sound files are never spatialized</b>. Be sure to convert your sound files to mono if you want to use them for 3D spatialized sound.</li>
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
    We simply ignore direction, minBack and maxBack fields.
    As far as I know, the ellipsoids model is not possible in OpenAL,
    so we're probably not the only viewer that doesn't support them.
    </p></li>

  <li><p>It's unclear from the specification whether playing should be limited
    only to the sounds in the active graph part
    (the subset of children chosen in LOD / Switch and similar nodes).
    To be on the safe side, always place your sounds in the active graph part,
    although our implementation will <i>usually</i> also play sounds in the inactive
    part (exceptions may happen for complicated situations with PROTOs).

    <p>Reports how other browsers handle this are welcome.</li>

    <!-- Exceptions because TAudioClipNode.BeforeTraverse
      is not called, see thunder.x3dv for testcase, when it's necessary:
      moving the Sound out of the PROTO 1st child makes sound not playing. -->

  <li><p>Right now, sounds are not sorted exactly like the specification says.
    In short, we only look at your given "priority" field,
    disregarding current sound distance to player and such.
    This will definitely by fixed some day, please speak up on forum
    if you need it.</li>
</ul>

<p><?php echo x3d_node_link('AudioClip'); ?> node
(the sound file to be played, basic playback properties,
events to start/stop playing of the sound) supported fields / events:</p>

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

<p>TODO: Only <code>AudioClip</code> works as sound source for now.
You cannot use <code>MovieTexture</code> as sound source.</p>

<?php echo $toc->html_section(); ?>

<p>VRML/X3D define the play/stop events
at the <code>AudioClip</code> node (not at higher-level <code>Sound</code> node,
which would be more useful IMO). This means that <code>USE</code>ing many times
the same <code>AudioClip</code> or <code>Sound</code> nodes doesn't make much sense.
You can only achieve the same sound, playing simultaneously the same thing,
from multiple 3D sources. Since such simultaneous sounds are usually useless,
we don't even implement them (so don't reUSE <code>AudioClip</code> or <code>Sound</code> nodes,
or an arbitrary one will be playing).
If you want to use the same sound file many times, you will usually
want to just add many <code>AudioClip</code> nodes, and set their <code>url</code>
field to the same value. Our implementation is optimized for this case,
we have an internal cache that will actually load the sound file only once,
even when it's referenced by many <code>AudioClip.url</code> values.</p>

<p>More detailed explanation:

<p>The question is: <i>where do we put start/stop events. At the Sound node,
or at the AudioClip node?</i>

<p>More precisely, which node has <code>X3DTimeDependentNode</code> as an ancestor (<code>X3DTimeDependentNode</code> contains <code>startTime</code>, <code>stopTime</code> and a lot of other playing-related fields and events).

<ol>
  <li><p>The decision of X3D specificaion was to put them at AudioClip.

    <p>The downside: <code>DEF</code>/<code>USE</code> for AudioClip doesn't make
    much sense, usually. You can write this:

<pre class="vrml_code">
  Sound DEF S1 { source DEF A1 AudioClip { url "sound.wav" } }
  Sound DEF S2 { source USE A1 }
</pre>

    <p>but it's not useful: you can only send <code>startTime</code> to
    the <code>A1</code>, making both sound sources playing simultaneously
    the same thing. To be able to independently start/stop playing
    of sounds on S1 and S2, you have to resign from DEF/USE, and write

<pre class="vrml_code">
  Sound DEF S1 { source DEF A1 AudioClip { url "sound.wav" } }
  Sound DEF S2 { source DEF A2 AudioClip { url "sound.wav" } }
</pre>

    <p>So you need two AudioClip nodes, even though their contents are equal.

    <p>The upside of X3D specification is that this way <code>MovieTexture</code>,
    which also descends from <code>X3DTimeDependentNode</code>,
    can be used inside <code>Sound</code> nodes. This way you can play audio
    track from movies.

  <li><p>The inverse decision would be to make Sound node a <code>X3DTimeDependentNode</code>.
    Then you could write

<pre class="vrml_code">
  Sound DEF S1 { source DEF A1 AudioClip { url "sound.wav" } }
  Sound DEF S2 { source USE A1 }
</pre>

    <p>and independently start/stop playing sounds S1 and S2.

    <p>The downside would be that playing audio tracks from <code>MovieTexture</code>
    is ugly, and probably should not be allowed by the specification.
    When both <code>MovieTexture</code>
    and <code>Sound</code> would be of <code>X3DTimeDependentNode</code>,
    it would be unclear which node controls the playing in case of this:

<pre class="vrml_code">
  Sound DEF S1 { source DEF M1 MoveTexture { url "movie.avi" } }
</pre>

    <p>Probably, the idea of playing sounds from <code>MovieTexture</code>
    should be just dropped in this case, otherwise it gets messy.
</ol>

<p>Personally, Michalis would choose the option 2. (But it's too late for that now,
and we implement spec-complaint decision 1.) I don't think that playing
audio tracks from movie files is a useful or common use case.
It's wasteful, anyway, to reference a movie just to play an audio track,
so authors are well adviced to avoid this. If you want to play
an audio track from a movie, consider just extracting the audio track
to a separate .wav/.ogg file and playing it using AudioClip node.
This way we will not have to download the whole movie just to play its audio.

<?php
  x3d_status_footer();
?>
