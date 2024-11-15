<?php
require_once 'castle_engine_functions.php';
castle_header('Sound');

$toc = new TableOfContents(
  array(
    new TocItem('Define common sounds in design file', 'sound_design_file'),
    new TocItem('Advises about creating sound files', 'advises'),
    new TocItem('Deprecated: Sample sounds XML file', 'sample'),
    new TocItem('Deprecated: Sound groups and aliases', 'groups_aliases'),
  )
);
?>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>It is often useful to define a common "repository" for all the sounds.

<p>To do this, create a design file in CGE editor starting from non-visual <?php echo cgeRef('TCastleComponent'); ?>
 component. As children, add <?php echo cgeRef('TCastleSound'); ?> components.
 Configure the sounds as needed, and load the design from code to play the sounds.

<p>See the <code>examples/fps_game/data/sounds.castle-component</code> design file
in <code>examples/fps_game/</code> demo.

<?php echo $toc->html_section(); ?>

<ul>
  <li><p>Right now we support OggVorbis and (uncompressed) WAV files.
    <!--
    Short sounds should be stored as WAV,
    long sounds (like level music) may be stored as OggVorbis files.
    -->

  <li><p>A general advice when creating sounds is to keep them
    <i>normalized</i>, which means "as loud as possible".
    It doesn't matter if you record a mouse squeak
    or a plane engine, the sound file should be equally loud.
    This allows to have best quality sound.

    <p>Scale the sound by changing the <code>volume</code> property
    in sound configuration.

  <li><p>If sound is supposed to be spatialized, make sure it is mono.
    Some OpenAL implementations never spatialize stereo sounds.

<!--
    <p>You can use any editor you like to convert your sounds to mono.
    I like this sox command-line:
    <pre>  sox input.wav -c 1 output.wav</pre>
    See also <code>data/sounds/scripts/example_make_mono.sh</code>
-->

  <li><p>Specifically when making footsteps sound: synchronize it's duration
    with the <?php echo cgeRef('TCastleWalkNavigation.HeadBobbingTime'); ?>.
    Synchronize these times, to make them
    feel right in the game &mdash; visuals (head bobbing) should match
    what you hear (footsteps sound).

<!--
    <ul>
      <li>Don't make the footsteps sound too long.
        Preferably you should put there only 2 footsteps. Reason ?
        When progress is displayed (e.g. because player changes levels),
        or when player enters the game menu, footsteps sound is not
        immediately stopped &mdash; it's just played until the end.
        Yes, this is desired, as it makes better effect than suddenly
        stopping all the sounds.
-->
</ul>

<?php echo $toc->html_section(); ?>

<p>You can use an XML file to configure "named sounds" in your game.
Such XML file can be set as
<?php echo cgeRef('TRepoSoundEngine.RepositoryUrl', 'SoundEngine.RepositoryUrl'); ?>
 (see <a href="sound">manual about sounds</a>
 for more information how to use sound from code).

<p>In the simplest case, the sounds XML file is just a list of <code>&lt;sound&gt;</code>
elements. Here's an example:

<?php echo xml_full_highlight(
'<?xml version="1.0"?>

<sounds>
  <sound
    name="player_sudden_pain"
    url=""
    volume="1.0"
    min_gain="0.0"
    max_gain="1.0"
    stream="false"
    priority="0.5"
  />

  <!-- And more <sound> elements... -->
  <sound name="test_sound_1" />
  <sound name="test_sound_2" />
  <sound name="test_sound_3" />
</sounds>'); ?>

<p>Each <code>&lt;sound&gt;</code> can have the following attributes:

<ul>
  <li><code>name</code> (the only require attribute; string).

    <p>This is a unique sound name.

  <li><code>url</code>

    <p>The URL (can be just a simple filename)
    from which to load sound data.

    <p>If you don't specify it, we will guess URL looking at <code>name</code>
    and appending various extensions.
    Right now we will try looking for <code>&lt;name&gt;.ogg</code>
    and then <code>&lt;name&gt;.wav</code>.

  <li><p><code>volume</code> (deprecated name: <code>gain</code>) (float, in range 0..infinity)

    <p>Volume. How loud the sound is.

    <p>Use this to indicate that
    e.g. a plane engine is louder than a mouse squeak (when heard
    from the same distance).

    <p>Note: Do <i>not</i> make the actual sound data (in wav, ogg and such files)
    louder/more silent for this purpose.
    This is usually bad for sound quality. Instead, keep your sound data
    at max loudness (normalized), and use this <code>volume</code> property
    to scale sound.

    <p>It can be anything from 0 to +infinity. The default is 1.
    Note that values &gt; 1 are allowed,
    but some sound backends (like OpenAL) may clip the resulting sound volume (after all
    spatial calculations are be done) to 1.0.

  <li><code>min_gain</code> (float, in range 0..1)

    <p>Force a minimum sound loudness, despite what volume would be calculated
    by the spatialization.
    This can be used to force sound to be audible, even when it's far away from
    the listener.

    <p>It must be in [0, 1] range. By default it is 0.

  <li><code>max_gain</code> (float, in range 0..1)

    <p>Force a maximum sound loudness, despite what volume would be calculated
    by the spatialization.
    This can be used to limit sound volume,
    regardless of the distance attenuation calculation.

    <p>It must be in [0, 1] range. By default it is 1.

  <li><code>stream</code> (boolean)

    <p>Play sound using <i>streaming</i>.
    This means that the sound is gradually decompressed in memory,
    which means that loading time is much smaller, although there may be a small
    overhead on CPU during playback.
    This is usually a good idea for longer sounds,
    e.g. music tracks.
    See <a href="https://castle-engine.io/wp/2019/08/18/streaming-sound-tracks-fmod-linking-improved/">news post about streaming</a>
    for more details.

  <li><code>priority</code> (float, in range 0..1)

    <p>How important the sound is. Influences what happens when we have a lot
    of sounds playing at once, and we need to stop some of them
    (we cannot have too many sounds playing at once,
    as then the cost of mixing would be significant,
    and human user cannot distinguish too many simultaneous sounds anyway).
    Larger priority increases the chance that the sound
    will keep playing.

    <p>By default it is <code>0.5</code>.

  <li><code>default_importance</code> (integer)

    <p>Deprecated alternative to specify <code>priority</code>.
    Migrate to using <code>priority</code>.
</ul>

<?php echo $toc->html_section(); ?>

<p>You can arrange sounds in <b>groups</b> using the <code>&lt;group&gt;</code> element.
Sound <i>group</i> is like a directory of sound files.
Sounds within a group named <code>"fight"</code> must be played using a qualified
name like <code>"fight/drum_beat"</code>.
This way sound names must only be unique within their group.

<p>Sound group can also (optionally) correspond to an actual subdirectory
of sound files, if it has a <code>subdirectory</code> attribute.
In this case, all the sounds inside are searched within that subdirectory.
For example <code>&lt;sound&gt;</code> with name <code>drum_beat</code>
is by default opened from file
<code>drum_beat.wav</code>. But if it's in a group <code>&lt;group subdirectory="fight"&gt;</code>,
then it's actually opened from filename <code>fight/drum_beat.wav</code>.

<p>You can make groups within groups (just like directories in directories).

<p>We also support <b>aliases</b>.
Alias allows to define a sound name that refers to another <code>&lt;sound&gt;</code> or <code>&lt;alias&gt;</code>.

<p>Note that <code>&lt;alias&gt;</code> may be placed within a <code>&lt;group&gt;</code> too.
Both alias names, and target names, are automatically qualified by the group name.

<p>Here's an example:

<?php echo xml_full_highlight(
'<?xml version="1.0"?>
<sounds>
  <sound name="test_sound_1" />
  <sound name="test_sound_2" />
  <sound name="test_sound_3" />

  <group name="fight" subdirectory="fight">
    <sound name="drum_beat" />
    <sound name="drum_ending" />
  </group>

  <alias name="alternative_name_for_test_sound_1">
    <target name="test_sound_1" />
  </alias>

  <alias name="alternative_name_for_fight_drum_beat">
    <target name="fight/drum_beat" />
  </alias>
</sounds>'); ?>

<?php
castle_footer();
?>
