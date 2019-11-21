<?php
require_once 'castle_engine_functions.php';
creating_data_header('Sound');

$toc = new TableOfContents(
  array(
    new TocItem('Sample sounds XML file', 'sample'),
    new TocItem('Sound groups and aliases', 'groups_aliases'),
    new TocItem('Advises about creating sound files', 'advises'),
  )
);
?>

<?php echo $toc->html_toc(); ?>

<p>You can use an XML file to configure "named sounds" in your game.
Such XML file can be set as
<?php api_link('SoundEngine.RepositoryURL', 'CastleSoundEngine.TRepoSoundEngine.html#RepositoryURL'); ?>
 (see <?php echo a_href_page('manual about sounds', 'manual_sound'); ?>
 for more information how to use sound from code).

<?php echo $toc->html_section(); ?>

<p>In the simplest case, the sounds XML file is just a list of <code>&lt;sound&gt;</code>
elements. Here's an example:

<?php echo xml_highlight(
'<?xml version="1.0"?>

<sounds>
  <sound
    name="player_sudden_pain"
    url=""
    default_importance="max"
    gain="1.0"
    min_gain="0.0"
    max_gain="1.0"
    stream="false" />

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

  <li><code>default_importance</code> (integer)

    <p>How important the sound is. Influences what happens when we have a lot
    of sounds playing at once.

  <li><code>gain</code> (float, in range 0..infinity)

    <p>Volume. How loud the sound is.

    <p>Use this to indicate that
    e.g. a plane engine is louder than a mouse squeak (when heard
    from the same distance).

    <p>Note: Do <i>not</i> make the actual sound data (in wav, ogg and such files)
    louder/more silent for this purpose.
    This is usually bad for sound quality. Instead, keep your sound data
    at max loudness (normalized), and use this <code>gain</code> property
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
Moreover, an alias may have more than one target,
which means that actual sound will be randomly chosen from the available options
each time you play this alias.

<p>Note that <code>&lt;alias&gt;</code> may be placed within a <code>&lt;group&gt;</code> too.
Both alias names, and target names, are automatically qualified by the group name.

<p>Here's an example:

<?php echo xml_highlight(
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

  <alias name="random_test_sound">
    <target name="test_sound_1" />
    <target name="test_sound_2" />
    <target name="test_sound_3" />
  </alias>
</sounds>'); ?>

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

    <p>Scale the sound by changing the <code>gain</code> property
    in sound configuration.

  <li><p>If sound is supposed to be spatialized (i.e. played
    by <?php api_link('Sound3D', 'CastleSoundEngine.TRepoSoundEngine.html#Sound3D'); ?> method), make sure it is mono.
    Some OpenAL implementations never spatialize stereo sounds.

<!--
    <p>You can use any editor you like to convert your sounds to mono.
    I like this sox command-line:
    <pre>  sox input.wav -c 1 output.wav</pre>
    See also <code>data/sounds/scripts/example_make_mono.sh</code>
-->

  <li><p>Specifically when making footsteps sound: synchronize it's duration
    with the <?php api_link('HeadBobbingTime', 'CastleCameras.TWalkCamera.html#HeadBobbingTime'); ?>,
    which you can set for example using
    <?php echo a_href_page('player XML configuration file', 'creating_data_player'); ?>
    (if your game loads it). By default it's 0.5, which means your footsteps
    sound should be around half-second long. Or you can record 2 footsteps
    and make it 1-second long.

    <p>The important thing is to synchronize these times, to make them
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

<?php
creating_data_footer();
?>
