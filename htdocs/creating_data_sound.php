<?php
require_once 'castle_engine_functions.php';
creating_data_header('Sound');
?>

<p>Below is a sample sound configuration,
with links to documentation for every attribute.
See <?php echo a_href_page('manual about sounds', 'manual_sound'); ?>
 for information how to initialize sound repository from such XML configuration.
</p>

<?php echo xml_highlight(
'<?xml version="1.0"?>

<sounds>
  <!--
    Contains a list of <sound> elements.
    Only the "name" attribute is required, and all names must be unique.
    The sound URL (filename) by default
    (if you don\'t specify other URL explicitly) is determined like this:
    - In CGE <= 6.4: <name>.wav
    - In CGE >= 6.5:
      <name>.ogg, or
      <name>.wav (whichever is found first), or
      none (this causes a warning).
  -->

  <sound
    [[CastleSoundEngine.TSoundInfo.html#Name|name]]="player_sudden_pain"
    [[CastleSoundEngine.TSoundInfo.html#URL|url]]=""
    [[CastleSoundEngine.TSoundInfo.html#DefaultImportance|default_importance]]="max"
    [[CastleSoundEngine.TSoundInfo.html#Gain|gain]]="1.0"
    [[CastleSoundEngine.TSoundInfo.html#MinGain|min_gain]]="0.0"
    [[CastleSoundEngine.TSoundInfo.html#MaxGain|max_gain]]="1.0"
    [[https://castle-engine.io/wp/2019/08/18/streaming-sound-tracks-fmod-linking-improved/|stream]]="false" />

  <!-- And more <sound> elements... -->
  <sound name="test_sound_1" />
  <sound name="test_sound_2" />
  <sound name="test_sound_3" />

  <!--
    (Since CGE >= 6.5):

    Sound group is like a directory of sound files.
    Sounds within a group named "fight" must be played using a qualified
    name like "fight/drum_beat".
    This way sound names must only be unique within their group.

    Sound group can also (optionally) correspond to an actual subdirectory
    of sound files, if it has a subdirectory attribute.
    In this case, all the sounds inside are searched within that subdirectory.
    For example <sound> with name drum_beat is by default opened from
    "drum_beat.wav". But if it\'s in a group with subdirectory="fight",
    then it\'s actually opened from "fight/drum_beat.wav".

    You can make groups within groups (just like directories in directories).
  -->
  <group name="fight" subdirectory="fight">
    <sound name="drum_beat" />
    <sound name="drum_ending" />
  </group>

  <!--
    (Since CGE >= 6.5):

    Alias allows to define a sound name that refers to another <sound> or <alias>.
    Moreover, an alias may have more than one target,
    which means that actual sound will be randomly chosen from the available options
    each time you play this alias.

    Note that <alias> may be placed within a <group> too.
    Both alias names, and target names, are automatically qualified by the group name.
  -->
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

<h2>Some notes about sound files</h2>

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
