<?php
require_once 'castle_engine_functions.php';
castle_header('Sound');

$toc = new TableOfContents(
  array(
    new TocItem('The most important classes and their usage', 'classes'),
    new TocItem('Examples', 'examples'),
    new TocItem('Editor sound features', 'editor'),
    new TocItem('Playing sound from Pascal code', 'code'),
    new TocItem('Using sounds collection (.castle-components file)', 'components_collection'),
    new TocItem('Sound backends', 'backends'),
  )
);
?>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<?php
echo castle_thumbs(array(
  array('filename' => 'sound_example_3d_game_sound.png', 'titlealt' => '3D game sound demo - TCastleSound'),
  array('filename' => 'sound_example_3d_game_source.png', 'titlealt' => '3D game sound demo - TCastleSoundSource'),
));
?>

<ul>
  <li>
    <p><?php echo cgeRef('TCastleSound', 'TCastleSound'); ?>: The most important class, you should use this always when you want to play any sound.

    <p>This is a non-visual component that represents a sound file with some playback parameters. The most important properties are:

    <ul>
      <li>
        <p>
          <?php echo cgeRef('TCastleSound.URL', 'URL'); ?> &mdash; undoubtedly the most important property, set this to actually load the sound file.

      <li>
        <p>
          <?php echo cgeRef('TCastleSound.Stream', 'Stream'); ?> &mdash; optionally use "streaming", which is an alternative loading method best suited for longer playing sounds (like music tracks).

      <li>
        <p>
          <?php echo cgeRef('TCastleSound.Volume', 'Volume'); ?> &mdash; how loud the sound is. This is multiplied by volume at <code>TCastlePlayingSound.Volume</code> and <code>TCastleSoundSource.Volume</code> and by spatial calculations.


      <li>
        <p>
          <?php echo cgeRef('TCastleSound.Pitch', 'Pitch'); ?> &mdash; sound playing speed. As with volume, the volume of <code>TCastleSound.Pitch</code> is multiplied by similar parameters controlled at <code>TCastlePlayingSound.Pitch</code> and <code>TCastleSoundSource.Pitch</code>.
    </ul>

    <p><code>TCastleSound</code> by itself doesn't manage playing the sound. You have to use <?php echo cgeRef('TSoundEngine.Play', 'SoundEngine.Play'); ?> to play the sound (the simplest way to play, for non-spatial sounds) or <?php echo cgeRef('TCastleSoundSource', 'TCastleSoundSource'); ?> (for sounds that can be spatial; assign to <?php echo cgeRef('TCastleSoundSource.Sound', 'TCastleSoundSource.Sound'); ?> for looping, use <?php echo cgeRef('TCastleSoundSource.Play', 'TCastleSoundSource.Play'); ?> for non-looping).

  <li>
    <p><?php echo cgeRef('TCastleSoundSource', 'TCastleSoundSource'); ?>: A way to play spatial (3D) sounds.

    <p>This is a behavior (see <a href="https://castle-engine.io/viewport_and_scenes_from_code#_behaviors">documentation about behaviors</a>) that enhances any <?php echo cgeRef('TCastleTransform', 'TCastleTransform'); ?> so that it emits (possibly spatial) sounds.

    <p><code>TCastleSoundSource</code> refers to <code>TCastleSound</code> for an actual sound information. You can set <?php echo cgeRef('TCastleSoundSource.Sound', 'TCastleSoundSource.Sound'); ?> (for looping). Or call <?php echo cgeRef('TCastleSoundSource.Play', 'TCastleSoundSource.Play'); ?> (for non-looping).

  <li>
    <p><?php echo cgeRef('TCastlePlayingSound', 'TCastlePlayingSound'); ?>: Optional, use if you need more control before and during the sound playback.
</ul>

<p>Both <code>TCastleSoundSource</code> and <code>TCastleSound</code> can be created, configured and linked in the <a href="https://castle-engine.io/manual_editor.php">CGE editor</a>, e.g. when designing your state. You can hear the 3D sounds in the editor. You can also create and control them from code, as all CGE components.

<?php echo $toc->html_section(); ?>

<?php
echo castle_thumbs(array(
//  array('filename' => 'sound_example_player.png', 'titlealt' => 'Example audio player'),
//  array('filename' => 'sound_example_doppler.png', 'titlealt' => 'Doppler demo'),
  array('filename' => 'sound_example_play_sounds.png', 'titlealt' => 'Play sounds demo'),
));
?>

<ul>
  <li>
    <p>See all the examples in <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/audio">examples/audio</a> subdirectory.

  <li>
    <p>In particular open the <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/audio/game_3d_sound">examples/audio/game_3d_sound</a> demo. It's a simple example of how <code>TCastleSoundSource</code> and <code>TCastleSound</code> can be set up in the CGE editor.

  <li>
    <p>See also <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/fixed_camera_game">examples/fixed_camera_game</a> as a demo how to design a collection of sounds. In particular:

      <ul>
        <li><code>examples/fixed_camera_game/data/sounds/all_sounds.castle-component</code> describes all sounds
        <li><code>examples/fixed_camera_game/code/gamesound.pas</code> loads them
      </ul>
</ul>

<?php echo $toc->html_section(); ?>

<?php
echo castle_thumbs(array(
  array('filename' => 'sound_editor_options.png', 'titlealt' => 'Sound editor options'),
));
?>

<ul>
  <li>
    <p>Sound volume, and "auto-mute on play" are available in the editor settings.

  <li>
    <p>Drag-and-drop sound files on the viewport to automatically create a spatial sound: <code>TCastleTransform</code>, <code>TCastleSoundSource</code>, <code>TCastleSound</code>.
</ul>

<?php echo $toc->html_section(); ?>

<p>Load a sound file as <?php echo cgeRef('TCastleSound', 'TCastleSound'); ?> like this:</p>

<ol>
  <li>
    <p>Add <code>CastleSoundEngine</code> to your uses clause.

  <li>
    <p>Declare variable to hold it like <code>MySound: TSound;</code>

  <li>
    <p>Initialize the variable and load sound file, e.g. in <code>Application.OnInitialize</code>:

<?php echo pascal_highlight(
'MySound := TCastleSound.Create(Application);
MySound.URL := \'castle-data:/my-sound.wav\';'); ?>

  <li>
    <p>Play the sound like this:

<?php echo pascal_highlight(
'SoundEngine.Play(MySound);'); ?>
</ol>

<p>See <a href="https://github.com/castle-engine/castle-engine/blob/master/examples/audio/simplest_play_sound/simplest_play_sound.lpr">source code of examples/audio/simplest_play_sound/</a> for a working simplest possible example of this.

<?php echo $toc->html_section(); ?>

<?php
echo castle_thumbs(array(
  array('filename' => 'sound_collection.png', 'titlealt' => 'Sounds collection designed in editor'),
));
?>

<p>It is often comfortable to define a <i>collection</i> of sounds, which means that each sound file is assigned a simple name and configuration (e.g. priority, default volume), and all the sound files can be loaded easily from any place in code (regardless of the current <a href="views">view</a>).

<p>Do it by using a <code>TCastleComponent</code> as a design root and adding <code>TCastleSound</code> children. Save the resulting design to a file like <code>all_sounds.castle-component</code>.

<p>See <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/fixed_camera_game">examples/fixed_camera_game</a> for an example of this approach. In particular:

<ul>
  <li><code>examples/fixed_camera_game/data/sounds/all_sounds.castle-component</code> describes all sounds
  <li><code>examples/fixed_camera_game/code/gamesound.pas</code> loads them
</ul>

<?php echo $toc->html_section(); ?>

<?php
echo castle_thumbs(array(
  array('filename' => 'sound_fmod.png', 'titlealt' => 'FMOD'),
));
?>

<p>By default we use <em>OpenAL</em> to play sounds. It's a great full-featured open-source audio library, perfect match for our open-source game engine.

<p>You can alternatively switch to use the <a href="https://castle-engine.io/fmod">FMOD sound backend</a>. This is just an option. FMOD is proprietary (not open-source) and <a href="https://www.fmod.com/licensing">commercial (though free in some cases, for indie devs)</a>.

<ul>
  <li>
    <p>Main advantage of FMOD in CGE for now is <a href="https://castle-engine.io/nintendo_switch">Nintendo Switch compatibility</a>.
  <li>
    <p>Big future advantage may be integration with the <a href="https://www.fmod.com/studio">FMOD Studio</a>. The goal of <i>FMOD Studio</i> is to make the work of <em>sound designer</em> easier. The sfx person can create sound effects in <i>FMOD Studio</i>, in a way that is agnostic to the game engine, and the code (like your game) simply sends <em>"events"</em> that may cause some sound effect (playing something, stopping something, fading in/out something...).
</ul>

<?php
castle_footer();
?>
