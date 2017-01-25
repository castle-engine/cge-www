<?php
require_once 'castle_engine_functions.php';
creating_data_header('Player configuration');
?>

<p>You can load an XML file configuring various player properties.
Below is a sample <code>player.xml</code> file
with links to documentation for every attribute.

<ul>
  <li><i>Every</i> attribute is optional, so in practice
    there's no need to specify them all in your files. Use only the ones
    where you need to change the defaults.
  <li>See <?php echo a_href_page('the manual chapter "Player"', 'manual_player'); ?>
    for notes how to use this from your game code.
</ul>

<?php echo xml_highlight(
'<?xml version="1.0"?>

<player
  [[Castle3D.T3DAlive.html#KnockBackSpeed|knockback_speed]]="20.0"
  [[CastleCameras.TWalkCamera.html#HeadBobbingTime|head_bobbing_time]]="0.5"
  [[CastlePlayer.TPlayer.html#HeadBobbing|head_bobbing]]="0.02"
  [[CastlePlayer.TPlayer.html#SickProjectionSpeed|sick_projection_speed]]="2.0">

  <jump
    [[CastleCameras.TWalkCamera.html#JumpMaxHeight|max_height]]="1.0"
    [[CastleCameras.TWalkCamera.html#JumpHorizontalSpeedMultiply|horizontal_speed_multiply]]="2.0"
    [[CastleCameras.TWalkCamera.html#JumpTime|time]]="0.125" />

  <fall>
    <sound
      [[CastlePlayer.TPlayer.html#FallMinHeightToSound|min_height]]="4.0"
      [[CastlePlayer.TPlayer.html#FallSound|name]]="player_fall" />

    <damage
      [[CastlePlayer.TPlayer.html#FallMinHeightToDamage|min_height]]="5.0"
      [[CastlePlayer.TPlayer.html#FallDamageScaleMin|scale_min]]="0.8"
      [[CastlePlayer.TPlayer.html#FallDamageScaleMax|scale_max]]="1.2" />
  </fall>

  <swim
    [[CastlePlayer.TPlayer.html#SwimBreath|breath]]="30.0"
    [[CastlePlayer.TPlayer.html#SwimSoundPause|sound_pause]]="5.0" />

  <drown
    [[CastlePlayer.TPlayer.html#DrownPause|pause]]="5.0">
    <damage
      [[CastlePlayer.TPlayer.html#DrownDamageConst|const]]="5.0"
      [[CastlePlayer.TPlayer.html#DrownDamageRandom|random]]="10.0" />
  </drown>
</player>'); ?>

<?php
creating_data_footer();
?>
