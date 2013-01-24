<?php
require_once 'castle_engine_functions.php';
creating_data_header('Player configuration');
?>

<p>You can load an XML file configuring various player properties.
See <?php echo a_href_page('tutorial about Player', 'tutorial_player'); ?>.
Below is a sample <tt>player.xml</tt> file
with links to documentation for every attribute.</p>

<?php echo xml_highlight(
'<?xml version="1.0"?>

<player
  [[Castle3D.T3DAlive.html#KnockBackSpeed|knockback_speed]]="1.2"
  [[CastleCameras.TWalkCamera.html#HeadBobbingTime|head_bobbing_time]]="9.1"
  [[CastlePlayer.TPlayer.html#HeadBobbing|head_bobbing]]="2.3"
  [[CastlePlayer.TPlayer.html#SickProjectionSpeed|sick_projection_speed]]="4.5">

  <jump
    [[CastleCameras.TWalkCamera.html#JumpMaxHeight|max_height]]="3.4"
    [[CastleCameras.TWalkCamera.html#JumpHorizontalSpeedMultiply|horizontal_speed_multiply]]="5.6"
    [[CastleCameras.TWalkCamera.html#JumpTime|time]]="7.8" />

  <fall>
    <sound
      [[CastlePlayer.TPlayer.html#FallMinHeightToSound|min_height]]="6.7"
      [[CastlePlayer.TPlayer.html#FallSound|name]]="test_sound_1" />

    <damage
      [[CastlePlayer.TPlayer.html#FallMinHeightToDamage|min_height]]="8.9"
      [[CastlePlayer.TPlayer.html#FallDamageScaleMin|scale_min]]="1.2"
      [[CastlePlayer.TPlayer.html#FallDamageScaleMax|scale_max]]="3.4" />
  </fall>

  <swim
    [[CastlePlayer.TPlayer.html#SwimBreath|breath]]="5.6"
    [[CastlePlayer.TPlayer.html#SwimSoundPause|sound_pause]]="6.7" />

  <drown
    [[CastlePlayer.TPlayer.html#DrownPause|pause]]="7.8">
    <damage
      [[CastlePlayer.TPlayer.html#DrownDamageConst|const]]="9.1"
      [[CastlePlayer.TPlayer.html#DrownDamageRandom|random]]="2.3" />
  </drown>
</player>'); ?>

<?php
creating_data_footer();
?>
