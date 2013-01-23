<?php
require_once 'castle_engine_functions.php';
castle_header('Player configuration | Creating Game Data', NULL, array('engine', 'creating_data_intro'));
echo pretty_heading('Player configuration');
?>

<p>TODO: get description of TPlayer.LoadFromFile

TODO: fill default values below, link all attributes to appropriate properties.

<?php echo xml_highlight(
'<?xml version="1.0"?>

<player
  knockback_speed="1.2"
  head_bobbing_time="9.1"
  head_bobbing="2.3"
  sick_projection_speed="4.5">

  <jump
    max_height="3.4"
    horizontal_speed_multiply="5.6"
    time="7.8" />

  <fall>
    <sound
      min_height="6.7"
      name="test_sound_1" />

    <damage
      min_height="8.9"
      scale_min="1.2"
      scale_max="3.4" />
  </fall>

  <swim
    breath="5.6"
    sound_pause="6.7" />

  <drown
    pause="7.8">
    <damage
      const="9.1"
      random="2.3" />
  </drown>
</player>'); ?>

<?php
castle_footer();
?>
