<?php
require_once 'castle_engine_functions.php';
creating_data_header('Material properties configuration');
?>

<p>TODO: get description of CastleMaterialProperties.pas

TODO: fill default values below, link all attributes to appropriate properties.

<?php echo xml_highlight(
'<?xml version="1.0"?>

<properties>
  <property
    texture_base_name="test_texture"
    footsteps_sound="test_sound_4">
    <toxic>
      <damage
        const="1.2"
	random="3.4"
	time="5.6" />
    </toxic>
  </property>

  <property
    texture_base_name="test_texture_2"
    normal_map="test_normal_map.png" />
</properties>'); ?>

<?php
creating_data_footer();
?>
