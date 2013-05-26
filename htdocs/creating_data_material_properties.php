<?php
require_once 'castle_engine_functions.php';
creating_data_header('Material properties configuration');
?>

<p>You can configure material properties using an XML file.
This is useful to configure behavior that is naturally dependent on
a given material or texture. Right now this allows to define things
like footsteps, toxic ground (hurts player), and bump mapping.
See <?php api_link('TMaterialProperty', 'CastleMaterialProperties.TMaterialProperty.html'); ?> docs.
</p>

<p>In code, you have to load this file like this:

<?php echo pascal_highlight(
'uses ..., CastleMaterialProperties;

...
MaterialProperties.URL := ProgramDataPath + \'data\' + PathDelim + \'material_properties.xml\';'); ?>

<p>Below is a sample <tt>material_properties.xml</tt> file
with links to documentation for every attribute.</p>

<?php echo xml_highlight(
'<?xml version="1.0"?>

<properties>
  <!--
    Contains a list of <property> elements.
    Only the "texture_base_name" attribute is required, and it must be unique.
  -->

  <property
    [[CastleMaterialProperties.TMaterialProperty.html#TextureBaseName|texture_base_name]]="test_texture"
    [[CastleMaterialProperties.TMaterialProperty.html#FootstepsSound|footsteps_sound]]="">
    <[[CastleMaterialProperties.TMaterialProperty.html#Toxic|toxic]]>
      <damage
        [[CastleMaterialProperties.TMaterialProperty.html#ToxicDamageConst|const]]="0.0"
        [[CastleMaterialProperties.TMaterialProperty.html#ToxicDamageRandom|random]]="0.0"
        [[CastleMaterialProperties.TMaterialProperty.html#ToxicDamageTime|time]]="0.0" />
    </toxic>
  </property>

  <property
    [[CastleMaterialProperties.TMaterialProperty.html#TextureBaseName|texture_base_name]]="test_texture_2"
    [[CastleMaterialProperties.TMaterialProperty.html#NormalMap|normal_map]]="test_normal_map.png" />

  <!-- And more <property> elements... -->
</properties>'); ?>

<?php
creating_data_footer();
?>
