<?php
require_once 'castle_engine_functions.php';
creating_data_header('Material properties configuration');

$toc = new TableOfContents(
  array(
    new TocItem('Usage', 'usage'),
    new TocItem('Example material_properties.xml file', 'example'),
  )
);
?>

<?php echo $toc->html_toc(); ?>
<?php echo $toc->html_section(); ?>

<p>You can configure material properties using an XML file.
This is useful to configure behavior that is naturally dependent on
a given material or texture. Right now this allows to define things like:

<ul>
  <li>footsteps,</li>
  <li>toxic ground (hurts player),</li>
  <li>bump mapping,</li>
  <li>texture GPU-compressed alternatives, and downscaled alternatives
    (this is documented in <a href="creating_data_auto_generated_textures.php">chapter about auto-generated textures</a>).</li>
</ul>

<p>See <?php api_link('TMaterialProperties', 'CastleMaterialProperties.TMaterialProperties.html'); ?> docs.

<p>In code, you have to load this file like this:

<?php echo pascal_highlight(
'uses ..., CastleMaterialProperties;

...
MaterialProperties.URL := ApplicationData(\'material_properties.xml\');'); ?>

<p>It's usually best to do it as early as possible, for example
at the beginning on <code>Application.OnInitialize</code> (if you
use <code>CastleWindow</code>) or <code>TForm.OnCreate</code>
(if you use <code>CastleControl</code> in Lazarus).

<?php echo $toc->html_section(); ?>

<p>This is a sample <code>material_properties.xml</code> file
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
    [[CastleMaterialProperties.TMaterialProperty.html#NormalMap|normal_map]]="test_normal_map.png"
    [[CastleMaterialProperties.TMaterialProperty.html#AlphaChannel|alpha_channel]]="TEST" />

  <!-- You can use as many <property> elements as you like... -->

  <!-- You can also use <auto_generated_textures> elements
       as documented in the next chapter:
  <auto_generated_textures>...</auto_generated_textures>
  -->
</properties>'); ?>

<?php
creating_data_footer();
?>
