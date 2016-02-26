<?php
require_once 'castle_engine_functions.php';
creating_data_header('Material properties configuration');
?>

<p>You can configure material properties using an XML file.
This is useful to configure behavior that is naturally dependent on
a given material or texture. Right now this allows to define things like:

<ul>
  <li>footsteps,</li>
  <li>toxic ground (hurts player),</li>
  <li>bump mapping,</li>
  <li>texture GPU-compressed alternatives.</li>
</ul>

<p>See <?php api_link('TMaterialProperty', 'CastleMaterialProperties.TMaterialProperty.html'); ?> docs.

<p>In code, you have to load this file like this:

<?php echo pascal_highlight(
'uses ..., CastleMaterialProperties;

...
MaterialProperties.URL := ApplicationData(\'material_properties.xml\');'); ?>

<p>It's usually best to do it as early as possible, for example
at the beginning on <code>Application.OnInitialize</code> (if you
use <code>CastleWindow</code>) or <code>TForm.OnCreate</code>
(if you use <code>CastleControl</code> in Lazarus).

<p>Below is a sample <code>material_properties.xml</code> file
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
    [[CastleMaterialProperties.TMaterialProperty.html#AlphaChannel|alpha_channel]]="SIMPLE_YES_NO" />

  <!-- And more <property> elements... -->

  <!-- Automatically compressed textures.

    For textures listed here:

    1. Running "castle-engine auto-compress-textures" will generate
       their GPU-compressed counterparts. See
       [[https://github.com/castle-engine/castle-engine/wiki/Build-Tool]] .
       They will be generated inside auto_compressed subdirectories
       of your data files.

    2. In game, trying to load an uncompressed texture URL will automatically
       load the GPU-compressed counterpart, if the GPU compression
       is supported. Just load the material_properties.xml early in your code.

    This allows to seamlessly enable GPU texture compression in your games,
    and work even when the GPU texture compression algorithm is not supported
    by the given device. Very useful on Android, with the price: you should
    distribute all compression variants.

    Note that GPU compression algorithms have their limitations.
    They may resize your texture (to the power of two), some may even force
    it to be square. Be sure your code supports it. Usually, this is Ok
    for 3D textures and texture atlases, but not OK for GUI images.
    Use the <include> / <exclude> elements to select only the sensible
    subset of your data textures.
    Include / exclude work just like in CastleEngineManifest.xml, documented on
    [[https://github.com/castle-engine/castle-engine/wiki/Build-Tool]] :
    we first include, then exclude.
  -->
  <auto_compressed_textures>
    <formats>
      <!--
        Format names are the same as TTextureCompression enum names
        (without leading "tc"). Generating them requires various tools:

        - PVRTexToolCLI (get it from [[https://community.imgtec.com/developers/powervr/tools/pvrtextool/]])

        - AMDCompressCLI (get it from [[http://developer.amd.com/tools-and-sdks/graphics-development/amdcompress/]])
          This is a new, maintained alternative to the old "ATI Compressonator".
          On non-Windows, it can be installed and run under Wine
          (install with "wine start xxx.msi".
          You may also need to do "winetricks vcrun2005".)
          On some Wine versions it works cool (even installation from msi!),
          unfortunately on others --- it doesn\'t work at all
          (use "ATI Compressonator" then).

        - ATI Compressonator (get it from [[http://developer.amd.com/tools-and-sdks/archive/legacy-cpu-gpu-tools/the-compressonator/]])
          On non-Windows, it can be run under Wine
          (You will need to do "winetricks vcrun2005" first.
          Installing it is troublesome under Wine, but a working installed dir can
          be copied from your Windows installation.
          We use it as a fallback in case AMDCompressCLI is not available.)

        Two most common RGBA compressions for mobiles are listed below. -->
      <format name="Pvrtc1_4bpp_RGBA"/>
      <format name="ATITC_RGBA_InterpolatedAlpha"/>
    </formats>

    <include path="spells/*" recursive="True" />
    <include path="castles/*" recursive="True" />
    <include path="player.png" />

    <exclude path="*.jpg" />
    <exclude path="spells/gui/*" />
    <exclude path="*/gui_hud.png" />
  </auto_compressed_textures>
</properties>'); ?>

<?php
creating_data_footer();
?>
