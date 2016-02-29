<?php
require_once 'castle_engine_functions.php';
creating_data_header('Material properties configuration');

$toc = new TableOfContents(
  array(
    new TocItem('Usage', 'usage'),
    new TocItem('Example material_properties.xml file', 'example'),
    new TocItem('How does the automatic texture compression work'),
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
  <li>texture GPU-compressed alternatives.</li>
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
    [[CastleMaterialProperties.TMaterialProperty.html#AlphaChannel|alpha_channel]]="SIMPLE_YES_NO" />

  <!-- And more <property> elements... -->

  <!-- Automatically compressed texture rules are defined below.
       See the description below. -->
  <auto_compressed_textures>
    <formats>
      <!-- Automatically compressed texture formats.
        Format names are the same as TTextureCompression enum names
        (without leading "tc").
        Two most common RGBA compressions for mobiles are listed below. -->
      <format name="Pvrtc1_4bpp_RGBA"/>
      <format name="ATITC_RGBA_InterpolatedAlpha"/>
    </formats>

    <!-- Rules that determine which textures are automatically
         compressed, by including / excluding appropriate paths. -->

    <include path="spells/*" recursive="True" />
    <include path="castles/*" recursive="True" />
    <include path="player.png" />

    <exclude path="*.jpg" />
    <exclude path="spells/gui/*" />
    <exclude path="*/gui_hud.png" />
  </auto_compressed_textures>
</properties>'); ?>

<?php echo $toc->html_section(); ?>

<p>Textures inside folders mentioned in <code>&lt;auto_compressed_textures&gt;</code>
will have GPU-compressed alternative versions automatically generated.
This allows to easily reduce the GPU texture memory usage,
which is especially crucial on mobile devices. The compressed
textures should be distributed as part of your application,
and at runtime we will automatically load a suitable GPU-compressed alternative version
(that can be used on the current device).

<ol>
  <li><p>Running <code>castle-engine auto-compress-textures</code> will generate
    the GPU-compressed counterparts. See <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">the castle-engine build tool documentation</a>.
    They will be generated inside auto_compressed subdirectories
    of your data files.

    <p>This process underneath may call various external tools:

    <ul>
      <li><p><a href="https://community.imgtec.com/developers/powervr/tools/pvrtextool/">PVRTexToolCLI</a>

      <li><p><a href="http://developer.amd.com/tools-and-sdks/archive/legacy-cpu-gpu-tools/the-compressonator/">ATI Compressonator</a></p>

        <p>On non-Windows, it can be run under Wine
        You will need to do "winetricks vcrun2005" first.
        Installing it is troublesome under Wine, but a working installed dir can
        be copied from your Windows installation.</p>
      </li>

      <li><p>Not used for now:
        <a href="http://developer.amd.com/tools-and-sdks/graphics-development/amdcompress/">AMDCompressCLI</a>

        <p>We have it implemented, but it's broken unfortunately,
        so it's disabled by default.
        Messes up colors in ATITC* texture compression modes.
        Also, on non-Windows, running in under wine is even more
        troublesome than running ATI Compressonator...
        Please send complaints to AMD, maybe one day they'll release
        a tool that can correctly compress to ATITC*, from command-line,
        cross-platform.

        <!--

        <p>On non-Windows, it can be installed and run under Wine.
        Install with "wine start xxx.msi".
        You may also need to do "winetricks vcrun2005"
        and/or "winetricks vcrun2015".
        On some Wine versions it works cool (even installation from msi!),
        unfortunately on others &mdash; it doesn't work at all.</p>

        -->
    </ul>

  <li><p>In game, trying to load an uncompressed texture URL will automatically
    load the GPU-compressed version instead, if the GPU compression
    is supported. Just load the <code>material_properties.xml</code>
    (setting <code>MaterialProperties.URL</code>)
    early in your code.
</ol>

<p>This workflow allows to easily enable GPU texture compression in your games,
and work even when the GPU texture compression algorithm is not supported
by the given device. Very useful on Android, with the price: you should
distribute all compression variants.

<p>Note that GPU compression algorithms have their limitations.
They may resize your texture (to the power of two), some may even force
it to be square. Be sure your code supports it. Usually, this is Ok
for 3D textures and texture atlases, but not OK for GUI images.
Use the &lt;include&gt; / &lt;exclude&gt; elements to select only the sensible
subset of your data textures.
Include / exclude work just like in <code>CastleEngineManifest.xml</code>, documented in
<a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">the castle-engine build tool documentation</a>.
We first include matching files, then exclude matching.

<?php
creating_data_footer();
?>
