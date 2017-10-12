<?php
require_once 'castle_engine_functions.php';
creating_data_header('Material properties configuration');

$toc = new TableOfContents(
  array(
    new TocItem('Usage', 'usage'),
    new TocItem('Example material_properties.xml file', 'example'),
    new TocItem('How does the automatic texture compression work', 'texture_compression'),
    new TocItem('How does the automatic downscaling work', 'texture_scale'),
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
  <li>texture GPU-compressed alternatives, and downscaled alternatives.</li>
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

  <!-- Automatically compressed and downscaled texture rules are defined below.
       See the description below. -->
  <auto_generated_textures>
    <compress>
      <!-- Automatically compressed texture formats.
        Format names are the same as TTextureCompression enum names
        (without leading "tc").
        Two most common RGBA compressions for mobiles are listed below. -->
      <format name="Pvrtc1_4bpp_RGBA"/>
      <format name="ATITC_RGBA_InterpolatedAlpha"/>
    </compress>

    <!-- Automatically downscaled texture versions.
        Value smallest="1" is the default, it means that no downscaling is done.
        Value smallest="2" says to generate downscaled
        versions (of uncompressed, and all compressed formats)
        of size 1/2. -->
    <scale smallest="1" />

    <!-- Rules that determine which textures are automatically
         compressed and downscaled, by including / excluding appropriate paths. -->

    <include path="spells/*" recursive="True" />
    <include path="castles/*" recursive="True" />
    <include path="player.png" />

    <exclude path="*.jpg" />
    <exclude path="spells/gui/*" />
    <exclude path="*/gui_hud.png" />
  </auto_generated_textures>

  <!-- You can use as many <auto_generated_textures> elements as you like,
       to specify different properties (compression, downscaling) for different
       texture groups.

       Just make sure that no image falls into more than one <auto_generated_textures>
       group (use include/exclude to make the groups disjoint).
       You will get a clean warning in case it happens, so don\'t worry:) -->
</properties>'); ?>

<?php echo $toc->html_section(); ?>

<p>Textures inside folders mentioned in <code>&lt;auto_generated_textures&gt;</code>
will have GPU-compressed alternative versions automatically generated.
This allows to easily reduce the GPU texture memory usage,
which is especially crucial on mobile devices. The compressed
textures should be distributed as part of your application,
and at runtime we will automatically load a suitable GPU-compressed alternative version
(that can be used on the current device).

<ol>
  <li><p>Running <code>castle-engine auto-generate-textures</code> will generate
    the GPU-compressed (and optionally scaled down) counterparts.
    See <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">the castle-engine build tool documentation</a>.
    They will be generated inside the <code>auto_generated</code> subdirectories
    of your data files.

    <p>This process underneath may call various external tools:

    <ul>
      <li><p><a href="https://developer.nvidia.com/gpu-accelerated-texture-compression"><code>nvcompress</code> from NVidia Texture Tools</a>. Cross-platform (Windows, Linux...), free and open source. On Debian and derivatives (like Ubuntu) install them simply by <code>sudo apt-get install libnvtt-bin</code>.

      <li><p><a href="https://community.imgtec.com/developers/powervr/tools/pvrtextool/"><code>PVRTexToolCLI</code> from PowerVR Texture Tools</a>. Cross-platform (Windows, Linux...), free to download. Include both command-line and GUI tools to convert and view image files.

      <li><p><a href="http://developer.amd.com/tools-and-sdks/archive/legacy-cpu-gpu-tools/the-compressonator/">ATI Compressonator</a></p>

        <p>On non-Windows, it can be run under Wine
        You will need to do "winetricks vcrun2005" first.
        Installing it is troublesome under Wine, but a working installed dir can
        be copied from your Windows installation.</p>
      </li>

      <li><p>Not used for now:
        <a href="http://gpuopen.com/gaming-product/compressonator/"><code>AMDCompressCLI</code> from AMD Compress</a>

        <p>We have it implemented, but it's broken unfortunately,
        so it's disabled by default.
        Messes up colors in ATITC* texture compression modes.
        Also, on non-Windows, running in under wine is even more
        troublesome than running ATI Compressonator...
        Please send complaints to AMD, maybe one day they'll release
        a tool that can correctly compress to ATITC*, from command-line,
        and is cross-platform.

        <p>Update on 2017-08: Possibly things are better now:
        <a href="http://gpuopen.com/compressonator-is-going-open-source/">AMD Compress</a> is now open-source. It is <a href="https://github.com/GPUOpen-Tools/Compressonator">available on GitHub</a>.
        See <a href="http://gpuopen.com/gaming-product/compressonator/">new AMD Compress page</a>.

        <p>It is still Windows-only, but possibly it's working better now.
        TODO: someone needs to check the latest version of AMD Compress.

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

<p>Note that some GPU compression algorithms have particular limitations.
They often require your texture to have a "power of two" size, some even require
it to be square (PVRTC1 on Apple devices &mdash; blame Apple here).
We do not forcefully resize your texture, as it could break your texture coordinates.
<!--
(depending on the input format, your texture may be addressed using various
coordinates &mdash; e.g. most 3D formats use texture coords in 0..1 range,
but Spine atlases address using original coords from the top).-->
Instead, we require your textures to already have the necessary size.
Usually, the compression is OK for 3D textures and texture atlases
(as they are usually created already with power-of-2 sizes),
but not OK for GUI images.
Use the &lt;include&gt; / &lt;exclude&gt; elements to select only the sensible
subset of your data textures.
Include / exclude work just like
<a href="https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples">including / excluding data to package inside the <code>CastleEngineManifest.xml</code></a>.
We first include matching files, then exclude matching.

<?php echo $toc->html_section(); ?>

<p>If you use the <code>&lt;scale&gt;</code> element in the <code>&lt;auto_generated_textures&gt;</code> group,
we will generate alternative downscaled versions of the textures.
The <code>castle-engine auto-generate-textures</code> call with generate
them, using a high-quality scaling algorithm.
<!-- (for now: using the external
<a href="http://www.imagemagick.org/">ImageMagick</a> tools).-->

<p>These textures will be automatically used if you set the global
<?php api_link('TextureLoadingScale', 'CastleMaterialProperties.html#TextureLoadingScale'); ?> variable in your code.

<p>The attribute <code>smallest</code> of the <code>&lt;scale&gt;</code> element
is interpreted analogous to <?php api_link('TextureLoadingScale', 'CastleMaterialProperties.html#TextureLoadingScale'); ?> variable, so

<ul>
  <li><code>&lt;scale smallest="1" /&gt;</code> (default) means that no downscaling actually occurs,
  <li><code>&lt;scale smallest="2" /&gt;</code> means that textures are scaled to 1/2 of their size,<!-- (for 2D textures, their area shrinks to 1/4),-->
  <li><code>&lt;scale smallest="3" /&gt;</code> means that each size is scaled to 1/4 and so on.
</ul>

<p>The advantages of downscaling this way:

<ul>
  <li><p>For uncompressed textures, this downscaling is high-quality.

    <p>Unlike the fast
    scaling at run-time which is done by <?php api_link('GLTextureScale', 'CastleGLImages.html#GLTextureScale'); ?>.
    Note that the runtime scaling can still be performed by
    <?php api_link('GLTextureScale', 'CastleGLImages.html#GLTextureScale'); ?>,
    if you set it.
    The effects of
    <?php api_link('TextureLoadingScale', 'CastleMaterialProperties.html#TextureLoadingScale'); ?>
    and
    <?php api_link('GLTextureScale', 'CastleGLImages.html#GLTextureScale'); ?>
    cummulate.

  <li><p>For compressed textures, this is the only way to get downscaled texture
    versions that can be chosen at runtime. We cannot downscale compressed textures
    at runtime, so <?php api_link('GLTextureScale', 'CastleGLImages.html#GLTextureScale'); ?>
    has no effect on compressed textures.
</ul>

<?php
creating_data_footer();
?>
