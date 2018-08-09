<?php
require_once 'castle_engine_functions.php';
creating_data_header('Auto-generated compressed and scaled textures');

$toc = new TableOfContents(
  array(
    new TocItem('Introduction', 'introduction'),
    new TocItem('Example material_properties.xml file using texture compression', 'example'),
    new TocItem('How does the automatic texture compression work', 'texture_compression'),
    new TocItem('How does the automatic downscaling work', 'texture_scale'),
  )
);
?>

<?php echo $toc->html_toc(); ?>
<?php echo $toc->html_section(); ?>

<p>Using the <a href="creating_data_material_properties.php">material_properties.xml file</a>
and <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">the castle-engine build tool</a>
you can automatically generate and use different versions of your textures.
The generated texture versions may be compressed (using GPU texture compression
formats, like S3TC on desktops, PVRTC and others on mobile) and/or
scaled down.

<p>This is a great way to make your game use less texture memory,
which often has a significant impact on the performance.

<ol>
  <li><p>Prepare the <code>material_properties.xml</code> file as <a href="#section_example">shown below</a>.

  <li><p>In code, load it, using:

    <?php echo pascal_highlight(
'uses ..., CastleMaterialProperties;

...
MaterialProperties.URL := ApplicationData(\'material_properties.xml\');'); ?>

    <p>As mentioned in the <a href="creating_data_material_properties.php">chapter introducing material_properties.xml</a>,
    it's best to do it as early as possible, for example
    at the beginning of <code>Application.OnInitialize</code> (if you
    use <code>CastleWindow</code>) or <code>TForm.OnCreate</code>
    (if you use <code>CastleControl</code> in Lazarus).

  <li><p>After changing your game data,
    always call <code>castle-engine auto-generate-textures</code> .
    It automatically rebuilds only the necessary textures,
    so it's usually simplest to just call it always before running your game,
    during development.

    <p>This requires that you have <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">the build tool</a>
    available on your $PATH. To make the GPU texture compression work,
    you also need to install some <a href="#section_texture_compression">required external programs</a>.

  <li><p>That's it. Auto-generated GPU compressed textures will be automatically
    used by the engine, instead of the original ones.

    <p>If you want to also use <i>downscaled</i> textures,
    change the <a href="https://castle-engine.io/apidoc/html/CastleMaterialProperties.html#TextureLoadingScale">TextureLoadingScale</a>
    global variable. By default it is 1. Setting it to 2 makes your textures
    use 1/2 size (their area will be 1/4, for 2D textures).
</ol>

<?php echo $toc->html_section(); ?>

<p>Below is a sample <code>material_properties.xml</code> file
requesting to compress and downscale some images.
Texture compression format names are <a href="https://castle-engine.io/apidoc/html/CastleImages.html#TTextureCompression">the same as TTextureCompression enum names but without leading "tc"</a>, e.g. <code>Dxt1_RGB</code>, <code>Pvrtc1_4bpp_RGBA</code> and so on.
</p>

<?php echo xml_highlight(
'<?xml version="1.0"?>

<properties>
  <!-- You can also use <property> elements
       as documented in the previous chapter:
  <property texture_base_name="grass_texture" footsteps_sound="grass_footsteps">
  </property>
  -->

  <!-- Automatically compressed and downscaled texture rules are defined below. -->
  <auto_generated_textures>
    <compress>
      <!-- Automatically compressed texture formats.
        Two most common RGBA compressions for mobiles are shown below. -->
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
    of your data files, and additionally a file
    <code>castle_engine_auto_generated.xml</code> will appear,
    describing the generated textures for internal purposes (e.g. to smartly
    update them later).

    <p>This process underneath may call various external tools:

    <ul>
      <li><p><a href="https://developer.nvidia.com/gpu-accelerated-texture-compression"><code>nvcompress</code> from NVidia Texture Tools</a>. Cross-platform (Windows, Linux...), free and open source. On Debian and derivatives (like Ubuntu) install them simply by <code>sudo apt-get install libnvtt-bin</code>.

      <li><p><a href="https://community.imgtec.com/developers/powervr/tools/pvrtextool/"><code>PVRTexToolCLI</code> from PowerVR Texture Tools</a>. Cross-platform (Windows, Linux...), free to download. Include both command-line and GUI tools to convert and view image files.

      <li><p><a href="http://developer.amd.com/tools-and-sdks/archive/legacy-cpu-gpu-tools/the-compressonator/">ATI Compressonator</a></p>

        <p>On non-Windows, it can be run under Wine
        You will need to do "winetricks vcrun2005" first.
        Installing it is troublesome under Wine, but a working installed dir can
        be copied from your Windows installation.</p>

        <p>TODO: This should be replaced with <a href="https://gpuopen.com/gaming-product/compressonator/">new AMD Compressonator</a>
        that is open-source, cross-platform and should provide
        much better experience.
      </li>

      <li><p>Not used for now:
        <a href="http://gpuopen.com/gaming-product/compressonator/"><code>AMDCompressCLI</code> from AMD Compress</a>

        <p>We have it implemented, but the old version was broken unfortunately,
        so it was disabled.
        Messed up colors in ATITC* texture compression modes.
        Also, on non-Windows, running it under wine is even more
        troublesome than running ancient ATI Compressonator.
        <!--
        <p>On non-Windows, it can be installed and run under Wine.
        Install with "wine start xxx.msi".
        You may also need to do "winetricks vcrun2005"
        and/or "winetricks vcrun2015".
        On some Wine versions it works cool (even installation from msi!),
        unfortunately on others &mdash; it doesn't work at all.</p>
        -->

        <p>Update on 2017-08: Possibly things are better now:
        <a href="http://gpuopen.com/compressonator-is-going-open-source/">AMD Compressonator</a> is now open-source. It is <a href="https://github.com/GPUOpen-Tools/Compressonator">available on GitHub</a>.
        It is also cross-platform.
        See <a href="http://gpuopen.com/gaming-product/compressonator/">new AMD Compressonator page</a>.

        <p>TODO: check and use the latest version of AMD Compress from above links.
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
