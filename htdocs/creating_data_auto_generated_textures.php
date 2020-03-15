<?php
require_once 'castle_engine_functions.php';
creating_data_header('Auto-generated compressed and scaled textures');

$toc = new TableOfContents(
  array(
    new TocItem('Introduction', 'introduction'),
    new TocItem('Example material_properties.xml file using texture compression', 'example'),
    new TocItem('Using specific compression formats only for specific platforms', 'platforms'),
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
MaterialProperties.URL := \'castle-data:/material_properties.xml\';'); ?>

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
    change the <?php api_link('TextureLoadingScale', 'CastleMaterialProperties.html#TextureLoadingScale'); ?>
    global variable. By default it is 1. Setting it to 2 makes your textures
    use 1/2 size (their area will be 1/4, for 2D textures).
</ol>

<?php echo $toc->html_section(); ?>

<p>Below is a sample <code>material_properties.xml</code> file
requesting to compress and downscale some images.
Texture compression format names are <?php api_link('the same as TTextureCompression enum names but without leading "tc"', 'CastleImages.html#TTextureCompression'); ?>, e.g. <code>Dxt1_RGB</code>, <code>Pvrtc1_4bpp_RGBA</code> and so on.
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
    <exclude path="*.jpg" />
    <exclude path="spells/gui/*" />
  </auto_generated_textures>

  <!-- You can use as many <auto_generated_textures> elements as you like,
       to specify different properties (compression, downscaling) for different
       texture groups.

       Just make sure that no image falls into more than one <auto_generated_textures>
       group (use include/exclude to make the groups disjoint).
       You will get a warning in case it happens. -->

  <!-- These textures will be downscaled (to be 2x smaller).
       You can use TextureLoadingScale in CGE to load downscaled textures. -->
  <auto_generated_textures>
    <scale smallest="2" />
    <include path="endings/*" recursive="True" />
  </auto_generated_textures>

  <!-- These textures will be converted to DDS format (which may load
       faster than other formats on some platforms).
       They will not be GPU-compressed, they will not be downscaled. -->
  <auto_generated_textures>
    <!--
      If specified, it sets the preferred output image format.

      If not specified, by default this is .png.
      It can be any image format that CGE can write.

      - This does affect the uncompressed (only downlscaled,
        or not even downscaled when trivial_uncompressed_convert) format.

      - This does not affect the GPU-compressed (e.g. to DXT5) textures now.
        For the GPU-compressed textures,
        their format depends on what the underlying tool can make,
        and it is hardcoded in CGE code (see the TextureCompressionInfo array)
        as .dds or .ktx, depending on the compression.
    -->
    <preferred_output_format extension=".dds" />

    <!--
      If this element is specified, we will also make
      not-compressed and not-downscaled texture version.
      It will have a format specified in preferred_output_format.
      This is useful if (in distributed game) you just prefer
      given format (e.g. because it\'s faster to read).
    -->
    <trivial_uncompressed_convert />

    <include path="gui/*" recursive="True" />
  </auto_generated_textures>
</properties>'); ?>

<?php echo $toc->html_section(); ?>

<p>The <code>&lt;format&gt;</code> element can optionally specify that the textures compressed to this format are only suitable for a particular platform. In effect, the textures will not be distributed on other platforms, i.e. <code>castle-engine package --target=xxx</code> (see <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">the build tool documentation</a>) will automatically exclude the unused textures on target platform. The code will also know that these compression formats are never packaged (so it will not try to use them, even if at runtime GPU happens to support them).</p>

<p>This is useful as some combinations of compression + target are seldom possible. E.g. most mobile devices<!-- (except some rare Android devices)--> do not support the DXTx compression.

<p>To do this, place <code>&lt;platforms&gt;</code> element within <code>&lt;format&gt;</code>, like this:</p>

<?php echo xml_highlight(
'<?xml version="1.0"?>
<properties>
  <auto_generated_textures>
    <compress>
      <format name="DXT5">
        <platforms>
          <platform>Desktop</platform>
          <platform>Nintendo Switch</platform>
        </platforms>
      </format>
      <format name="Pvrtc1_4bpp_RGBA">
        <platforms>
          <platform>iOS</platform>
        </platforms>
      </format>
    </compress>
    <include path="*" recursive="True" />
  </auto_generated_textures>
</properties>'); ?>

<p>Possible values inside <code>&lt;platform&gt;</code> are:</p>

<ul>
  <li>Nintendo Switch</li>
  <li>Android</li>
  <li>iOS</li>
  <li>Desktop</li>
</ul>

<p>Note: Not specyfing <code>&lt;platforms&gt;</code> at all means that the format should be distributed on <em>all</em> platforms (current and future). On the other hand, specifying empty <code>&lt;platforms&gt;&lt;/platforms&gt;</code> means that the format is not distributed on <em>any</em> platform (this makes commenting out a particular <code>&lt;platform&gt;xxx&lt;/platform&gt;</code> line behave naturally).</p>

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

      <li><p><a href="https://developer.arm.com/tools-and-software/graphics-and-gaming/graphics-development-tools/mali-texture-compression-tool"><code>astcenc</code>, part of Mali Texture Compression Tool</a>. It is called by <code>PVRTexToolCLI</code> to encode textures to ASTC. <a href="https://github.com/ARM-software/astc-encoder">See also it's source code</a>.

      <li><p><a href="https://gpuopen.com/gaming-product/compressonator/"><code>CompressonatorCLI</code> from AMD Compressonator</a>. Cross-platform (Windows, Linux...), free and open source. <a href="https://github.com/GPUOpen-Tools/Compressonator">Source code is on GitHub</a>, <a href="https://github.com/GPUOpen-Tools/Compressonator/releases">binary releases can be downloaded from here</a>.

        <p>This is a successor of the old (great, but Windows-only and closed-source)
        <i>ATI Compressonator</i> and later <i>AMD Compress</i>.
        In particular, it is capable of compression to AMD GPU compression
        formats <code>ATITC*</code>, that are common on Android devices.
    </ul>

    <p>The location of these tools is searched using your <code>PATH</code>
    environment variable. If you don't know what it means or how to set
    your <code>PATH</code> environment variable,
    please search the Internet, there are step-by-step
    instrucions for all operating systems.
    Some of these tools have also a standardized install location,
    we look there too.

    <p>The <i>build tool</i> automatically calls an appropriate
    compression tool with the appropriate options.
    Some formats require specific tools (e.g. ATI compression formats
    require <code>CompressonatorCLI</code>),
    other formats can be produced using a 1st available tool
    (e.g. DXT* compression formats can be done either using <code>nvcompress</code>
    or <code>CompressonatorCLI</code>).

    <p>For macOS, getting some of these tools is not easy.
    <a href="https://github.com/floooh/oryol/tree/master/tools">Here you can find precompiled versions
    for various systems, including macOS</a>.

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
