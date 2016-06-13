<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Texturing', 'texturing',
    'This component provides extensive textures support.
     2D textures may be loaded from image files (<code>ImageTexture</code>),
     movie files (<code>MovieTexture</code>) or encoded directly in VRML/X3D files
     (<code>PixelTexture</code>, also <code>ImageTexture</code> with <a href="http://en.wikipedia.org/wiki/Data_URI_scheme">data</a> urls).
     Multiple textures may be overlayed on a single polygon in a variety
     of ways. Texture coordinates may be explicitly provided or automatically
     calculated, and may be transformed.');

  $toc = new TableOfContents(
    array(
      new TocItem('Demos', 'demos'),
      new TocItem('Supported nodes', 'support'),
      new TocItem('Supported image file formats', 'support_formats'),
      new TocItem('Clarifications to X3D multi-texturing specification', 'multi_texturing'),
      new TocItem('DDS (DirectDraw Surface) support details', 'dds'),
    ));
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<p>See also <?php echo a_href_page('Castle Game Engine (and view3dscene) extensions related to texturing','x3d_implementation_texturing_extensions'); ?>.

<?php echo $toc->html_section(); ?>

<p>For demos and tests of these features,
see the <code>texturing_advanced</code> and <code>movie_texture</code>
and <code>multi_texturing</code> subdirectories inside <?php
echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>.</p>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p><?php echo x3d_node_link('ImageTexture'); ?>,
    <?php echo x3d_node_link('TextureTransform'); ?>,
    <?php echo x3d_node_link('TextureCoordinate'); ?>,
    <?php echo x3d_node_link('PixelTexture'); ?></p>

    <p><i>Note</i>: ImageTexture allows various texture formats,
    including JPEG, PNG, GIF, BMP, PPM, RGBE.
    See <?php echo a_href_page('glViewImage', 'glviewimage'); ?>
    documentation for more detailed list.

    <p><i>Note about alpha channel</i>: alpha channel of the textures
    is fully supported, both a simple yes-no transparency (done
    by alpha_test in OpenGL) and full range transparency
    (done by blending in OpenGL, just like partially transparent materials).
    See <?php echo a_href_page_hashlink('"override alpha channel detection"
    extension description', 'x3d_extensions',
    'section_ext_alpha_channel_detection'); ?> for details.
    The bottom line is: everything will magically work fast and look perfect.

  <li><p><?php echo x3d_node_link('MovieTexture'); ?>

    <p><i>TODO</i>: for now, the sound of the movie is not played.

    <p><i>Notes</i>:

    <ul>
      <li><p>Current implementation keeps the whole encoded video in memory
        (images may be discarded after loading (by TCastleSceneCore.FreeResources
        feature), but still the textures for all frames are kept in memory).
        The <i>disadvantage</i> is that this makes it impractical to load "real"
        movies, normal 2-hour movie will most usually eat all of your memory.
        The <i>advantage</i> is that once the movie is loaded, the playback is
        super-fast, just like you would display normal nodes with static
        textures. Since there's no streaming, decoding etc. in the background
        while you browse your models.

        <p>In other words, this is quite perfect for movie textures
        with game effects, like smoke or flame. But it's not a substitute
        for your "real" multimedia movie player.

      <li><p><a href="http://ffmpeg.mplayerhq.hu/">ffmpeg</a> must be
        installed and available on $PATH to actually open any movie format.
        See <?php echo a_href_page_hashlink('instructions for
        installing ffmpeg in view3dscene docs', 'view3dscene', 'section_depends'); ?>.
        Thanks to ffmpeg, we can handle probably any movie format you will
        ever need to open.

      <li><p>We can also open movies from images sequence.
        This doesn't require ffmpeg, and allows for some tricks
        (movie texture with alpha channel).
        See <?php echo a_href_page_hashlink('"Movies from images sequence"
        extension description', 'x3d_extensions',
        'section_ext_movie_from_image_sequence'); ?>.
    </ul>

  <li><?php echo x3d_node_link('MultiTexture'); ?>,
      <?php echo x3d_node_link('MultiTextureCoordinate'); ?>,
      <?php echo x3d_node_link('MultiTextureTransform'); ?>

    <p>Support for all fields (unless mentioned below).

    <p>Note that using <code>MultiTexture.function</code>
    forces shader pipeline for given shape (so it will not work on really
    old GPUs).
    There is no way to reasonably do this using OpenGL fixed-function pipeline,
    as corresponding OpenGL settings
    (GL_OPERANDx) operate <i>before</i> normal texture unit calculations
    are done, while X3D spec requires <code>function</code> to act afterwards.

    <p><i>TODO</i>: modes
    <code>MODULATEALPHA_ADDCOLOR</code>,
    <code>MODULATEINVALPHA_ADDCOLOR</code>,
    <code>MODULATEINVCOLOR_ADDALPHA</code>
    are temporarily not supported.</p>

    <p><i>TODO</i>: source values "DIFFUSE" and "SPECULAR" are treated
    the same, as <code>PRIMARY_COLOR</code> (in the sense of OpenGL
    ARB_texture_env_combine extension). Primary color contains
    material ambient, diffuse and specular factors,
    multiplied by lighting properties, summed over all lights.
    I don't know of any way to efficiently implement separate
    diffuse / specular sources &mdash; please report if you do,
    otherwise there's no way this can be fixed (note that engine's
    multi-texturing must work without shaders too).</p>
  </li>

  <li><?php echo x3d_node_link('TextureCoordinateGenerator'); ?>

    <p>Supported modes are now "SPHERE", "COORD", "COORD-EYE",
    "CAMERASPACEPOSITION", "CAMERASPACENORMAL", "CAMERASPACEREFLECTIONVECTOR".</p>

    <p>Note that "CAMERASPACEPOSITION" and
    "COORD-EYE" are exactly the same thing. Google confirms it
    (<a href="https://www.h3dapi.org:8090/H3DAPI/trunk/H3DAPI/src/TextureCoordinateGenerator.cpp">e.g.
    this source code also treats them as equal</a> and
    <a href="http://www.bitmanagement.com/developer/contact/labs/chrome.html">in this old
    bitmanagement spec they mention they are equal</a>).</p>

    <p>As an extension, we also allow <?php echo a_href_page_hashlink('"WORLDSPACEREFLECTIONVECTOR"
    and "WORLDSPACENORMAL" texture generation modes', 'x3d_extensions',
    'section_ext_tex_coord_worldspace'); ?>.</p>

    <p>TODO: not implemented modes: "SPHERE-LOCAL", "NOISE", "NOISE-EYE",
    "SPHERE-REFLECT", "SPHERE-REFLECT-LOCAL".

  <li><p><?php echo x3d_node_link('TextureProperties'); ?>

    <p><code>minificationFilter</code>, <code>magnificationFilter</code>,
    <code>anisotropicDegree</code> fields are supported.

    <p><i>TODO</i>: the rest
    of the <code>TextureProperties</code> fields are not supported yet.
    In particular, if you're looking for a way to use GPU compressed textures,
    <a href="#section_dds">simply place the GPU compressed texture data in DDS files</a>
    (it's a better idea than using the
    <code>TextureProperties.textureCompression</code> field).

    <p>Note <b>a common mistake when using the <code>TextureProperties</code> node:
    texture filtering gets ugly</b>. This is a common mistake,
    because the X3D specification makes the behavior unexpected:
    <code>TextureProperties</code> node by default has
    <code>magnificationFilter</code> and
    <code>minificationFilter</code> fields set to <code>"FASTEST"</code>,
    which instructs us (and every other valid X3D browser) to use
    the most ugly filtering (without mipmaps, without even linear filtering).
    To avoid it, always override these fields to <code>"DEFAULT"</code>.
    For example, instead of using this:

<pre class="vrml_code">
&lt;TextureProperties anisotropicDegree="4"/&gt;
</pre>

    <p>use this:

<pre class="vrml_code">
&lt;TextureProperties
  anisotropicDegree="4"
  magnificationFilter="DEFAULT"
  minificationFilter="DEFAULT" //&gt;
</pre>

    <p>The <code>"DEFAULT"</code> above means to use the browser-specific texture
    filtering modes, for us this means to use values in
    <?php api_link('Scene.Attributes.MinificationFilter', 'CastleRenderer.TRenderingAttributes.html#MinificationFilter'); ?>,
    <?php api_link('Scene.Attributes.MagnificationFilter', 'CastleRenderer.TRenderingAttributes.html#MagnificationFilter'); ?>
    which by default are nice
    (<code>minLinearMipmapLinear</code>, <code>magLinear</code>).
</ul>

<?php echo $toc->html_section(); ?>

<p>See <?php echo a_href_page("glViewImage", "glviewimage") ?> features
for the full list of 2D image formats we can handle.
See <a href="#section_dds">lower on this page for details about DDS format
support</a>.</p>

<?php echo $toc->html_section(); ?>

<p><?php echo a_href_page('Comments about X3D MultiTexturing problems
and solutions (used in our engine, and proposed for future X3D spec) are here.',
'x3d_multi_texturing'); ?>

<?php echo $toc->html_section(); ?>

<p><a href="http://en.wikipedia.org/wiki/DirectDraw_Surface">DirectDraw
Surface (DDS) image format</a> is supported. This format allows to store textures
compressed for GPU (S3TC), storing mipmaps, cube maps, and volume (3D) textures.
A number of technical details about DDS implementation are below, but in short:
we try to support all formats and all options of DDS in a standard way.
You may find example DDS images inside <?php
echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?> (look inside <code>textures</code> subdirectory for <code>*.dds</code> files). You can open them directly with our image viewer <?php echo a_href_page("glViewImage", "glviewimage") ?>.</p>

<p>Implementation history:

<ul>
  <li>DDS support is natively built into the engine. Since I knew that I want to use many of DDS features, like cube maps, 3D textures, mipmaps, compression, I decided the best way to go will be to create my own reader, instead of relying on external tools.

  <li>Other notable open-source implementations of DDS are by <a href="http://nifelheim.dyndns.org/~cocidius/dds/">GIMP-DDS plugin</a> and new <a href="http://www.imagemagick.org/">ImageMagick</a> (<a href="http://www.imagemagick.org/discourse-server/viewtopic.php?f=2&amp;t=10729">since 6.3.9</a>).

  <li>While implementing, I was looking at GIMP DDS source code (it's on GNU GPL >= 2 :) ) and <a href="http://msdn.microsoft.com/en-us/library/bb943990(VS.85).aspx">MS documentation for DDS</a>.
</ul>

<p>Cube maps in DDS are supposed to be oriented as usual for DDS:

<ol>
  <li><p>Which means that they match Direct X "positive/negative x/y/z". For OpenGL rendering we swap positive/negative Y faces (because Direct X has left-handed coordinate system, <a href="http://doc.51windows.net/directx9_sdk/graphics/programmingguide/advancedtopics/PixelPipe/envmap/cubicenvironmentmapping.htm">see here for drawing of DirectX cube map images orientation</a> and compare with <a href="http://www.opengl.org/registry/specs/ARB/texture_cube_map.txt">OpenGL cube map orientation</a>).

  <li><p>It's also a different orientation then the one of X3D ComposedCubeMap specification (left/right, bottom/top, front/back, with bottom/top on Y axis; X3D orientation needs rotating left,right,front,back images by 180 degrees for OpenGL orientation).
</ol>

<p>Images in DDS are supposed to be written from top to bottom row, as is the standard in DDS. (One particular tool, AMD CubeMapGen, allows to invert rows of the DDS images to match OpenGL bottom-to-top ordering; don't use this &mdash; we expect rows ordered as is standard in DDS, top-to-bottom.) Internally, our engine just inverts the rows for OpenGL (yes, <a href="http://users.telenet.be/tfautre/softdev/ddsload/explanation.htm">this is doable also for S3TC compressed images</a>.)</p>

<p>Pixel formats supported:</p>

<ol>
  <li><p>Absolutely <i>all uncompressed non-float pixel formats are supported</i>.</p>

    <p>Details:</p>

    <p>The formats that are currently loaded optimally are ABGR8, BGR8, AL8, L8. They translate to RGBA8, RGB8 etc. OpenGL formats (reversed order, as DDS color masks are little-endian). Popular ARGB8 and RGB8 are also loaded very fast.</p>

    <p>Grayscale (luminance) images are allowed. AL8 and L8 are optimized. Note that grayscale images aren't officially allowed by DDS docs, but at least GIMP-DDS plugin can write it (just sets all R, G and B masks equal, and doesn't set any of DDPF_RGB, DDPF_FOURCC, DDPF_PALETTEINDEXED8).</p>

    <p>Also only-alpha images are allowed (another undocumented DDS feature, GIMP-DDS can write it, for now they will result in grayscale(white) with alpha image).</p></li>

  <li><p>Compressed texture formats handled: DXT1, DXT3, DXT5 are supported.
    Texture with DXT1 is always treated like a texture with simple (yes/no)
    alpha channel (so it will be rendered with alpha testing) and
    DXT3 / DXT5 are always treated like a texture with full range
    alpha channel (so they will be rendered with blending).</p>

    <p>Both normal (2D) textures and cube maps may be compressed. (There is no compression possible for 3D textures &mdash; neither DDS format allows it, nor do common graphic cards.)</p></li>

  <li><p>Reading float textures from DDS is for now not supported.
    Our engine supports float textures (see TRGBFloatImage class),
    but DDS reader doesn't support them yet. Please submit a feature request
    (<?php echo a_href_page('through forum or ticket system', 'forum'); ?>),
    preferably with some test images, if you need them.
    </p></li>
</ol>

<p>If DDS file includes mipmaps, and mipmaps are required for texture minification filter, we will use DDS mipmaps (instead of generating mipmaps automatically). Works for all 2D, 3D, cubemap DDS files.</p>

<?php
  x3d_status_footer();
?>
