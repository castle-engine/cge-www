<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Texturing', 'texturing',
    'This component provides extensive textures support.
     2D textures may be loaded from image files (<code>ImageTexture</code>),
     movie files (<code>MovieTexture</code>) or encoded directly in VRML/X3D files
     (<code>PixelTexture</code>, also <code>ImageTexture</code> with <a href="https://en.wikipedia.org/wiki/Data_URI_scheme">data</a> urls).
     Multiple textures may be overlayed on a single polygon in a variety
     of ways. Texture coordinates may be explicitly provided or automatically
     calculated, and may be transformed.');

  $toc = new TableOfContents(
    array(
      new TocItem('Demos', 'demos'),
      new TocItem('Supported nodes', 'support'),
      new TocItem('Supported image file formats', 'support_formats'),
      new TocItem('Clarifications to X3D multi-texturing specification', 'multi_texturing'),
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
  <li><p><?php echo x3d_node_link('ImageTexture'); ?>,<br>
    <?php echo x3d_node_link('PixelTexture'); ?>.</p>

    <p>These nodes define a texture,
    from an external file (in the usual case of <code>ImageTexture</code>)
    or embedded inside the X3D file (in case of <code>PixelTexture</code>,
    or <code>ImageTexture</code> using <a href="manual_network.php">data URI</a>).

    <p><code>ImageTexture</code> allows various texture formats,
    including JPEG, PNG, GIF, BMP, PPM, RGBE, KTX, DDS.
    See <a href="castle-image-viewer">Castle Image Viewer</a>
    documentation for a detailed list.

    <p><i>Note about alpha channel</i>: alpha channel of the textures
    is fully supported, both a simple yes-no transparency (done
    by alpha_test in OpenGL) and full range transparency
    (done by blending in OpenGL, just like partially transparent materials).
    See <?php echo a_href_page_hashlink('"override alpha channel detection"
    extension description', 'x3d_extensions',
    'section_ext_alpha_channel_detection'); ?> for details.
    The bottom line is: everything will magically work fast and look perfect.

  <li><p><?php echo x3d_node_link('TextureCoordinate'); ?>,<br>
    <?php echo x3d_node_link('TextureTransform'); ?>.

    <p>These nodes specify how the texture is mapped onto the shape.
    All X3D nodes have some "default" texture mapping, if you don't provide
    explicit texture coordinates.

  <li><p><?php echo x3d_node_link('MovieTexture'); ?>

    <p>This node defines a texture that is actually a (playing) movie file.

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

  <li><p><?php echo x3d_node_link('MultiTexture'); ?>,<br>
      <?php echo x3d_node_link('MultiTextureCoordinate'); ?>,<br>
      <?php echo x3d_node_link('MultiTextureTransform'); ?>

    <p><i>Multi-texturing</i> means that you can use multiple textures on a single polygon (face). A number of textures can be mixed, in a way resembling the "layers" concept you from image editing applications (like GIMP or Photoshop). For each layer, you specify which texture to use, the "mode" (how it is mixed), also each layer may have it's own texture coordinates.

    <p>Some examples are present in the
    <a href="https://github.com/castle-engine/demo-models/">demo-models</a>,
    in particular in
    <a href="https://github.com/castle-engine/demo-models/tree/master/multi_texturing">multi_texturing subdirectory</a>.

    <p>We implement various modes (in <code>MultiTexture.mode</code>), sources
    (<code>MultiTexture.source</code>) and functions (in <code>MultiTexture.function</code>).
    As an extension, we also allow separate mode and sources for RGB and alpha, as documented in
    <a href="https://castle-engine.io/x3d_multi_texturing.php#section_proposed_mode">Proposed improved MultiTexture.mode specification</a>.

    <p><i>TODO:</i> Modes not implemented yet:
    <code>MODULATEALPHA_ADDCOLOR</code>,
    <code>MODULATEINVALPHA_ADDCOLOR</code>,
    <code>MODULATEINVCOLOR_ADDALPHA</code>.</p>

    <p><code>MultiTexture.source</code> values <code>"DIFFUSE"</code> and <code>"SPECULAR"</code> are treated the same. They don't make much sense in the context of PBR material parameters anyway.

    <p>Note that the multi-texturing specification of X3D has unfortunately some issues. <a href="x3d_multi_texturing.php">Clarifications and our extensions are documented here</a>.

    <p>Note that in case of 2D games, when the camera cannot move freely, you can often use multiple polygons (each with a single texture) rendered on top of each other (positioned with slight shift), instead of using a single polygon with multi-texturing. Both choices are fine, in principle: use whichever is more comfortable for your case.
  </li>

  <li><p><?php echo x3d_node_link('TextureCoordinateGenerator'); ?>

    <p>This node can be used instead of
    <?php echo x3d_node_link('TextureCoordinate'); ?>
    to automatically calculate the coordinates based on some algorithm.

    <p>Supported modes are now "SPHERE", "COORD", "COORD-EYE",
    "CAMERASPACEPOSITION", "CAMERASPACENORMAL", "CAMERASPACEREFLECTIONVECTOR".
    We also allow some extensions:
    <?php echo a_href_page_hashlink('"WORLDSPACEREFLECTIONVECTOR"
    and "WORLDSPACENORMAL"', 'x3d_implementation_texturing_extensions',
    'section_ext_tex_coord_worldspace'); ?>,
    <?php echo a_href_page_hashlink('"BOUNDS", "BOUNDS2D"
    and "BOUNDS3D"', 'x3d_implementation_texturing_extensions',
    'section_ext_tex_coord_bounds'); ?>.
    </p>

    <p>Note that "CAMERASPACEPOSITION" and
    "COORD-EYE" are exactly the same thing. Google confirms it
    (<a href="https://www.h3dapi.org:8090/H3DAPI/trunk/H3DAPI/src/TextureCoordinateGenerator.cpp">e.g.
    this source code also treats them as equal</a> and
    <a href="http://www.bitmanagement.com/developer/contact/labs/chrome.html">in this old
    bitmanagement spec they mention they are equal</a>).</p>

    <p>TODO: not implemented modes: "SPHERE-LOCAL", "NOISE", "NOISE-EYE",
    "SPHERE-REFLECT", "SPHERE-REFLECT-LOCAL".

  <li><p><?php echo x3d_node_link('TextureProperties'); ?>

    <p>Adjust various texture filtering/wrapping properties. Supported fields now are:

    <ul>
      <li><code>minificationFilter</code>, <code>magnificationFilter</code>,
      <li><code>anisotropicDegree</code>,
      <li><code>boundaryModeS</code>, <code>boundaryModeT</code>, <code>boundaryModeR</code>.
    </ul>

    <p>Some details about supported and unsupported fields:

    <dl>
      <dt>anisotropicDegree (fully supported)</dt>
      <dd><p>See
        <a href="https://github.com/castle-engine/castle-engine/blob/master/examples/viewport_and_scenes/anisotropic_filtering/">examples/viewport_and_scenes/anisotropic_filtering/</a>
        for example Pascal code that creates <code>TextureProperties</code> nodes
        and requests anisotropic filtering for specific textures.
        We plan to enable adjusting this in CGE editor at some point too.
      </dd>

      <dt>textureCompression (we do not support compressing texture data at runtime)</dt>
      <dd><p>We don't plan to support the this field now.

       <p>Reason:
        If you're looking for a way to use GPU compressed textures,
        <a href="dds">simply place the GPU compressed texture data in DDS or KTX files</a>.
        Using GPU compressed textures in DDS and KTX is fully supported,
        and always will be a more efficient solution anyway.
      </dd>

      <dt>boundaryModeS/T/R overrides texture repeatS/T/R</dt>
      <dd><p>Be aware that using <code>TextureProperties</code> means that texture fields
        <code>repeatS/T/R</code> is ignored.

        <p>E.g. <code>ImageTexture.repeatS</code> is ignored,
        instead we'll use <code>TextureProperties.boundaryModeS</code> to determine whether
        texture repeats or not in S coordinate.

        <p>You should instead adjust <code>boundaryModeS/T/R</code>, which is by default <code>"REPEAT"</code>
        (regardless of the texture type, 2D or 3D). You get more options for it now:
        <code>"CLAMP_TO_EDGE"</code>, <code>"REPEAT"</code> and <code>"MIRRORED_REPEAT"</code>.

        <p>This is a particular trap for 3D texture nodes, like <code>ImageTexture3D</code>,
        that have by default <code>repeatS/T/R</code> = <code>FALSE</code> (unlike 2D textures like <code>ImageTexture</code>
        that have by default <code>repeatS/T</code> = <code>TRUE</code>).
        So merely adding a <code>TextureProperties</code> node to 3D texture,
        with everything left as default, changes the default (for 3D textures) "clamp" mode into "repeat".
        Adjust the <code>boundaryModeS/T/R</code> to <code>"CLAMP_TO_EDGE"</code> to restore "clamp" mode.

      <dt>clamp to border (unsupported)</dt>
      <dd><p>We don't plan to support the <i>"clamp to border"</i> wrapping modes
        for <code>boundaryModeXxx</code> fields. For the same reason, we don't plan to support
        related <code>borderColor</code> and <code>borderWidth</code> fields, that only make sense
        for <i>"clamp to border"</i> mode.
        The idea of "clamping to border" is from OpenGL but it does not have much actual usage
        and it was removed/rejected from various other modern APIs:

        <ul>
          <li>It is not present in OpenGLES 3 (see <a href="https://www.khronos.org/registry/OpenGL-Refpages/es3.0/html/glTexParameter.xhtml">OpenGLES 3 glTexParameter</a>).
          <li>Though it remains supported in OpenGL 4 (see <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexParameter.xhtml">OpenGL 4 glTexParameter</a>).
          <li>glTF doesn't support it (see <a href="https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html">gltF spec - it only has "Repeat", "Mirrored Repeat", "Clamp to edge"</a>).
        </ul>

        <p>For this reason, <code>"CLAMP"</code>, <code>"CLAMP_TO_EDGE"</code>,
        <code>"CLAMP_TO_BOUNDARY"</code> are all equivalent and work like
        <code>"CLAMP_TO_EDGE"</code>.
      </dd>

      <dt>meaning of <code>"DEFAULT"</code> minification / magnification filters</dt>
      <dd>

    <!-- Fixed in X3D 4.

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
  minificationFilter="DEFAULT" /&gt;
</pre>

    -->

    <p>The default value of
    <code>magnificationFilter</code> and
    <code>minificationFilter</code> is <code>"DEFAULT"</code>.
    Which means to use the texture filtering mode specified by
    <?php echo cgeRef('TCastleRenderOptions.MinificationFilter'); ?>,
    <?php echo cgeRef('TCastleRenderOptions.MagnificationFilter'); ?>
    (by default nice <code>minLinearMipmapLinear</code>, <code>magLinear</code>),
    matching behavior when <code>TextureProperties</code> was not specified.

    <p>So this behavior is most natural.

    <p>(It was a bit more tricky in older CGE versions &lt;= 7.0-alpha.1, before 2022-03-09.)
  </dd>
</ul>

<?php echo $toc->html_section(); ?>

<p>See <a href="castle-image-viewer">Castle Image Viewer</a> for
the full list of image formats we can handle.

<p>See also details about <a href="dds">DDS format support</a> and <a href="ktx">KTX format support</a>.</p>

<?php echo $toc->html_section(); ?>

<p><?php echo a_href_page('Comments about X3D MultiTexturing problems
and solutions (used in our engine, and proposed for future X3D spec) are here.',
'x3d_multi_texturing'); ?>

<?php
  castle_footer();
?>
