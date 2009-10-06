<?php
  define('X3D_COMPONENT_NAME', 'Texturing');
  require_once 'vrml_implementation_common.php';
  x3d_status_header();
?>

<p>Supported nodes:</p>

<ul>
  <li><p><tt>ImageTexture</tt>, <tt>TextureTransform</tt>, <tt>TextureCoordinate</tt>, <tt>PixelTexture</tt></p>

    <p><i>Note</i>: ImageTexture allows various texture formats,
    including JPEG, PNG, BMP, PPM, RGBE. GIF format is supported
    by running <tt>convert</tt> program from
    <a href="http://www.imagemagick.org/">ImageMagick</a>
    package "under the hood".
    See <?php echo a_href_page('glViewImage', 'glviewimage'); ?>
    documentation for more detailed list.

    <p><i>Note about alpha channel</i>: alpha channel of the textures
    is fully supported, both a simple yes-no transparency (done
    by alpha_test in OpenGL) and full range transparency
    (done by blending in OpenGL, just like partially transparent materials).
    See <?php echo a_href_page_hashlink('"override alpha channel detection"
    extension description', 'kambi_vrml_extensions',
    'section_ext_alpha_channel_detection'); ?> for details.
    The bottom line is: everything will magically work fast and look perfect.

    <p><a name="default_texture_mode_modulate"></a><b>Note
    about REPLACE vs MODULATE modes</b>: VRML 2 / X3D specifications
    say that RGB textures should <tt>REPLACE</tt> the color (as opposed
    to <tt>MODULATE</tt> the color from lighting calculations, material etc.).
    The problem with that is that this makes RGB textures nearly useless
    in typical 3D world (when you usually expect textured surfaces to be
    properly lit, regardless of RGB or grayscale format).
    That's why the default engine behavior contradicts the specification:
    it's <tt>MODULATE</tt>, making an RGB texture modulated by lighting just
    like a grayscale texture.</p>

    <p>I didn't decide it lightly (noone likes
    to deliberately contradict the specification...), but I think this case
    is justified --- <tt>MODULATE</tt> behavior is much more useful and usually
    desired, IMO. Feel welcome to send me emails and argument against this.
    After all, I'm trying to fit the needs of most people with default
    behavior. If many people think that specification is right and I'm dumb,
    and the default behavior should follow the spec and be <tt>REPLACE</tt>,
    I'll obey :)</p>

    <p>You have menu item in view3dscene <i>RGB Textures Color Mode -&gt;
    GL_REPLACE</i> to change this (from code, use
    <tt>Scene.Attributes.TextureModeRGB := GL_REPLACE;</tt>).</p>

  <li><p><tt>MovieTexture</tt>

    <p><i>TODO</i>: for now, the sound of the movie is not played.

    <p><i>Notes</i>:

    <ul>
      <li><p>Current implementation keeps the whole encoded video in memory
        (images may be discarded after loading (by TVRMLScene.FreeResources
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
        extension description', 'kambi_vrml_extensions',
        'section_ext_movie_from_image_sequence'); ?>.
    </ul>

  <li><tt>MultiTexture</tt>,
      <tt>MultiTextureCoordinate</tt>,
      <tt>MultiTextureTransform</tt>

    <p><i>TODO</i>: modes
    <tt>MODULATEALPHA_ADDCOLOR</tt>,
    <tt>MODULATEINVALPHA_ADDCOLOR</tt>,
    <tt>MODULATEINVCOLOR_ADDALPHA</tt>
    are temporarily not supported.</p>

    <p><i>TODO</i>: source values "DIFFUSE" and "SPECULAR" are treated
    the same, as <tt>PRIMARY_COLOR</tt> (in the sense of OpenGL
    ARB_texture_env_combine extension). Primary color contains
    material ambient, diffuse and specular factors,
    multiplied by lighting properties, summed over all lights.
    I don't know of any way to efficiently implement separate
    diffuse / specular sources &mdash; please report if you do,
    otherwise there's no way this can be fixed (note that engine's
    multi-texturing must work without shaders too).</p>

    <p><i>TODO</i>: <tt>function</tt> field is not supported for now.
    It's somewhat uncomfortable, corresponding OpenGL settings
    (GL_OPERANDx) operate <i>before</i> normal texture unit calculations
    are done, while X3D spec requires <tt>function</tt> to act afterwards.
    To implement it generally, I'd have to use 1 more texture unit than
    requested (if the last texture unit will use any non-default function).</p>

    <p>See <a href="#section_x3d_multitex_clarifications">clarifications to
    X3D multi-texturing specification</a> below for more details about
    multi-texture handling.
  </li>

  <li><tt>TextureCoordinateGenerator</tt>

    <p>Supported modes are now "SPHERE", "COORD", "COORD-EYE",
    "CAMERASPACEPOSITION", "CAMERASPACENORMAL", "CAMERASPACEREFLECTIONVECTOR".</p>

    <p>Note that "CAMERASPACEPOSITION" and
    "COORD-EYE" are exactly the same thing. Google confirms it
    (<a href="https://www.h3dapi.org:8090/H3DAPI/trunk/H3DAPI/src/TextureCoordinateGenerator.cpp">e.g.
    this source code also treats them as equal</a> and
    <a href="http://www.bitmanagement.com/developer/contact/labs/chrome.html">in this old
    bitmanagement spec they mention they are equal</a>).</p>

    <p>As an extension, we also allow <?php echo a_href_page_hashlink('"WORLDSPACEREFLECTIONVECTOR"
    and "WORLDSPACENORMAL" texture generation modes', 'kambi_vrml_extensions',
    'section_ext_tex_coord_worldspace'); ?>.</p>

    <p>TODO: not implemented modes: "SPHERE-LOCAL", "NOISE", "NOISE-EYE",
    "SPHERE-REFLECT", "SPHERE-REFLECT-LOCAL".

  <li><p><tt>TextureProperties</tt>

    <p><tt>minificationFilter</tt>, <tt>magnificationFilter</tt>,
    <tt>anisotropicDegree</tt> are supported. <i>TODO</i>: rest is not.
</ul>

<?php
  x3d_status_footer();
?>
