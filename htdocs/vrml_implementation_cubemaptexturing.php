<?php
  define('X3D_COMPONENT_NAME', 'Cube map environmental texturing');
  require_once 'vrml_implementation_common.php';
  x3d_status_header();
?>

<p>Supported nodes:</p>

<ul>
  <li><tt>ComposedCubeMapTexture</tt>

    <p><i>Orientation notes:</i>
    The images are expected to be oriented just like for
    the VRML/X3D Background node. This is suggested by the drawing in the spec,
    although the spec doesn't specify exact orientation of the images.
    We use Background node orientation, as this is definitely sensible.
    See Background node spec, paragraph with words
    "<i>... when viewed from the origin looking down the negative Z-axis ...</i>".</p>

    <p><i>Size notes:</i>
    Texture size for cube maps is automatically adjusted to be power of two,
    square, and within OpenGL limits (GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB).
    So your textures do not necessarily have to be provided
    with required sizes (if you don't mind a little quality loss because
    of scaling).</p>

    <p>You still must provide equally sized images
    for all six cube map sides. Our engine makes sure to scale them
    to be square and power-of-two, but we currently do not attempt
    to make all six textures equal &mdash; so you have to provide textures
    already satisfying this.

    <p>We add <tt>textureProperties</tt> field to the <tt>ComposedCubeMapTexture</tt>
    node, intended for <tt>TextureProperties</tt> child, working just like
    in other texture nodes (you can use it to set minification / magnification
    filter, anisotropy and such). Although X3D 3.2 specification doesn't mention this,
    it seems natural, and <a href="http://www.instantreality.org/documentation/nodetype/ComposedCubeMapTexture/">instantreality
    also uses this</a>.
    We support for cube maps all normal texture filterings, including mipmaps.</p></li>

  <li><tt>ImageCubeMapTexture</tt>

    <p><?php echo a_href_page_hashlink('DDS file format', 'vrml_implementation_status',
    'section_dds'); ?> to specify cube maps
    (including S3TC compressed cube maps) is supported.</p></li>

  <li><tt>GeneratedCubeMapTexture</tt>

    <p>Texture is rendered from the middle 3D point of bounding box
    of the shape using this texture. You cannot reUSE the same <tt>GeneratedCubeMapTexture</tt>
    node for various shapes (as then we would not know from which shape
    to generate).</p>

    <p>The texture before generation (e.g. if you have update = 'NONE'
    at the start) has pink color (RGB(255, 0, 255)), so you can easily
    recognize it.</p>

    <p>All the generated textures are rendered in a separate
    pass before actual rendering, and during this generation other shapes
    use existing values of their textures. This means that recursive mirrors,
    i.e. mirror that can be seen in another mirror, works to any level
    (each frame rendered uses textures generated in the previous frame).
    You can see recursive mirrors in some examples in <?php
    echo a_href_page('Kambi VRML test suite', 'kambi_vrml_test_suite'); ?>
    (see <tt>x3d/cubemap_generated_recursive.x3dv</tt>
    <tt>x3d/cubemap_generated_in_dynamic_world.x3dv</tt>).</p>

    <p>Provided <tt>size</tt> will automatically be adjusted to be power of two,
    and within OpenGL limits (GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB).</p>

    <p>Current player camera doesn't affect how cube map is generated.
    This is good, it means that generated texture is usable for as long
    as the scene remains unchanged, and doesn't have to be regenerated
    each time when the player moves.</p>

    <ul>
      <li><p>When <tt>update = "ALWAYS"</tt>, this optimization is automatically
        used under the hood. Texture is internally not updated every frame
        &mdash; when we know nothing visible changed on the scene, we do
        not regenerate the texture (since it would be generated the same).
        Note that using the headlight, or any other geometry/light following
        the player, makes this optimization less effective (as then every
        camera move changes the look of the scene, so rendered textures
        have to be regenerated on every camera move).</p></li>

      <li><p>This also means that generated cube map texture
        is similar to static (from <tt>ImageCubeMapTexture</tt>
        and <tt>ComposedCubeMapTexture</tt>), and you usually want to
        use <?php echo a_href_page_hashlink('"WORLDSPACEREFLECTIONVECTOR"
        texture generation', 'kambi_vrml_extensions',
        'section_ext_tex_coord_worldspace'); ?> to simulate mirror.
        When using cube maps with GLSL shaders, this often forces the need to
        transform directions from eye-space to world-space,
        you can obtain appropriate matrix easily by
        <?php echo a_href_page_hashlink('Viewpoint.cameraRotationInverseMatrix output event', 'kambi_vrml_extensions',
        'section_ext_viewpoint_camera_matrix'); ?>.</p></li>
    </ul>
  </li>
</ul>

<?php
  x3d_status_footer();
?>
