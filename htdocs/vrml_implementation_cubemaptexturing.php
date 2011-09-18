<?php
  require_once 'vrml_implementation_common.php';
  x3d_status_header('Cube map environmental texturing', 'env_texture',
    'This component defines nodes for using cube map textures.
     Such textures generate a color based on a direction.
     They are one of the common methods for environment mapping
     (that is, simulation of mirror-like surfaces).
     <tt>ComposedCubeMapTexture</tt> and <tt>ImageCubeMapTexture</tt>
     allow loading a cube map texture from file(s).
     <tt>GeneratedCubeMapTexture</tt> allows to create and use
     an environment map capturing actual environment in your virtual 3D world,
     thus making true realtime mirror.');

echo castle_thumbs(array(
  array('filename' => 'water_reflections.png', 'titlealt' => 'Water reflections by optimized GeneratedCubeMapTexture'),
  array('filename' => 'cubemap_teapot.png', 'titlealt' => 'Teapot with cube map reflections'),
));

  $toc = new TableOfContents(
    array(
      new TocItem('Demos', 'demos'),
      new TocItem('Tutorial: How to make a mirror', 'example'),
      new TocItem('Supported nodes', 'support'),
    ));
  $toc->echo_numbers = true;
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>For demos and tests of these features,
see the <tt>cube_environment_mapping</tt> subdirectory inside <?php
echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>.</p>

<?php echo $toc->html_section(); ?>

<p>The <tt>GeneratedCubeMapTexture</tt> node is a ready solution
to simulate mirror-like surfaces. It should be coupled with
<?php echo a_href_page_hashlink('texture coordinates to reflect in world space',
'kambi_vrml_extensions', 'section_ext_tex_coord_worldspace'); ?>
 to produce a mirror effect in a <i>really easy way</i>.</p>

<ol>
  <li><p>Choose a shape in your VRML/X3D model that should act like a mirror.</p></li>
  <li><p>As it's texture set <tt>GeneratedCubeMapTexture</tt> node.</p>

    <p>This texture will be automatically generated to represent
    the environment around the given shape (the shape itself
    is not rendered into this texture, it doesn't mirror itself).
    Set <tt>GeneratedCubeMapTexture.update</tt> to specify when this texture
    should be generated. Two sensible values are <tt>ALWAYS</tt>
    (if the world around is dynamic) or <tt>NEXT_FRAME_ONLY</tt>
    (if the world around is completely static).</p>

    <p>The texture is actually kept as six square 2D textures inside
    the graphic card. You can control the size of these squares
    by <tt>GeneratedCubeMapTexture.size</tt>: larger values
    mean better quality, but also worse speed.</p>

    <p>Note that if your shape already uses some texture,
    you will have to convert it's textures into <tt>MultiTexture</tt>
    node, with the 1st item containing the old texture,
    and the 2nd item containing the new <tt>GeneratedCubeMapTexture</tt>.
    See <?php echo a_href_page('"Texturing" component',
    'vrml_implementation_texturing'); ?> for multi-texture documentation.</p></li>

  <li><p>As the texture coordinates set <tt>TextureCoordinateGenerator</tt>
    node with <tt>mode</tt> field set to <tt>WORLDSPACEREFLECTIONVECTOR</tt>.</p>

    <p>Note that if your shape already uses some texture coordinates,
    you will have to convert them into <tt>MultiTextureCoordinate</tt>,
    see notes above about <tt>MultiTexture</tt>.</p>

    <p>Note that in our engine all 3D geometry nodes have the <tt>texCoord</tt>
    field, so you can do this really with every shape.
    Even with the primitives
    like  <?php echo a_href_page_hashlink('Box / Cone / Cylinder / Sphere',
    'kambi_vrml_extensions', 'section_ext_tex_coord'); ?>.</p>
  </li>
</ol>

<p>As an example, consider this teapot, with bold text to emphasize
the mirror stuff:</p>

<pre class="vrml_code">
Shape {
  appearance Appearance {
    material Material { }
    <b>texture GeneratedCubeMapTexture {
      update "ALWAYS"
      size 512
    }</b>
  }
  geometry Teapot {
    <b>texCoord TextureCoordinateGenerator { mode "WORLDSPACEREFLECTIONVECTOR" }</b>
  }
}
</pre>

<p>Place this in some interesting environment to see the nice mirror :)
<!--
 (otherwise, the mirror
will just reflect the background color, which isn't really impressive :)
-->
This approach works best for curvy
surfaces (perfectly flat surfaces usually look bad unless you use
really large size), and only if the mirror object is small compared
to the surrounding enviroment (as there are are no self-reflections).</p>

<!--p>(Note that
< ?php echo a_href_page_hashlink('<tt>Teapot</tt> node is our extension (compatible with InstantReality)',
'kambi_vrml_extensions', 'section_ext_teapot'); ? >)</p-->

<?php echo $toc->html_section(); ?>

<ul>
  <li><?php echo x3d_node_link('ComposedCubeMapTexture'); ?>

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

  <li><?php echo x3d_node_link('ImageCubeMapTexture'); ?>

    <p><?php echo a_href_page_hashlink('DDS file format', 'vrml_implementation_texturing',
    'section_dds'); ?> to specify cube maps
    (including S3TC compressed cube maps) is supported.</p></li>

  <li><?php echo x3d_node_link('GeneratedCubeMapTexture'); ?>

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
    You can see recursive mirrors in our <?php
    echo a_href_page('VRML/X3D demo models', 'demo_models'); ?>
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
