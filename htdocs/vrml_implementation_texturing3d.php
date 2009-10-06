<?php
  define('X3D_COMPONENT_NAME', 'Texturing3D');
  require_once 'vrml_implementation_common.php';
  x3d_status_header();
?>

<p>Supported nodes:</p>

<ul>
  <li><p><tt>TextureTransformMatrix3D</tt>, <tt>TextureTransform3D</tt>,<br/>
      <tt>TextureCoordinate3D</tt>, <tt>TextureCoordinate4D</tt>,<br/>
      <tt>ImageTexture3D</tt>, <tt>ComposedTexture3D</tt>, <tt>PixelTexture3D</tt></p>

    <p>3D textures, coordinates for 3D textures, transforming
    coordinates for 3D textures &mdash; all done.</p>

    <p>Note that 3D and 4D (homogeneous) coordinates, and transformations
    in 3D space / by 4x4 matrix, may be used to transform 2D textures as well.
    In case of 2D textures, the 3rd component is just ignored
    and the 4th is normal divisor (as usual for homogeneous coordinates).</p>

    <p><?php echo a_href_page_hashlink('DDS file format', 'vrml_implementation_status',
    'section_dds'); ?> to specify 3d (volume)
    textures is supported by <tt>ImageTexture3D</tt>.</p>

    <p>Note that <tt>PixelTexture3D</tt> with RGBA values has a
    problematic behavior because it uses <tt>MFInt32</tt> field for colors.
    When you encode RGBA colors (following <tt>SFImage</tt> specification),
    the most significant byte of Int32 may have to be non-zero,
    which means that you will have to write negative values
    inside <tt>PixelTexture3D.image</tt>. (Of course we handle it correctly,
    I'm just signalling there's a strangeness here. For normal
    <tt>SFImage</tt> fields this problem doesn't exist because our lexer
    can actually understand integer values outside of int32 range, so when parsing
    SFImage this is handled Ok, without going through int32.)</p>

    <p>Automatic 3D texture coord generation for primitives (<tt>Box</tt>,
    <tt>Sphere</tt>, <tt>Cone</tt>, <tt>Cylinder</tt>) is done
    (according to X3D spec, <i>33.2.4 Texture coordinate generation for primitive objects</i>).
    TODO: Although it's reversed on the bottom disk of
    <tt>Cone</tt> and <tt>Cylinder</tt> for now.
</ul>

<?php
  x3d_status_footer();
?>
