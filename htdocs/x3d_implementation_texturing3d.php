<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Texturing3D', 'texture3D',
    'This component defines nodes for using 3D textures.
     3D textures often allow a simple coordinate mapping,
     and enable a variety of graphic effects,
     particularly since they may be utilized by <i>Programmable shaders</i>
     component shaders.
     3D textures may be loaded from image files or inlined in the VRML/X3D file.
     3D texture coordinates may be specified explicitly or
     automatically generated, and they may be transformed.'
  );
?>

<p>Supported nodes:</p>

<ul>
  <li><p><?php echo x3d_node_link('TextureTransformMatrix3D'); ?>,<br>
      <?php echo x3d_node_link('TextureTransform3D'); ?>,<br>

      <?php echo x3d_node_link('TextureCoordinate3D'); ?>,<br>
      <?php echo x3d_node_link('TextureCoordinate4D'); ?>,<br>

      <?php echo x3d_node_link('ImageTexture3D'); ?>,<br>
      <?php echo x3d_node_link('ComposedTexture3D'); ?>,<br>
      <?php echo x3d_node_link('PixelTexture3D'); ?></p>

    <p>3D textures, coordinates for 3D textures, transforming
    coordinates for 3D textures &mdash; all supported.</p>

    <p>Note that 3D and 4D (homogeneous) coordinates, and transformations
    in 3D space / by 4x4 matrix, may be used to transform 2D textures as well.
    In case of 2D textures, the 3rd component is just ignored
    and the 4th is normal divisor (as usual for homogeneous coordinates).</p>

    <p><a href="dds">DDS</a> and <a href="ktx">KTX</a> file formats
    to specify 3D (volume)
    textures are supported by <code>ImageTexture3D</code>.</p>

    <p>Note that <code>PixelTexture3D</code> with RGBA values has a
    problematic behavior because it uses <code>MFInt32</code> field for colors.
    When you encode RGBA colors (following <code>SFImage</code> specification),
    the most significant byte of Int32 may have to be non-zero,
    which means that you will have to write negative values
    inside <code>PixelTexture3D.image</code>. (Of course we handle it correctly,
    I'm just signalling there's a strangeness here. For normal
    <code>SFImage</code> fields this problem doesn't exist because our lexer
    can actually understand integer values outside of int32 range, so when parsing
    SFImage this is handled Ok, without going through int32.)</p>

    <p>Automatic 3D texture coord generation for primitives (<code>Box</code>,
    <code>Sphere</code>, <code>Cone</code>, <code>Cylinder</code>) is done
    (according to X3D spec, <i>33.2.4 Texture coordinate generation for primitive objects</i>).
</ul>

<?php
  x3d_status_footer();
?>
