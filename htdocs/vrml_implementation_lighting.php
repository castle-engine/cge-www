<?php
  require_once 'vrml_implementation_common.php';
  x3d_status_header('Lighting', 'lighting',
    'This component defines light source nodes.');
?>

<p>Supported nodes:</p>

<ul>
  <li><?php echo x3d_node_link('DirectionalLight'); ?>,
    <?php echo x3d_node_link('PointLight'); ?>,
    <?php echo x3d_node_link('SpotLight'); ?>

    <p><i>Note</i>: VRML 2.0 <tt>SpotLight.beamWidth</tt>
    idea cannot be translated to a standard
    OpenGL spotlight, so if you set beamWidth &lt; cutOffAngle then the light
    will not look exactly VRML 2.0-spec compliant.
    Honestly I don't see any sensible way to fix this
    (as long as we talk about real-time rendering using OpenGL).
    And other open-source VRML implementations rendering to OpenGL
    also don't seem to do anything better.

    <p>VRML 2.0 spec requires that at least 8 lights
    are supported. My units can support as many lights as are
    allowed by your OpenGL implementation, which is <i>at least</i> 8.

    <p><i>TODO</i>: <tt>global</tt> field from X3D is not supported for now. We handle this following VRML 97 spec, which means that the default <tt>global</tt> value is always used: directional lights are never global, positional/spot ligthts are always global.</p>
</ul>

<?php
  x3d_status_footer();
?>
