<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Environmental effects', 'enveffects',
    'This component defines special rendering effects.
     <code>Background</code> defines the sky colors and textures.
     <code>Fog</code> and <code>LocalFog</code> simulate natural fog.'
  );
?>

<p>This component defines nodes that change the environment ("how all the scene content looks").
In particular it deals with <i>fog</i> and <i>background</i> features.

<p>Moreover, see also <?php echo a_href_page('Castle Game Engine (and view3dscene) extensions related to environment effects', 'x3d_implementation_environmentaleffects_extensions'); ?>.

<p>Supported nodes:</p>

<ul>
  <li><p><?php echo x3d_node_link('Background'); ?>

    <p>Display a 3D background (visible around the scene content)
    using a textured cube (skybox) around,
    or a gradient of colors.

  <li><p><?php echo x3d_node_link('TextureBackground'); ?>

    <p>Similar to <code>Background</code> but allows to specify
    each skybox side as a texture node.
    This allows e.g. using <code>MovieTexture</code> to animate the background
    like a movie.

  <li><p><?php echo x3d_node_link('Fog'); ?>

    <p>Specifies global fog, making objects that are more distant blend to a constant
    fog color.

  <li><p><?php echo x3d_node_link('LocalFog'); ?>

    <p>Similar to <code>Fog</code>, but the effect is limited to a group of X3D shapes.

  <li><p><?php echo x3d_node_link('FogCoordinate'); ?>

    <p>Can be used within <a href="x3d_implementation_geometry3d.php">various geometry nodes,
    like <code>IndexedFaceSet</code></a>,
    to provide explicit fog densities for each point
    (instead of calculating fog density based on the distance to observer).
    This allows for various for effects, e.g. a fog may be concentrated at some
    3D position (regardless of the observer).

    <p>Note that depths (in <code>FogCoordinate.depth</code>) are sensible
    only in the [0, 1] range (like allowed by the specification),
    and are not affected by the <code>Fog.visibilityRange</code>
    (also following the spec). Effectively, this means that <code>FogCoordinate.depth</code>
    does <i>not</i> substitute the mere distance to the eye in the fog equation.
    Instead, it specifies directly the fog intensity.
    I understand that this is the intention of the spec, although the wording
    is clumsy (not saying directly what "implicit depth" is &mdash;
    fog input distance or fog output intensity).</p>
</ul>

<?php
  x3d_status_footer();
?>
