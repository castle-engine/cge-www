<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Environmental effects', 'enveffects',
    'This component defines special rendering effects.
     <code>Background</code> defines the sky colors and textures.
     <code>Fog</code> and <code>LocalFog</code> simulate natural fog.'
  );
?>

<p>Supported nodes:</p>

<ul>
  <li><p><?php echo x3d_node_link('Background'); ?>,<br>
    <?php echo x3d_node_link('TextureBackground'); ?>,<br>
    <?php echo x3d_node_link('Fog'); ?>,<br>
    <?php echo x3d_node_link('LocalFog'); ?></p></li>

  <li><p><?php echo x3d_node_link('FogCoordinate'); ?> is also supported.

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
