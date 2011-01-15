<?php
  require_once 'vrml_implementation_common.php';
  x3d_status_header('Environmental effects', 'enveffects',
    'This component defines special rendering effects.
     <tt>Background</tt> defines the sky colors and textures.
     <tt>Fog</tt> simulates natural fog.'
  );
?>

<p>Supported nodes:</p>

<ul>
  <li><p><?php echo x3d_node_link('Background'); ?>,
    <?php echo x3d_node_link('Fog'); ?></p></li>

  <li><p><?php echo x3d_node_link('FogCoordinate'); ?> is also supported.

    <p>Note that depths (in <tt>FogCoordinate.depth</tt>) are sensible
    only in the [0, 1] range (like allowed by the specification),
    and are not affected by the <tt>Fog.visibilityRange</tt>
    (also following the spec). Effectively, this means that <tt>FogCoordinate.depth</tt>
    does <i>not</i> substitute the mere distance to the eye in the fog equation.
    Instead, it specifies directly the fog intensity.
    I understand that this is the intention of the spec, although the wording
    is clumsy (not saying directly what "implicit depth" is &mdash;
    fog input distance or fog output intensity).</p>
</ul>

<p><i>TODO</i>: LocalFog not supported. TextureBackground not supported (waits on Background rendering rewrite &mdash; our <tt>Background</tt> rendering is quite specialized to VRML 97 background case, and it doesn't use our texture cache. This prevents from adapting it easily to use normal texture nodes.).</p>

<?php
  x3d_status_footer();
?>
