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
</ul>

<p><i>TODO</i>: FogCoordinate, LocalFog not supported. TextureBackground not supported (waits on Background rendering rewrite &mdash; our <tt>Background</tt> rendering is quite specialized to VRML 97 background case, and it doesn't use our texture cache. This prevents from adapting it easily to use normal texture nodes.).</p>

<?php
  x3d_status_footer();
?>
