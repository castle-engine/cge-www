<?php
  define('X3D_COMPONENT_NAME', 'Environmental effects');
  require_once 'vrml_implementation_common.php';
  x3d_status_header();
?>

<p>Supported nodes:</p>

<ul>
  <li><p><tt>Background</tt>, <tt>Fog</tt></p></li>
</ul>

<p><i>TODO</i>: FogCoordinate, LocalFog not supported. TextureBackground not supported (waits on Background rendering rewrite &mdash; our <tt>Background</tt> rendering is quite specialized to VRML 97 background case, and it doesn't use our texture cache. This prevents from adapting it easily to use normal texture nodes.).</p>

<?php
  x3d_status_footer();
?>
