<?php
  require_once 'x3d_implementation_common.php';
  require_once 'x3d_extensions_functions.php';
  x3d_extensions_header('Shape', 'shape', 'shape',
    'Extensions introduced in <a href="' . CURRENT_URL . '">Castle Game Engine</a> related to shapes.');

  $toc = new TableOfContents(
    array(
      new TocItem('Toggle shape rendering (<code>Shape.render</code>)', 'ext_shape_render'),
    ));
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<?php
  echo node_begin('Shape') .
  node_dots() .
  node_field('SFBool', '[in,out]', 'render', 'TRUE') .
  node_end();
?>

<p>The field <code>render</code> allows to easily hide and show
the given <code>Shape</code>. A <i>hidden shape</i> is not rendered, but otherwise
it's still processed (for example, it is used for collision detection).

<p>If you want to hide a particular shape, sometimes this may be a simpler method
than using standard X3D <code>Switch</code> node. It doesn't require to wrap
the <code>Shape</code> in a <code>Switch</code> node.
<!-- (which may be
cumbersome if you already have a generated X3D file, and you don't
want to process it to add <code>Switch</code> nodes).
-->

<!--
A similar effect (sans collisions0 can be achieved using a Switch node with Shape inside,
but sometimes using this field is just simpler, since you can avoid editing
the X3D file (which could be prepared by another tool, and editing them
could be unhandy). With our engine, you could always process the X3D graph
by code, and "silently" wrap the Shape in a Switch at loading --- but this
is still some work, and it uses non-zero memory to add an extra layer in X3D graph.
Using this field is just simpler.

It's also ultra-fast, hiding / showing the shape has zero cost.
Although toggling Switch node is also ultra-fast.
-->

<p>This is compatible with <a href="http://doc.instantreality.org/documentation/nodetype/Shape/">InstantReality Shape extension</a>.

<?php
  x3d_status_footer();
?>