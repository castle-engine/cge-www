<?php
  require_once 'x3d_implementation_common.php';
  require_once 'x3d_extensions_functions.php';
  x3d_extensions_header('Rendering', 'rendering', 'rendering',
    'Extensions introduced in <a href="' . page_url('index') . '">Castle Game Engine</a> related to rendering.');

$toc = new TableOfContents(
  array(
    new TocItem('Explicit tangent vectors'),
  ));
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>We add a new node:

<?php
  echo node_begin('Tangent : X3DGeometricPropertyNode') .
  node_field('MFVec3f', '[in,out]', 'vector', '[]') .
  node_end();
?>

<p>This node can be used within the new field <code>tangent</code> of <code>X3DComposedGeometryNode</code>:

<?php
  echo node_begin('X3DComposedGeometryNode') .
  node_dots() .
  node_field('SFNode', '[in,out]', 'tangent', 'NULL') .
  node_end();
?>

<p>If specified, this node holds tangent vectors information, necessary for bump mapping.
The <i>vector</i> field should contain normalized tangent vectors, in the right-handed coordinate system.

<p>The ordering of the tangent vectors is exactly the same as ordering of normal vectors would be,
so e.g. <i>X3DComposedGeometryNode.NormalPerVertex</i> affects the interpretation of tangent vectors as well.

<p>When reading data from <a href="https://castle-engine.io/creating_data_model_formats.php#section_gltf">glTF</a>,
we automatically import glTF information about tangent vectors into this node.

<?php
  x3d_status_footer();
?>
