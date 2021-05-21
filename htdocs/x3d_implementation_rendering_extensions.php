<?php
  require_once 'x3d_implementation_common.php';
  require_once 'x3d_extensions_functions.php';
  x3d_extensions_header('Rendering', 'rendering', 'rendering',
    'Extensions introduced in <a href="' . page_url('index') . '">Castle Game Engine</a> related to rendering.');

$toc = new TableOfContents(
  array(
    new TocItem('Explicit tangent vectors (<code>Tangent</code> node, <code>X3DComposedGeometryNode.tangent</code> field)', 'ext_tangent'),
    new TocItem('Mode to specify how per-vertex colors are applied (<code>X3DColorNode.mode</code>)', 'ext_color_mode'),
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

<?php echo $toc->html_section(); ?>

<p>We add a new field to <code>X3DColorNode</code> (ancestor of <code>Color</code> and <code>ColorRGBA</code>
that specify per-vertex colors):

<?php
  echo node_begin('X3DColorNode') .
  node_dots() .
  node_field('SFString', '[]', 'mode', '"REPLACE"', '["REPLACE","MODULATE"]') .
  node_end();
?>

<ul>
  <li><p><code>"REPLACE"</code> is the default, and is compatible with X3D 3.

  <li><p><code>"MODULATE"</code> means to multiply per-vertex colors (with the same value as was replaced by `"REPLACE"`, like <code>Material.diffuseColor</code> or <code>PhysicalMaterial.baseColor</code> or <code>UnlitMaterial.emissiveColor</code>, with alpha added from <code>XxxMaterial.transparency</code>). This allows to achieve glTF-compatible behavior.
</ul>

<?php
  x3d_status_footer();
?>
