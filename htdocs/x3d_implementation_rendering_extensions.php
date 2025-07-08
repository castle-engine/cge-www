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
  node_field('MFVec4f', '[in,out]', 'vector', '[]') .
  node_end();
?>

<p>This node can be used within the new field <code>tangent</code> of <code>X3DComposedGeometryNode</code>:

<?php
  echo node_begin('X3DComposedGeometryNode') .
  node_dots() .
  node_field('SFNode', '[in,out]', 'tangent', 'NULL') .
  node_end();
?>

<p><b>This node is not an extension anymore, it was <a href="https://www.web3d.org/specifications/X3Dv4Draft/ISO-IEC19775-1v4.1-CD//Part01/components/rendering.html#Tangent">adopted in the X3D 4.1 specification</a> and is now part of the X3D standard.</b> We have <a href="https://github.com/michaliskambi/x3d-tests/wiki/Tangent-node-in-X3D">recommended it and helped with the prose</a> and we're happy with the end result!

<p><i>( To be precise, X3D 4.1 is only a draft spec version now. X3D 4.0 is the last officially approved X3D version, as of this writing, 2025-07. But, just like all previous X3D versions, sooner or later X3D 4.1 will be approved as well. )</i>

<p>When reading data from <a href="https://castle-engine.io/creating_data_model_formats.php#section_gltf">glTF</a>,
we automatically import glTF information about tangent vectors into this node.

<p>While not strictly necessary, it is advised to put tangent vectors in X3D file when doing bump mapping
(using <i>Material.normalTexture</i>, <i>PhysicalMaterial.normalTexture</i> fields).
This way the X3D browser knows the tangent vectors, with exactly the same values as were used
when generating ("baking") the normalmap texture.
This, in turn, allows 1. perfectly correct rendering, 2. faster loading and animating of X3D files with bump mapping
&mdash; as the tangent vectors don't have to be calculated.

<p>When the <code>Tangent</code> node is missing, but the relevant information is required
(e.g. for bump mapping) X3D browsers automatically calculate the tangent vectors.

<p><i>Backward compatibility break at 2025-07-08:</i> Our definition and implementation changed a bit on 2025-07-08: <code>vector</code> field changed from <code>MFVec3f</code> to <code>MFVec4f</code>. We did recommend this change ourselves (we proposed to add <code>MFVec4f</code> to X3D 4.1 spec, knowing it will imply a change in our extension), to be better aligned with glTF and X_ITE. The new 4th component is a sign value (-1 or +1) indicating handedness of the tangent basis. See the X3D 4.1 and glTF specifications for details.

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
  castle_footer();
?>
