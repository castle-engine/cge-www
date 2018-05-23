<?php
  require_once 'x3d_implementation_common.php';
  require_once 'x3d_extensions_functions.php';
  x3d_extensions_header('Text', 'text', 'text',
    'Extensions introduced in <a href="' . page_url('index') . '">Castle Game Engine</a> related to text.');

  $toc = new TableOfContents(
    array(
      new TocItem('Text transparency mode (<code>FontStyle.blending</code>)', 'ext_fontstyle_blending'),
      new TocItem('DEPRECATED: 3D text (node <code>Text3D</code>)', 'ext_text3d'),
    ));
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<?php
  echo node_begin('FontStyle') .
  node_dots() .
  node_field('SFBool', '[]', 'blending', 'TRUE') .
  node_end();
?>

<p>X3D text is rendered using transparent textures.
This allows very efficient rendering on all possible 3D devices.
But it also means that you have the usual choice whether to use
"alpha testing" transparency ("all or nothing",
when the <code>FontStyle.blending</code>
is <code>FALSE</code>) or smooth blending (default, when
the <code>FontStyle.blending</code> is <code>TRUE</code>).
Each one has it's benefits and drawbacks:

<ol>
  <li><p>With alpha blending, glyph borders looks smooth (currenly our alpha test will make weirdly jagged letter borders).</p></li>

  <li><p>With alpha testing, you do not have to worry about the order of blending. You will never see any artifacts. With alpha blending, you may need to <?php echo a_href_page_hashlink('set NavigationInfo.blendingSort field to "3D"', 'x3d_implementation_navigation_extensions', 'section_ext_blending_sort'); ?>.</p></li>

  <li><p>With alpha testing, the text casts correct shadows using shadow maps.</p></li>
</ol>

<p>This is available for all <code>FontStyle</code> node versions
(VRML 1.0 and VRML 2.0 / X3D).

<?php echo $toc->html_section(); ?>

<p><i>Since version 5.1.0 of Castle Game Engine (corresponding
to version 3.15.0 of view3dscene), this node is deprecated,
and it is rendered only as a flat <code>Text</code> node.
<!--
Our new text rendering method (using texture) offered new features
and optimizations, but unfortunately it's impossible to render
true 3D text using it.
-->
If you need 3D text, use a 3D modelling software, like
<a href="http://www.blender.org/">Blender</a>, to create 3D mesh
for text.</i></p>

<p>We add new node:

<?php
  echo node_begin('Text3D : X3DGeometryNode');
  echo
  node_field('MFString', '[in,out]', 'string', '[]') .
  node_field('SFNode', '[in,out]', 'fontStyle', 'NULL') .
  node_field('MFFloat', '[in,out]', 'length', '[]') .
  node_field('SFFloat', '[in,out]', 'maxExtent', '0') .
  node_field('SFFloat', '[in,out]', 'depth', '0.1', 'must be &gt;= 0') .
  node_field('SFBool', '[in,out]', 'solid', 'TRUE') .
  node_end();
?>

<p>This renders the text, pretty much like <code>Text</code> node from
VRML 97 (see VRML 97 specification about <code>string</code>, <code>fontStyle</code>,
<code>length</code>, <code>maxExtent</code> fields). But the text is 3D:
it's "pushed" by the amount <code>depth</code> into negative Z. The normal
text is on Z = 0, the 3D text had front cap on Z = 0, back cap on Z = -Depth,
and of course the extrusion (sides).</p>

<p>Also, it's natural to apply backface culling to such text, so we have
a <code>solid</code> field. When true (default), then backface culling is done.
This may provide much speedup, unless camera is able to enter
"inside" the text geometry (in which case solid should be set to <code>FALSE</code>).</p>

<p>If <code>depth</code> is zero, then normal 2D text is rendered.
However, backface culling may still be applied (if <code>solid</code> is true)
&mdash; so this node also allows you to make 2D text that's supposed to be
visible from only front side.</p>

<p>See our <?php echo a_href_page('VRML/X3D demo models',
'demo_models'); ?>, file <code>text/text_depth.wrl</code> for example use of this.</p>

<p>Compatibility:

<ul>
  <li>You should specify external prototype before using this node:

        <pre>
EXTERNPROTO Text3D [
  exposedField MFString string
  exposedField SFNode fontStyle
  exposedField MFFloat length
  exposedField SFFloat maxExtent
  exposedField SFFloat depth
  exposedField SFBool solid
] [ "urn:castle-engine.io:node:Text3D",
    "https://castle-engine.io/fallback_prototypes.wrl#Text3D" ]
</pre>

    <p>This way other VRML browsers should be able to
    render Text3D node like normal 2D Text.</p></li>

  <li>This is somewhat compatible to <a href="http://www.parallelgraphics.com/developer/products/cortona/extensions/text3d/">Text3D
    node from Parallel Graphics</a>. At the beginning I implemented this
    extension differently (<code>kambiDepth</code>, <code>kambiSolid</code> fields
    for <code>AsciiText</code> and <code>Text</code> nodes). But later I found
    these Parallel Graphics <code>Text3D</code> definition, so I decided
    to make my version compatible.</li>
</ul>

<?php
  x3d_status_footer();
?>