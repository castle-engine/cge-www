<?php
  require_once 'x3d_implementation_common.php';
  require_once 'x3d_extensions_functions.php';
  x3d_extensions_header('Shape', 'shape', 'shape',
    'Extensions introduced in <a href="' . CURRENT_URL . '">Castle Game Engine</a> related to shapes.');

  $toc = new TableOfContents(
    array(
      new TocItem('Toggle shape rendering (<code>Shape.render</code>)', 'ext_shape_render'),
      new TocItem('Specify shading, e.g. to force Phong shading or wireframe for a shape (<code>Shape.shading</code>)', 'ext_shading'),
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
by code, and "silently" wrap the Shape in a Switch at loading - - - but this
is still some work, and it uses non-zero memory to add an extra layer in X3D graph.
Using this field is just simpler.

It's also ultra-fast, hiding / showing the shape has zero cost.
Although toggling Switch node is also ultra-fast.
-->

<p>This is compatible with <a href="http://doc.instantreality.org/documentation/nodetype/Shape/">InstantReality Shape extension</a>.

<?php echo $toc->html_section(); ?>


<p>We add a <code>shading</code> field to the <code>Shape</code> node
(more precisely, to the abstract <code>X3DShapeNode</code>):</p>

<?php echo node_begin("X3DShapeNode (e.g. Shape)");

  echo
  node_dots('all normal X3DShapeNode fields') .
  node_field('SFString', '[in,out]', "shading", '"DEFAULT"', '["DEFAULT"|"GOURAUD"|"PHONG"|"WIREFRAME"]') .
  node_end();
?>

<p>The allowed values for the <code>shading</code> field:</p>

<ul>
  <li><p><code>DEFAULT</code>: use the default shading.
    In <?php echo a_href_page("view3dscene", "view3dscene") ?>
    you control this using the <i>"View -&gt; Phong Shading on Everything"</i> checkbox.
    In your own games you control this using the
    <code>Scene.Attributes.PhongShading</code> property in Pascal code.
  </li>

  <li><p><code>GOURAUD</code>: fast per-vertex lighting calculation.
    It is the default shading for now.

    <p>Explicitly specifying the <code>"GOURAUD"</code> indicates that this shape wants to use Gouraud shading, even if the default scene shading is Phong. Note that some features (like bump mapping and shadow maps) will override this and require Phong shading anyway, since it's impossible to realize them with Gouraud shading.

    <p>Note that the <code>"GOURAUD"</code> shading performs only one-sided lighting in the shader pipeline. This means that only one face side receives lighting. TODO: For now, this is always the CCW side, regardless of the front face indicated by the "ccw" field.

  <li><p><code>PHONG</code>: pretty per-pixel lighting calculation.
    This also means always using shader pipeline to render this shape.
    This also performs two-sided lighting.
  </li>

  <li><p><code>WIREFFRAME</code>: render as a wireframe.
    The rendering model matches the <code>IndexedLineSet</code> specification,
    in particular: the shape is not lit.

    <p>For now this is only honored by the <code>Box</code>, <code>Sphere</code> nodes.
    It will be extended to all geometry nodes when necessary in the future.
  </li>
</ul>

<p>These shading names are consistent with <a href="<?php echo x3d_spec_latest_url('networking'); ?>#t-BrowserProperties">"Browser options" in X3D spec</a>
(with <code>DEFAULT</code> added by us).</p>

<?php
  x3d_status_footer();
?>