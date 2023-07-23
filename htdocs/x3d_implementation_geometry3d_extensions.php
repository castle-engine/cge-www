<?php
  require_once 'x3d_implementation_common.php';
  require_once 'x3d_extensions_functions.php';
  x3d_extensions_header('Geometry3D', 'geometry3d', 'geometry3D',
    'Extensions introduced in <a href="' . page_url('index') . '">Castle Game Engine</a> related to 3D geometry.');

  $toc = new TableOfContents(
    array(
      new TocItem('Triangulation of primitives (<code>Cone</code>, <code>Cylinder</code>, <code>Sphere</code>, <code>Circle2D</code>, <code>Disk2D</code>)', 'triangulation'),
      new TocItem('Specify orientation of primitives (<code>Box.ccw</code>)', 'ccw'),
    ));
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<?php
echo castle_thumbs(array(
  array('filename' => "kambi_triangulation_demo.png", 'titlealt' => 'Triangulation demo screenshot'),
));
?>

<p>You can control the triangulation of various geometry nodes:

<ul>
  <li>
    <p>
    <?php echo node_begin("Cone");
      echo
      node_dots('') .
      node_field('SFInt32', '[in,out]', "slices", "-1", "{-1} + [3, infinity)") .
      node_field('SFInt32', '[in,out]', "stacks", "-1", "{-1} + [2, infinity)") .
      node_end();
    ?>

    <p>In Pascal see <?php echo cgeRef('TConeNode'); ?>.

  <li>
    <p>
    <?php echo node_begin("Cylinder");
      echo
      node_dots('') .
      node_field('SFInt32', '[in,out]', "slices", "-1", "{-1} + [3, infinity)") .
      node_field('SFInt32', '[in,out]', "stacks", "-1", "{-1} + [2, infinity)") .
      node_end();
    ?>

    <p>In Pascal see <?php echo cgeRef('TCylinderNode'); ?>.

  <li>
    <p>
    <?php echo node_begin("Sphere");
      echo
      node_dots('') .
      node_field('SFInt32', '[in,out]', "slices", "-1", "{-1} + [3, infinity)") .
      node_field('SFInt32', '[in,out]', "stacks", "-1", "{-1} + [2, infinity)") .
      node_end();
    ?>

    <p>In Pascal see <?php echo cgeRef('TSphereNode'); ?>.

  <li>
    <p>
    <?php echo node_begin("Circle2D");
      echo
      node_dots('') .
      node_field('SFInt32', '[in,out]', "slices", "-1", "{-1} + [3, infinity)") .
      node_end();
    ?>

    <p>In Pascal see <?php echo cgeRef('TCircle2DNode'); ?>.

  <li>
    <p>
    <?php echo node_begin("Disk2D");
      echo
      node_dots('') .
      node_field('SFInt32', '[in,out]', "slices", "-1", "{-1} + [3, infinity)") .
      node_end();
    ?>

    <p>In Pascal see <?php echo cgeRef('TDisk2DNode'); ?>.
</ul>

<p><i>Slices</i> divide the objects like <i>"slices of a pizza"</i>.

<p><i>Stacks</i> divide the objects like <i>"stacks of a tower"</i>.

<p>You can easily test the effects of these fields
by investigating your models in <?php echo a_href_page("view3dscene", "view3dscene") ?>.
You can view in <i>Wireframe</i> mode to see the triangulation.
You can try it on our <?php echo a_href_page('demo models', 'demo_models'); ?>,
for example

<ul>
  <li><a href="https://github.com/castle-engine/demo-models/blob/master/x3d/castle_extensions/triangulation.x3dv">x3d/castle_extensions/triangulation.x3dv</a>
  <li><a href="https://github.com/castle-engine/demo-models/blob/master/2d/2d_geometry_nodes.x3dv">2d/2d_geometry_nodes.x3dv</a>
</ul>

<p>Special value -1 for any of these fields means
that we use a default value specified by the global variables
 <?php echo cgeRef('DefaultTriangulationSlices') ?>,
 <?php echo cgeRef('DefaultTriangulationStacks') ?>.

<p>Generally, triangulate more if the object is large and you want to see
the geometry precisely.

<P>If you use lighting with <i>Gouraud shading</i> then also you need
a reasonable triangulation to see the light effects (like spotlight cone effect)
precisely. But by default in CGE we now use <i>Phong shading</i> which looks good
regardless of the triangulation.

<?php echo $toc->html_section(); ?>

<p>We add <code>Box.ccw</code> field.
The default <code>TRUE</code> means that box is visible only from the outside.
You can specify <code>FALSE</code> to make it visible only from the inside.
Note that you can also use <code>solid FALSE</code> to make it visible from both sides.

<?php echo node_begin("Box");
  echo
  node_dots('') .
  node_field('SFBool', '[]', 'ccw', 'TRUE') .
  node_end();
?>

<?php
  x3d_status_footer();
?>