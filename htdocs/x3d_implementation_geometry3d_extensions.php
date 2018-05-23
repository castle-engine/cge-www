<?php
  require_once 'x3d_implementation_common.php';
  require_once 'x3d_extensions_functions.php';
  x3d_extensions_header('Geometry3D', 'geometry3d', 'geometry3D',
    'Extensions introduced in <a href="' . page_url('index') . '">Castle Game Engine</a> related to 3D geometry.');

  $toc = new TableOfContents(
    array(
      new TocItem('Triangulation of primitives (<code>Sphere</code>, <code>Cone</code>, <code>Cylinder</code>, <code>Box</code>, <code>Circle2D</code>)', 'triangulation'),
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

<p>We add some fields to primitive geometry nodes to control their triangulation:

<?php echo node_begin("Box");
  echo
  node_dots('') .
  node_field('SFInt32', '[in,out]', "divisions", "-1", "[-1, infinity)") .
  node_end();
?>

<?php echo node_begin("Cone");
  echo
  node_dots('') .
  node_field('SFInt32', '[in,out]', "slices", "-1", "{-1} + [3, infinity)") .
  node_field('SFInt32', '[in,out]', "stacks", "-1", "{-1} + [2, infinity)") .
  node_end();
?>

<?php echo node_begin("Cylinder");
  echo
  node_dots('') .
  node_field('SFInt32', '[in,out]', "slices", "-1", "{-1} + [3, infinity)") .
  node_field('SFInt32', '[in,out]', "stacks", "-1", "{-1} + [2, infinity)") .
  node_end();
?>

<?php echo node_begin("Sphere");
  echo
  node_dots('') .
  node_field('SFInt32', '[in,out]', "slices", "-1", "{-1} + [3, infinity)") .
  node_field('SFInt32', '[in,out]', "stacks", "-1", "{-1} + [2, infinity)") .
  node_end();
?>

<?php echo node_begin("Circle2D");
  echo
  node_dots('') .
  node_field('SFInt32', '[in,out]', "slices", "-1", "{-1} + [3, infinity)") .
  node_end();
?>

<p>For spheres, cones and cylinders you can set
how many <i>slices</i> (like slices of a pizza)
and how many <i>stacks</i> (like stacks of a tower) to create.
For boxes, you can set how to triangulate faces of cubes.

<p>You can easily test the effects of these fields
by investigating your models in <?php echo a_href_page("view3dscene", "view3dscene") ?>.
You can view in <i>Wireframe</i> mode to see the triangulation.
You can try it on our test file:
see <?php echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>, file
<code>x3d/castle_extensions/triangulation.x3dv</code>.

<p>Note that our programs do two different variants of triangulation,
and they automatically decide which variant to use in each case:

<ol>
  <li>Triangulation for collision detection and ray-tracer.
    This is intended to approximate quadrics as triangle meshes.

  <li>Triangulation for <i>Gouraud</i> shading.
    We call this <i>over-triangulation</i>. To improve the shading,
    it's worth triangulating a little more, even the flat faces.
    This is used when rendering models.

    <p>In this variant we do a little more triangulation.
    In particular, we divide cones and cylinders into stacks,
    and we divide cube faces. For collision detection,
    this additional triangulation is useless (as it doesn't give
    any better approximation of an object). But it improves
    how objects look with Gouraud shading.
</ol>

<p>Special value -1 for any of these fields means
that we use some default, sensible value.

<p>
<!--Note that this node gives only a <i>hints</i> to the renderer.
Various algorithms and programs may realize triangulation differently,
and then hints given by this node may be interpreted somewhat
differently or just ignored.
-->
<!-- np. program może ustalać jakość triangulacji w zależności
od odległości obiektu od patrzącego i wtedy zaimplementowanie
obsługi tego węzła wiązałoby się z dodatkowymi komplikacjami -->
<!--That said, this node is useful when you're designing some
VRML models and you want to fine-tune the compromise
between OpenGL rendering speed and quality of some objects.
-->
Generally, triangulate more if the object is large or you
want to see light effects (like light spot) looking good.
If the object is small you can triangulate less, to get
better rendering time.

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