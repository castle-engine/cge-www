<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Rendering', 'rendering',
    'This component defines the basic properties of geometric objects,
     and nodes to render triangles, lines and points.
     See <i>Geometry3D</i> component for more comfortable
     geometric objects like polygons and spheres.');
?>

<p>Supported nodes:</p>

<ul>
  <li><p><?php echo x3d_node_link('Coordinate'); ?>,
    <?php echo x3d_node_link('Color'); ?>,
    <?php echo x3d_node_link('ColorRGBA'); ?>,
    <?php echo x3d_node_link('Normal'); ?></p>

  <li><p>Triangles, lines and points:

    <p><?php echo x3d_node_link('IndexedTriangleSet'); ?>,
    <?php echo x3d_node_link('TriangleSet'); ?>,
    (also analogous <code>IndexedQuadSet</code>, <code>QuadSet</code>
    from <?php echo a_href_page('CAD geometry', 'x3d_implementation_cadgeometry'); ?> component).

    <p><?php echo x3d_node_link('IndexedTriangleFanSet'); ?>,
    <?php echo x3d_node_link('TriangleFanSet'); ?>,
    <?php echo x3d_node_link('IndexedTriangleStripSet'); ?>,
    <?php echo x3d_node_link('TriangleStripSet'); ?>

    <p><?php echo x3d_node_link('LineSet'); ?>,
    <?php echo x3d_node_link('IndexedLineSet'); ?>

    <p><?php echo x3d_node_link('PointSet'); ?>

    <p>Support includes the implementation of new X3D fields
    <code>attrib</code> (per-vertex attributes for shaders)
    and <code>fogCoord</code> (per-vertex fog depth).

    <p><i>TODO</i>: for <code>TriangleFanSet</code> and <code>TriangleStripSet</code>,
    a special constraint is present: if you will use colors
    (colors are always per-vertex on these primitives,
    according to X3D spec) and request generation of per-face normals
    at the same time, for the same lit (with material) node,
    then shading results will be slightly incorrect.
    Like this:

<pre class="vrml_code">
#X3D V3.0 utf8
PROFILE Interchange

Shape {
  appearance Appearance { material Material { } }
  geometry TriangleFanSet {
    coord Coordinate { point [ 0 0 0, 1 0 0, 1 1 0, 0.5 1.5 0.5 ] }
    fanCount 4
    color Color { color [ 1 0 0, 0 1 0, 0 0 1, 1 1 1 ] }
    normalPerVertex FALSE
  }
}
</pre>

    <p>Unfortunately, this is quite unfixable without falling back to
    worse rendering methods. Shading has to be smooth to interpolate
    per-vertex colors, and at the same time the same vertex may require
    different normals on a different faces. So to render this correctly one has
    to decompose triangle fans and strips into separate triangles
    (like to <code>IndexedTriangleSet</code>) which means that rendering is
    non-optimal.

    <p>Ideas how to implement this without sacrificing rendering time
    are welcome. Eventually, a fallback to internally convert fans and strips
    to <code>IndexedTriangleSet</code> in such special case will be
    implemented some day.

    <p><i>Note</i>: As far as I see, X3D specification doesn't specify what to do
    for triangle/quad sets when appearance specify a texture but no
    <code>texCoord</code> is given.
    Our engine currently takes the <code>IndexedFaceSet</code> approach for
    automatic generation of texture coords in this case, let me know
    if this is wrong for whatever reason.

  <li><p><?php echo x3d_node_link('ClipPlane'); ?></p>

    <p>OpenGL limits the number of clipping planes that may be enabled
    at the same time on a single shape. This limit is <i>at least six</i>
    (see view3dscene <i>"Help -&gt; OpenGL Information"</i>, look
    at <i>"Max clip planes"</i> line, to know the limit of your GPU).
    Following X3D spec, we treat the "more global" clipping planes
    as more important.</p>

    <p>Note that clipping planes are purely visual effect.
    The clipped geometry is still present for collision detection,
    picking, bounding volumes etc.</p>

    <p>TODO: clip planes don't work on background nodes
    (<code>X3DBackgroundNode</code>) yet.</p>
</ul>

<?php
  x3d_status_footer();
?>
