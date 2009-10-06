<?php
  define('X3D_COMPONENT_NAME', 'Rendering');
  require_once 'vrml_implementation_common.php';
  x3d_status_header();
?>

<p>Supported nodes:</p>

<ul>
  <li><p><tt>PointSet</tt>, <tt>IndexedLineSet</tt>, <tt>Coordinate</tt>, <tt>Color</tt>, <tt>Normal</tt></p>

  <li><p>New X3D rendering primitives implemented:

    <p><tt>IndexedTriangleSet</tt>, <tt>TriangleSet</tt>,
    (also analogous <tt>IndexedQuadSet</tt>, <tt>QuadSet</tt>
    from <?php echo a_href_page('CAD geometry', 'vrml_implementation_cadgeometry'); ?> component).

    <p><tt>IndexedTriangleFanSet</tt>, <tt>TriangleFanSet</tt>,
    <tt>IndexedTriangleStripSet</tt>, <tt>TriangleStripSet</tt>

    <p><tt>LineSet</tt> (<tt>IndexedLineSet</tt> is also handled,
    this is part of VRML 2.0)

    <p><i>TODO</i>: missing is only the implementation of new X3D fields
    <tt>attrib</tt> and <tt>fogCoord</tt>.

    <p><i>TODO</i>: for <tt>TriangleFanSet</tt> and <tt>TriangleStripSet</tt>,
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
    (like to <tt>IndexedTriangleSet</tt>) which means that rendering is
    non-optimal.

    <p>Ideas how to implement this without sacrificing rendering time
    are welcome. Eventually, a fallback to internally convert fans and strips
    to <tt>IndexedTriangleSet</tt> in such special case will be
    implemented some day.

    <p><i>Note</i>: As far as I see, X3D specification doesn't specify what to do
    for triangle/quad sets when appearance specify a texture but no
    <tt>texCoord</tt> is given.
    Our engine currently takes the <tt>IndexedFaceSet</tt> approach for
    automatic generation of texture coords in this case, let me know
    if this is wrong for whatever reason.

</ul>

<p><i>TODO</i>: ClipPlane, ColorRGBA is missing.</p>

<?php
  x3d_status_footer();
?>
