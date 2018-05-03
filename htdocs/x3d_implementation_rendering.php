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
  <li><p><?php echo x3d_node_link('Coordinate'); ?>,<br>
    <?php echo x3d_node_link('Color'); ?>,<br>
    <?php echo x3d_node_link('ColorRGBA'); ?>,<br>
    <?php echo x3d_node_link('Normal'); ?></p>

    <p>These nodes provide information about positions/colors/normal vectors
    to various geometry nodes. They are used e.g. by various
    nodes in this component (like <?php echo x3d_node_link2('IndexedTriangleSet', 'rendering'); ?>),
    and in <a href="x3d_implementation_geometry3d.php">Geometry3D</a> component
    (like <?php echo x3d_node_link2('IndexedFaceSet', 'geometry3D'); ?>).

  <li><p>Triangles:

    <p><?php echo x3d_node_link('IndexedTriangleSet'); ?>,<br>
    <?php echo x3d_node_link('TriangleSet'); ?>,<br>
    <p><?php echo x3d_node_link('IndexedTriangleFanSet'); ?>,<br>
    <?php echo x3d_node_link('TriangleFanSet'); ?>,<br>
    <?php echo x3d_node_link('IndexedTriangleStripSet'); ?>,<br>
    <?php echo x3d_node_link('TriangleStripSet'); ?>

    <p>Notes:

    <ul>
      <li><p>If you're looking for quads,
        use instead <code>IndexedQuadSet</code>, <code>QuadSet</code>
        from <?php echo a_href_page('CAD geometry component', 'x3d_implementation_cadgeometry'); ?>.

      <li><p>If you're looking for polygons (with arbitrary number of vertexes for each face),
        use instead <code>IndexedFaceSet</code> from
        <a href="x3d_implementation_geometry3d.php">Geometry3D component</a>.

      <li><p>The geometry nodes have many fields to specify
        positions, and various per-vertex information (normal vectors,
        colors, <code>attrib</code> with per-vertex attributes for shaders,
        <code>fogCoord</code> with per-vertex fog depth).

      <li><p>The <code>TriangleFanSet</code> and <code>TriangleStripSet</code>
        nodes
        have a special limitation: if you will use colors
        (colors are always per-vertex on these primitives,
        according to X3D spec) and request generation of per-face normals
        at the same time, and the shape is lit (has material),
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

        <p>Unfortunately, this is unfixable without falling back to
        worse rendering methods (instead of using triangle fan/strip on GPU).
        Shading has to be smooth to interpolate
        per-vertex colors, and at the same time the same vertex may require
        different normals on different faces.
        One day we will implement a special fallback for this case
        (to internally convert fans and strips
        to <code>IndexedTriangleSet</code> in such special case).

      <li><p>X3D specification doesn't specify what to do
        for triangle/quad sets when appearance specify a texture but no
        <code>texCoord</code> is given.
        Our engine currently takes the <code>IndexedFaceSet</code> approach for
        automatic generation of texture coords in this case.
    </ul>

  <li><p>Lines:
    <p><?php echo x3d_node_link('LineSet'); ?>,<br>
    <?php echo x3d_node_link('IndexedLineSet'); ?><br>

  <li><p>Points:
    <p><?php echo x3d_node_link('PointSet'); ?>

    <p>Using <code>PointSet</code> is the fastest and easiest way to render points.
    See <a href="https://github.com/castle-engine/demo-models/blob/master/x3d/points_colorrgba.x3dv">an example X3D file</a>. You can write the points in an X3D file or define the nodes using Pascal code, as always, see <a href="x3d_implementation_geometry2d.php">an example how to build X3D graph in Pascal</a>.

    <p>To control the size of the point, set <code>TCastleScene.Attributes.PointSize</code>.

    <p>This uses OpenGL(ES) rendering of points. So:
    <ul>
      <li><p>It's really fast (milions of points are not a problem, when placed in one PointSet).
      <li><p>There are GPU-specific (OpenGL-implementation specific, actually) limits on the possible point sizes. For desktop OpenGL, small sizes like 1-8 are usually supported, but (strictly speaking) only size 1 is absolutely guaranteed to be supported (see <a jref="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGet.xhtml">GL_POINT_SIZE_RANGE</a>). For mobile OpenGLES, controlling the point size is not possible (so it's always like 1).
      <li><p>Using anti-aliasing <!-- TODO: does Window.AntiAliasing affect this? --> affects how the large points look (as a rect or circle). See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPointSize.xml">glPointSize</a> docs.
    </ul>

    <p>This is the fastest method to render points (fastest to use, and fastest to render) but you need to watch for the limitations mentioned above.

    <p>Note: To display a number of 2D points, you can also use <code>DrawPrimitive2D</code> procedure with <code>Mode</code> = <code>pmPoints</code>. It has a parameter where you specify <code>PointSize</code>. Note that this also uses native OpenGL(ES) points, so controlling the point size is limited, just like noted above.

    <!--p>If you want to have large points and/or control their sizes also on mobile, then instead of PointSet you can create a new <code>Sphere</code> or a new <code>Box</code> shape for each point. But note that it will be much slower, it's not really a good idea for milions of points. You can make it faster by defininig an <code>IndexedFaceSet</code> with a number of spheres or boxes inside, although this requires more coding.-->

  <li><p><?php echo x3d_node_link('ClipPlane'); ?></p>

    <p>This node allows to clip the geometry by an arbitrary plane in 3D space.
    In effect, the geometry is "cut" in the middle, with only one part visible.

    <p>Notes:

    <ul>
      <li><p>There is a limit on the number of clipping planes that may be enabled
        at the same time on a single shape. This limit is <i>at least six</i>
        (see view3dscene <i>"Help -&gt; OpenGL Information"</i>, look
        at <i>"Max clip planes"</i> line, to know the limit of your GPU).
        Following X3D spec, we treat the "more global" clipping planes
        as more important.</p>

      <li><p>Clipping planes are a purely visual effect.
        The clipped (invisible) geometry is still taken into account
        for collision detection,
        mouse picking, bounding volume calculation etc.</p>

      <li><p>Clipping planes don't affect background nodes
        (<code>X3DBackgroundNode</code>).</p>
    </ul>
</ul>

<?php
  x3d_status_footer();
?>
