<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Shape', 'shape',
    'This component defines the materials and containers for geometric
     nodes and their appearance.'
    );
?>

<p>Supported nodes:</p>

<ul>
  <li><p><?php echo x3d_node_link('Shape'); ?>,<br>
    <?php echo x3d_node_link('Appearance'); ?>,<br>
    <?php echo x3d_node_link('Material'); ?></p>

    <p>Fully implemented.</li>

  <li><p><?php echo x3d_node_link('LineProperties'); ?>.<br>
    Fullly implemented.

    <p>Allows to change line width and type (pattern).
    Suitable for all line geometry,
    in particular <code>IndexedLineSet</code> and <code>LineSet</code>.
    It also affects normal (filled) geometry (like <code>IndexedFaceSet</code>)
    when viewed in wireframe mode (see view3dscene "View -&gt; Fill Mode"
    menu).</p>

    <p>We only support values 1..5 (<i>Solid</i> .. <i>Dash-dot-dot</i>) for <code>linetype</code> field.

    <p>We use the GPU (OpenGL/OpenGLES) features to render lines with
    custom width and pattern (type).

    <ul>
      <li><p>This has the advantage that it's ultra-fast. Changing line width or
        using line pattern has virtually zero cost.

      <li><p>But the disadvantage is that we only support what the GPU (OpenGL/OpenGLES) supports.

        <p>This particularly hurts on OpenGLES (Android, iOS), where

        <ol>
          <li><code>glLineStipple</code> is not supported (so X3D <code>linetype</code> field will unfortunately do nothing).

          <li>Line width is limited. You can see the maxium line width on the console (output of <code>GLInformationString</code>), as <i>"Max line width"</i>. Unfortunately, on many Android devices it's just 1. Which means that <code>glLineWidth</code> is supported, but actually completely ignored by the device.
        </ol>

        <p>If you need 100% reliable line width or patterns on Android/iOS, you may need to render them differently, using filled rectangles (e.g. using <code>IndexedFaceSet</code> or <code>Rectangle2D</code> nodes). To simulate line patterns, use small repeatable textures with transparent pixels (you can set texture filtering to <code>"NONE"</code> using <code>TextureProperties</code> node to avoid smoothing the alpha channel).
    </ul>

  <li><p><b>Notes about VRML 1.0 and multiple materials</b>: multiple materials
    within a single VRML 1.0 <code>Material</code> node work 100%
    correctly if you change only emissive and transparency,
    or only diffuse and transparency for each index.
    For complicated cases (like when you change diffuse, and specular,
    and emissive...) for each material index -&gt; they will fail.</p>

    <p>This is a wontfix. For OpenGL fixed-function pipeline,
    changing all <code>glMaterial</code> settings too often (like for
    a vertex or a face) is prohibitively slow.
    It's also terribly memory consuming (for
    <?php echo a_href_page("castle", "castle") ?>, display lists of animations
    (in the older days when we used display lists)
    of spider and spider queen were eating 130 MB with naive implementation,
    vs 10 MB with current implementation).</p>

    <p>VRML 2.0 and X3D removed this idea, replacing it with much
    saner <code>Color</code> and <code>ColorRGBA</code> nodes, that are implemented
    fully.</p>

</ul>

<p><i>TODO</i>: FillProperties, TwoSidedMaterial are missing.</p>

<?php
  x3d_status_footer();
?>
