<?php
  require_once 'vrml_implementation_common.php';
  x3d_status_header('Shape', 'shape',
    'This component defines the materials and containers for geometric
     nodes and their appearance.'
    );
?>

<p>Supported nodes:</p>

<ul>
  <li><p><?php echo x3d_node_link('Shape'); ?>,
    <?php echo x3d_node_link('Appearance'); ?>,
    <?php echo x3d_node_link('Material'); ?></p></li>

  <li><p><b>VRML 1.0 and multiple materials</b>: multiple materials
    within a single VRML 1.0 <tt>Material</tt> node work 100%
    correctly if you change only emissive and transparency,
    or only diffuse and transparency for each index.
    For complicated cases (like when you change diffuse, and specular,
    and emissive...) for each material index -&gt; they will fail.</p>

    <p>This is a wontfix. For OpenGL fixed-function pipeline,
    changing all <tt>glMaterial</tt> settings too often (like for
    a vertex or a face) is prohibitively slow.
    It's also terribly memory consuming (for
    <?php echo a_href_page("castle", "castle") ?>, display lists of animations
    of spider and spider queen were eating 130 MB with naive implementation,
    vs 10 MB with current implementation).</p>

    <p>VRML 2.0 and X3D removed this idea, replacing it with much
    saner <tt>Color</tt> and <tt>ColorRGBA</tt> nodes, that are implemented
    fully.</p>
</ul>

<p><i>TODO</i>: FillProperties, LineProperties, TwoSidedMaterial are missing.</p>

<?php
  x3d_status_footer();
?>
