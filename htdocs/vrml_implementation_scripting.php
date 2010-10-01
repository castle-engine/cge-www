<?php
  define('X3D_COMPONENT_NAME', 'Scripting');
  require_once 'vrml_implementation_common.php';
  x3d_status_header();
?>

<p>Supported nodes:</p>

<ul>
  <li><p><tt>Script</tt>

    <p>We handle special script protocols <?php echo a_href_page_hashlink('compiled:
    (to link scripts with handlers written in compiled language (ObjectPascal))',
    'kambi_vrml_extensions',
    'section_ext_script_compiled'); ?> and
    <?php echo a_href_page('kambiscript:
    (simple scripting language specific to our engine)',
    'kambi_script'); ?>.

    <p><i>TODO</i>: no standard scripting language, like ECMAScript,
    is implemented now. <tt>directOutput</tt> field of script node
    is ignored (<tt>compiled:</tt> scripts have always direct access
    to whole VRML scene, <tt>kambiscript:</tt> has never access to VRML nodes).

    <p><tt>mustEvaluate</tt> is also ignored for now. This is non-optimal but
    valid behavior. Our current scripting protocols have no "loading"
    overhead (we don't initialize any scripting engine, kambiscript: and
    compiled: scripts are just tightly built-in the engine) so this doesn't
    hurt us in practice.
</ul>

<?php
  x3d_status_footer();
?>
