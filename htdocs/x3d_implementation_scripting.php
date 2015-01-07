<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Scripting', 'scripting',
    'This component defines scripting support. <code>Script</code> is the
     only node defined here, it\'s like a black box that receives
     VRML/X3D events, processes them with some external language,
     and sends new VRML/X3D events.');

  $toc = new TableOfContents(
    array(
      new TocItem('Demos', 'demos'),
      new TocItem('Supported nodes', 'support'),
    ));
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>For demos and tests of <?php echo a_href_page('CastleScript', 'castle_script'); ?>,
see the <code>castle_script</code> subdirectory inside <?php
echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>.</p>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p><?php echo x3d_node_link('Script'); ?>

    <p>We handle special script protocols <?php echo a_href_page_hashlink('compiled:
    (to link scripts with handlers written in compiled language (ObjectPascal))',
    'x3d_extensions',
    'section_ext_script_compiled'); ?> and
    <?php echo a_href_page('castlescript:
    (simple scripting language specific to our engine)',
    'castle_script'); ?>.

    <p><i>TODO</i>: no standard scripting language, like ECMAScript,
    is implemented now. <code>directOutput</code> field of script node
    is ignored (<code>compiled:</code> scripts have always direct access
    to whole VRML scene, <code>castlescript:</code> has never access to VRML nodes).

    <p><code>mustEvaluate</code> is also ignored for now. This is non-optimal but
    valid behavior. Our current scripting protocols have no "loading"
    overhead (we don't initialize any scripting engine, castlescript: and
    compiled: scripts are just tightly built-in the engine) so this doesn't
    hurt us in practice.
</ul>

<?php
  x3d_status_footer();
?>
