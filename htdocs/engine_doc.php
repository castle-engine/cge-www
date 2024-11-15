<?php
define('CASTLE_GITHUB_NAME', 'cge-documentation');

require_once 'castle_engine_functions.php';
castle_header("Engine internals documentation", array(
  'path' => array('documentation')
));
?>

<?php echo pretty_heading('Castle Game Engine internals'); ?>

<p>Most of this documentation was originally written as my master's thesis,
passed in September 2006. Although I was occasionally updating and adding content
to this document later.</p>

<p><b>This is an outdated overview of the Castle Game Engine and VRML 1.0.</b>

<p><b>Warning: This document isn't a good introduction to the Castle Game Engine (anymore)!</b>

<ul>
  <li>It talks too much about engine internals.
  <li>It shows too little of the engine API (and features) used during normal
    game creation by a developer. It doesn't even mention
    many of the key engine features added in engine &gt;= 3.
  <li>It discusses many features of VRML 1.0, which is a very ancient 3D format
    nowadays. The document doesn't show (in depth) features of VRML 2.0,
    modern X3D, and many enhancements of X3D in Castle Game Engine.
    Or glTF.
</ul>

<p>View in various formats:</p>

<ul>
  <li><a href="vrml_engine_doc/output/xsl/html/">
    HTML (split in many pages)</a></li>
  <li><a href="vrml_engine_doc/output/xsl/html-nochunks/vrml_engine.html">
    HTML (one big file)</a></li>
  <li><?php echo current_www_a_href_size(
    'PDF', 'vrml_engine_doc/output/xsl/vrml_engine.pdf'); ?></li>
</ul>

<h2>Sources</h2>

<p>See <a href="https://github.com/castle-engine/cge-documentation">cge-documentation</a>
repository, subdirectory
<a href="https://github.com/castle-engine/cge-documentation/tree/master/vrml_engine_internals">vrml_engine_internals</a>.
This contains full DocBook sources, Makefile, XSL, images &mdash;
everything you need to remake the HTML / PDF outputs above.
See the README for information how to regenerate it.</p>

<p>The example VRML 1.0/2.0 models, used for screenshots
in this work, are available inside <?php echo a_href_page('our demo models', 'demo_models'); ?>
 (see in <code>vrml_engine_doc_simple_examples</code> subdirectory).
So you can view them comfortably in
<a href="castle-model-viewer">Castle Model Viewer</a> etc.

<?php
  castle_footer();
?>
