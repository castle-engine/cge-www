<?php
define('CASTLE_GITHUB_NAME', 'cge-documentation');

require_once 'castle_engine_functions.php';
castle_header("Engine internals documentation", array(
  'path' => array('documentation')
));
?>

<?php echo pretty_heading('Engine internals documentation', NULL,
  'Overview of the engine and VRML, with some information about engine internals'); ?>

<p>Most of this documentation was originally written as my master's thesis,
passed in September 2006. Although I was occasionally updating and adding content
to this document later.</p>

<p>To be honest, <b>this document isn't the best introduction to the engine</b>.
It talks too much about engine internals, and too little about engine API
and features for game developers.
Also, it doesn't discuss some of the key features added in engine &gt;= 3.
Also, it discusses many features of VRML 1.0, which is a very ancient 3D format
nowadays, and doesn't show (in depth) features of VRML 2.0 and modern X3D.
</p>

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

<p>Sources archive (full DocBook sources, Makefile, XSL, images &mdash;
everything you need to remake the HTML / PDF outputs above) :<br />
<?php echo current_www_a_href_size(
  'vrml_engine_doc.tar.gz',
  'vrml_engine_doc/vrml_engine_doc.tar.gz'); ?></p>

<p>You will need docbook (search for <code>docbook</code>,
<code>docbook-xsl</code>, <code>xmlto</code> packages)
to make HTML version of the document from DocBook sources.
Additionally you will need <code>fop</code> (version &gt;= 0.9x)
and <code>ttf-dejavu</code> font if you want to make PDF version.</p>

<p>You can also download the latest version from Subversion by:</p>

<pre><?php echo sf_checkout_link(true, 'documentation/vrml_engine_internals'); ?></pre>

<p>The example VRML 1.0/2.0 models, used for screenshots
in this work, are available inside <?php echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>
 (see in <code>vrml_engine_doc_simple_examples</code> subdirectory).
So you can view them comfortably in
<?php echo a_href_page('view3dscene', 'view3dscene'); ?> etc.

<?php
  castle_footer();
?>
