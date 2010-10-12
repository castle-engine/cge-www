<?php
  require_once 'vrmlengine_functions.php';

  vrmlengine_header("Documentation", NULL, array('kambi_vrml_game_engine'));
?>

<?php echo pretty_heading('Documentation', NULL,
  'Overview of the engine and VRML, with some information about engine internals'); ?>

<p>Most of this documentation was originally written as my master's thesis,
passed in September 2006. Although I keep updating and adding content
to this as the engine develops.</p>

<p>View in various formats:</p>

<ul>
  <li><a href="vrml_engine_doc/output/xsl/html/">
    HTML (split in many pages)</a><li>
  <li><a href="vrml_engine_doc/output/xsl/html-nochunks/vrml_engine.html">
    HTML (one big file)</a><li>
  <li><?php echo current_www_a_href_size(
    'PDF', 'vrml_engine_doc/output/xsl/vrml_engine.pdf'); ?></li>
</ul>

<h2>Sources</h2>

<p>Sources archive (full DocBook sources, Makefile, XSL, images &mdash;
everything you need to remake the HTML / PDF outputs above; also
includes VRML files sources, so you can view them comfortably in
<?php echo a_href_page('view3dscene', 'view3dscene'); ?> etc.) :<br />
<?php echo current_www_a_href_size(
  'vrml_engine_doc.tar.gz',
  'vrml_engine_doc/vrml_engine_doc.tar.gz'); ?></p>

<p>You will need docbook (search for <tt>docbook</tt>,
<tt>docbook-xsl</tt>, <tt>xmlto</tt> packages)
to make HTML version of the document from DocBook sources.
Additionally you will need <tt>fop</tt> (version &gt;= 0.9x)
and <tt>ttf-dejavu</tt> font if you want to make PDF version.</p>

<p>You can also download the latest version from Subversion by:</p>

<pre class="terminal small"><?php echo sf_checkout_link(true, 'vrml_engine_doc'); ?></pre>

<?php
  vrmlengine_footer();
?>
