<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Networking', 'networking',
    'This component contains the basic tools to combine various VRML/X3D models.
     <code>Inline</code> allows to insert another 3D model into
     the current file. <code>Anchor</code> makes a link that, when clicked,
     will jump to the referenced file (similar to HTML &lt;a href="..."&gt;).'
    );
?>

<p>Supported:</p>

<ul>
  <li><p>URLs everywhere are supported. <code>file</code>, <code>http</code>, and
    <code>data</code> URIs are loaded, as well as special protocols for scripting
    languages. Absolute as well as relative URLs are handled, of course.

    <p>For network support (<code>http</code>) in
    <?php echo a_href_page("view3dscene", "view3dscene") ?>
    remember to enable first
    <i>Preferences -&gt; Download Resources From Network</i> option
    (in your own programs using <i>Castle Game Engine</i>:
    see variable <code>CastleDownload.EnableNetwork</code>).

  <li><p><?php echo x3d_node_link('Anchor'); ?>

    <p>Files recognized as 3D models are loaded.
    We jump to viewpoint in a 3D model (loaded or current), if specified.
    Other documents (URLs not recognized as 3D models) are opened with
    default WWW browser on your operating system.</p>

    <p><i>TODO</i>: <code>parameter</code> field is ignored.</p>
    </li>

  <li><p><?php echo x3d_node_link('Inline'); ?>,<br>
    (VRML 97) <code>InlineLoadControl</code></p>

    <p>Yes, this includes handling of <code>InlineLoadControl</code> features
    to react to <code>load</code>, <code>url</code> and generate <code>children</code>
    events. For X3D, basic <code>Inline</code> node already has
    <code>load</code>, <code>url</code> features and they also work perfectly.</p></li>

  <li><p><code>IMPORT / EXPORT</code> feature is fully supported.
    In both XML and classic encodings, of course.
    Also supported in VRML 97 (including the ability to import in X3D nodes
    exported in VRML 97, and vice versa).</p>

    <ul>
      <li><p><b>Both <code>importedDEF</code> (preferred) and
        <code>exportedDEF</code> (obsolete) are supported.</b></p>

        <p>For the XML encoding, note that the <a href="http://www.web3d.org/documents/specifications/19776-1/V3.3/Part01/concepts.html#IMPORT_EXPORTStatementSyntax">X3D XML encoding
        spec</a> errorneously talks about <code>exportedDEF</code>
        attribute of the <code>&lt;IMPORT&gt;</code> element.
        It should be called <code>importedDEF</code>,
        this is what is used in the schemas (like <a href="http://www.web3d.org/specifications/x3d-3.2.xsd">x3d-3.2.xsd</a>),
        looking at <a href="http://www.web3d.org/specifications/x3d-dtd-changelog.txt">changelog</a>
        it's clear that the <code>exportedDEF</code> was deliberately renamed to
        <code>importedDEF</code>.

        <p>See <a href="http://web3d.org/pipermail/x3d-public_web3d.org/2010-April/000743.html">my mail on x3d-public</a>,
        submitted as X3D spec feedback.</p>

      <li><p><b><code>IMPORT / EXPORT</code> declarations allowed inside nodes,
        just like <code>ROUTE</code>s</b>.</p>

        <p>This means that <code>IMPORT / EXPORT</code> do not have to be at the top-level
        of the VRML file, they can be specified inside nodes.
        While it's clearly allowed in XML encoding
        (xsd says so, as far as I understand it,
        also <a href="http://www.web3d.org/x3d/content/examples/Basic/development/ImportExportSyntax.x3d">example ImportExportSyntax.x3d</a>
        uses it), in classic encoding it seems not allowed (grammar rules
        suggest that they are only allowed at "statements").
        But we allow it also inside nodes (this is consistent,
        and makes converting XML-&gt;classic reliable).

        <p>See <a href="http://web3d.org/pipermail/x3d-public_web3d.org/2010-April/000744.html">my mail on x3d-public</a>,
        submitted as X3D spec feedback.</p>
    </ul>
</ul>


<p><i>TODO:</i> LoadSensor is missing.</p>

<?php
  x3d_status_footer();
?>
