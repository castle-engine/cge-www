<?php
  require_once 'vrml_implementation_common.php';
  x3d_status_header('Networking', 'networking');
?>

<p>Supported:</p>

<ul>
  <li><p><tt>Anchor</tt>

    <p><i>TODO</i>: <tt>parameter</tt> field is ignored, everything else
    is handled (according to X3D spec).</p></li>

  <li><p><tt>Inline</tt>, (VRML 97) <tt>InlineLoadControl</tt></p>

    <p>Yes, this includes handling of <tt>InlineLoadControl</tt> features
    to react to <tt>load</tt>, <tt>url</tt> and generate <tt>children</tt>
    events. For X3D, basic <tt>Inline</tt> node already has
    <tt>load</tt>, <tt>url</tt> features and they also work perfectly.</p></li>

  <li><p><tt>IMPORT / EXPORT</tt> feature is fully supported.
    In both XML and classic encodings, of course.
    Also supported in VRML 97 (including the ability to import in X3D nodes
    exported in VRML 97, and vice versa).</p>

    <ul>
      <li><p><b>Both <tt>importedDEF</tt> (preferred) and
        <tt>exportedDEF</tt> (obsolete) are supported.</b></p>

        <p>For the XML encoding, note that the <a href="http://www.web3d.org/x3d/specifications/ISO-IEC-19776-1.2-X3DEncodings-XML/Part01/concepts.html#IMPORT_EXPORTStatementSyntax">X3D XML encoding
        spec</a> errorneously talks about <tt>exportedDEF</tt>
        attribute of the <tt>&lt;IMPORT&gt;</tt> element.
        It should be called <tt>importedDEF</tt>,
        this is what is used in the schemas (like <a href="http://www.web3d.org/specifications/x3d-3.2.xsd">x3d-3.2.xsd</a>),
        looking at <a href="http://www.web3d.org/specifications/x3d-dtd-changelog.txt">changelog</a>
        it's clear that the <tt>exportedDEF</tt> was deliberately renamed to
        <tt>importedDEF</tt>.

        <p>See <a href="http://web3d.org/pipermail/x3d-public_web3d.org/2010-April/000743.html">my mail on x3d-public</a>,
        submitted as X3D spec feedback.</p>

      <li><p><b><tt>IMPORT / EXPORT</tt> declarations allowed inside nodes,
        just like <tt>ROUTE</tt>s</b>.</p>

        <p>This means that <tt>IMPORT / EXPORT</tt> do not have to be at the top-level
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
