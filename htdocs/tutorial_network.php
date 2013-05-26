<?php
require_once 'castle_engine_functions.php';
tutorial_header('Network and downloading');
?>

<i>This page describes upcoming Castle Game Engine 4.1.0 capabilities,
not available yet in the last stable engine version.</i>

<p>All methods in our engine take URL as the parameter,
not just a FileName (although in almost all cases you can also pass
a filename, if that's all you want to load).</p>

<ul>
  <li><p>Support for network protocols: <tt>http</tt>.

    <p>Our engine automatically handles downloading data from the network.
    Currently, only <tt>http</tt> is supported (through
    <a href="http://wiki.freepascal.org/fphttpclient">FpHttpClient unit</a>),
    although we plan to add other network protocols (like <tt>ftp</tt>
    and <tt>https</tt>) in the future.
    <!--
    support (using Synapse or LNet,
    also FpHttpClient may be extended in the future to enable https).
    -->
    All you have to do to enable network usage is set global
    <?php api_link('CastleDownload.EnableNetwork', 'CastleDownload.html#EnableNetwork'); ?>
    variable to <tt>true</tt>. By default it is <tt>false</tt>,
    because for now the downloads are not user-friendly &mdash;
    they are blocking (we wait for them to finish, there's no way
    to cancel a download). This will be improved in the future, and eventually
    downloading from network may be enabled by default.

    <p>All of the attributes in various files are URLs,
    so you can refer from a local files to resources on the network.

    <p>For example, your game level data may be actually downloaded from the
    network when loading level. To do this,
    <tt>level.xml</tt> may use an http protocol when referring to
    a <tt>scene</tt>. Like this:

<?php echo xml_highlight(
'<?xml version="1.0"?>
<level
  name="pits"
  type="Level"
  scene="http://svn.code.sf.net/p/castle-engine/code/trunk/castle/data/levels/fountain/fountain_final.x3dv"
  title="The Pits of Azeroth"
  placeholders="blender"
/>'); ?>

    and the scene with all associated resources will be downloaded.

    <p>Inside 3D models (like X3D, VRML and others), you can use network resources,
    for example you can <tt>Inline</tt> them (add a 3D model from another file),
    you can make <tt>Anchor</tt> to them,
    you can refer to textures and sounds and scripts and everything else
    from the network. Relative URLs are always resolved
    with respect to the containing document.

  <li><p><tt>file</tt> protocol.

    <p>Always (regardless of <tt>EnableNetwork</tt> value)
    we support getting resources from <tt>file</tt> URLs.
    These are just local filenames encoded as URL.
    <?php api_link('CastleURIUtils', 'CastleURIUtils.html'); ?>
    contains routines to operate on URLs (and more general URIs),
    including converting between regular filenames and URLs with
    <tt>file:</tt> protocol.

    <ul>
      <li><p>Use this to convert a FileName (relative or absolute)
      to an absolute URL.

<?php echo pascal_highlight(
'URL := FilenameToURISafe(FileName);'); ?>

      <li>Use this to convert something that may be a FileName or URL to an URL.
      This is safer than <tt>FilenameToURISafe(...)</tt>, in that it will
      never touch something that already is an URL.
      On the other hand, there are some obscure cases
      (when a relative filename starts with a component with colon inside)
      when it may think that it has URL, while in fact it has a filename that
      should be converted to <tt>file:</tt> URL.

<?php echo pascal_highlight(
'URL := AbsoluteURI(FileNameOrURL);'); ?>

      <li>Use this to convert URL back to a FileName.
      When the URL is a <tt>file:</tt> protocol, it will decode back
      the simple filename. Right now, URL without protcol is also
      returned back as a simple filename. When the URL uses a different
      protocol (like <tt>http</tt>), returns empty string.

<?php echo pascal_highlight(
'FileName := URIToFilenameSafe(URL);'); ?>
    </ul>

    <p>See reference of
    <?php api_link('FilenameToURISafe', 'CastleURIUtils.html#FilenameToURISafe'); ?>,
    <?php api_link('AbsoluteURI', 'CastleURIUtils.html#AbsoluteURI'); ?>,
    <?php api_link('URIToFilenameSafe', 'CastleURIUtils.html#URIToFilenameSafe'); ?>.
    See <tt>castle_game_engine/doc/uri_filename.txt</tt> in sources
    for more internal comments.

  <li><p><tt>data</tt> protocol.

    <p>We can always load resources from <tt>data</tt> URIs.
    The <tt>data</tt> URIs allow you to embed various resources
    (like textures, sounds, other 3D models) inside a parent file,
    for example instead of referring to the texture URL from 3D model &mdash;
    you can embed the actual texture contents inside 3D model file.
    This allows you to pack your data into a single file,
    which is sometimes desired (it may be easier to distribute),
    and <tt>data</tt> URIs are very handy for this. See
    <a href="http://en.wikipedia.org/wiki/Data_URI_scheme">data: URI specification</a>.

    <p>Our engine includes a tool <tt>to_data_uri</tt> (see inside
    <tt>castle_game_engine/examples/tools/</tt>)
    that can turn any file
    into a data URI, and you can use such data URI everywhere where we expect URL.

    <p>Demos of using data URI are inside <?php
    echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>,
    see in particular <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/x3d/data_uri.x3dv">x3d/data_uri.x3dv</a>.
</ul>

<p>If you want to handle network and URLs in your own code,
use the trivial
<?php api_link('Download', 'CastleDownload.html#Download'); ?> function.
It automatically handles all the details for you,
including downloading from network (if <tt>EnableNetwork</tt>),
and returns you a <tt>TStream</tt> that contains the resource indicated
by the URL. You could use it to trivally easy add network (and other URLs)
support to your application. There is an example in the engine code
<tt>examples/tools/castle_download</tt> that uses this to implement a simple
command-line downloading tool (like <tt>wget</tt>).

<?php
tutorial_footer();
?>
