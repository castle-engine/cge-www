<?php
require_once 'castle_engine_functions.php';
tutorial_header('Network and downloading');
?>

<i>This page describes upcoming Castle Game Engine 4.1.0 capabilities,
not available yet in the last stable engine version.</i>

<p>All methods in our engine take URL as the parameter,
not just a FileName. Although in almost all cases you can also pass
a filename (absolute or relative to the current directory),
if that's all you're interested in.

<p>All loading and saving routines (for 3D models, images, sounds, and all
other resources) automatically deal with URLs. To actually load network
URLs (like http) you only need to set
<?php api_link('CastleDownload.EnableNetwork', 'CastleDownload.html#EnableNetwork'); ?>
 to <tt>true</tt>.

<p>To directly load or save files (as ObjectPascal <tt>TStream</tt>)
in your own code:

<ul>
  <li><p>To load, use our simple
    <?php api_link('Download', 'CastleDownload.html#Download'); ?> function.
    It automatically handles all the details for you,
    including downloading from network (if <tt>EnableNetwork</tt>),
    and returns you a <tt>TStream</tt> that contains the resource indicated
    by the URL. There is an example in the engine code
    <tt>examples/tools/castle_download</tt> that uses this to implement a simple
    command-line downloading tool (like <tt>wget</tt>).

    <p>Details about supported URL protocols are below.

  <li><p>To save, use trivial
    <?php api_link('URLSaveStream', 'CastleDownload.html#URLSaveStream'); ?>
    function. Right now, it can only save to a local file,
    so it merely translates a URL to local filename and creates a <tt>TFileStream</tt>
    for you. Still, it's a good idea to use it, to uniformly deal with
    URLs throughout your application.
</ul>

<p>If you want to read or write text files from an URL, you may
also find useful classes
<?php api_link('TTextReader', 'CastleClassUtils.TTextReader.html'); ?> and
<?php api_link('TTextWriter', 'CastleClassUtils.TTextWriter.html'); ?>.

<h2>Supported protocols</h2>

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

    <p>If you read/write filenames from/to <a href="http://www.lazarus.freepascal.org/">Lazarus</a> classes,
    for example if you use Lazarus <tt>TOpenDialog.FileName</tt> or
    <tt>TSaveDialog.FileName</tt>, use the UTF-8 variants instead:
    <?php api_link('URIToFilenameSafeUTF8', 'CastleLCLUtils.html#URIToFilenameSafeUTF8'); ?> and
    <?php api_link('FilenameToURISafeUTF8', 'CastleLCLUtils.html#FilenameToURISafeUTF8'); ?>.
    That is because Lazarus uses UTF-8 for all strings (as opposed to FPC RTL
    that uses system encoding).

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

<h2>Notes about terminology: URI vs URL</h2>

<p><a href="http://en.wikipedia.org/wiki/Uniform_resource_identifier">URI</a>
is a more general term. URI uniquely identifies a resource but does not
necessarily tell us how to load (download) or save (upload) it.
We have many routines in <?php api_link('CastleURIUtils', 'CastleURIUtils.html'); ?>
 unit that process URIs (strings), they use the more general term <tt>URI</tt>.
They complement standard FPC <tt>URIParser</tt> routines.

<p><a href="http://en.wikipedia.org/wiki/Uniform_resource_locator">URL</a>
is a specific type of URI that also tells you how to load or save the resource.
For example <tt>http</tt> and <tt>file</tt> protocols define URLs.
Most of our routines that load or save use the term <tt>URL</tt>.

<p>Things get a little more cloudy when you realize there's also
<a href="http://en.wikipedia.org/wiki/Data_URI_scheme">data URI scheme</a>.
It's not precisely an URL (it's not an address of a resource),
but you <i>can</i> load it (since the URI itself contains the resource).
And we support it fully (our
<?php api_link('Download', 'CastleDownload.html#Download'); ?> method
loads it automatically). Admittedly, this means that our loading routines
should rather use the term <i>URL or data URI</i>, but that's just long
and (for those who don't use data URI) confusing, so for simplicity we
just keep (over-)using the term <i>URL</i>. Also, other standards (like CSS
and X3D and VRML) allow placing <i>data URIs</i> inside fields called <tt>url</tt>.

<p>If you enjoy reading about Internet terminology,
note that we use in our engine also
<a href="http://en.wikipedia.org/wiki/Uniform_resource_name">URNs</a>
(another subtype of URI). They are used by X3D external prototypes,
see <?php echo a_href_page('X3D extensions introduction', 'x3d_extensions'); ?>.

<?php
tutorial_footer();
?>
