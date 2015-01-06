<?php
require_once 'castle_engine_functions.php';
tutorial_header('Network and downloading');
?>

<p>All methods in our engine take URL as the parameter,
not just a FileName. Although in almost all cases you can also pass
a filename (absolute or relative to the current directory),
if that's all you're interested in.

<p>All loading and saving routines (for 3D models, images, sounds, and all
other resources) automatically deal with URLs. To actually load network
URLs (like http) you only need to set
<?php api_link('CastleDownload.EnableNetwork', 'CastleDownload.html#EnableNetwork'); ?>
 to <code>true</code>.

<p>To directly load or save files (as ObjectPascal <code>TStream</code>)
in your own code:

<ul>
  <li><p>To load, use our simple
    <?php api_link('Download', 'CastleDownload.html#Download'); ?> function.
    It automatically handles all the details for you,
    including downloading from network (if <code>EnableNetwork</code>),
    and returns you a <code>TStream</code> that contains the resource indicated
    by the URL. There is an example in the engine code
    <code>examples/tools/castle_download</code> that uses this to implement a simple
    command-line downloading tool (like <code>wget</code>).

    <p>Details about supported URL protocols are below.

  <li><p>To save, use trivial
    <?php api_link('URLSaveStream', 'CastleDownload.html#URLSaveStream'); ?>
    function. Right now, it can only save to a local file,
    so it merely translates a URL to local filename and creates a <code>TFileStream</code>
    for you. Still, it's a good idea to use it, to uniformly deal with
    URLs throughout your application.
</ul>

<p>If you want to read or write text files from an URL, you may
also find useful classes
<?php api_link('TTextReader', 'CastleClassUtils.TTextReader.html'); ?> and
<?php api_link('TTextWriter', 'CastleClassUtils.TTextWriter.html'); ?>.

<h2>Supported protocols</h2>

<ul>
  <li><p>Support for network protocols: <code>http</code>.

    <p>Our engine automatically handles downloading data from the network.
    Currently, only <code>http</code> is supported (through
    <a href="http://wiki.freepascal.org/fphttpclient">FpHttpClient unit</a>),
    although we plan to add other network protocols (like <code>ftp</code>
    and <code>https</code>) in the future.
    <!--
    support (using Synapse or LNet,
    also FpHttpClient may be extended in the future to enable https).
    -->
    All you have to do to enable network usage is set global
    <?php api_link('CastleDownload.EnableNetwork', 'CastleDownload.html#EnableNetwork'); ?>
    variable to <code>true</code>. By default it is <code>false</code>,
    because for now the downloads are not user-friendly &mdash;
    they are blocking (we wait for them to finish, there's no way
    to cancel a download). This will be improved in the future, and eventually
    downloading from network may be enabled by default.

    <p>All of the attributes in various files are URLs,
    so you can refer from a local files to resources on the network.

    <p>For example, your game level data may be actually downloaded from the
    network when loading level. To do this,
    <code>level.xml</code> may use an http protocol when referring to
    a <code>scene</code>. Like this:

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
    for example you can <code>Inline</code> them (add a 3D model from another file),
    you can make <code>Anchor</code> to them,
    you can refer to textures and sounds and scripts and everything else
    from the network. Relative URLs are always resolved
    with respect to the containing document.

  <li><p><code>file</code> protocol.

    <p>Always (regardless of <code>EnableNetwork</code> value)
    we support getting resources from <code>file</code> URLs.
    These are just local filenames encoded as URL.
    <?php api_link('CastleURIUtils', 'CastleURIUtils.html'); ?>
    contains routines to operate on URLs (and more general URIs),
    including converting between regular filenames and URLs with
    <code>file:</code> protocol.

    <ul>
      <li><p>Use this to convert a FileName (relative or absolute)
      to an absolute URL.

<?php echo pascal_highlight(
'URL := FilenameToURISafe(FileName);'); ?>

      <li>Use this to convert something that may be a FileName or URL to an URL.
      This is safer than <code>FilenameToURISafe(...)</code>, in that it will
      never touch something that already is an URL.
      On the other hand, there are some obscure cases
      (when a relative filename starts with a component with colon inside)
      when it may think that it has URL, while in fact it has a filename that
      should be converted to <code>file:</code> URL.

<?php echo pascal_highlight(
'URL := AbsoluteURI(FileNameOrURL);'); ?>

      <li>Use this to convert URL back to a FileName.
      When the URL is a <code>file:</code> protocol, it will decode back
      the simple filename. Right now, URL without protcol is also
      returned back as a simple filename. When the URL uses a different
      protocol (like <code>http</code>), returns empty string.

<?php echo pascal_highlight(
'FileName := URIToFilenameSafe(URL);'); ?>
    </ul>

    <p>See reference of
    <?php api_link('FilenameToURISafe', 'CastleURIUtils.html#FilenameToURISafe'); ?>,
    <?php api_link('AbsoluteURI', 'CastleURIUtils.html#AbsoluteURI'); ?>,
    <?php api_link('URIToFilenameSafe', 'CastleURIUtils.html#URIToFilenameSafe'); ?>.
    See <code>castle_game_engine/doc/uri_filename.txt</code> in sources
    for more internal comments.

    <p>If you read/write filenames from/to <a href="http://www.lazarus.freepascal.org/">Lazarus</a> classes,
    for example if you use Lazarus <code>TOpenDialog.FileName</code> or
    <code>TSaveDialog.FileName</code>, use the UTF-8 variants instead:
    <?php api_link('URIToFilenameSafeUTF8', 'CastleLCLUtils.html#URIToFilenameSafeUTF8'); ?> and
    <?php api_link('FilenameToURISafeUTF8', 'CastleLCLUtils.html#FilenameToURISafeUTF8'); ?>.
    That is because Lazarus uses UTF-8 for all strings (as opposed to FPC RTL
    that uses system encoding).

  <li><p><code>data</code> protocol.

    <p>We can always load resources from <code>data</code> URIs.
    The <code>data</code> URIs allow you to embed various resources
    (like textures, sounds, other 3D models) inside a parent file,
    for example instead of referring to the texture URL from 3D model &mdash;
    you can embed the actual texture contents inside 3D model file.
    This allows you to pack your data into a single file,
    which is sometimes desired (it may be easier to distribute),
    and <code>data</code> URIs are very handy for this. See
    <a href="http://en.wikipedia.org/wiki/Data_URI_scheme">data: URI specification</a>.

    <p>Our engine includes a tool <code>to_data_uri</code> (see inside
    <code>castle_game_engine/examples/tools/</code>)
    that can turn any file
    into a data URI, and you can use such data URI everywhere where we expect URL.

    <p>Demos of using data URI are inside <?php
    echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>,
    see in particular <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/x3d/data_uri.x3dv">x3d/data_uri.x3dv</a>.

  <li><p><code>assets</code> protocol (Android only).

    <p>Used to access Android asset files from the Android application.
    Asset files live inside your application's .apk file, together with
    your compiled game.
    Typically you just copy/place the asset files to the
    <code>assets/</code> subdirectory of your Android project,
    and then build the .apk using normal Android tools.
    For example, file placed in <code>.../assets/my_texture.png</code>
    can be accessed (from the Android app) using the URL
    <code>assets:/my_texture.png</code>.

    <p>Usage of <code>assets:/</code> protocol to access Anroid assets by URLs is
    consistent <a href="http://qt-project.org/doc/qt-5.1/qtdoc/platform-notes-android.html#assets-file-system">at least with Qt</a>.
    See also <a href="http://developer.android.com/tools/projects/index.html">Android
    docs for more information about assets and project layout</a>.
</ul>

<h2>Dialog windows</h2>

<p>If you use
<?php api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html'); ?>,
it gives you a ready
<?php api_link('TCastleWindowBase.FileDialog', 'CastleWindow.TCastleWindowBase.html#FileDialog'); ?>
 that takes and returns URLs.

<p>If you use Lazarus with
<?php api_link('TCastleControl', 'CastleControl.TCastleControl.html'); ?>,
we advise to use our dialog componets:
<?php api_link('TCastleOpenDialog', 'CastleDialogs.TCastleOpenDialog.html'); ?>,
<?php api_link('TCastleSaveDialog', 'CastleDialogs.TCastleSaveDialog.html'); ?>,
<?php api_link('TCastleOpen3DDialog', 'CastleDialogs.TCastleOpen3DDialog.html'); ?>,
<?php api_link('TCastleOpenPictureDialog', 'CastleDialogs.TCastleOpenPictureDialog.html'); ?>,
<?php api_link('TCastleSavePictureDialog', 'CastleDialogs.TCastleSavePictureDialog.html'); ?>.
You can also continue using standard Lazarus dialog components.
Our routines (almost) always handle a filename instead of an URL,
or you can explicitly convert between filenames and URLs using functions
mentioned earlier.

<h2>Notes about terminology: URI vs URL</h2>

<p><a href="http://en.wikipedia.org/wiki/Uniform_resource_identifier">URI</a>
is a more general term. URI uniquely identifies a resource but does not
necessarily tell us how to load (download) or save (upload) it.
We have many routines in <?php api_link('CastleURIUtils', 'CastleURIUtils.html'); ?>
 unit that process URIs (strings), they use the more general term <code>URI</code>.
They complement standard FPC <code>URIParser</code> routines.

<p><a href="http://en.wikipedia.org/wiki/Uniform_resource_locator">URL</a>
is a specific type of URI that also tells you how to load or save the resource.
For example <code>http</code> and <code>file</code> protocols define URLs.
Most of our routines that load or save use the term <code>URL</code>.

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
and X3D and VRML) allow placing <i>data URIs</i> inside fields called <code>url</code>.

<p>If you enjoy reading about Internet terminology,
note that we use in our engine also
<a href="http://en.wikipedia.org/wiki/Uniform_resource_name">URNs</a>
(another subtype of URI). They are used by X3D external prototypes,
see <?php echo a_href_page('X3D extensions introduction', 'x3d_extensions'); ?>.

<?php
tutorial_footer();
?>
