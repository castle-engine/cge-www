<?php
require_once 'castle_engine_functions.php';
manual_header('Network and downloading');

$toc = new TableOfContents(
  array(
    new TocItem('Loading and saving files using URLs', 'loading_saving'),
    new TocItem('Supported protocols', 'protocols'),
      new TocItem('Downloading from the network: <code>http</code> and <code>https</code>', 'https_http', 1),
      new TocItem('Loading local files: <code>file</code>', 'file', 1),
      new TocItem('Loading data files: <code>castle-data</code>', 'castle_data', 1),
      new TocItem('Embedded data: <code>data</code>', 'data', 1),
      new TocItem('(Internal) Android assets: <code>castle-android-assets</code>', 'castle_android_assets', 1),
    new TocItem('Dialog windows that support URLs', 'dialogs'),
    new TocItem('Notes about terminology: URI vs URL', 'terminology'),
  )
);
?>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>All methods in our engine take URL as the parameter,
not just a FileName. Although in most cases you can also pass
a filename (absolute or relative to the current directory),
and it will also work as expected.

<p>All loading and saving routines (for 3D models, images, sounds, and all
other resources) automatically deal with URLs. To actually load network
URLs (like http or https) you only need to set
<?php api_link('CastleDownload.EnableNetwork', 'CastleDownload.html#EnableNetwork'); ?>
 to <code>true</code>.

<p>To directly load or save <i>your own binary file formats</i>
(as ObjectPascal <code>TStream</code>):

<ul>
  <li><p>To load, use a simple
    <?php api_link('Download', 'CastleDownload.html#Download'); ?> function.
    It automatically handles all the details for you,
    including downloading from network (if <code>EnableNetwork</code>),
    and returns you a <code>TStream</code> that contains the resource indicated
    by the URL.

    <p>An example
    <a href="https://github.com/castle-engine/castle-engine/blob/master/examples/tools/castle_download.lpr">examples/tools/castle_download.lpr</a>
    uses this to implement a simple command-line downloading tool
    (like <code>wget</code>) using the engine.

  <li><p>To save, use
    <?php api_link('URLSaveStream', 'CastleDownload.html#URLSaveStream'); ?>
    function. Right now, it can only save to a local file,
    so it merely translates a URL to local filename and creates a <code>TFileStream</code>
    for you. Still, it's a good idea to use it, to uniformly deal with
    URLs throughout your application.
</ul>

<p>If you want to read or write text files from an URL, use
<?php api_link('TTextReader', 'CastleClassUtils.TTextReader.html'); ?> and
<?php api_link('TTextWriter', 'CastleClassUtils.TTextWriter.html'); ?>.

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

<p>Our engine can automatically download data from the network.
All you have to do is set global
<?php api_link('EnableNetwork variable (from CastleDownload unit)', 'CastleDownload.html#EnableNetwork'); ?>
 to <code>true</code>. By default it is <code>false</code>,
because for now the downloads are not user-friendly &mdash;
they are blocking (we wait for them to finish, there's no way
to cancel a download). This will be improved in the future, and eventually
downloading from network may be enabled by default.

<p>Internally, we use
<a href="http://wiki.freepascal.org/fphttpclient">FpHttpClient unit</a>,
which supports <code>http</code> and (since FPC 3.0.2) <code>https</code>.

<p>In order for the <code>https</code> to work, make sure that
OpenSSL library is available.
On Windows, you will probably want to place the appropriate DLLs alongside
your exe file. You can find these DLLs inside the engine
<code>tools/build-tool/data/external_libraries/</code> subdirectory.
These DLLs are also automatically included when packaging your application
using the <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">build tool</a>,
if you include <code>&lt;dependency name="Https" /&gt;</code> in your
<a href="https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples">CastleEngineManifest.xml</a>.

<!--
support (using Synapse or LNet,
also FpHttpClient may be extended in the future to enable https).
-->

<p>Note that (almost) all of the parameters and attributes
in the engine API are URLs. So you can refer to network resources
(http, https) anywhere, and it should "just work".

<p>For example, your game level data may be actually downloaded from the
network when loading level. To do this,
<code>level.xml</code> may use an http protocol when referring to
a <code>scene</code>. Like this:

<?php echo xml_highlight(
'<?xml version="1.0"?>
<level
  name="pits"
  type="Level"
  scene="https://raw.githubusercontent.com/castle-engine/castle-game/master/data/levels/fountain/fountain_final.x3dv"
  title="The Pits of Azeroth"
  placeholders="blender"
/>'); ?>

<p>and the scene with all associated resources will be downloaded.

<p>Inside 3D models (like X3D, VRML and others), you can use network resources,
for example you can <code>Inline</code> them (add a 3D model from another file),
you can make <code>Anchor</code> to them,
you can refer to textures and sounds and scripts and everything else
from the network. Relative URLs are always resolved
with respect to the containing document.

<?php echo $toc->html_section(); ?>

<p>To load simple files from disk, just use a <code>file</code> URL.
Of course this works regardless of the <code>EnableNetwork</code> value.
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
  the simple filename. Right now, URL without protocol is also
  returned back as a simple filename. When the URL uses a different
  protocol (like <code>http</code>), returns empty string.

<?php echo pascal_highlight(
'FileName := URIToFilenameSafe(URL);'); ?>
</ul>

<p>See reference of
<?php api_link('FilenameToURISafe', 'CastleURIUtils.html#FilenameToURISafe'); ?>,
<?php api_link('AbsoluteURI', 'CastleURIUtils.html#AbsoluteURI'); ?>,
<?php api_link('URIToFilenameSafe', 'CastleURIUtils.html#URIToFilenameSafe'); ?>.
See <a href="https://github.com/castle-engine/castle-engine/blob/master/doc/miscellaneous_notes/uri_filename.txt">>doc/miscellaneous_notes/uri_filename.txt</a> for more internal comments.

<p>If you read/write filenames from/to <a href="http://www.lazarus.freepascal.org/">Lazarus</a> classes,
for example if you use Lazarus <code>TOpenDialog.FileName</code> or
<code>TSaveDialog.FileName</code>, use the UTF-8 variants instead:
<?php api_link('URIToFilenameSafeUTF8', 'CastleLCLUtils.html#URIToFilenameSafeUTF8'); ?> and
<?php api_link('FilenameToURISafeUTF8', 'CastleLCLUtils.html#FilenameToURISafeUTF8'); ?>.
That is because Lazarus uses UTF-8 for all strings (as opposed to FPC RTL
that uses system encoding).

<?php echo $toc->html_section(); ?>

<p>(Since <i>Castle Game Engine &gt;= 6.5</i>).

<p>This protocol allows to load
<a href="manual_data_directory.php">data files</a> of your project.
Loading from the <code>castle-data:/images/my_image.png</code>
is equivalent to using <?php api_link('ApplicationData', 'CastleFilesUtils.html#ApplicationData'); ?>
 in code and loading from the <code>ApplicationData('images/my_image.png')</code>.

<p>During development of a normal cross-plaform game,
the <i>data files</i> are simply things
inside the <code>data</code> subdirectory of your project.
See <a href="manual_data_directory.php">documentation about the data directory</a>.

<p>The data location is usually (at least on desktop systems) just
a regular directory on disk. So loading files from <code>castle-data:/xxx</code>
protocol <i>usually</i> boils down to loading from a <code>file:/xxx</code> protocol.
But you can adjust
<?php api_link('ApplicationDataOverride', 'CastleFilesUtils.html#ApplicationDataOverride'); ?>
 to host your data files wherever you want, this way
data files may even be loaded from <code>http</code> location.

<?php echo $toc->html_section(); ?>

<p><code>data</code> is a special protocol that doesn't refer to
an external resource. Instead, the complete data URI <i>contains</i> the contents.
<!-- (yes, it's often quite long, longer than normal URLs). -->
This allows to embed various resources
(like textures, sounds, other 3D models) inside a parent file.
For example instead of referring to the texture filename from 3D model &mdash;
you can embed the actual texture contents inside 3D model file.
This is sometimes a very nice feature (it makes the file easier to distribute).

<p>See <a href="https://en.wikipedia.org/wiki/Data_URI_scheme">data: URI specification</a>.
Our engine includes a tool <a href="https://github.com/castle-engine/castle-engine/blob/master/examples/tools/to_data_uri.lpr">examples/tools/to_data_uri.lpr</a>
that can turn any file
into a data URI, and you can use such data URI everywhere where we expect URL.

<p>Wherever our engine, or X3D, says that it expects a URL &mdash; you can use data URI
to provide the contents "right there", without using any additional file.
<!-- You can say that the data URI is "self-contained". -->

<p>Demos of using data URI are inside <?php
echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>,
see in particular <a href="https://github.com/castle-engine/demo-models/blob/master/x3d/data_uri.x3dv">x3d/data_uri.x3dv</a>.

<?php echo $toc->html_section(); ?>

<p>This protocol is called <code>castle-android-assets</code> or (deprecated name) <code>assets</code>. It is only available on Android.

<p>Used to access data files in an Android application.
"Asset" files live inside your application's .apk file, together with your compiled game.
The <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">build tool</a>
will copy the <code>data</code> directory of your game to Android assets.
For example, file that was in <code>data/my_texture.png</code> in your source code
can be accessed (from the Android app) using the URL <code>assets:/my_texture.png</code>.

<p><i>You should never explicitly use this protocol name</i>,
as it does not work on other platforms than Android.
Instead, use <?php api_link('ApplicationData', 'CastleFilesUtils.html#ApplicationData'); ?>
 to refer to your data files from code. The
<?php api_link('ApplicationData', 'CastleFilesUtils.html#ApplicationData'); ?>
 will always return an absolute URL to the data file location on current platform.
On Android it will start with <code>castle-android-assets:/...</code> but you should treat this
as an internal detail.
<!--
Inside your data, when referrring from one data file to another,
e.g. when an 3D file refers to a texture,
use relative URLs.
-->

<!--
<p>Usage of <code>assets:/</code> protocol to access Android assets by URLs is
consistent <a href="http://qt-project.org/doc/qt-5.1/qtdoc/platform-notes-android.html#assets-file-system">at least with Qt</a>.
See also <a href="http://developer.android.com/tools/projects/index.html">Android
docs for more information about assets and project layout</a>.
-->
</ul>

<?php echo $toc->html_section(); ?>

<p>If you use
<?php api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html'); ?>,
it gives you a ready
<?php api_link('TCastleWindowCustom.FileDialog', 'CastleWindow.TCastleWindowCustom.html#FileDialog'); ?>
 that takes and returns URLs.

<p>If you use Lazarus with
<?php api_link('TCastleControl', 'CastleControl.TCastleControl.html'); ?>,
we advise to use our dialog components:
<?php api_link('TCastleOpenDialog', 'CastleDialogs.TCastleOpenDialog.html'); ?>,
<?php api_link('TCastleSaveDialog', 'CastleDialogs.TCastleSaveDialog.html'); ?>,
<?php api_link('TCastleOpen3DDialog', 'CastleDialogs.TCastleOpen3DDialog.html'); ?>,
<?php api_link('TCastleOpenPictureDialog', 'CastleDialogs.TCastleOpenPictureDialog.html'); ?>,
<?php api_link('TCastleSavePictureDialog', 'CastleDialogs.TCastleSavePictureDialog.html'); ?>.
You can also continue using standard Lazarus dialog components.
Our routines (almost) always handle a filename instead of an URL,
or you can explicitly convert between filenames and URLs using functions
mentioned earlier.

<?php echo $toc->html_section(); ?>

<p><a href="https://en.wikipedia.org/wiki/Uniform_resource_identifier">URI</a>
is a more general term. URI uniquely identifies a resource but does not
necessarily tell us how to load (download) or save (upload) it.
We have many routines in <?php api_link('CastleURIUtils', 'CastleURIUtils.html'); ?>
 unit that process URIs (strings), they use the more general term <code>URI</code>.
They complement standard FPC <code>URIParser</code> routines.

<p><a href="https://en.wikipedia.org/wiki/Uniform_resource_locator">URL</a>
is a specific type of URI that also tells you how to load or save the resource.
For example <code>http</code> and <code>file</code> protocols define URLs.
Most of our routines that load or save use the term <code>URL</code>.

<p>Things get a little more cloudy when you realize there's also
<a href="https://en.wikipedia.org/wiki/Data_URI_scheme">data URI scheme</a>.
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
<a href="https://en.wikipedia.org/wiki/Uniform_resource_name">URNs</a>
(another subtype of URI). They are used by X3D external prototypes,
see <?php echo a_href_page('X3D extensions introduction', 'x3d_extensions'); ?>.

<?php
manual_footer();
?>
