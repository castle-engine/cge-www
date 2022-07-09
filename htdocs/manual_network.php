<?php
require_once 'castle_engine_functions.php';
castle_header('Network, downloading and using URLs');

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
    new TocItem('Multi-player options', 'multi_player'),
      new TocItem('Indy (CGE example using TCP streams)', 'indy', 1),
      new TocItem('RNL (CGE example of real-time online shooter)', 'rnl', 1),
      new TocItem('Other options', 'other', 1),
  )
);
?>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>All methods in our engine take URL as the parameter,
not just a FileName. Although in most cases you can also pass
a filename (absolute or relative to the current directory),
and it will also work as expected.

<p>All loading and saving routines (for models, images, sounds, and all
other resources) automatically deal with URLs.

<p>To directly load or save <i>your own binary file formats</i>
(as ObjectPascal <code>TStream</code>):

<ul>
  <li><p><b>To load (easily)</b>, use a simple
    <?php api_link('Download', 'CastleDownload.html#Download'); ?> function.
    It simply returns a <code>TStream</code> that contains the resource indicated by the URL.
    It supports all the protocols mentioned below, e.g. <code>file</code>,
    <code>castle-data</code>.

    <p>It can even download data using <code>http</code> or <code>https</code> protocols,
    although you need to set <?php api_link('EnableBlockingDownloads', 'CastleDownload.html#EnableBlockingDownloads'); ?>
    to <code>true</code> for this.

  <li><p><b>To load asynchronously</b>
    (to continue the flow of your application while the download takes place in the background),
    use the
    <?php api_link('TCastleDownload', 'CastleDownload.TCastleDownload.html'); ?> class.
    It presents a trivial API to start and watch the download progress,
    and offers a lot of features for HTTP requests.
    When the
    <?php api_link('TCastleDownload.Status', 'CastleDownload.TCastleDownload.html#Status'); ?>
    is
    <?php api_link('dsSuccess', 'CastleDownload.html#dsSuccess'); ?>
    you have the data
    (as a <code>TStream</code>) inside
    <?php api_link('TCastleDownload.Contents', 'CastleDownload.TCastleDownload.html#Contents'); ?>.

    <p>It supports all our procotols.
    It can download data using <code>http</code> or <code>https</code> protocols
    without any issues.
    It can be used to communicate with a REST server.

    <p>Examples:

    <ul>
      <li><p><a href="https://github.com/castle-engine/castle-engine/blob/master/examples/network/asynchronous_download/">examples/network/asynchronous_download/</a>
        demonstrates multiple simultaneous downloads, along with a 3D animation,
        running smoothly.

      <li><p><a href="https://github.com/castle-engine/castle-engine/blob/master/examples/network/castle_download/">examples/network/castle_download/</a>
        implements a simple command-line downloading tool
        (like <code>wget</code> or <code>curl</code> using our engine).

      <li><p><a href="https://github.com/castle-engine/castle-engine/blob/master/examples/network/remote_logging/">examples/network/remote_logging/</a>
        sends asynchronous HTTP POST message.
    </ul>

  <li><p><b>To save</b>, use
    <?php api_link('URLSaveStream', 'CastleDownload.html#URLSaveStream'); ?>
    function. Right now, it can only save to a local file,
    so it merely translates a URL to local filename and creates a <code>TFileStream</code>
    for you. Still, it's a good idea to use it, to uniformly deal with
    URLs throughout your application.
</ul>

<p>If you want to read or write text files from an URL, use
<?php api_link('TTextReader', 'CastleDownload.TTextReader.html'); ?> and
<?php api_link('TTextWriter', 'CastleDownload.TTextWriter.html'); ?>.

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

<p><code>http</code> and <code>https</code> work.
You can download data from the Internet,
and the <?php api_link('TCastleDownload', 'CastleDownload.TCastleDownload.html'); ?>
 has a support for various HTTP methods (GET, POST).
You can use this for simple downloading, or for full-featured communication with a REST
server.

<p>Asynchronous <?php api_link('TCastleDownload', 'CastleDownload.TCastleDownload.html'); ?>
 supports <code>http</code> and <code>https</code> automatically.
It is perfect to use with unreliable / slow network.

<p>Synchronous <?php api_link('Download', 'CastleDownload.html#Download'); ?>
 supports these protocols only if you set global variable
 <?php api_link('EnableBlockingDownloads', 'CastleDownload.html#EnableBlockingDownloads'); ?>
 to <code>true</code>.
We call them "blocking downloads" because the application simply waits for
the un-interruptible download to finish.
This is easy to use, but may cause your application to hang,
as network may be slow / unreliable.
We advise using <?php api_link('TCastleDownload', 'CastleDownload.html#TCastleDownload'); ?>
 for network protocols, although it requires a bit more effort.

<p>For the <code>https</code> (encrypted version of <code>http</code>) protocol to work:
<ol>
  <li><p>You need to use FPC &gt;= 3.2.0. Older FPC versions have critical problem with this.

  <li><p>Use the <code>OpenSSLSockets</code> unit. Simply add this to the uses clause
    of one of your units (like <code>GameInitialize</code>,
    <a href="manual_cross_platform.php">if you follow our conventions for cross-platform games)</a>):

    <pre>{$ifndef VER3_0} OpenSSLSockets, {$endif} // support HTTPS</pre>

  <li><p>You need to also distribute OpenSSL library. On Linux and FreeBSD, it is almost for sure
    installed on user's system already.
    On Windows, use the appropriate DLL.

    <p>Our <a href="https://castle-engine.io/build_tool">build tool</a>
    (used also if you use <a href="manual_editor.php">our editor</a>)
    takes care of it for you.
    Simply add <code>&lt;dependency name="Https" /&gt;</code> in your
    <a href="https://castle-engine.io/project_manifest">CastleEngineManifest.xml</a>.
</ol>

<p>Internally, we use
<a href="http://wiki.freepascal.org/fphttpclient">FpHttpClient unit</a>,
which supports <code>http</code> and <code>https</code>.

<p>Note that (almost) all of the parameters and attributes
in the engine API are URLs. So you can refer to network resources
(http, https) anywhere, and it should "just work".

<p>For example, your game level data may be actually downloaded from the
network when loading level. To do this,
<code>level.xml</code> may use an http protocol when referring to
a <code>scene</code>. Like this:

<?php echo xml_full_highlight(
'<?xml version="1.0"?>
<level
  name="pits"
  type="Level"
  scene="https://raw.githubusercontent.com/castle-engine/castle-engine/master/tools/castle-editor/data/project_templates/3d_fps_game/files/data/level/level-dungeon.gltf"
  title="The Pits of Azeroth"
/>'); ?>

<p>and the scene with all associated resources will be downloaded.

<p>Inside models (like X3D, glTF and other), you can also refer to network resources,
and it will "just work".
For example you can use X3D <code>Inline</code> node to inline a model from given URL,
you can use X3D <code>Anchor</code> node to switch to given model on click,
you can refer to textures and sounds and scripts and everything else
from the network. Relative URLs are always resolved
with respect to the containing document.

<p><i>On Android</i>, you should use the <a href="https://github.com/castle-engine/castle-engine/blob/master/tools/build-tool/data/android/integrated-services/download_urls/README.md">download_urls service</a> to support <code>http</code> and <code>https</code> protocols.

<?php echo $toc->html_section(); ?>

<p>To load normal files from disk, use a <code>file</code> URL.

<p>Absolute file URLs look like this:
<code>file:///c:/windows/clock.avi</code> (on Windows)
or <code>file:///etc/fstab</code> (on Unix).
You can also use normal absolute filenames with most CGE routines, like
<code>c:\windows\clock.avi</code> (on Windows; you can use slash or backslash)
or <code>/etc/fstab</code> (on Unix).

<p>In most cases absolute filenames are not very useful
(since they would be specific to a particular system).
Using relative URLs makes more sense, like <code>textures/wood.png</code>.
Relative URLs should use slashes, and work naturally when used
in other files (relative URL is then relative to the containing file)
or code (relative URL is then relative to <i>the current working directory</i>).
Note that the <i>current working directory</i> depends on how the user
runs your application.

<p>To reliably load game data from code you should
use <a href="manual_data_directory.php"><code>castle-data</code> protocol</a>,
not <code>file</code> protocol.

<p><?php api_link('CastleURIUtils', 'CastleURIUtils.html'); ?>
 contains routines to operate on URLs (and more general URIs),
including converting between regular filenames and URLs with
<code>file:</code> protocol.

<ul>
  <li><p>Use this to convert a FileName (relative or absolute)
  to an absolute URL.

<?php echo pascal_highlight(
'URL := FilenameToURISafe(FileName);'); ?>

  <li><p>Use this to convert something that may be a FileName or URL to an URL.
  This is safer than <code>FilenameToURISafe(...)</code>, in that it will
  never touch something that already is an URL.
  <!--
  On the other hand, there are some obscure cases
  (when a relative filename starts with a component with colon inside)
  when it may think that it has URL, while in fact it has a filename that
  should be converted to <code>file:</code> URL.
  -->

<?php echo pascal_highlight(
'URL := AbsoluteURI(FileNameOrURL);'); ?>

  <li><p>Use this to convert URL back to a FileName.
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
<!--
See <a href="https://github.com/castle-engine/castle-engine/blob/master/doc/miscellaneous_notes/uri_filename.txt">doc/miscellaneous_notes/uri_filename.txt</a> for more internal comments.
-->

<p>If you read/write filenames from/to <a href="http://www.lazarus.freepascal.org/">Lazarus</a> classes,
for example if you use Lazarus <code>TOpenDialog.FileName</code> or
<code>TSaveDialog.FileName</code>, you can use the UTF-8 variants instead:
<?php api_link('URIToFilenameSafeUTF8', 'CastleLCLUtils.html#URIToFilenameSafeUTF8'); ?> and
<?php api_link('FilenameToURISafeUTF8', 'CastleLCLUtils.html#FilenameToURISafeUTF8'); ?>.
But it doesn't matter in practice.
Both <i>Castle Game Engine</i> and <i>Lazarus</i> configure FPC RTL to use UTF-8 for all strings
(by default FPC RTL uses system-dependent encoding).

<p><i>On Android</i>, you should use the <a href="https://github.com/castle-engine/castle-engine/blob/master/tools/build-tool/data/android/integrated-services/read_external_storage/README.md">read_external_storage service</a> to be able to read storage files (e.g. from SD card)
through the <code>file</code> protocol.

<?php echo $toc->html_section(); ?>

<p>This protocol should be used to load
<a href="manual_data_directory.php">data files</a> of your project.
During development, on normal desktop systems (Windows, Linux etc.),
the <i>data files</i> are simply files
inside the <code>data</code> subdirectory of your project.
You should place there all the files loaded at runtime by your application.

<p>When the application is packaged for some systems,
like Android or iOS, the data directory may be treated in a special way.
If you access all the data files using the <code>castle-data</code> protocol
(or using URLs relative to files loaded using the <code>castle-data</code> protocol)
then your application will "just wok" on all systems.

<p>See the <a href="manual_data_directory.php">documentation
about the data directory</a>.

<p>Note that you can adjust
<?php api_link('ApplicationDataOverride', 'CastleFilesUtils.html#ApplicationDataOverride'); ?>
 to host your data files wherever you want.
This way data files may even be loaded from <code>http</code> location.
On desktop systems, the data location is by default
just a regular directory on disk, but you can change it.

<p>Loading from the <code>castle-data:/images/my_image.png</code>
is equivalent to using <?php api_link('ApplicationData', 'CastleFilesUtils.html#ApplicationData'); ?>
 in code and loading from the <code>ApplicationData('images/my_image.png')</code>.
Since <i>Castle Game Engine 6.5</i>, we advise using
<code>castle-data:/images/my_image.png</code> form.


<!--
The castle-data:/ URL designates files inside the application data directory. When developing, this is usually the subdirectory "data/" of your project. When the application is installed, this is sometimes a special directory within the filesystem, sometimes a special filesystem (read-only "assets" on Android). Use this for all your game's URLs to have a completely self-contained game that will work on any OS.

When setting the URLs of various components from Lazarus, by clicking on "..." in the Lazarus Object Inspector, we automatically detect when you chose a file inside the "data/" subdirectory, and change the URL into appropriate castle-data:/ (instead of a file:/ URL).
-->


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
Our engine includes a tool <code>to-data-uri</code> that can turn any file
into a data URI, and you can use such data URI everywhere where we expect URL.
<code>to-data-uri</code> is <a href="https://castle-engine.io/">provided
in the regular engine download in the <code>bin/</code> subdirectory</a>, also
<a href="https://github.com/castle-engine/castle-engine/blob/master/tools/to-data-uri/to-data-uri.lpr">source code is here</a>.

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
The <a href="https://castle-engine.io/build_tool">build tool</a>
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
<?php api_link('TCastleWindow.FileDialog', 'CastleWindow.TCastleWindow.html#FileDialog'); ?>
 that takes and returns URLs.

<p>If you use Lazarus with
<?php api_link('TCastleControl', 'CastleControl.TCastleControl.html'); ?>,
we advise to use our dialog components:
<?php api_link('TCastleOpenDialog', 'CastleDialogs.TCastleOpenDialog.html'); ?>,
<?php api_link('TCastleSaveDialog', 'CastleDialogs.TCastleSaveDialog.html'); ?>,
<?php api_link('TCastleOpen3DDialog', 'CastleDialogs.TCastleOpen3DDialog.html'); ?>,
<?php api_link('TCastleOpenImageDialog', 'CastleDialogs.TCastleOpenImageDialog.html'); ?>,
<?php api_link('TCastleSaveImageDialog', 'CastleDialogs.TCastleSaveImageDialog.html'); ?>.
They expose <code>URL</code> property which works naturally with CGE.

<!--
You can also continue using standard Lazarus dialog components.
Our routines (almost) always handle a filename instead of an URL,
or you can explicitly convert between filenames and URLs using functions
mentioned earlier.
-->

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

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

<p>The <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/network/tcp_connection">examples/network/tcp_connection</a> directory in CGE sources demonstrates how to create and use a <b>classic client/server solution, where multiple clients talk to a server over a TCP/IP connection</b>.

<p>It's a good cross-platform solution when:

<ul>
  <li><p>The client/server architecture fits your design. That is: one player "hosts" a game, and everyone can reach the host over IP (which typically means that either 1. the host IP, and the relevant port, are accessible publicly on the Internet, 2. or that everyone is within the same local network).

  <li><p>You want reliability (not maximum speed), since it uses TCP connection in a standard fashion.

  <li><p><?php api_link('CastleClientServer', 'CastleClientServer.html'); ?> and Indy use standard TCP connection in a standard way, which is good for simplicity and interoperability. E.g. you could develop a server in Java or C++ if needed, to communicate with Pascal clients.
</ul>

<p>This approach uses the <?php api_link('CastleClientServer', 'CastleClientServer.html'); ?> unit, which uses <a href="https://www.indyproject.org/">Indy</a> (with threads) on most platforms, except on Android where we utilize dedicated asynchronous Android API for this.

<p>To compile it, <b>be sure to install Indy</b> using one of the options below, and then also compile in Lazarus package <code>castle_indy</code>. To install Indy:

<ul>
  <li><p>You can install Indy through the <a href="https://wiki.freepascal.org/Online_Package_Manager">Online Package Manager</a>. The OPM is a great way to install Lazarus packages in general, go ahead and try it :)

  <li><p>You can install "indy" module using <a href="https://castle-engine.io/fpcupdeluxe">fpcupdeluxe</a>. You can install your own FPC and Lazarus using fpcupdeluxe, and add an "indy" module to it.

  <li><p>Alternatively, you can download Indy from <a href="http://packages.lazarus-ide.org/">packages.lazarus-ide.org (same packages that OPM uses)</a> . Search for "indy" there, download and unpack the zip, open the package <code>indylaz.lpk</code> inside Lazarus and compile it. Here's a command-line version:

<pre>
wget http://packages.lazarus-ide.org/Indy10.zip
unzip Indy10.zip
lazbuild Indy10/indylaz.lpk
</pre>
</ul>

<p>In all cases, you should get an additional package <code>indylaz</code> known by Lazarus. Remember to also install <code>packages/castle_indy.lpk</code> package, and use it in your projects.

<?php echo $toc->html_section(); ?>

<p><a href="https://github.com/BeRo1985/rnl">RNL (Realtime Network Library)</a> is an open-source, reliable UDP network library, for FPC and Delphi, cross-platform, developed by <a href="https://www.patreon.com/bero">Benjamin Rosseaux</a> . If you want to make real-time communication over a network (e.g. an FPS game like Quake) this is a good start.

<p>In fact, we have already made it :) <a href="https://github.com/castle-engine/not-quake">Not Quake</a> is an example of online first-person shooter, developed using <i>Castle Game Engine</i> and RNL. You can
<ul>
  <li>
    <p><a href="https://github.com/castle-engine/not-quake">Get source code and binary releases of client and server from GitHub</a>
  <li>
    <p><a href="https://cat-astrophe-games.itch.io/not-quake">Get binary releases of client from Itch.io</a>
  <li>
    <p><a href="https://castle-engine.io/wp/2022/06/24/not-quake-an-example-of-real-time-multi-player-shooter-a-bit-like-quake-using-castle-game-engine-and-rnl/">Read the news post</a>
</ul>

<?php echo $toc->html_section(); ?>

<p>There are various other networing solutions for Pascal &mdash; and you can use any of them together with <i>Castle Game Engine</i>. Links:

<ul>
  <li><p>Aforementioned <a href="https://www.indyproject.org/">Indy</a> is a big library providing a lot of networking options. You can use it directly in many ways. See the <a href="http://ww2.indyproject.org/docsite/html/frames.html">online documentation</a>.

  <li><p><a href="http://ararat.cz/synapse/doku.php">Synapse</a> is a cross-platform networking library. See also <a href="https://wiki.freepascal.org/Synapse">FPC wiki about Synapse</a>.

  <li><p><a href="https://lnet.wordpress.com/news/">lNet</a> is a cross-platform lightweight networking library for FPC. It's much smaller (in terms of API and implementation) than Synapse and Indy, which may be an advantage, depending on what you need. See <a href="https://lnet.wordpress.com/usage/faq/">lNet FAQ</a> and <a href="https://wiki.freepascal.org/lNet">FPC wiki about lNET</a>.

  <li><p>FPC includes some networking units in the standard installation already. They work at various levels. In particular if you just want HTTP (REST) networking, FPC has <a href="https://wiki.freepascal.org/fcl-web">fcl-web</a> which allows to create HTTP(S) servers and clients.

  <li><p>Remember about our <?php api_link('Download', 'CastleDownload.html#Download'); ?> function and <?php api_link('TCastleDownload', 'CastleDownload.TCastleDownload.html'); ?> class. They use <a href="https://wiki.freepascal.org/fphttpclient">FpHttpClient</a> under the hood. You can use <?php api_link('TCastleDownload', 'CastleDownload.TCastleDownload.html'); ?> for HTTP(S) communiction with a REST server written in any language (Pascal or not).

</ul>

<p>The future may bring to <i>Castle Game Engine</i> more networking options (at a higher-level, to replicate some game state across multiple clients).

<p>Mote that you can also make <a href="https://en.wikipedia.org/wiki/Hotseat_(multiplayer_mode)">hot seat</a> and <a href="https://en.wikipedia.org/wiki/Split_screen_(video_games)">split screen</a> games, in which case multiple people just play on the same computer. We fully support multiple joysticks connected to a single desktop application, and connecting / disconnecting them at runtime, which allows to handle input from multiple people in one game.

<?php
castle_footer();
?>
