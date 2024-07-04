<?php
require_once 'castle_engine_functions.php';
castle_header('Network, downloading and using URLs');

$toc = new TableOfContents(
  array(
    new TocItem('Loading and saving files using URLs', 'loading_saving'),
    new TocItem('Note about case-sensitive filesystems and URLs', 'case_sensitive'),
    new TocItem('Supported protocols', 'protocols'),
      new TocItem('Downloading from the network: <code>http</code> and <code>https</code>', 'https_http', 1),
      new TocItem('Loading local files: <code>file</code>', 'file', 1),
      new TocItem('Loading data files: <code>castle-data</code>', 'castle_data', 1),
      new TocItem('Embedded data: <code>data</code>', 'data', 1),
      new TocItem('(Internal) Android assets: <code>castle-android-assets</code>', 'castle_android_assets', 1),
    new TocItem('Dialog windows that support URLs', 'dialogs'),
    new TocItem('Notes about terminology: URI vs URL', 'terminology'),
    new TocItem('Multi-player options', 'multi_player'),
      new TocItem('Use TCastleDownload for HTTP REST communication with a backend', 'castle_download', 1),
      new TocItem('Indy (CGE example using TCP streams)', 'indy', 1),
        new TocItem('Known memory leaks with Indy', 'indy_memory_leaks', 2),
      new TocItem('RNL (CGE example of real-time online shooter)', 'rnl', 1),
      new TocItem('Planned: Nakama integration', 'rnl', 1),
      new TocItem('Other options', 'other', 1),
  )
);
?>

<?php echo $toc->html_toc(); ?>

<?php
echo cgeImg('float', array(
  array('filename' => 'delphi_linux_downloads.png', 'titlealt' => 'asynchronous_download example, downloading multiple URLs asynchronously (without blocking the UI)'),
));
?>

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
    <?php echo cgeRef('Download'); ?> function.
    It simply returns a <code>TStream</code> that contains the resource indicated by the URL.
    It supports all the protocols mentioned below, e.g. <code>file</code>,
    <code>castle-data</code>.

    <p>It can even download data using <code>http</code> or <code>https</code> protocols,
    although you need to set <?php echo cgeRef('EnableBlockingDownloads'); ?>
    to <code>true</code> for this.

  <li><p><b>To load asynchronously</b>
    (to continue the flow of your application while the download takes place in the background),
    use the
    <?php echo cgeRef('TCastleDownload'); ?> class.
    It presents a trivial API to start and watch the download progress,
    and offers a lot of features for HTTP requests.
    When the
    <?php echo cgeRef('TCastleDownload.Status'); ?>
    is
    <?php echo cgeRef('dsSuccess'); ?>
    you have the data
    (as a <code>TStream</code>) inside
    <?php echo cgeRef('TCastleDownload.Contents'); ?>.

    <p>It supports all our protocols.
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

      <li><p><a href="https://github.com/castle-engine/castle-openai">castle-openai: Talking with OpenAI assistant (essentially your own, customized ChatGPT)</a> is an example of using <code>TCastleDownload</code> to communicate with a REST server.
    </ul>

  <li><p><b>To save</b>, use
    <?php echo cgeRef('UrlSaveStream'); ?>
    function. Right now, it can only save to a local file,
    so it merely translates a URL to local filename and creates a <code>TFileStream</code>
    for you. Still, it's a good idea to use it, to uniformly deal with
    URLs throughout your application.
</ul>

<p>If you want to read or write text files from an URL, use
<?php echo cgeRef('TTextReader'); ?> and
<?php echo cgeRef('TTextWriter'); ?>.

<?php echo $toc->html_section(); ?>

<p>Often, URLs just refer to files on the filesystem. Like <code>file://...</code> URLs, or <code>castle-data:/...</code> URLs (on platforms where the <a href="data">application data</a> is just a set of regular files). In this case, the underlying filesystem determines whether the names are case-sensitive (so e.g. <code>foobar</code> vs <code>FooBar</code> mean something else) or not. On Unix (like Linux, FreeBSD, macOS) the filesystems are typically case-sensitive. On Windows, the filesystems are typically <i>not</i> case-sensitive.

<p>To make the application work on all platforms, be sure to always specify the same case in URLs as your files.

<p>E.g. take care if you load <code>castle-data:/FooBar.png</code> or <code>castle-data:/foobar.png</code>. Using wrong letter case may be an easy mistake, because on Windows both versions will work, but on Linux only the version with correct case.

<p>If you test and develop mostly on Windows, a simple option is also be to set global <code>CastleDataIgnoreCase:=true</code>. See <?php echo cgeRef('CastleDataIgnoreCase'); ?>.

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

<p><code>http</code> and <code>https</code> work.
You can download data from the Internet,
and the <?php echo cgeRef('TCastleDownload'); ?>
 has a support for various HTTP methods (GET, POST).
You can use this for simple downloading, or for full-featured communication with a REST
server.

<p>Asynchronous <?php echo cgeRef('TCastleDownload'); ?>
 supports <code>http</code> and <code>https</code> automatically.
It is perfect to use with unreliable / slow network.

<p>Synchronous <?php echo cgeRef('Download'); ?>
 supports these protocols only if you set global variable
 <?php echo cgeRef('EnableBlockingDownloads'); ?>
 to <code>true</code>.
We call them "blocking downloads" because the application has to simply wait for
the download to finish and there's no way to interrupt the download
(without just killing the application) or even watch the progress.
<!--So these "blocking downloads" are easy to use from code,
but may cause your application to hang,
as network may be slow / unreliable.-->
We advise always using <?php echo cgeRef('TCastleDownload'); ?>
 to get data from the network &mdash; although it requires
 a bit more effort from code, but allows to observe and interrupt the download.

<p>For the <code>https</code> (encrypted version of <code>http</code>) protocol to work:

<ol>
  <!--
    // CGE now requires FPC >= 3.2.0
    li><p>You need to use FPC &gt;= 3.2.0. Older FPC versions have critical problem with this.
  -->

  <li><p>For FPC: Use the <code>OpenSSLSockets</code> unit. Simply add this to the uses clause
    of one of your units (like <code>GameInitialize</code>):

    <pre>{$ifdef FPC} OpenSSLSockets, {$endif} // support HTTPS</pre>

  <li><p>Make sure users have the OpenSSL library installed.

    <p>On Unix (Linux, FreeBSD, macOS...), it is standard to have OpenSSL
    installed already system-wide. Developers and users likely don't need to do
    anything.

    <p>On Windows, use the appropriate DLLs.
    Our <a href="editor">editor</a> (or command-line <a href="build_tool">build tool</a>)
    will automatically place the proper DLLs alongside your EXE file on the first build.
    You only need to add <code>&lt;dependency name="Https" /&gt;</code> in your
    <a href="project_manifest">CastleEngineManifest.xml</a>.
</ol>

<p>See an example like
<a href="https://github.com/castle-engine/castle-engine/blob/master/examples/network/asynchronous_download/">examples/network/asynchronous_download/</a>
that does both things above, to make <code>https</code> work.

<p>Note that we use URLs, not filenames, throughout the entire engine API.
So to load something from the network, you can just pass e.g. <code>https://...</code>
to <?php echo cgeRef('TCastleSceneCore.Load'); ?>.

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
use <a href="data"><code>castle-data</code> protocol</a>,
not <code>file</code> protocol.

<p><?php echo cgeRef('CastleUriUtils'); ?>
 contains routines to operate on URLs (and more general URIs),
including converting between regular filenames and URLs with
<code>file:</code> protocol.

<ul>
  <li><p>Use this to convert a FileName (relative or absolute)
  to an absolute URL.

<?php echo pascal_highlight(
'URL := FilenameToUriSafe(FileName);'); ?>

  <li><p>Use this to convert something that may be a FileName or URL to an URL.
  This is safer than <code>FilenameToUriSafe(...)</code>, in that it will
  never touch something that already is an URL.
  <!--
  On the other hand, there are some obscure cases
  (when a relative filename starts with a component with colon inside)
  when it may think that it has URL, while in fact it has a filename that
  should be converted to <code>file:</code> URL.
  -->

<?php echo pascal_highlight(
'URL := AbsoluteUri(FileNameOrUrl);'); ?>

  <li><p>Use this to convert URL back to a FileName.
  When the URL is a <code>file:</code> protocol, it will decode back
  the simple filename. Right now, URL without protocol is also
  returned back as a simple filename. When the URL uses a different
  protocol (like <code>http</code>), returns empty string.

<?php echo pascal_highlight(
'FileName := UriToFilenameSafe(URL);'); ?>
</ul>

<p>See reference of
<?php echo cgeRef('FilenameToUriSafe'); ?>,
<?php echo cgeRef('AbsoluteUri'); ?>,
<?php echo cgeRef('UriToFilenameSafe'); ?>.
<!--
See <a href="https://github.com/castle-engine/castle-engine/blob/master/doc/miscellaneous_notes/uri_filename.txt">doc/miscellaneous_notes/uri_filename.txt</a> for more internal comments.
-->

<!--
<p>If you read/write filenames from/to <a href="http://www.lazarus.freepascal.org/">Lazarus</a> classes,
for example if you use Lazarus <code>TOpenDialog.FileName</code> or
<code>TSaveDialog.FileName</code>, you can use the UTF-8 variants instead:
<?php echo cgeRef('UriToFilenameSafeUTF8'); ?> and
<?php echo cgeRef('FilenameToUriSafeUTF8'); ?>.
But it doesn't matter in practice.
Both <i>Castle Game Engine</i> and <i>Lazarus</i> configure FPC RTL to use UTF-8 for all strings
(by default FPC RTL uses system-dependent encoding).
-->

<p><i>On Android</i>, you should use the <a href="https://github.com/castle-engine/castle-engine/blob/master/tools/build-tool/data/android/integrated-services/read_external_storage/README.md">read_external_storage service</a> to be able to read storage files (e.g. from SD card)
through the <code>file</code> protocol.

<?php echo $toc->html_section(); ?>

<p>This protocol should be used to load
<a href="data">data files</a> of your project.
During development, on normal desktop systems (Windows, Linux etc.),
the <i>data files</i> are simply files
inside the <code>data</code> subdirectory of your project.
You should place there all the files loaded at runtime by your application.

<p>When the application is packaged for some systems,
like Android or iOS, the data directory may be treated in a special way.
If you access all the data files using the <code>castle-data</code> protocol
(or using URLs relative to files loaded using the <code>castle-data</code> protocol)
then your application will "just wok" on all systems.

<p>See the <a href="data">documentation
about the data directory</a>.

<p>Note that you can adjust
<?php echo cgeRef('ApplicationDataOverride'); ?>
 to host your data files wherever you want.
This way data files may even be loaded from <code>http</code> location.
On desktop systems, the data location is by default
just a regular directory on disk, but you can change it.

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
echo a_href_page('our demo models', 'demo_models'); ?>,
see in particular <a href="https://github.com/castle-engine/demo-models/blob/master/x3d/data_uri.x3dv">x3d/data_uri.x3dv</a>.

<?php echo $toc->html_section(); ?>

<p>This protocol is called <code>castle-android-assets</code> or (deprecated name) <code>assets</code>. It is only available on Android.

<p>Used to access data files in an Android application.
"Asset" files live inside your application's .apk file, together with your compiled game.
The <a href="build_tool">build tool</a>
will copy the <code>data</code> directory of your game to Android assets.
For example, file that was in <code>data/my_texture.png</code> in your source code
can be accessed (from the Android app) using the URL <code>assets:/my_texture.png</code>.

<p><i>You should never explicitly use this protocol name</i>,
as it does not work on other platforms than Android.
Instead, use <?php echo cgeRef('ApplicationData'); ?>
 to refer to your data files from code. The
<?php echo cgeRef('ApplicationData'); ?>
 will always return an absolute URL to the data file location on current platform.
On Android it will start with <code>castle-android-assets:/...</code> but you should treat this
as an internal detail.
<!--
Inside your data, when referring from one data file to another,
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
<?php echo cgeRef('TCastleWindow'); ?>,
it gives you a ready
<?php echo cgeRef('TCastleWindow.FileDialog'); ?>
 that takes and returns URLs.

<p>If you use Lazarus with
<?php echo cgeRef('TCastleControl'); ?>,
we advise to use our dialog components:
<?php echo cgeRef('TCastleOpenDialog'); ?>,
<?php echo cgeRef('TCastleSaveDialog'); ?>,
<?php echo cgeRef('TCastleOpenSceneDialog'); ?>,
<?php echo cgeRef('TCastleOpenImageDialog'); ?>,
<?php echo cgeRef('TCastleSaveImageDialog'); ?>.
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
We have many routines in <?php echo cgeRef('CastleUriUtils'); ?>
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
<?php echo cgeRef('Download'); ?> method
loads it automatically). Admittedly, this means that our loading routines
should rather use the term <i>URL or data URI</i>, but that's just long
and (for those who don't use data URI) confusing, so for simplicity we
just keep (over-)using the term <i>URL</i>. Also, other standards (like CSS
and X3D) allow placing <i>data URIs</i> inside fields called <code>url</code>.

<p>If you enjoy reading about Internet terminology,
note that we use in our engine also
<a href="https://en.wikipedia.org/wiki/Uniform_resource_name">URNs</a>
(another subtype of URI). They are used by X3D external prototypes,
see <?php echo a_href_page('X3D extensions introduction', 'x3d_extensions'); ?>.

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

<p>The engine provides a cross-platform component <?php echo cgeRef('TCastleDownload'); ?> to asynchronously download any URL and to perform HTTP(S) web requests. In effect, this can be used to communicate with any backend (written using Pascal or not) that uses HTTP(S) protocol.

<p>Features of <?php echo cgeRef('TCastleDownload'); ?>:

<ul>
  <li>
    <p>Supports various HTTP(S) methods (GET, POST, PUT...).
  <li>
    <p>Handles HTTP(S) redirects automatically.
  <li>
    <p>Allows to send custom HTTP headers.
  <li>
    <p>Gets MIME type from server automatically.
  <li>
    <p>Exposes HTTP response headers and code.
  <li>
    <p>It is really cross-platform and cross-compiler, covering all platforms supported by <i>Castle Game Engine</i>. E.g. it uses <a href="https://wiki.freepascal.org/fphttpclient">FpHttpClient</a> with FPC on most desktops, uses special <a href="https://github.com/castle-engine/castle-engine/blob/master/tools/build-tool/data/android/integrated-services/download_urls/README.adoc">Android service on Android</a>, on Delphi uses <i>Indy</i> or <code>TNetHTTPClient</code> (depending on what works better on given platform).
  <li>
    <p>Supports encrypted HTTPS out-of-the-box as much as possible, since HTTPS is standard nowadays. To this end, we adjust some <i>Indy</i> and <i>FpHttpClient</i> to make HTTPS just work.

    <p>FPC applications only have to use <code>OpenSSLSockets</code> unit, e.g. add

    <?php echo pascal_highlight('{$ifdef FPC} OpenSSLSockets, {$endif}'); ?>

    <p>to the uses clause of one of your units (like <code>GameInitialize</code>).
</ul>

<p>Examples:

<ul>
  <li>
    <p><a href="https://github.com/castle-engine/castle-engine/blob/master/examples/network/remote_logging/">examples/network/remote_logging/</a> (sends asynchronous HTTP POST message),
  <li>
    <p><a href="https://github.com/castle-engine/castle-engine/tree/master/examples/network/put_data">examples/network/put_data/</a> (send HTTP PUT),
  <li>
    <p><a href="https://github.com/castle-engine/castle-openai">castle-openai: Talking with OpenAI assistant (essentially your own, customized ChatGPT)</a> (sends a series of HTTP POST and GET messages to talk with OpenAI server).
</ul>

<?php echo $toc->html_section(); ?>

<p>The <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/network/tcp_connection">examples/network/tcp_connection</a> directory in CGE sources demonstrates how to create and use a <b>classic client/server solution, where multiple clients talk to a server over a TCP/IP connection</b>.

<?php
echo cgeImg('block', array(
  array('filename' => 'android_client.png', 'titlealt' => 'TCP client, on Android'),
  array('filename' => 'server_linux.png', 'titlealt' => 'TCP server, on Linux'),
  array('filename' => 'server_and_2_clients.png', 'titlealt' => 'TCP server and 2 clients, on Windows'),
));
?>

<p>It's a good cross-platform solution when:

<ul>
  <li><p>The client/server architecture fits your design. That is: one player "hosts" a game, and everyone can reach the host over IP (which typically means that either 1. the host IP, and the relevant port, are accessible publicly on the Internet, 2. or that everyone is within the same local network).

  <li><p>You want reliability (not maximum speed), since it uses TCP connection in a standard fashion.

  <li><p><?php echo cgeRef('CastleClientServer'); ?> and Indy use standard TCP connection in a standard way, which is good for simplicity and interoperability. E.g. you could develop a server in Java or C++ if needed, to communicate with Pascal clients.
</ul>

<p>This approach uses the <?php echo cgeRef('CastleClientServer'); ?> unit, which uses <a href="https://www.indyproject.org/">Indy</a> (with threads) on most platforms, except on Android where we utilize dedicated asynchronous Android API for this.

<p>To compile it with FPC/Lazarus, <b>be sure to install Indy</b> using one of the options below, and then also compile in Lazarus package <code>castle_indy</code>. To install Indy:

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

<p>Delphi users don't need to do anything in this regard, as Indy is already included in Delphi.

<?php echo $toc->html_section(); ?>

<p>All applications using Indy (whether with <i>Castle Game Engine</i> or not) have memory leaks, by default. This is a design choice of Indy &mdash; letting the memory leak was better than crashing in some edge-cases at unit finalization.

<p>If you are sure that all the Indy threads are given sufficient time to terminate gracefully before the application exits, you can define the symbol <code>FREE_ON_FINAL</code> in Indy sources. Search for <code>define FREE_ON_FINAL</code> inside Indy sources, as of now it means you will find 5 include files doing this:

<?php echo pascal_highlight('{.$DEFINE FREE_ON_FINAL}
{$UNDEF FREE_ON_FINAL}'); ?>

<p>You need to change these lines to

<?php echo pascal_highlight('{$DEFINE FREE_ON_FINAL}
{.$UNDEF FREE_ON_FINAL}'); ?>

<?php echo cgeImg('block', array(
  array('filename' => 'indy_free_on_final.png', 'titlealt' => 'FREE_ON_FINAL in Indy sources'),
)); ?>

<p>Please note that it isn't always easy to guarantee that <i>"threads are given sufficient time to terminate gracefully before the application exits"</i>. It means that disconnecting (by clients) and stopping (by server) cannot be done right when the application exits (or you have to wait for it to finish, potentially hanging your application on exit). Basically, Indy developers had a good reason to not enable this by default.

<p>For similar reason, the internal thread inside <?php echo cgeRef('CastleClientServer'); ?> actually may leak memory in case it didn't have time to terminate gracefully before the application exits. (But in our case, by default, it will not leak if exits cleanly, i.e. you disconnect client before application exit. It only leaks if the application exits too quickly, disconnecting the client.)

<p>Note: There's a code in Indy to "register known leak" with some memory managers, but it is not active by default for FPC.

<p>You can test for memory leaks following <a href="memory_leaks">our instructions about memory leaks</a>. We use FPC HeapTrc to detect memory leaks, and you can just set <code>&lt;compiler_options detect_memory_leaks="true"&gt;</code> in <a href="project_manifest">CastleEngineManifest.xml</a> to enable it.

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

<p>We plan <a href="roadmap#_integration_with_nakama_scalable_server_for_social_and_real_time_games_and_apps">integration with Nakama</a> in the future. It's a great open-source solution that provides out-of-the-box common multi-player features and can be customized to each particular project.

<?php echo $toc->html_section(); ?>

<p>There are various other networking solutions for Pascal &mdash; and you can use any of them together with <i>Castle Game Engine</i>. Links:

<ul>
  <li><p>Aforementioned <a href="https://www.indyproject.org/">Indy</a> is a big library providing a lot of networking options. You can use it directly in many ways. See the <a href="http://ww2.indyproject.org/docsite/html/frames.html">online documentation</a>.

  <li><p><a href="http://ararat.cz/synapse/doku.php">Synapse</a> is a cross-platform networking library. See also <a href="https://wiki.freepascal.org/Synapse">FPC wiki about Synapse</a>.

  <li><p><a href="https://lnet.wordpress.com/news/">lNet</a> is a cross-platform lightweight networking library for FPC. It's much smaller (in terms of API and implementation) than Synapse and Indy, which may be an advantage, depending on what you need. See <a href="https://lnet.wordpress.com/usage/faq/">lNet FAQ</a> and <a href="https://wiki.freepascal.org/lNet">FPC wiki about lNET</a>.

  <li><p>FPC includes some networking units in the standard installation already. They work at various levels. In particular if you just want HTTP (REST) networking, FPC has <a href="https://wiki.freepascal.org/fcl-web">fcl-web</a> which allows to create HTTP(S) servers and clients.
</ul>

<p>The future may bring to <i>Castle Game Engine</i> more networking options (at a higher-level, to replicate some game state across multiple clients).

<p>Mote that you can also make <a href="https://en.wikipedia.org/wiki/Hotseat_(multiplayer_mode)">hot seat</a> and <a href="https://en.wikipedia.org/wiki/Split_screen_(video_games)">split screen</a> games, in which case multiple people just play on the same computer. We fully support multiple joysticks connected to a single desktop application, and connecting / disconnecting them at runtime, which allows to handle input from multiple people in one game.

<?php
castle_footer();
?>
