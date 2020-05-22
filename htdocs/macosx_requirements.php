<?php
require_once "castle_engine_functions.php";
castle_header("macOS notes and requirements");

$toc = new TableOfContents(
  array(
    new TocItem('Compiling on macOS', 'compiling'),
    new TocItem('GUI libraries: Carbon? Cocoa? X11? X11 + GTK?', 'options'),
      new TocItem('Advantages and disadvantages of using LCL (Carbon/Cocoa)', 'lcl', 1),
      new TocItem('TODO: CastleWindow backend using Cocoa', 'castle_window_cocoa', 1),
    new TocItem('Other libraries that may be required', 'other_libs'),
    new TocItem('Notes specific to particular macOS package managers', 'package_managers'),
      new TocItem('Installing FPC and Lazarus through macOS package managers', 'fpc_and_laz', 1),
      new TocItem('MacPorts', 'macports', 1),
      new TocItem('Homebrew', 'homebrew', 1),
      new TocItem('Fink', 'fink', 1),
    new TocItem('Creating macOS applications', 'creating_apps'),
      new TocItem('Packaging data with macOS applications', 'data', 1),
  )
);
?>

<div class="fixed-width-content">

<?php echo pretty_heading($page_title); ?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>The FPC compiler needs the <i>XCode command-line developer tools</i> installed. In short, open <i>/Applications/Utilities/Terminal</i> and execute <code>xcode-select --install</code>.

<!-- <p>Old notes: You may also need to accept license: -->

<!-- sudo xcodebuild -license # press "q" and type "agree" -->

<p>This is covered in more details in the FPC / Lazarus documentation:

<ul>
  <li><a href="https://wiki.freepascal.org/Mac_Installation_FAQ">FPC Mac Installation FAQ</a>
  <li><a href="https://wiki.freepascal.org/Installing_Lazarus_on_MacOS_X">Installing Lazarus on MacOS X</a>
</ul>

<p>Also, it seems that the latest macOS doesn't include gdb (a debugger, user underneath by Lazarus). Lazarus will warn you about this on the 1st run. You can install GDB e.g. using <a href="https://brew.sh/">HomeBrew</a>, just execute <code>brew install gdb</code>. See <a href="http://wiki.lazarus.freepascal.org/GDB_on_OS_X_Mavericks_or_newer_and_Xcode_5_or_newer">GDB on OS X</a>.

<!-- TODO: do we need codesigning? -->

<?php echo $toc->html_section(); ?>

<p>You can create your macOS applications in two ways:

<ol>
  <li><p>You can use <a href="http://www.lazarus.freepascal.org/">Lazarus</a> forms,
    and LCL (<i>Lazarus Component Library</i>).
    To use the engine, simply drop a
    <?php api_link('TCastleControlBase', 'CastleControl.TCastleControlBase.html'); ?>
    component on a Lazarus form.

    <p>This, in practice, means that your applications will use <i>Carbon</i> or <i>Cocoa</i>.
    These are built-in native libraries on macOS.
    So, your programs don't require anything extra to distribute.

  <li><p>Alternatively, you can create your windows using our own
    <?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?>
    class. Our <?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?>
    can use various <i>backends</i> under the hood:

    <ul>
      <li><p>The <b>default backend on macOS is <code>CASTLE_WINDOW_XLIB</code></b>.
        It's easiest to setup,
        although it does not look pretty, and does not show a menu bar
        (<?php api_link('TCastleWindowBase.MainMenu', 'CastleWindow.TCastleWindowBase.html#MainMenu'); ?>).
        <!-- or native dialog boxes -->
        <!-- (< ?php api_link('TCastleWindowBase.FileDialog', 'CastleWindow.TCastleWindowBase.html#FileDialog'); ? >) -->

        <p>It requires installing <a href="https://www.xquartz.org/">XQuartz</a>.
        (On older macOS versions, install instead
        <i>X11 Server</i> from your macOS install CD.
        <a href="https://support.apple.com/en-us/HT201341">For newer macOS, XQuartz is the way to go</a>.)

        <p><!-- Install it, and run before executing our programs. -->
        Our programs will appear as part of "<i>X11 server</i>" on your desktop.

        <p>To compile, you also need to add this line to your <code>fpc.cfg</code> file
        (<?php echo FPC_CFG_DOCS; ?>):</p>

<pre>
-k-L/usr/X11/lib/

# Old version:
# -Fl/usr/X11/lib/
# Should work equally well as far as I know, but it doesn't, for FPC 3.0.4/3.0.5
</pre>

        <!--
        If you start our program outside of X11
        (like by double-clicking the application
        in the <i>Finder</i>), the window may not be "focused"
        (receiving key events) at start. Just click the window to make it focused.
        </p-->

        <!--p><i>Somewhat internal notes:</i> if you start our program from within
        X11 xterm, it will be focused at the start. When you start us outside of X11,
        and we detect that the default X display doesn't work,
        we automatically use display name <code>:0</code> under macOS.
        This way we attach to the running X server, even if you execute us from some
        other application.
        <i>You can also explicitly start
        with command-line option <code>- -display=:0</code>, or always run us from X11 xterm,
        to use given X display.</i></p></li
        -->

        <p>If, when running our applications, you get an error
        that <i>glX extension not found</i>:
        Check do you have a <code>/usr/X11R6</code> symlink (e.g. by running
        <code>ls /usr/X11R6</code> in the terminal).
        Some versions of XQuartz
        seem to not install it (<a href="http://bugs.freepascal.org/view.php?id=31651">see here</a>).
        You can fix it on your system by:

        <pre>
sudo ln -s /usr/X11 /usr/X11R6
</pre>

        <p>Alternative fix (<a href="https://lists.apple.com/archives/x11-users/2015/Oct/msg00012.html">see here</a>):

        <pre>
sudo /usr/libexec/x11-select /opt/X11
sudo chmod a+rX /usr/X11/ /usr/X11R6/
</pre>

      <li><p>Alternatively,
        <b>you can switch the backend to <code>CASTLE_WINDOW_GTK_2</code></b>.
        Do this by adding this line to your <code>fpc.cfg</code> file (<?php echo FPC_CFG_DOCS; ?>):

        <pre>-dCASTLE_WINDOW_GTK_2</pre>

        <p>This looks better (the program will still be part
        of the "<i>X11 server</i>", but it will have a nice menu bar and dialog
        windows using GTK).

        <p>In addition to <b>X11</b> (see above), your
        application will use <b>GTK</b> libraries.
        Install them using
        <a href="https://www.macports.org/">MacPorts</a>,
        <a href="https://brew.sh/">Homebrew</a> or
        <a href="http://www.finkproject.org/">Fink</a>.
        Yes, all three options are tested and OK. Look for packages called <code>gtk2</code>.

        <!--
        " should install all
        the libraries we require (if in trouble, look for packages by
        "<code>fink list xxx</code>").</p>

        <p>Note that binary fink packages are usually too old (if available at all...).
        So just go with the "source" installation. Don't be afraid if you're not a programmer
        &mdash; the process goes smoothly, just follow the instructions
        and be patient.</p>

        <p>It is <i>not</i> necessary to use any fink packages from unstable
        branch.</p></li-->

      <li><p>Alternatively, <b>you can switch the backend of
        <?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?>
        to <code>CASTLE_WINDOW_LCL</code></b>.
        This uses Lazarus under the hood, and this way we will use <i>Carbon</i> or <i>Cocoa</i>
        instead.
        This means that <?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?>
        will actually use Lazarus <code>TForm</code> and <code>TOpenGLControl</code>
        internally.

        <p>This looks nice and native.

        <p>To do this, use the package <code>alternative_castle_window_based_on_lcl.lpk</code>
        instead of <code>castle_window.lpk</code>.
        This will give you <code>CastleWindow</code> unit that uses LCL and requires
        the <i>castle_components</i> package.
        If you open an existing source code,
        like view3dscene, you will have to change the dependencies in Lazarus <i>Project inspector</i>
        to use <code>alternative_castle_window_based_on_lcl</code> instead of <code>castle_window</code>.

        <!--
        We hope to make it easier in the future by using the <i>"Provides"</i> option of Lazarus packages,
        but right now it doesn't do what we want &mdash; it can not "pull" extra depencies
        present in alternative package but not present in original.
        -->
    </ul>
  </li>
</ol>

<?php echo $toc->html_section(); ?>

<p>On macOS, the default LCL widgetset is
<a href="http://wiki.freepascal.org/Carbon_Interface">Carbon</a> right now.

<p>Although the default will change to <i>Cocoa</i> most probably at some point,
as only <i>Cocoa</i> supports 64-bit applications
and latest <i>macOS Catalina</i> requires all applications to be 64-bit.
So you just have to use <i>Cocoa</i> on new macOS.

<p>Advantages and disadvantages of using LCL (as opposed to X11 / GTK solutions):

<ul>
  <li><p><i>Good:</i> native look, application has a normal menu bar,
    shows native dialog boxes (to open/save file, choose color and such)
    and generally looks and feels like every other macOS application.
    Lazarus compiles it into a <i>bundle</i> like <code>view3dscene.app</code>
    that can be easily installed by dragging to your <i>Applications</i>
    directory.

  <li><p><i>Good:</i> no extra dependencies, Carbon and Cocoa are already part of every
    macOS installation. (No dependencies on X11, GTK.)

<?php /* Cocoa is necessary now in new macOS, so should be more stable now:

  <li><p><i>Bad:</i> Carbon is deprecated by Apple.
    It is available only for 32-bit applications.

    <p>You can alternatively use the <a href="http://wiki.freepascal.org/Cocoa_Interface">Cocoa</a>
    widgetset, but it is not as stable.
    As of Lazarus 1.8, CastleWindow + LCL backend + Cocoa widgetset
    doesn't work reliably. So, in practice, you are stuck with 32-bit applications
    if you want native look on macOS.
*/
?>

  <li><p><i>Bad:</i> There are issues with LCL event loop. Some of them
    (not being able to get Update events continuously) are in bare LCL,
    some of them (the need to call Application.Run, not just loop
    using Application.ProcessMessages) are specific to LCL-Carbon.
    The former (Update issues when using mouse look or
    dragging with mouse) is somewhat workarounded on our side now
    (to not "stutter" when using mouse look), but the problem is still
    noticeable (mouse look under other TCastleWindowBase backends is much smoother).
</ul>

<?php echo $toc->html_section(); ?>

<p>It would be good to implement
<code>CastleWindow</code> backend based on Cocoa (without using Lazarus LCL).
This would provide all above advantages, without disadvantages.

<p>Can you help us with this?

<ul>
  <li><p>It's a matter of creating and implementing a file
    <code>castle_game_engine/src/window/castlewindow_cocoa.inc</code>,
    based on
    <code>castle_game_engine/src/window/castlewindow_backend_template.inc</code>.
    See <?php echo a_href_page('engine sources', 'index'); ?>.
    See at other "backends" (like GTK, WinAPI, Xlib, LCL)
    for examples how to implement such thing, everything is inside
    <code>src/window/</code> dir.

  <li><p>Alternatively, send Michalis a simple and clear example of FPC program
using Cocoa that 1. creates and shows a window
2. with menu bar 3. and with OpenGL context area covering the window.
I should be able to port such example to my "CastleWindow" then.

  <li><p>See e.g. <a href="http://wiki.freepascal.org/OS_X_Programming_Tips">FPC "OS_X Programming Tips"</a>
    for pointers.
    If you're a developer familiar with macOS
    native toolkit and
    <a href="http://www.freepascal.org/">Free Pascal Compiler</a>, your help
    will be much appreciated.
</ul>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p><b>For macOS older than Tiger (that is, 10.3 or older):
    install OpenAL to have game sounds</b>. It may be downloaded from
    Creative, see download links from
    <a href="http://connect.creativelabs.com/openal/Downloads/Forms/AllItems.aspx">openal.org downloads</a>.
    For macOS since Tiger, OpenAL comes already preinstalled.
    Even without OpenAL installed, all our programs will still work fine,
    you just will not get any sound.</p>

  <li><p><b>Install vorbisfile library to read OggVorbis sound/music files</b>.
    It may be installed using
    <a href="https://www.macports.org/">MacPorts</a>,
    <a href="https://brew.sh/">Homebrew</a> or
    <a href="http://www.finkproject.org/">Fink</a>.

    <p>Without vorbisfile, all our programs will still work fine, but you will not
    hear OggVorbis music.</p></li>

  <li><p>If you want to use <i>external libpng implementation</i>
    in your programs, that you will also need <b>libpng
    to open and save images and textures in PNG format</b>.
    It may be installed using
    <a href="https://www.macports.org/">MacPorts</a>,
    <a href="https://brew.sh/">Homebrew</a> or
    <a href="http://www.finkproject.org/">Fink</a>.

    <p>Note that <b>this is not necessary</b>.
    If we don't find libpng,
    we will fallback to reading PNG using <i>FpImage</i>,
    which works too (but is slower, which may be significant if you load a lot of PNG files).
</ul>

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

<p>To actually compile our programs on macOS, you need
the <a href="http://freepascal.org/">Free Pascal Compiler</a>.
For comfortable RAD development, install also
<a href="http://www.lazarus.freepascal.org/">Lazarus</a> (FPC is already included
inside Lazarus installations, if you choose normal packages).</p>

<p>You can install them by downloading from their respective webpages,
or you can install them by a macOS package manager like
<a href="https://www.macports.org/">MacPorts</a>,
<a href="https://brew.sh/">Homebrew</a> or
<a href="http://www.finkproject.org/">Fink</a>.

<p>Besides easy upgrades, the advantage of using the package manager to install
the compiler is that it's easier then to install the libraries
(like <i>GTK2</i>)
with the same architecture (32-bit vs 64-bit).
Otherwise, be careful to watch for it yourself: your system may host
both 32-bit and 64-bit binaries and libraries, but <i>to compile a 64-bit
application, you will need a compiler (FPC) that targets 64-bit processors
<b>and</b> 64-bit libraries</i>.

<p>On modern macOS versions, you usually just want to install x86_64 libraries,
and the FPC compiler for Darwin+x86_64.</p>

<?php echo $toc->html_section(); ?>

<p>To install GTK and FPC, simply do:

<pre>
sudo port install gtkglext # TODO: update this command to only install gtk2
sudo port install fpc
</pre>

<p>Then create your local FPC config, and add there macports libraries:

<ul>
  <li>Copy <code>/opt/local/lib/fpc/etc/fpc.cfg</code> to <code>~/.fpc.cfg</code>.<br>
    Or, you can create an empty file <code>~/.fpc.cfg</code> and paste there
<pre>
#INCLUDE /opt/local/lib/fpc/etc/fpc.cfg
</pre>

  <li>At the end of <code>~/.fpc.cfg</code> add this line:
<pre>
-Fl/opt/local/lib
</pre>

  <li>We also advice adding FPC compiler and tools to your $PATH, to make it easier to use. Macports install them to <code>/opt/local/lib/fpc/bin</code>, so add something like this to your <code>~/.bashrc</code>:

<pre>
export PATH=/opt/local/lib/fpc/bin:"$PATH"
</pre>
</ul>

<?php echo $toc->html_section(); ?>

<p>To install FPC with Homebrew, you simply do

<pre>
brew install fpc
</pre>

<p>The <code>fpc</code> is already on $PATH, so it's comfortable out-of-the-box.

<p>Installing additional libraries, like GTK, is equally trivial.

<?php echo $toc->html_section(); ?>

<ul>
  <li><p>To use the libraries installed by fink, you may have to add them to your libraries
    path. Execute a command like this (in the same shell (terminal), before executing our
    programs):

<pre>
export LD_LIBRARY_PATH=/sw/lib:"$LD_LIBRARY_PATH"
</pre>

    <p>You can add this line to your <code>~/.bashrc</code> to have it automatically
    done.

  <li><p>To compile applications using GTK (<code>CASTLE_WINDOW_GTK_2</code> backend),
    <!-- as of fink 0.29.21 (on macOS 10.6.7), --> you should additionally install
    the fink "<code>pango1-xft2-shlibs</code>" package. Simple "<code>fink install pango1-xft2-shlibs</code>"
    should do the trick. This is necessary for successful linking.</p>

  <li><p>If you use any libraries from fink, the linker must know their locations.
    Add this line to your <code>fpc.cfg</code> file (<?php echo FPC_CFG_DOCS; ?>):</p>

<pre>
-Fl/sw/lib/
</pre>
</ul>

<?php echo $toc->html_section(); ?>

<p>A bunch of information about packaging your programs for macOS follows.

<!--
Note that our engine doesn't force you to do anything special to distribute
your programs &mdash; you get a normal (Unix) binary that you can use however you like.

use any packaging, the rules below
are just general rules for all macOS programs.
make any constraints here.
You compile and package your program as usual, using FPC or Lazarus,
and  that you package however you like.</p>
-->

<ol>
  <li><p><i>Make "macOS bundle"</i>. It's basically a directory pretending
    to be an application.

    <p>You can use Lazarus to create macOS bundle.
    Or you can use our <a href="https://github.com/castle-engine/cge-scripts/blob/master/create_macosx_bundle.sh">create_macosx_bundle.sh script</a>.
    The example usage is inside view3dscene and castle-view-image sources.</p>
    </li>

  <li><p>Optionally, <i>add libraries (like libpng and vorbisfile) to the bundle</i>.
    If you link to them dynamically (e.g. using our <code>TDynLib.Load</code>),
    you should load them from a path relative to <code>BundlePath</code>, like
    <code>BundlePath + 'Contents/MacOS/libpng.dylib'</code>.

    <p>See
    <a href="http://wiki.freepascal.org/OS_X_Programming_Tips#Mac_OS_X_Libraries">macOS Libraries on FPC wiki</a>
    for general instructions how to include library inside a bundle.</p>
    </li>

  <li><p>Pack the bundle into a <code>.dmg</code> file.
    See <a href="http://el-tramo.be/guides/fancy-dmg/">Building Fancy DMG Images on macOS</a>
    for nice description how to make the directories inside dmg look pretty,
    so you can visually suggest user to drag your application in the <i>Applications</i>
    folder.

    <p>Alternatively, you can pack the bundle into a <i>regular zip file</i>.
    <a href="https://daringfireball.net/2009/09/how_should_mac_apps_be_distributed">There are convincing arguments that using ZIP is actually more user-friendly than DMG</a>
    (users can just double-click to unpack, and they have the application;
    they don't need to understand how "disk image" works).
    See also <a href="https://stackoverflow.com/questions/3954506/dmg-or-zip-file-for-distribution-to-macs">here</a> for discussion.
    And making zip is definitely simpler.

    <p>Alternative method of distribution macOS applications is the
    <a href="http://wiki.freepascal.org/Deploying_Your_Application#Using_PackageMaker_on_Mac_OS_X">package manager (.pkg)</a>.
    For normal applications (like games) the simpler .dmg is a better choice.
    </li>
</ol>

<?php echo $toc->html_section(); ?>

<p>Behaviour of the <a href="manual_data_directory.php">data directory</a> (<code>castle-data:</code> protocol) on macOS:

<ul>
  <li><p>On macOS, if you run the application through the "application bundle", then we expect the data to be found inside <code>MyApplication.app/Contents/Resources/data</code> subdirectory. This way user can move around <code>MyApplication.app</code> to move, install and uninstall the application.

  <li><p>If the <code>MyApplication.app/Contents/Resources/data</code> subdirectory is not found, we will use the <code>data</code> subdirectory that is sibling to <code>MyApplication.app</code>. This feature is intended to be used only <b>during development</b>. This way things works "out of the box" if you run through Lazarus, with checkbox <i>“Use Application Bundle for running and debugging”</i>.

  <li><p><b>When distributing the application to the end users</b>, you should manually copy the “data” subdirectory inside the bunde (to <code>MyApplication.App/Contents/Resources/data</code>).
    <ul>
      <li><p>In case of compiling and packaging using our <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">build tool</a> we could do it automatically at some point (although it’s not implemented for now)
      <li><p>In case of using Lazarus to create the bundle &mdash; you will have to do it manually always.
      <li><p>I recommend writing a simple shell script to create the bundle. Copy the data like this:

<pre>
rm -Rf MyApplication.app/Contents/Resources/data
cp -R data/ MyApplication.app/Contents/Resources
</pre>
     </ul>
</ul>

</div> <!-- class="fixed-width-content" -->

<?php
  castle_footer();
?>
