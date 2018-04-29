<?php
require_once "castle_engine_functions.php";
castle_header("Mac OS X notes and requirements", array(
  'path' => array('all_programs')
));

$toc = new TableOfContents(
  array(
    new TocItem('Compiling on Mac OS X', 'compiling'),
    new TocItem('GUI libraries: Carbon? X11? X11 + GTK + GtkGlExt?', 'options'),
    new TocItem('Advantages and disadvantages of using Carbon', 'carbon'),
    new TocItem('Other libraries that may be required', 'other_libs'),
    new TocItem('Notes specific to particular Mac OS X package managers', 'package_managers'),
      new TocItem('Consider installing FPC and Lazarus through them too', 'fpc_and_laz', 1),
      new TocItem('MacPorts', 'macports', 1),
      new TocItem('Homebrew', 'homebrew', 1),
      new TocItem('Fink', 'fink', 1),
    new TocItem('Creating Mac OS X applications', 'creating_apps'),
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
  <li><a href="http://freepascal.stack.nl/down/i386/macosx-netherlands.html">FPC installation instructions for Mac OS X</a>
  <li><a href="http://wiki.freepascal.org/OS_X_Programming_Tips#Installing_Lazarus_and_Free_Pascal">OS X Programming Tips -&gt; Installing the Apple Developer Tools</a> and
  <li><a href="http://wiki.lazarus.freepascal.org/Installing_Lazarus_on_MacOS_X#Prerequisites:_Apple_Developer_Tools">Installing Lazarus on MacOS X -&gt; Prerequisites: Apple Developer Tools</a>
</ul>

<p>Also, it seems that the latest Mac OS X doesn't include gdb (a debugger, user underneath by Lazarus). Lazarus will warn you about this on the 1st run. You can install GDB e.g. using <a href="https://brew.sh/">HomeBrew</a>, just execute <code>brew install gdb</code>. See <a href="http://wiki.lazarus.freepascal.org/GDB_on_OS_X_Mavericks_or_newer_and_Xcode_5_or_newer">GDB on OS X</a>.

<!-- TODO: do we need codesigning? -->

<?php echo $toc->html_section(); ?>

<p>You can create your Mac OS X applications in two ways:

<ol>
  <li><p>You can use <a href="http://www.lazarus.freepascal.org/">Lazarus</a> forms,
    and LCL (<i>Lazarus Component Library</i>).
    To use the engine, simply drop a
    <?php api_link('TCastleControl', 'CastleControl.TCastleControl.html'); ?>
    component on a Lazarus form.

    <p>By default, on Mac OS X, this means that your applications
    will use <i>Carbon</i>, which is a built-in library
    on Mac OS X. So, your programs don't require anything extra.

    <p>(You can switch Lazarus widgetset, though, to use a different library
    under the hood.)

  <li><p>Alternatively, you can create your windows using our own
    <?php api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html'); ?>
    class. Our <?php api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html'); ?>
    can use various <i>backends</i> under the hood:

    <ul>
      <li><p>The <b>default backend on Mac OS X is <code>CASTLE_WINDOW_XLIB</code></b>.
        It's easiest to setup,
        although it does not look pretty, and does not show a menu bar
        (<?php api_link('TCastleWindowCustom.MainMenu', 'CastleWindow.TCastleWindowCustom.html#MainMenu'); ?>).
        <!-- or native dialog boxes -->
        <!-- (< ?php api_link('TCastleWindowCustom.FileDialog', 'CastleWindow.TCastleWindowCustom.html#FileDialog'); ? >) -->

        <p>It requires installing <a href="https://www.xquartz.org/">XQuartz</a>.
        (On older Mac OS X versions, install instead
        <i>X11 Server</i> from your Mac OS X install CD.
        <a href="https://support.apple.com/en-us/HT201341">For newer Mac OS X, XQuartz is the way to go</a>.)

        <p><!-- Install it, and run before executing our programs. -->
        Our programs will appear as part of "<i>X11 server</i>" on your desktop.

        <p>To compile, you also need to add this line to your <code>fpc.cfg</code> file
        (<?php echo FPC_CFG_DOCS; ?>):</p>

<pre>
-Fl/usr/X11/lib/
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
        we automatically use display name <code>:0</code> under Mac OS X.
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
        application will use <b>GTK and GtkGLExt</b> libraries.
        Install them using
        <a href="https://www.macports.org/">MacPorts</a>,
        <a href="https://brew.sh/">Homebrew</a> or
        <a href="http://www.finkproject.org/">Fink</a>.
        Yes, all three options are tested and OK. Look for packages called <code>gtk2</code>
        and <code>gtkglext</code>.

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
        <?php api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html'); ?>
        to <code>CASTLE_WINDOW_LCL</code></b>.
        This uses Lazarus under the hood, and this way we will use <i>Carbon</i> instead.
        This means that <?php api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html'); ?>
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

<p>On Mac OS X, the default LCL widgetset is
<a href="http://wiki.freepascal.org/Carbon_Interface">Carbon</a> right now.</p>

<ul>
  <li><p><i>Good:</i> native look, application has a normal menu bar,
    shows native dialog boxes (to open/save file, choose color and such)
    and generally looks and feels like every other Mac OS X application.
    Lazarus compiles it into a <i>bundle</i> like <code>view3dscene.app</code>
    that can be easily installed by dragging to your <i>Applications</i>
    directory.

  <li><p><i>Good:</i> no extra dependencies, Carbon is already part of every
    Mac OS X installation. (No dependencies on X11, GTK, GTKGlExt etc.)

  <li><p><i>Bad:</i> Unfortunately, Carbon is deprecated by Apple
    (although there are many applications using it, including Lazarus IDE itself).
    It is available only for 32-bit applications.

    <p>In time, this will be resolved in LCL when
    <a href="http://wiki.freepascal.org/Cocoa_Interface">Cocoa</a>
    widgetset will be more complete.
    Right now, <a href="http://wiki.freepascal.org/Roadmap#Status_of_features_on_each_widgetset">Carbon
    implementation is just more complete than Cocoa in LCL</a>,
    see also <a href="http://wiki.freepascal.org/OS_X_Programming_Tips#Choice_of_Lazarus_LCL_widgetsets">Choice of Lazarus LCL widgetsets
    (for Mac OS X)</a>.

    <p>It may also be resolved on our side if we ever make direct
    <code>CastleWindow</code> backend based on Cocoa (without using Lazarus LCL):

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
	If you're a developer familiar with Mac OS X
	native toolkit and
	<a href="http://www.freepascal.org/">Free Pascal Compiler</a>, your help
	will be much appreciated.
    </ul>

  <li><p><i>Bad:</i> There are issues with LCL event loop. Some of them
    (not being able to get Update events continously) are in bare LCL,
    some of them (the need to call Application.Run, not just loop
    using Application.ProcessMessages) are specific to LCL-Carbon.
    The former (Update issues when using mouse look or
    dragging with mouse) is somewhat workarounded on our side now
    (to not "stutter" when using mouse look), but the problem is still
    noticeable (mouse look under other TCastleWindow backends is much smoother).

    <p>For the above reasons:
    <ul>
      <li><b>If you make normal game (that doesn't need
        any menu or dialog boxes)</b> consider using
        the (default) CASTLE_WINDOW_XLIB backend on Mac OS X.
        Using X11 (Xlib) is not a problem on Mac OS X.
        And this way you get a perfect speed and smoothly working event loop
        (with smooth mouse look).

        <p>Although full-screen is ugly on Xlib,
        there's no way to hide dock over fullscreen application in this situation...
        Easiest solution in this case is to keep your game windowed on Mac OS X.

      <li>Only <b>if you make tool-like program (that needs menu and dialog
        boxes)</b> then CASTLE_WINDOW_LCL is a good choice.
        It will look perfectly native. Although at the cost of not perfect mouse look.
        Alternatively, you can use CASTLE_WINDOW_GTK_2 on Mac OS X,
        that has both nice menu/dialogs and a smooth mouse look,
        but it requires installation of some special libraries
        (GTK etc. from fink).
    </ul>
</ul>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p><b>For Mac OS X older than Tiger (that is, 10.3 or older):
    install OpenAL to have game sounds</b>. It may be downloaded from
    Creative, see download links from
    <a href="http://connect.creativelabs.com/openal/Downloads/Forms/AllItems.aspx">openal.org downloads</a>.
    For Mac OS X since Tiger, OpenAL comes already preinstalled.
    Even without OpenAL installed, all our programs will still work fine,
    you just will not get any sound.</p>

  <li><p><b>Install vorbisfile library to read OggVorbis sound/music files</b>.
    It may be installed using
    <a href="https://www.macports.org/">MacPorts</a>,
    <a href="https://brew.sh/">Homebrew</a> or
    <a href="http://www.finkproject.org/">Fink</a>.

    <p>Without vorbisfile, all our programs will still work fine, but you will not
    hear OggVorbis music.</p></li>

  <li><p>If you decide to use <i>external libpng implementation</i>
    in your programs, that you will also need <b>libpng
    to open and save images and textures in PNG format</b>.
    It may be installed using
    <a href="https://www.macports.org/">MacPorts</a>,
    <a href="https://brew.sh/">Homebrew</a> or
    <a href="http://www.finkproject.org/">Fink</a>.

    <p>Note that <b>this is not necessary by default</b>.
    By default, we use <i>FpImage</i> right now to read PNG,
    and it works without the need for libpng.
    You can edit the <code>castle-engine/src/base/castleconf.inc</code>
    configuration file to change this.
</ul>

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

<p>To actually compile our programs on Mac OS X, you need
the <a href="http://freepascal.org/">Free Pascal Compiler</a>.
For comfortable RAD development, install also
<a href="http://www.lazarus.freepascal.org/">Lazarus</a> (FPC is already included
inside Lazarus installations, if you choose normal packages).</p>

<p>You can install them by downloading from their respective webpages,
or you can install them by a Mac OS X package manager like
<a href="https://www.macports.org/">MacPorts</a>,
<a href="https://brew.sh/">Homebrew</a> or
<a href="http://www.finkproject.org/">Fink</a>.

<p>Besides easy upgrades, the advantage of using the package manager to install
the compiler is that it's easier then to install the libraries
(like <i>GTK2</i> and <i>GtkGLExt</i>)
with the same architecture (32-bit vs 64-bit).
Otherwise, be careful to watch for it yourself: your system may host
both 32-bit and 64-bit binaries and libraries, but <i>to compile a 64-bit
application, you will need a compiler (FPC) that targets 64-bit processors
<b>and</b> 64-bit libraries</i>.

<p>On modern Mac OS X versions, you usually just want to install x86_64 libraries,
and the FPC compiler for Darwin+x86_64.</p>

<?php echo $toc->html_section(); ?>

<p>To install GTK, GtkGLExt and FPC, simply do:

<pre>
sudo port install gtkglext
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

<p>Installing additional libraries, like GTK or GtkGLExt, is equally trivial.

<!--
Also, brew has now FPC 3.0.0, while MacPorts only 2.6.4.

  You can also install gtkglext using brew, and reference it like above.
  TODO: untested.
  You can even mix MacPorts and brew - - use gtkglext from MacPorts, fpc from brew.
-->

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
    <!-- as of fink 0.29.21 (on Mac OS X 10.6.7), --> you should additionally install
    the fink "<code>pango1-xft2-shlibs</code>" package. Simple "<code>fink install pango1-xft2-shlibs</code>"
    should do the trick. This is necessary for successful linking.</p>

  <li><p>If you use any libraries from fink, the linker must know their locations.
    Add this line to your <code>fpc.cfg</code> file (<?php echo FPC_CFG_DOCS; ?>):</p>

<pre>
-Fl/sw/lib/
</pre>
</ul>

<?php echo $toc->html_section(); ?>

<p>A bunch of information about packaging your programs for Mac OS X follows.

<!--
Note that our engine doesn't force you to do anything special to distribute
your programs &mdash; you get a normal (Unix) binary that you can use however you like.

use any packaging, the rules below
are just general rules for all Mac OS X programs.
make any constraints here.
You compile and package your program as usual, using FPC or Lazarus,
and  that you package however you like.</p>
-->

<ol>
  <li><p><i>Make "Mac OS X bundle"</i>. It's basically a directory pretending
    to be an application.

    <p>You can use Lazarus to create Mac OS X bundle.
    Or you can use our <a href="https://github.com/castle-engine/cge-scripts/blob/master/create_macosx_bundle.sh">create_macosx_bundle.sh script</a>.
    The example usage is inside view3dscene and glViewImage sources.</p>
    </li>

  <li><p>Optionally, <i>add libraries (like libpng and vorbisfile) to the bundle</i>.
    If you link to them dynamically (e.g. using our <code>TDynLib.Load</code>),
    you should load them from a path relative to <code>BundlePath</code>, like
    <code>BundlePath + 'Contents/MacOS/libpng.dylib'</code>.

    <p>See
    <a href="http://wiki.freepascal.org/OS_X_Programming_Tips#Mac_OS_X_Libraries">Mac OS X Libraries on FPC wiki</a>
    for general instructions how to include library inside a bundle.</p>
    </li>

  <li><p>Pack into a nice .dmg file.
    See <a href="http://el-tramo.be/guides/fancy-dmg/">Building Fancy DMG Images on Mac OS X</a>
    for nice description how to make the directories inside dmg look pretty,
    so you can visually suggest user to drag your application in the <i>Applications</i>
    folder.

    <p>Alternative method of distribution Mac OS X applications is the
    <a href="http://wiki.freepascal.org/Deploying_Your_Application#Using_PackageMaker_on_Mac_OS_X">package manager (.pkg)</a>.
    For normal applications (like games) the simpler .dmg is a better choice.
    </li>
</ol>

</div> <!-- class="fixed-width-content" -->

<?php
  castle_footer();
?>
