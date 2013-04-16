<?php
  require_once "castle_engine_functions.php";
  castle_header("Dependencies of our programs on Mac OS X", NULL, array('all_programs'));

  $toc = new TableOfContents(
    array(
      new TocItem('Mac OS X Carbon applications (new)', 'carbon'),
        new TocItem('Developers: Technical details', 'carbon_details', 1),
        new TocItem('Console', 'console', 1),
      new TocItem('Mac OS X GTK2 applications (old)', 'gtk'),
        new TocItem('Dependencies to install', 'requirements', 1),
        new TocItem('Developers: additional stuff to install/configure', 'developers_libs', 1),
      new TocItem('Developers: help wanted', 'help_wanted'),
    )
  );
  $toc->echo_numbers = true;

  echo pretty_heading($page_title);
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>Since <?php echo a_href_page('engine', 'engine'); ?> version 4.1.0
(<?php echo a_href_page('view3dscene', 'view3dscene'); ?> 3.13.0),
our applications have a native look on Mac OS X, and do not need
any extra dependencies (like X11 and GTK).

<?php echo $toc->html_section(); ?>

<p>We did it by using the LCL backend of <tt>CastleWindow</tt>. This uses
<a href="http://www.lazarus.freepascal.org/">Lazarus</a> LCL under the hood,
wrapping Lazarus <tt>TForm</tt> and <tt>TOpenGLControl</tt> inside a
<tt>TCastleWindow</tt>.
Although it still has some issues (see below), it gives us native look
and a lot of stuff "for free".

<p>Developers: When compiling programs under Mac OS X,
remember to add <i>castle_components</i> Lazarus package
to the requirements of the <i>castle_window</i> package.
This is a necessary manual step, as Lazarus does not allow (yet)
to define a package requirement that is specific to given OS.

<p>This change allows <tt>CastleWindow</tt>-based programs,
like <?php echo a_href_page("view3dscene", "view3dscene") ?>,
to have native look on Mac OS X.
Of course in your own programs you can also use Lazarus
forms directly (with our <tt>TCastleControl</tt>) &mdash; this was always possible,
and gives you the same native look through <a href="http://www.lazarus.freepascal.org/">Lazarus</a>.

<p>On Mac OS X, the default LCL widgetset is
<a href="http://wiki.freepascal.org/Carbon_Interface">Carbon</a> right now.</p>

<ul>
  <li><p>Good: native look, application has a normal menu bar,
    shows native dialog boxes (to open/save file, choose color and such)
    and generally looks and feels like every other Mac OS X application.
    Lazarus compiles it into a <i>bundle</i> like <tt>view3dscene.app</tt>
    that can be easily installed by dragging to your <i>Applications</i>
    directory.

  <li><p>Good: no extra dependencies, Carbon is already part of every
    Mac OS X installation. (No dependencies on X11, GTK, GTKGlExt etc.)

  <li><p>Bad: Unfortunately, Carbon is deprecated by Apple
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
    <tt>CastleWindow</tt> backend based on Cocoa (without using Lazarus LCL):
    <ul>
      <li>It's a matter of creating and implementing a file
        <tt>castle_game_engine/src/window/castlewindow_cocoa.inc</tt>,
        based on
        <tt>castle_game_engine/src/window/castlewindow_backend_template.inc</tt>.
        See <?php echo a_href_page('engine sources', 'engine'); ?>.
        See at other "backends" (like GTK, WinAPI, Xlib, LCL)
        for examples how to implement such thing, everything is inside
        <tt>src/window/</tt> dir.
      <li>Alternatively, send Michalis a simple and clear example of FPC program
	using Cocoa that 1. creates and shows a window
	2. with menu bar 3. and with OpenGL context area covering the window.
	I should be able to port such example to my "CastleWindow" then.
      <li>See e.g. <a href="http://wiki.freepascal.org/OS_X_Programming_Tips">FPC "OS_X Programming Tips"</a>
        for pointers.
	If you're a developer familiar with Mac OS X
	native toolkit and
	<a href="http://www.freepascal.org/">FreePascal</a>, your help
	will be much appreciated.
    </ul>

  <li><p>Bad: There are issues with LCL event loop. Some of them
    (not being able to get Update events continously) are in bare LCL,
    some of them (the need to call Application.Run, not just loop
    using Application.ProcessMessages) are specific to LCL-Carbon.
    The former (Update issues when using mouse look or
    dragging with mouse) is somewhat workarounded on our side now
    (to not "stutter" when using mouse look), but the problem is still
    noticeable (mouse look under other TCastleWindow backends is much smoother).

    <p>For this reason, if you make normal game (that doesn't need
    any menu or dialog boxes) you may still consider using
    CASTLE_WINDOW_X11 backend instead of CASTLE_WINDOW_LCL &mdash;
    the X11 is pretty easy to install on Mac OS X.
    For normal tools (that need menu and dialog boxes)
    CASTLE_WINDOW_LCL is probably already better than CASTLE_WINDOW_GTK,
    thanks to not needing difficult dependencies (GTK etc. from fink).
</ul>

<?php echo $toc->html_section(); ?>

<p><?php echo a_href_page('view3dscene', 'view3dscene'); ?> uses a standard OS console
for some stuff (e.g. for <i>Console -&gt; Print Current Camera...</i>). You will not see this
console if you run the program using the bundle (by double-clicking on the app
in Finder, or in dock...).

<p>To see the console output, run view3dscene from terminal:

<pre class="bordered_code">
cd directory-where-you-installed-view3dscene/
./view3dscene.app/Contents/MacOS/view3dscene
</pre>

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

<p>Before you run our programs on Mac OS X, you may need to install
some additional software:

<ul>
  <li><p>For OpenGL programs, <b>X11</b> is needed (may be found on Mac OS X install CD).
    Install it, and run before executing our programs.</p>

    <p>Our programs will appear as part of "<i>X11 server</i>" on your desktop.
    If you start our program outside of X11
    (like by double-clicking the application
    in the <i>Finder</i>), the window may not be "focused"
    (receiving key events) at start. Just click the window to make it focused.</p>

    <p><i>Somewhat internal notes:</i> if you start our program from within
    X11 xterm, it will be focused at the start. When you start us outside of X11,
    and we detect that the default X display doesn't work,
    we automatically use display name <tt>:0</tt> under Mac OS X.
    This way we attach to the running X server, even if you execute us from some
    other application.
    <i>For programs released before 2011-01-10: you should explicitly start
    with command-line option <tt>--display=:0</tt>, or always run us from X11 xterm.</i></p></li>

  <li><p><b>libpng</b> (may be installed by
    <a href="http://www.finkproject.org/">fink</a>),
    if you want to be able to open any
    images/textures etc. stored in PNG images.</p></li>

  <li><p>When CastleWindow uses GTK backend (view3dscene compiled by default script
    does use GTK, "The Castle" compiled by default script doesn't use GTK):
    <b>GTK and GtkGLExt</b> are required.
    They should be installed using <a href="http://www.finkproject.org/">fink</a>.
    Follow fink installation instructions, then
    simple command "<tt>fink install gtkglext1</tt>" should install all
    the libraries we require (if in trouble, look for packages by "<tt>fink list xxx</tt>").</p>

    <p>Note that binary fink packages are usually too old (if available at all...).
    So just go with the "source" installation. Don't be afraid if you're not a programmer
    &mdash; the process goes smoothly, just follow the instructions
    and be patient.</p>

    <p>It is <i>not</i> necessary to use any fink packages from unstable
    branch.</p></li>

  <li><p><b>vorbisfile</b> (may be installed by
    <a href="http://www.finkproject.org/">fink</a>).
    Without this, all our programs will still work fine, but you will not
    hear OggVorbis music.</p></li>

  <li><p><b>For Mac OS X older than Tiger (that is, 10.3 or older):
    get OpenAL</b> for game sound. It may be downloaded from
    Creative, see download links from
    <a href="http://connect.creativelabs.com/openal/Downloads/Forms/AllItems.aspx">openal.org downloads</a>.
    For Mac OS X since Tiger, OpenAL comes already preinstalled.
    Even without OpenAL installed, all our programs will still work fine,
    you just will not get any sound.</p>
</ul>

<p>For libs installed by fink, you may have to add them to your libs
path by command like
<pre class="bordered_code">
  export LD_LIBRARY_PATH=/sw/lib:"$LD_LIBRARY_PATH"
</pre>
before executing programs. (you can add this to your <tt>.bashrc</tt>
or similar file for comfort).

<?php echo $toc->html_section(); ?>

<p>To actually compile our programs on Mac OS X, developers may need to install
a little more stuff. Of course <a href="http://freepascal.org/">FreePascal Compiler</a>
is required. For comfortable RAD development, you may also consider
<a href="http://www.lazarus.freepascal.org/">Lazarus</a> (FPC is already included
inside Lazarus installations, if you choose normal packages).</p>

<p>As of fink 0.29.21 (on Mac OS X 10.6.7), you should additionally install
the fink "<tt>pango1-xft2-shlibs</tt>" package. Simple "<tt>fink install pango1-xft2-shlibs</tt>"
should do the trick. This is necessary for successful linking.</p>

<p>The linker must know the location of fink and X11
libraries. If you have installed fink and X11 in standard locations,
you can simply add these lines to your <tt>/etc/fpc.cfg</tt> file:</p>

<pre class="sourcecode">
-Fl/sw/lib/
-Fl/usr/X11/lib/
</pre>

<?php echo $toc->html_section(); ?>

<p>The plan:</p>

<ol>
  <li><p><i>Make "Mac OS X bundle"</i> (it's basically a directory pretending
    to be an application). You can find view3dscene SVG icon in the sources.</p>

    <p>Actually, this step is already somewhat done at least for view3dscene.
    A script <tt>create_macosx_bundle.sh</tt> will make a nice view3dscene.app,
    that you should be able to execute. Unfortunately, it doesn't really work yet.

    <p>You can also just try using Lazarus bundle creator.
    </li>

  <li><p>Optionally, <i>add libpng and vorbisfile libraries to the bundle</i>.
    We will still be able to link to them dynamically, as far as I know.
    libpng is required for PNG reading (unless new Mac OS X already includes it?).
    vorbisfile is required for OggVorbis playing (used by some castle
    music tracks, used by view3dscene <tt>Sound</tt> demos).</p>

    <p>See
    <a href="http://wiki.freepascal.org/OS_X_Programming_Tips#Mac_OS_X_Libraries">Mac OS X Libraries on FPC wiki</a>
    for instructions how to include library inside a bundle.</p>
    </li>

  <li><p>Finally, prepare a script to pack a nice .dmg (instead of current
    .tar.gz) distribution (of view3dscene, or castle &mdash; doesn't matter,
    I'll adjust it to be more general).</p>

    <p>See http://el-tramo.be/guides/fancy-dmg/
    Note: we do not need .pkg (package manager),
    http://wiki.freepascal.org/Deploying_Your_Application#Using_PackageMaker_on_Mac_OS_X
    </li>
</ol>

<?php
  castle_footer();
?>
