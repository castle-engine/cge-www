<?php
  require_once "castle_engine_functions.php";
  castle_header("Dependencies of my programs on Mac OS X", NULL, array('all_programs'));

  $toc = new TableOfContents(
    array(
      new TocItem('What do you need to use my programs on Mac OS X now', 'requirements_now'),
      new TocItem('Developers: additional stuff to install/configure', 'developers_libs'),
      new TocItem('Developers: help wanted for making more native Mac OS X port', 'help_wanted'),
    )
  );
  $toc->echo_numbers = true;

  echo pretty_heading($page_title);
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

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

  <li><p>When GLWindow uses GTK backend (view3dscene compiled by default script
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
    Without this, all my programs will still work fine, you just will not
    hear OggVorbis music.</p></li>

  <li><p><b>For Mac OS X older than Tiger (that is, 10.3 or older):
    get OpenAL</b> for game sound. It may be downloaded from
    Creative, see download links from
    <a href="http://connect.creativelabs.com/openal/Downloads/Forms/AllItems.aspx">openal.org downloads</a>.
    For Mac OS X since Tiger, OpenAL comes already preinstalled.
    Even without OpenAL installed, all my programs will still work fine,
    you just will not get any sound.</p>
</ul>

<p>For libs installed by fink, you may have to add them to your libs
path by command like
<pre>
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

<p>The current way my engine and programs (view3dscene, castle etc.)
work on Mac OS X is admittedly a simple and straightforward port from Linux.
It doesn't look natively (like other normal Mac OS X programs),
it requires some uncommon libraries (not only X11, but also GTK and GtkGLExt
from fink) and so on. If you're a developer familiar with Mac OS X
native toolkit and
<a href="http://www.freepascal.org/">FreePascal</a> &mdash; you're most
welcome to help.</p>

<p><i>Mac OS X native toolkit</i> as far as I know means
<a href="http://wiki.freepascal.org/Carbon_Interface">Carbon</a>
or <a href="http://wiki.freepascal.org/Cocoa_Interface">Cocoa</a>.
<a href="http://lists.lazarus.freepascal.org/pipermail/lazarus/2010-December/058470.html">This thread on Lazarus mailing list</a> (these are people actually making native Mac OS X stuff using FPC, so they know what they are talking about :) suggests the <i>Cocoa</i> is the right choice.</p>

<p>The plan:</p>

<ol>
  <li><p><i>Port our <tt>GLWindow</tt> unit to native Mac OS X toolkit</i>.
    This is the main work, it will make dependencies on X11, GTK, GtkGLExt
    disappear and will provide a native look.</p>

    <p>It's a matter of creating and implementing a file like
    <tt>castle_game_engine/src/glwindow/glwindow_(cocoa|carbon).inc</tt>,
    based on
    <tt>castle_game_engine/src/glwindow/glwindow_backend_template.inc</tt>.
    See <?php echo a_href_page('engine sources', 'engine'); ?>.
    See at other "backends" (currently GTK, WinAPI, Xlib, Glut are available)
    for examples how to implement such thing, everything is inside
    <tt>src/glwindow/</tt> dir.</p>

    <p>Alternatively, send me a simple and clear example of FPC program
    using Carbon/Cocoa that 1. creates and shows a window
    2. with menu bar 3. and with OpenGL context area covering the window.
    I should be able to port such example to my "GLWindow" then.</p>

    <p>See e.g. <a href="http://wiki.freepascal.org/OS_X_Programming_Tips">FPC "OS_X Programming Tips"</a>
    for pointers</p></li>

  <li><p><i>Make "Mac OS X bundle"</i> (it's basically a directory pretending
    to be an application). You can find view3dscene SVG icon in the sources.</p>

    <p>Actually, this step is already somewhat done at least for view3dscene.
    A script <tt>create_macosx_bundle.sh</tt> will make a nice view3dscene.app,
    that you should be able to execute. Unfortunately, it doesn't really work yet,
    and probably would be awkward anyway as our window is part of X server.
    But anyway, once above step is done, finishing the "bundle" work is probably trivial.</p>
    </li>

  <li><p>Optionally, <i>add libpng and vorbisfile libraries to the bundle</i>.
    We will still be able to link to them dynamically, as far as I know.
    libpng is required for PNG reading (unless new Mac OS X already includes it?).
    vorbisfile is required for OggVorbis playing (used by some castle
    music tracks, used by view3dscene <tt>Sound</tt> demos).</p></li>

  <li><p>Finally, prepare a script to pack a nice .dmg (instead of current
    .tar.gz) distribution (of view3dscene, or castle &mdash; doesn't matter,
    I'll adjust it to be more general).</p></li>
</ol>

<p>Fame, fortune, and eternal gratitude from Michalis and all Mac OS X
users of our programs will be your reward :) At such moments,
I wish I had a kingdom and three daughters.</p>

<?php
  castle_footer();
?>
