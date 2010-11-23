<?php
  require_once "vrmlengine_functions.php";
  vrmlengine_header("Dependencies of my programs on Mac OS X", NULL, array('other'));

  $toc = new TableOfContents(
    array(
      new TocItem('What do you need to use my programs on Mac OS X now', 'requirements_now'),
      new TocItem('Developers: help wanted for making more native Mac OS X port', 'help_wanted'),
    )
  );
  $toc->echo_numbers = true;

  echo pretty_heading($page_title);
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>Library requirements of using my engine on Mac OS X:

<ul>
  <li><p>For OpenGL programs, <b>X11</b> (may be found on Mac OS X install CD).</p>

    <p>And you should run my programs from within X11 xterm, to use your display.
    Or you will have to specify your X display by command-line option explicitly,
    like <tt>--display=:0</tt></p>

    <p>This requirement will be removed if at some time I'll implement
    GLWindow unit using some native Mac OS X libraries.
    Contributions are most welcome if you're experienced with programming
    under Mac OS X !</p></li>

  <li><p><b>libpng</b> (may be installed by
    <a href="http://www.finkproject.org/">fink</a>),
    if you want to be able to open any
    images/textures etc. stored in PNG images.</p></li>

  <li><p>When GLWindow uses GTK backend (view3dscene compiled by default script
    does use GTK, "The Castle" compiled by default script doesn't use GTK):
    <b>GTK and GtkGLExt</b> are required.
    May be installed by <a href="http://www.finkproject.org/">fink</a>
    (look for packages <tt>gtk+2</tt>, <tt>gtkglext1</tt>).

    <p><i>Note:</i> Binary <tt>gtk+2</tt> packages in fink may be too old.
    If you get errors on the console like</p>

<pre>
  dyld: Library not loaded: /sw/lib/libgtk-x11-2.0.0.dylib
  Referenced from: /xxx/view3dscene
  Reason: Incompatible library version: view3dscene requires version 601.0.0 or later, but libgtk-x11-2.0.0.dylib provides version 401.0.0
</pre>

    <p>then try installing gtk from source packages, they have newer version
    (or bug fink to update binary packages more often :) ).
    It is <i>not</i> necessary to use any fink packages from unstable
    branch !</p></li>

  <li><p><b>OpenAL</b> for game sound (may be downloaded from Creative,
    see download links from
    <a href="http://connect.creativelabs.com/openal/Downloads/Forms/AllItems.aspx">openal.org downloads</a>,
    newer Mac OS X has this pre-installed ?)
    along with <b>vorbisfile</b> (may be installed by
    <a href="http://www.finkproject.org/">fink</a>).</p></li>
</ul>

<p>For libs installed by fink, you may have to add them to your libs
path by command like
<pre>
  export LD_LIBRARY_PATH=/sw/lib:"$LD_LIBRARY_PATH"
</pre>
before executing programs. (you can add this to your <tt>.bashrc</tt>
or similar file for comfort).

<?php echo $toc->html_section(); ?>

<p>The current way my engine and programs (view3dscene, castle etc.)
work on Mac OS X is admittedly a simple and straightforward port from Linux.
It doesn't look natively (like other normal Mac OS X programs),
it requires some uncommon libraries (not only X11, but also GTK and GtkGLExt
from fink) and so on. If you're a developer familiar with Mac OS X
native toolkit (as far as I know, this means
<a href="http://wiki.freepascal.org/Carbon_Interface">Carbon</a>
or <a href="http://wiki.freepascal.org/Cocoa_Interface">Cocoa</a>)
and <a href="http://www.freepascal.org/">FreePascal</a> &mdash; you're most
welcome to help.</p>

<p>The plan:</p>

<ol>
  <li><p><i>Port our <tt>GLWindow</tt> unit to native Mac OS X toolkit</i>.
    This is the main work, it will make dependencies on X11, GTK, GtkGLExt
    disappear and will provide a native look.</p>

    <p>It's a matter of creating and implementing a file like
    <tt>kambi_vrml_game_engine/src/glwindow/glwindow_(cocoa|carbon).inc</tt>,
    based on
    <tt>kambi_vrml_game_engine/src/glwindow/glwindow_implementation_template.inc</tt>.
    See <?php echo a_href_page('engine sources', 'kambi_vrml_game_engine'); ?>.
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
    to be an application). You can find view3dscene SVG icon in the sources.</p></li>

  <li><p>Optionally, <i>add libpng and vorbisfile libraries to the bundle</i>.
    We will still be able to link to them dynamically, as far as I know.
    libpng is required for PNG reading (unless new Mac OS X already includes it?).
    vorbisfile is required for OggVorbis playing (coming soon in view3dscene
    3.8.0, already available in castle).</p></li>

  <li><p>Finally, prepare a script to pack a nice .dmg (instead of current
    .tar.gz) distribution (of view3dscene, or castle &mdash; doesn't matter,
    I'll adjust it to be more general).</p></li>
</ol>

<p>Fame, fortune, and eternal gratitude from Michalis and all Mac OS X
users of our programs will be your reward :) At such moments,
I wish I had a kingdom and three daughters.</p>

<?php
  vrmlengine_footer();
?>
