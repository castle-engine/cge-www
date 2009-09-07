<?php
  require_once "vrmlengine_functions.php";
  common_header("Dependencies of my programs on Mac OS X", LANG_EN, '');
?>

<h1>Dependencies of my programs on Mac OS X</h1>

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

<?php
  if (!IS_GEN_LOCAL) {
    $counter = php_counter("macosx_requirements", TRUE);
  };

  common_footer();
?>
