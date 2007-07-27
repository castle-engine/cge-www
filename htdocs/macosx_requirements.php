<?php
  require "vrmlengine_functions.php";
  camelot_header("Dependencies of my programs on Mac OS X", LANG_EN, '');
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
    <b>GTK and GtkGLExt</b> are required (may be installed by
    <a href="http://www.finkproject.org/">fink</a>).</p></li>

  <li><p><b>OpenAL</b> for game sound (may be downloaded from Creative,
    see download links from
    <a href="http://www.openal.org/downloads.html">openal.org downloads</a>,
    newer Mac OS X has this pre-installed ?)
    along with <b>vorbisfile</b> (may be installed by
    <a href="http://www.finkproject.org/">fink</a>).</p></li>
</ul>

<p>For libs installed by fink, you may have to add them to your libs
path by command like
<pre>
  export LD_LIBRARY_PATH=/sw/lib:"$LD_LIBRARY_PATH"
</pre>
before executing programs. (of course you can add this to your <tt>.bashrc</tt>
file or whatever).

<?php
  if (!IS_GEN_LOCAL) {
    $counter = php_counter("macosx_requirements", TRUE);
  };

  camelot_footer();
?>
