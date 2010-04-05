<?php
  require_once 'vrmlengine_functions.php';

  common_header("glinformation", LANG_EN,
    'Output OpenGL information. This tiny program queries your OpenGL implementation (which is normally related to your graphic card model and drivers), printing information about your OpenGL version and features.');
?>

<?php echo pretty_heading("glinformation", VERSION_GLINFORMATION); ?>
<h4>Print OpenGL capabilities</h4>

<!-- ============================================================= -->

<p>This tiny program tests that OpenGL library is installed and
writes on standard output a lot of information about default
OpenGL context.

<ul>
  <li>If you don't know what OpenGL is then you probably don't need to
    use this program. Unless some developer asked you to use glinformation
    to check do you have a good OpenGL implementation installed.</li>

  <li>If you know what OpenGL is then you probably realize how trivial
    this program is :) All is done by creating a temporary
    window with OpenGL context and asking for many values with glGet*
    functions.</li>
</ul>

<p>Download glinformation:
<?php echo_standard_program_download('glinformation', 'glinformation', VERSION_GLINFORMATION,
  $std_releases_post_1_8_0); ?>

<p><?php echo SOURCES_OF_THIS_PROG_ARE_AVAIL; ?>

<h3>Optional command-line options</h3>

<p>You can call glinformation with some of the options listed below.
Requested OpenGL context will have given capabilities
(in case of bit sizes, you provide the <i>minimum</i>
requested bit size). If you know OpenGL, then the meaning
of these options should be self-explanatory. They are useful to check
e.g. is your graphic card able to provide 16-bit stencil buffer.

<pre>
  -s / --stencil-bits STENCIL-BUFFER-BIT-SIZE
  -a / --alpha-bits ALPHA-CHANNEL-BIT-SIZE
  -d / --depth-bits DEPTH-BUFFER-BIT-SIZE
  --accum-red-bits ACCUM-RED
  --accum-green-bits ACCUM-GREEN
  --accum-blue-bits ACCUM-BLUE
  --accum-alpha-bits ACCUM-ALPHA
  --accum-bits ACCUM-RED ACCUM-GREEN ACCUM-BLUE ACCUM-ALPHA
  --multi-sampling SAMPLES
    (1 means "no multisampling")
  --single
  --double
</pre>

<p>Also all
<?php echo a_href_page("standard options understood by my OpenGL programs",
"opengl_options") ?> are allowed, see also <?php echo a_href_page(
"some notes about command-line options understood by my programs",
"common_options") ?>.

<h3>GLUT versions</h3>

I don't use GLUT in my programs. But, just in case (since
glinformation is intended to be a diagnostical program, that may be used
in case there is some error somewhere &mdash; maybe in my own code),
I provide also a special GLUT version of glinformation. This initializes
OpenGL context using GLUT, then writes the same info as normal
version.

<?php echo_standard_program_download('glinformation_glut', 'glinformation_glut',
  VERSION_GLINFORMATION_GLUT, $std_releases_post_1_8_0); ?>

<p><?php echo SOURCES_OF_THIS_PROG_ARE_AVAIL; ?>

<h3>glxinfo notes</h3>

<p>XWindows users can also use <tt>glxinfo</tt> program.
It's purpose is similar to glinformation (but it's only for GLX,
so it's able to write some info like all possible visual configurations).

<?php
/* Too short to mention, but actually:
 echo depends_ul( array(
  DEPENDS_OPENGL,
  (and the glut version obviously depends on glut),
  DEPENDS_MACOSX) );
*/
?>

<?php
  if (!IS_GEN_LOCAL) {
    $counter = php_counter("glinformation", TRUE);
  };

  common_footer();
?>
