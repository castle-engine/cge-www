<?php
  require_once 'vrmlengine_functions.php';

  common_header("malfunction", LANG_EN, 'Small 3d game. ' .
    'You fly a spaceship and try to shoot down all alien ships.');

  $toc = new TableOfContents(
    array(
      new TocItem('Download', 'download'),
      new TocItem('Optional command-line options', 'options'),
      new TocItem('Things used when developing this game', 'external_resources')
    )
  );
?>

<?php
  echo pretty_heading("malfunction", VERSION_MALFUNCTION);
  echo default_medium_image_progs_demo("malfunction");
?>

<p>If you want, you can dream that you're a saviour of galaxy
or something like that. The truth is that
<ol>
  <li>You sit inside the most junky and malfunctioning space
    ship in the whole universe
  <li>Noone knows what's going on but there are some freakin'
    <b>alien spaceships</b> everywhere around, and
    <b>they just got down on you</b>
</ol>

<p><b>Malfunction</b> is a small 3D game made by Michalis Kamburelis.

<?php echo $toc->html_section(); ?>

<?php echo_standard_program_download(
  'malfunction', 'malfunction', VERSION_MALFUNCTION, true); ?>

<p>Installing:</p>

<dl>
  <dt>Linux, FreeBSD and Mac OS X users:</dt>

  <dd>Extract downloaded archive to
    <tt>/usr/local/share/malfunction/</tt> or <tt>$HOME/.malfunction.data/</tt>.
    You can move or symlink the executable <tt>malfunction</tt> to any place
    you like (e.g. <tt>$HOME/bin</tt> or <tt>/usr/local/bin</tt>).
    Run the game by running <tt>malfunction</tt>.</dd>

  <dt>Windows users:</dt>

  <dd>Extract downloaded archive to any directory.
    Run the game by running <tt>malfunction.exe</tt>.
</dl>

<p><?php echo SOURCES_OF_THIS_PROG_ARE_AVAIL; ?>

<p>When you run the game, first thing to do is
read the "<i>short game instructions</i>".
They tell you what keys are available during the game etc.

<?php echo $toc->html_section(); ?>

<p>By default, program will run in window mode, unless
your screen resolution is 640x480, in which case fullscreen
mode will be used. You can change this by using
<?php echo a_href_page("standard options understood by my OpenGL programs.",
"opengl_options") ?>. E.g.
<pre>
  malfunction --geometry 400x300
</pre>
will force using window sized 400x300.
<pre>
  malfunction --fullscreen
</pre>
will force using fullscreen mode (in it's current resolution).
<pre>
  malfunction --fullscreen-custom 1024x768
</pre>
will force using fullscreen mode in 1024x768 resolution.

<p>Although every window and screen size are acceptable, the game
looks best in 640x480 or 800x600 resolution.

<p>See also <?php echo a_href_page(
"notes about command-line options understood by my programs", "common_options") ?>.

<?php
/* Too little to show this to user:

  echo '<p>' . DEPENDS . ':';
  echo depends_ul(array(
    DEPENDS_OPENGL,
    DEPENDS_LIBPNG_AND_ZLIB,
    DEPENDS_MACOSX));
*/
?>

<?php echo $toc->html_section(); ?>

<p>Compiled by <a href="http://www.freepascal.org/">Free Pascal Compiler (FPC)</a>.
PNG graphics are loaded thanks to <a href="http://www.libpng.org">libpng</a>,
JPEG graphics are loaded thanks to a modified version of
<a href="http://www.nomssi.de/pasjpeg/pasjpeg.html">
pasjpeg</a> (which is the translated to Pascal version of
original ANSI C code written by
<a href="http://www.ijg.org/">Independent JPEG Group</a>).

<p>Skies in the files <tt>skies/lake_sky*</tt> and <tt>skies/foggy_sky*</tt>
are generated using a cute program
<a href="http://www.planetside.co.uk/">Terragen</a>.
<tt>skies/thespace</tt> made from graphics from
<a href="http://www.grsites.com">http://www.grsites.com</a>,
<tt>textures/water002.jpg</tt> is also from there.
<tt>textures/metal{5,6}</tt>, <tt>textures/rock{2,6,8}</tt> from
<a href="http://www.amazing3d.com">http://www.amazing3d.com</a>
(from <i>free</i> textures),
<tt>textures/bridger_road_top.jpg</tt> from
<a href="http://www.wolfiesden.com/golgotha/golgotha.asp">public domain Golgotha textures</a>.
Menu items use "<i>I suck at golf</i>" font from
<a href="http://fonts.tom7.com/">[Divide by zero]</a>.

<p>The rest of graphics were made by me, mostly using
<a href="http://www.gimp.org/">GIMP</a>.

<p>"<i>Tie fighter</i>" and "<i>destroyer</i>" models are modified
versions of <tt>tieftr.wrl</tt> and <tt>station.3ds</tt> from
<a href="http://www.3dcafe.com">http://www.3dcafe.com</a>
(from <i>free</i> models), the rest of VRMLs made by me by hand
or using <a href="http://www.blender.org/">Blender</a>
(and processed by some Emacs macros).

<?php
  if (!IS_GEN_LOCAL) {
    $counter = php_counter("malfunction", TRUE);
  };

  common_footer();
?>
