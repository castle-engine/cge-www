<?php
define('CASTLE_GITHUB_NAME', 'malfunction');

require_once 'castle_engine_functions.php';
castle_header("malfunction", array(
  'meta_description' => 'Small 3D game. You fly a spaceship and shoot down alien ships.',
));

$toc = new TableOfContents(
  array(
    new TocItem('Download', 'download'),
    new TocItem('Optional command-line options', 'options'),
    new TocItem('Things used when developing this game', 'external_resources')
  )
);

echo pretty_heading("malfunction");
echo $toc->html_toc();
echo default_program_thumbnail("malfunction");
?>

<p><b>Malfunction</b> is a small 3D game made by Michalis Kamburelis
with a very very early <i>Castle Game Engine</i> version.

<p>You sit inside the most malfunctioning space ship in the whole universe.
You are surrounded by alien spaceships. Destroy them all!

<?php echo $toc->html_section(); ?>

<?php
echo cge_download_application(
  '1.3.0',
  'snapshot',
  'castle-engine',
  'malfunction',
  'malfunction',
  array(
    'win64-x86_64',
    'linux-x86_64'
  )
);
?>

<p><i>Installation</i>: just extract the downloaded archive anywhere.
Run the game binary (<code>./malfunction</code> on Unix,
<code>malfunction.exe</code> on Windows).</p>

<p><?php echo castle_sources_notice(); ?>

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
    DEPENDS_LIBPNG_AND_ZLIB));
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

<p>Skies in the files <code>skies/lake_sky*</code> and <code>skies/foggy_sky*</code>
are generated using a cute program
<a href="http://www.planetside.co.uk/">Terragen</a>.
<code>skies/thespace</code> made from graphics from
<a href="http://www.grsites.com">http://www.grsites.com</a>,
<code>textures/water002.jpg</code> is also from there.
<code>textures/metal{5,6}</code>, <code>textures/rock{2,6,8}</code> from
<a href="http://www.amazing3d.com">http://www.amazing3d.com</a>
(from <i>free</i> textures),
<code>textures/bridger_road_top.jpg</code> from
<a href="http://www.wolfiesden.com/golgotha/golgotha.asp">public domain Golgotha textures</a>.
Menu items use "<i>I suck at golf</i>" font from
<a href="http://fonts.tom7.com/">[Divide by zero]</a>.

<p>The rest of graphics were made by me, mostly using
<a href="http://www.gimp.org/">GIMP</a>.

<p>"<i>Tie fighter</i>" and "<i>destroyer</i>" models are modified
versions of <code>tieftr.wrl</code> and <code>station.3ds</code> from
<a href="http://www.3dcafe.com">http://www.3dcafe.com</a>
(from <i>free</i> models), the rest of VRMLs made by me by hand
or using <a href="http://www.blender.org/">Blender</a>
(and processed by some Emacs macros).

<?php
  castle_footer();
?>
