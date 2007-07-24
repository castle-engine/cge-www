<?php
  require "camelot_funcs.php";
  require_once 'vrmlengine_functions.php';

  camelot_header("Kambi VRML game engine", LANG_EN);

  $toc = new TableOfContents(
    array(
      new TocItem('Download sources', 'download_src'),
      new TocItem('Documentation', 'docs'),
      new TocItem('Automatic tests', 'tests'),
      new TocItem('Author', 'author')
    )
  );
?>

<?php echo pretty_heading('Kambi VRML game engine',
  VERSION_KAMBI_VRML_GAME_ENGINE); ?>

<p>An open-source game engine written in ObjectPascal.
Features include:</p>

<ul>
  <li>Optimized OpenGL rendering of models in
    VRML 1.0 and 2.0 (aka VRML 97) formats.</li>
  <li>3DS file format is also supported.</li>
  <li>Animations are supported, by interpolation.</li>
  <li>Octrees are used for various collision detection tasks.</li>
  <li>Shadows by shadow volumes.</li>
  <li>GLWindow unit is available to easily create windows with OpenGL context.</li>
  <li>Reading and writing of images in various formats, processing them
    and using as OpenGL textures.</li>
  <li>Handling of fonts, including rendering them with OpenGL,
    as bitmap or outline (3D) fonts.</li>
  <li>Many OpenAL helpers, including intelligent OpenAL sound manager
    and OggVorbis format handling.</li>
  <li>Ray-tracer based on VRML models is implemented.</li>
  <!-- <li>Evaluating mathematical expressions -->
  <!-- li>Curves handling.</li -->
</ul>

<p>The engine is portable, currently tested and used on Linux,
FreeBSD, Mac OS X and Windows. It was used to develop all programs on
these pages. It should be compiled by
<a href="http://www.freepascal.org">FreePascal</a>.
</p>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p><?php echo a_href_page('Download sources of the engine and many related
programs/demos', 'sources'); ?>.</p>

<?php echo $toc->html_section(); ?>

<p><?php echo a_href_page("My master's thesis describing how the VRML " .
"format is handled", 'vrml_engine_doc'); ?>.</p>

<p>There's also the <?php echo a_href_page('generated documentation of units',
'sources_docs'); ?>.</p>

<?php echo $toc->html_section(); ?>

<p>I'm managing a suite of automatic tests,
in the spirit of <a href="http://www.extremeprogramming.org/">Extreme Programming</a>.
On 2005-04-25 I converted my tests to use
<a href="http://camelos.sourceforge.net/fpcUnit.html">fpcunit</a>
(this is a close FPC analogy to <a href="http://www.junit.org/">JUnit for Java</a>)
and it's <a href="http://www.lazarus.freepascal.org/">Lazarus</a> GUI runner.

<p>The tests are included with the rest of engine sources,
see subdirectory <tt>tests/</tt>. This is a GUI program, so you can
compile it from Lazarus. You can also compile a console version
(that doesn't require any part of Lazarus LCL) by <tt>compile_console.sh</tt>
script inside.

<p>I will not give you a compiled executable of the testing program
(after all, it would have little sense, because all tests would succeed,
unless there's some problem specific to your OS configuration),
but I am generous enough to show you a snapshot of a happy test_kambi_units
program after successfully running all 33 tests:<br>
<?php echo
  medium_image_progs_demo('test_kambi_units_screen_demo.png', 'test_kambi_units', false)
?>

<?php echo $toc->html_section(); ?>

<p>Michalis Kamburelis (aka Kambi).

<p>If you will use any part of this engine, then make me happy and
<?php echo michalis_mailto('drop me a note about this'); ?>.

<?php
  if (!IS_GEN_LOCAL) {
    $counter = php_counter("kambi_vrml_game_engine", TRUE);
  };

  camelot_footer();
?>