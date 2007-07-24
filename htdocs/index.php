<?php
  require "index_funcs.php";
  index_header("Kambi VRML game engine", LANG_EN,
    'A game engine written using ObjectPascal. ' .
    'Main features: processing and OpenGL rendering of 3D models in VRML ' .
    '(and some other) formats, animation, collision detection, ' .
    'shadows, 3d sound, ray-tracer.');
?>

<?php echo pretty_heading("Kambi VRML game engine"); ?>

<p>Welcome ! This is my home page, basically just a collection of various things
that I do.

<p>I'm a programmer, and some of the keywords that I enjoy are
<i>free software</i>, <i>3D graphic</i> and <i>ObjectPascal</i>.
All programs presented here are free sofware (<?php echo a_href_page(
'sources are available', 'sources'); ?> on terms of GNU GPL license).
Most of these programs were compiled with
<a href="http://www.freepascal.org">FreePascal</a> for Linux, FreeBSD, Mac OS X
and Windows.

<?php require 'last_update.php'; ?>

<?php echo main_list_begin(); ?>
<?php echo main_list_item("Programs: Main games, VRML tools");

function program_image_link($title, $subtitle, $image_name, $page_name)
{
  echo '<td class="program_image_link"><p>' .
    a_href_page("<img src=\"images/progs_demo/program_link_size/$image_name\"
      alt=\"$title\" />", $page_name) .
    '</p><p class="program_image_link_title">' .
    a_href_page("<b>$title</b>", $page_name) .
    '</p><p>' .
    $subtitle . '</p></td>';
}

function program_image_links_table_begin()
{
  /* No way to nicely express this cellspacing in CSS ? */
  echo '<table class="program_image_links" cellspacing="20">';
}

?>

<?php program_image_links_table_begin(); ?>
  <tr>
    <?php program_image_link('&quot;The Castle&quot;',
      'First-person perspective game, in a dark fantasy setting.',
      "castle_screen_demo_1.png", 'castle'); ?>

    <?php program_image_link('view3dscene',
      'Viewer for VRML 1.0, VRML 2.0 (aka VRML 97),
      3DS, MD3, OBJ and GEO models. Move in the virtual scene,
      with collision-checking and gravity,
      use embedded ray-tracer, convert 3DS, MD3 etc. files to VRML 1.0.',
      "view3dscene_2.0.0_screen_demo.png",
      'view3dscene'); ?>
  </tr>

  <tr>
    <?php program_image_link("rayhunter",
      'Command-line simple ray-tracer (classic deterministic ray-tracer and
      basic Monte Carlo path tracer implemented)',
      'rayhunter_graz_demo.png',
      "rayhunter"); ?>
  </tr>
</table>

<p>Also take a look at
<ul>
  <li><?php echo a_href_page('Kambi VRML game engine
    &mdash; informations for developers', 'kambi_vrml_game_engine'); ?>
    and it's <?php echo a_href_page('documentation', 'sources_docs') ?>
  <li><?php echo a_href_page("My master's thesis about my VRML engine",
    'vrml_engine_doc'); ?>
  <li><?php echo a_href_page("Specification of my extensions to VRML",
    "kambi_vrml_extensions"); ?>
  <li><?php echo a_href_page("My VRML test suite",
    "kambi_vrml_test_suite"); ?>
  <li><?php echo a_href_page('VRML implementation status',
    'vrml_implementation_status'); ?>
  <li><?php echo a_href_page(
    "The small gallery of images rendered using rayhunter","raytr_gallery"); ?>
  <li><?php echo a_href_page("kambi_mgf2inv","kambi_mgf2inv") ?> &mdash;
    modified version of the mgf2inv program by Greg Ward,
    it outputs some additional info about physical material properties
    (concerning Phong BRDF)
  <li>Read my document describing
    <a href="src/pascal/docs/html/introduction.html#OpenGLOptimization">
    how current VRML rendering optimization works,
    what are the possible drawbacks and what are the possible
    alternatives (and what drawbacks are hidden in those alternatives :)</a>.
</ul>

<?php echo main_list_item("Programs: Older games"); ?>

<?php program_image_links_table_begin(); ?>
  <tr>
    <?php program_image_link('lets_take_a_walk',
      "Small 3d game-like toy, demonstrating OpenGL integrated with OpenALs
      spatial sound.",
      'lets_take_a_walk_screen_demo.png',
      'lets_take_a_walk'); ?>

    <?php program_image_link('malfunction',
      '3D space-shooter. This was the first game made by me that used VRML models.',
      'malfunction_screen_demo.png',
      'malfunction'); ?>
   </tr>

   <tr>
     <?php program_image_link('kambi_lines',
       'Arrange colored balls in lines. Quickly.',
       'kambi_lines_screen_demo.png', 'kambi_lines'); ?>
   </tr>
</table>

<p>These games are not extended anymore. They are mainly small toys
and demos of my VRML game engine. I like them very much,
and I keep them in working and compilable state, but that's it.

<?php echo main_list_item("Programs: Computer graphics, others"); ?>

<ul>
  <li><?php echo a_href_page("glViewImage","glviewimage") ?> &mdash;
    image viewer, it can fully handle PNG, JPEG, PPM, BMP and RGBE formats
  <!-- li><?php echo a_href_page("bezcurve3d", "bezcurve3d") ?> - -
    just a toy allowing you to plot Bezier curves in 3D -->
  <li><?php echo a_href_page("glplotter", "glplotter"); ?> &mdash;
    plotting graphs in OpenGL
  <li><?php echo a_href_page("gen_funkcja", "gen_funkcja"); ?> &mdash;
    generate function's graphs for glplotter
  <li><?php echo a_href_page('bezier_curves', 'bezier_curves'); ?> &mdash;
    plotting rational Bezier curves
  <li>And something special &mdash; <?php echo a_href_page("glcaps", "glcaps") ?>,
    program that outputs some useful information about OpenGL libraries
    installed on your system.
</ul>

<?php echo main_list_item("Additional information"); ?>

<p>Below you can find some additional things concerning many of
the programs above.

<ul>
  <li><?php echo a_href_page(
    "Standard command-line options understood by all my OpenGL programs",
    "opengl_options"); ?>
  <li><?php echo a_href_page(
    'Notes related to all my programs using OpenAL', 'openal_notes'); ?>
  <li><?php echo a_href_page(
    "Some notes about command-line options understood by my programs", "common_options"); ?>

  <li><?php echo a_href_page(
    'Sources of Kambi VRML game engine and related programs', 'sources'); ?>

  <li><?php echo a_href_page('Versioning scheme of my things', 'versioning'); ?>

  <li><?php echo general_a_href_size('All Windows DLLs used by my programs',
    'miscella/win32_dlls.zip', false); ?>

  <li><?php echo a_href_page('Dependencies of my programs on Mac OS X',
    'macosx_requirements'); ?>
</ul>

<?php echo main_list_item("A few general words about programs listed above"); ?>

<p>There is absolutely no warranty for any of these programs.
The only thing I can promise is that I'm really trying.
I don't think your computer will blow up or something because of
my programs but everything is possible.

<p><?php echo michalis_mailto(
  'Any suggestions about this page and my programs are welcome.'); ?>
 Including bug-reports, of course (remember to describe in detail
your system and situation that led into a trouble &mdash;
when, where, after what).
<?php /*
<i>And one more thing : if the bug concerns one of my OpenGL programs,
remember to attach to your bug report output of the
< ?php echo a_href_page("glcaps","glcaps") ? > program.</i> */ ?>

<?php
/* No suitable place for this text:

<p>Notes about FreeBSD versions of programs:
<ul>
  <li><p>As of 2006-10, I use FreeBSD 6.1 so this is the system
    where my programs get compiled and tested.</p>

    <p>My programs should also work on any earlier 4.x or 5.x FreeBSD kernel,
    but you will have to upgrade many packages (in particular
    GTK 2 and GtkGLExt libraries as distributed with FreeBSD 5.2.1
    are too old; the ones from FreeBSD 5.3 are good).
    Alternatively you can grab the sources of my programs
    and compile them against GTK 1 (simply pass <tt>-dGLWINDOW_GTK_1</tt>
    option to fpc), such binaries can run on system with packages
    distributed with FreeBSD 5.2.1 version
    (<tt>gtk12</tt> and <tt>gtkglarea</tt>).
</ul>
*/
?>

<?php echo main_list_end(); ?>

<hr>  <!-- ===================================================== -->

<address>
<p>This is the
<?php echo michalis_mailto("Michalis Kamburelis' (aka Kambi)"); ?> page.

<p>You can fetch my <a href="michalis-gpg-public-key.asc">public GPG key</a>
and use it to encrypt messages to me or verify my signed emails.
(You can also fetch it from any popular keyserver by
command like <tt>gpg --recv-key 0xB240711F</tt>). I use
<a href="http://www.mozilla.com/thunderbird/">Thunderbird</a> with
<a href="http://enigmail.mozdev.org/">Enigmail</a> extension.
</address>

<?php
  if (!IS_GEN_LOCAL) {
    $counter = php_counter("index", TRUE);
    /* echo "<p>Stronê odwiedzono " . $counter . " razy.";
       // chwilowo licznik dziala ukryty
    */
  };

  camelot_footer();
?>
