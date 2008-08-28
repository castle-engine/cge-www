<?php
  require "index_funcs.php";

  index_header("Kambi VRML game engine", LANG_EN,
    'A game engine written using ObjectPascal. ' .
    'Main features: processing and OpenGL rendering of 3D models in VRML ' .
    '(and some other) formats, animation, collision detection, ' .
    'shadows, 3d sound, ray-tracer.');
?>

<img src="images/kambi_vrml_game_engine_icon.png"
  alt="Kambi VRML game engine icon"
  align="right"/>

<?php echo pretty_heading("Kambi VRML game engine"); ?>

<p><b>For developers:</b> this is a free/open-source engine
that is especially oriented towards working with VRML / X3D worlds
(although other model formats are also supported).
For more information, documentation and downloads for developers
see <a href="#support">lower on this page</a>.</p>

<p><b>For normal human beings:</b> this page is a collection of various
games and tools. They were developed using the VRML game engine,
but you don't really need to know what VRML is in order to use them.
Two most important programs available here are
<a href="castle.php">"The Castle"</a> game and
<a href="view3dscene.php">view3dscene</a> 3D model viewer.

<p><b>Latest update:</b>

<!-- To force engine icon separated from "latest update" frame on wide pages -->
<br clear="all"/>

<div class="latest_update_description">
<div class="rss_link"><a href="changes_log_feed.php">RSS</a></div>
<?php
  require 'changes_log_common.php';
  echo last_change_log_to_html(false);
?>
</div>

<p>See <?php echo
a_href_page('the log of previous changes to these pages', 'changes_log') ?>.</p>

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

function program_image_links_table_begin_half()
{
  /* No way to nicely express this cellspacing in CSS ? */
  echo '<table class="program_image_links_half" cellspacing="20">';
}

?>

<?php program_image_links_table_begin(); ?>
  <tr>
    <?php program_image_link('&quot;The Castle&quot;',
      'First-person perspective game, in a dark fantasy setting.',
      "castle_screen_demo_1.png", 'castle'); ?>

    <?php program_image_link('view3dscene',
      'VRML / X3D browser, and viewer for other 3D model formats
      (3DS, MD3, Wavefront OBJ and Collada). Move in the virtual scene,
      with collision-checking and gravity,
      use embedded ray-tracer, convert 3DS, MD3 etc. files to VRML.',
      "view3dscene_2.0.0_screen_demo.png",
      'view3dscene'); ?>
  </tr>

  <tr>
    <?php program_image_link("rayhunter",
      "Command-line simple ray-tracer (classic deterministic ray-tracer and
      basic Monte Carlo path tracer implemented).<br/>" .
      a_href_page("See also it's gallery.","raytr_gallery"),
      'rayhunter_graz_demo.png',
      "rayhunter"); ?>
  </tr>
</table>

<?php echo main_list_item("Programs: Older games"); ?>

<p>Games below are not extended anymore. They are mainly small toys
and demos of the VRML game engine. I like them very much,
and I keep them in working and compilable state, but that's it.

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

<?php echo main_list_item("Programs: Computer graphics, others"); ?>

<?php program_image_links_table_begin_half(); ?>
  <tr>
    <?php program_image_link('glplotter',
      'Plotting graphs (e.g. of functions).',
      "glplotter_screen_demo_1.png", 'glplotter_and_gen_function'); ?>
  </tr>
</table>

<ul>
  <li><?php echo a_href_page("glViewImage","glviewimage") ?> &mdash;
    image viewer, it can fully handle PNG, JPEG, PPM, BMP and RGBE formats
  <!-- li><?php echo a_href_page("bezcurve3d", "bezcurve3d") ?> - -
    just a toy allowing you to plot Bezier curves in 3D -->
  <li><?php echo a_href_page('bezier_curves', 'bezier_curves'); ?> &mdash;
    plotting rational Bezier curves
  <li> <?php echo a_href_page("glcaps", "glcaps") ?> &mdash;
    output some useful information about OpenGL libraries
    installed on your system.
</ul>

<?php echo main_list_item('Support', 'support'); ?>

<p>Subscribe to <?php echo MAILING_LIST_LINK; ?>.
This list is for any discussion, questions,
announcements related to our VRML engine and programs on this page.</p>

<p>Submit
<a href="<?php echo BUGS_TRACKER_URL; ?>">bugs</a>,
<a href="<?php echo FEATURE_REQUESTS_TRACKER_URL; ?>">feature requests</a>,
<a href="<?php echo PATCHES_TRACKER_URL; ?>">patches</a>
to appropriate tracker.</p>

<p>If you really want to contact the author directly,
<?php echo michalis_mailto('send email to Michalis Kamburelis'); ?>.</p>

<?php /*
<i>And one more thing : if the bug concerns one of my OpenGL programs,
remember to attach to your bug report output of the
< ?php echo a_href_page("glcaps","glcaps") ? > program.</i> */ ?>

<?php echo main_list_item("Development", 'development'); ?>

<p>So, you want to actually see how it all works inside, huh ?</p>

<p>Documentation and sources:</p>

<ul>
  <li><?php echo a_href_page('Kambi VRML game engine
    &mdash; overview and sources for developers', 'kambi_vrml_game_engine'); ?></li>
  <li><?php echo a_href_page('VRML engine reference (generated by pasdoc)', 'reference') ?></li>
  <li><?php echo a_href_page("VRML engine documentation",
    'vrml_engine_doc'); ?> (more general overview of how the engine works).</li>
</ul>

<p>More details about VRML format handling:</p>

<ul>
  <li><?php echo a_href_page("Kambi VRML extensions, handled by our engine",
    "kambi_vrml_extensions"); ?>
  <li><?php echo a_href_page("Kanim (Kambi VRML engine animations) file format",
    'kanim_format'); ?>
  <li><?php echo a_href_page("Kambi VRML test suite",
    "kambi_vrml_test_suite"); ?>
  <li><?php echo a_href_page('VRML implementation status',
    'vrml_implementation_status'); ?>
  <li>Read my document describing
    <a href="apidoc/html/introduction.html#OpenGLOptimization">
    how current VRML rendering optimization works,
    what are the possible drawbacks and what are the possible
    alternatives (and what drawbacks are hidden in those alternatives :)</a>.
</ul>

<p>And some more information, randomly related to the engine and
the programs developed with it:</p>

<ul>
  <li><?php echo a_href_page(
    "Blender VRML stuff", "blender_stuff"); ?></li>
  <li><?php echo a_href_page(
    "Standard command-line options understood by all OpenGL programs",
    "opengl_options"); ?>
  <li><?php echo a_href_page(
    'Notes related to programs using OpenAL', 'openal_notes'); ?>
  <li><?php echo a_href_page(
    "Command-line options understood by all programs here",
    "common_options"); ?>

  <li><?php echo a_href_page('Versioning scheme', 'versioning'); ?>

  <li><?php echo current_www_a_href_size('All Windows DLLs used by programs here',
    'miscella/win32_dlls.zip'); ?>

  <li><?php echo a_href_page('Dependencies on Mac OS X',
    'macosx_requirements'); ?>

  <li><?php echo a_href_page("kambi_mgf2inv","kambi_mgf2inv") ?> &mdash;
    modified version of the mgf2inv program by Greg Ward,
    it outputs some additional info about physical material properties
    (concerning Phong BRDF)

  <li>Programmers may be interested in:
    <ul>
      <li><a href="http://vrmlengine.svn.sourceforge.net/viewvc/*checkout*/vrmlengine/trunk/kambi_vrml_game_engine/3dmodels.gl/examples/bump_mapping/README">Bump
        mapping techniques quick overview</a></li>
      <li><a href="http://vrmlengine.svn.sourceforge.net/viewvc/*checkout*/vrmlengine/trunk/kambi_vrml_game_engine/opengl/examples/shading_langs/README">Shading
        languages (in particular ARB vertex/fragment assembly programs,
        Cg and mostly GLSL) quick overview</a></li>
    </ul>
    I wrote these documents when I was learning these techniques myself,
    they contain some summary and my findings, and links to other useful resources.
    You can take a look into our engine sources (in <tt>kambi_vrml_game_engine/.../examples/...</tt>
    directories, just look at the URLs of these documents) to see practical
    implementation of all these things.
  </li>
</ul>

<p>See also <a href="http://sourceforge.net/projects/vrmlengine">vrmlengine
project page on SourceForge</a>.

<p>For amusement (mostly), you can watch commits to vrmlengine through
<a href="http://cia.vc/stats/project/vrmlengine">Kambi VRML game engine on
cia.vc</a>.

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
<p>Author: Michalis Kamburelis (aka Kambi) (<a
href="http://michalis.ii.uni.wroc.pl/~michalis/">home page</a>,
<?php echo michalis_mailto('email'); ?>).
If you will use any part of this engine, then make me happy and
drop me a note about this&nbsp;!
</address>

<?php
  if (!IS_GEN_LOCAL) {
    $counter = php_counter("index", TRUE);
    /* echo "<p>Stronê odwiedzono " . $counter . " razy.";
       // chwilowo licznik dziala ukryty
    */
  };

  common_footer();
?>
