<?php
require_once 'castle_engine_functions.php';
castle_header('All Programs');

/* Some functions ----------------------------------------------------------- */

$main_list_item_num = 0;
function main_list_item($item_title, $anchor_name = '')
{
  global $main_list_item_num;
  $main_list_item_num++;

  /* The leading <p> is needed for IE to keep appropriate vertical
     distance. */
  return "<p><div class=\"main_list_item\">" .
    ($anchor_name != '' ? "<a name=\"$anchor_name\">": '') .
    "$item_title" .
    ($anchor_name != '' ? "</a>": '') .
    '</div>';
}

/* Image used here for $image_name must have "program_link_size"
   generated, so make sure it's listed in images/Makefile in PROGRAM_LINK_SIZE. */
function program_image_link($title, $subtitle, $image_name, $page_name)
{
  echo '<td class="program_image_link"><p>' .
    a_href_page("<img src=\"images/program_link_size/$image_name\"
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

/* Functions end ------------------------------------------------------------ */

echo pretty_heading($page_title, NULL, 'developed using our engine');
?>

<a href="http://michaliskambi.itch.io/">You can also download some of these
games from Michalis Kamburelis itch.io profile</a>.

<p>Developers: remember you can
<?php echo a_href_page('also download sources of all these programs',
'all_programs_sources'); ?>.</p>

<?php echo main_list_item("Games"); ?>

<?php program_image_links_table_begin(); ?>
  <tr>
    <?php program_image_link('Mountains Of Fire',
      '3D game with split-screen view where human and worm cooperate to survive. For single player or 2 players.',
      "mountains_of_fire_screen_1.png", 'mountains_of_fire'); ?>
    <?php program_image_link('Darkest Before the Dawn',
      'Scary 3D game, for Android and standalone.',
      "darkest_before_dawn_2.png", 'darkest_before_dawn'); ?>
   </tr>

   <tr>
    <?php program_image_link('&quot;The Castle&quot;',
      'First-person perspective game, in a dark fantasy setting.',
      "castle_screen_demo_1.png", 'castle'); ?>
    <?php program_image_link('lets_take_a_walk',
      "Small 3D toy, demonstrating rendering integrated with 3D sound using Castle Game Engine.",
      'lets_take_a_walk_screen_demo.png',
      'lets_take_a_walk'); ?>
   </tr>

   <tr>
    <?php program_image_link('malfunction',
      'Small 3D space-shooter. One of the first games made using (an ancient version of) Castle Game Engine, with VRML models.',
      'malfunction_screen_demo.png',
      'malfunction'); ?>
     <?php program_image_link('kambi_lines',
       'Arrange colored balls in lines. Quickly.',
       'kambi_lines_screen_demo.png', 'kambi_lines'); ?>
   </tr>
</table>

<?php echo main_list_item("Tools"); ?>

<?php program_image_links_table_begin(); ?>
  <tr>
    <?php program_image_link('view3dscene',
      'VRML / X3D browser, and a viewer for other 3D model formats
      (Collada, 3DS, MD3, Wavefront OBJ, some others).
      Explore the virtual world, with collision-checking, gravity, interactive animations, shadows, mirrors, shaders and more.
      Convert various models to VRML/X3D.',
      "view3dscene_2.0.0_screen_demo.png",
      'view3dscene'); ?>

    <?php program_image_link('glViewImage',
      'Image viewer, handles many image formats (including some exotic ones: DDS, RGBE).',
      "glviewimage_dds.png", 'glviewimage'); ?>
  </tr>

  <tr>
    <?php program_image_link('glplotter',
      'Plotting graphs (e.g. of functions).',
      "glplotter_screen_demo_1.png", 'glplotter_and_gen_function'); ?>

    <?php program_image_link("rayhunter",
      "Command-line simple ray-tracer (classic deterministic ray-tracer and basic Monte Carlo path tracer).<br/>Handles VRML/X3D and other 3D model formats.<br/>" .
      a_href_page("See also it's gallery.","raytr_gallery"),
      'rayhunter_graz_demo.png',
      "rayhunter"); ?>
  </tr>
</table>

<ul>
  <!-- li><?php echo a_href_page("bezcurve3d", "bezcurve3d") ?> - -
    just a toy allowing you to plot Bezier curves in 3D -->
  <li><?php echo a_href_page('bezier_curves', 'bezier_curves'); ?> &mdash;
    plotting rational Bezier curves
  <li> <?php echo a_href_page("glinformation", "glinformation") ?> &mdash;
    output information about OpenGL installed on your system.
  <li><?php echo a_href_page("kambi_mgf2inv","kambi_mgf2inv") ?> &mdash;
    convert MGF to Inventor / VRML 1.0 preserving info about
    physical material properties.
</ul>

<?php echo main_list_item("Miscellaneous documentation"); ?>

<p>Some miscellaneous documentation applicable to all programs here:

<ul>
  <li><?php echo a_href_page("Standard command-line options", "common_options"); ?>
  <li><?php echo a_href_page("Standard command-line options for OpenGL programs", "opengl_options"); ?>
  <li><?php echo a_href_page('OpenAL notes', 'openal'); ?>
  <li><?php echo a_href_page('Dependencies on Mac OS X', 'macosx_requirements'); ?>
  <li><?php echo a_href_page('Versioning scheme', 'versioning'); ?>
</ul>

<?php castle_footer(); ?>
