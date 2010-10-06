<?php
require_once 'vrmlengine_functions.php';
vrmlengine_header('All Programs');

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

/* Functions end ------------------------------------------------------------ */

echo pretty_heading($page_title, NULL, 'developed using our engine');
?>

<?php echo main_list_item("Programs: Main tools"); ?>

<?php program_image_links_table_begin_half(); ?>
  <tr>
    <?php program_image_link('view3dscene',
      'VRML / X3D browser, and a viewer for other 3D model formats
      (3DS, MD3, Wavefront OBJ and Collada). Move in the virtual scene,
      with collision-checking and gravity,
      use embedded ray-tracer, convert 3DS, MD3 etc. files to VRML.',
      "view3dscene_2.0.0_screen_demo.png",
      'view3dscene'); ?>
  </tr>
</table>

<?php echo main_list_item("Programs: Finished games"); ?>

<p>Some of the games below are large and definitely playable,
like <a href="castle.php">"The Castle"</a>.
Some others are just small toys, demos of our engine.
I consider them all <i>finished</i> &mdash; I like them very much,
and I keep them in working and compilable state, but that's it.</p>

<?php program_image_links_table_begin(); ?>
  <tr>
    <?php program_image_link('&quot;The Castle&quot;',
      'First-person perspective game, in a dark fantasy setting.',
      "castle_screen_demo_1.png", 'castle'); ?>

    <?php program_image_link('lets_take_a_walk',
      "Small 3d game-like toy, demonstrating OpenGL integrated with OpenALs
      spatial sound.",
      'lets_take_a_walk_screen_demo.png',
      'lets_take_a_walk'); ?>
   </tr>

   <tr>
    <?php program_image_link('malfunction',
      'Small 3D space-shooter. This was the first game made by me that used VRML models.',
      'malfunction_screen_demo.png',
      'malfunction'); ?>

     <?php program_image_link('kambi_lines',
       'Arrange colored balls in lines. Quickly.',
       'kambi_lines_screen_demo.png', 'kambi_lines'); ?>
   </tr>
</table>

<?php echo main_list_item("Programs: Other tools"); ?>

<?php program_image_links_table_begin(); ?>
  <tr>
    <?php program_image_link('glViewImage',
      'Image viewer, handles many image formats (including some exotic ones: DDS, RGBE).',
      "glviewimage_dds.png", 'glviewimage'); ?>

    <?php program_image_link('glplotter',
      'Plotting graphs (e.g. of functions).',
      "glplotter_screen_demo_1.png", 'glplotter_and_gen_function'); ?>
  </tr>

  <tr>
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
</ul>

<?php vrmlengine_footer(); ?>
