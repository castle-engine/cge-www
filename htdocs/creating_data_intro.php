<?php
require_once 'castle_engine_functions.php';
creating_data_header('Introduction to Creating Game Data', 'for games using Castle Game Engine');
?>

<p>This guide discusses various aspects of preparing game data for use
in <?php echo a_href_page('Castle Game Engine', 'engine'); ?>. We'll talk about how to make the 3D
stuff (levels, creatures, items &mdash; everything), and also how to
write various helper data files (<code>level.xml</code>, <code>resource.xml</code> and such)
that are read by our engine.

<p>Contents:

<?php echo _castle_sidebar_menu($castle_sitemap['engine']['sub']['creating_data_intro']['sub']); ?>

<?php
creating_data_footer();
?>
