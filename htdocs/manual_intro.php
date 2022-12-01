<?php
require_once 'castle_engine_functions.php';
castle_header('Manual');
?>

<!-- <p>This manual shows all the major parts of the -->
<!-- < ?php echo a_href_page('Castle Game Engine', 'index'); ? >. -->
<!-- engine,
demonstrating how to make simple games and applications using -->
<!-- shows how to make a simple 3D first-person game using -->
<!-- We'll
explain the basics of our engine, show you some examples and
mention the most important classes.</p -->

<?php /*

<p>Throughout this manual you will see a links to our
<a href="<?php echo reference_link(); ?>">reference</a>, and you will
eventually want to follow them to read detailed reference of stuff
that interests you. Note that you can also just read the units source
code &mdash; the reference is automatically generated from the
comments in the units interface, so if you're brave enough, you can
just dive straight into the source.</p>

<p>The <?php echo a_href_page('overview of engine classes', 'manual_classes_overview'); ?>
 may also be useful as a quick cheatsheet &mdash; summary of the most important
classes and concepts of our engine.</p>

*/ ?>

<p>Contents:

<?php echo _castle_sidebar_menu($castle_sitemap['documentation']['sub']['manual_intro']['sub']); ?>

<?php
  castle_footer();
?>
