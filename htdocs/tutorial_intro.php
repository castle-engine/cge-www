<?php
require_once 'castle_engine_functions.php';
tutorial_header('Tutorial introduction');
?>

<p>This tutorial shows how to make a simple 3D first-person game using
our <?php echo a_href_page('Castle Game Engine', 'engine'); ?>. We'll
explain the basics of our engine, show you some examples and
mention the most important classes.</p>

<p>Throughout this tutorial you will see a links to our
<a href="<?php echo reference_link(); ?>">reference</a>, and you will
eventually want to follow them to read detailed reference of stuff
that interests you. Note that you can also just read the units source
code &mdash; the reference is automatically generated from the
comments in the units interface, so if you're brave enough, you can
just dive straight into the source.</p>

<p>The <?php echo a_href_page('overview of engine classes', 'tutorial_classes_overview'); ?>
 may also be useful as a quick cheatsheet &mdash; summary of the most important
classes and concepts of our engine.</p>

<p>Contents:

<?php echo _castle_sidebar_menu($castle_sitemap['engine']['sub']['tutorial_intro']['sub']); ?>

<?php
  tutorial_footer();
?>
