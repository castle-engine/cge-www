<?php
require_once 'castle_engine_functions.php';
castle_header('Helping in the engine development');

$toc = new TableOfContents(
  array(
    new TocItem('Everyone', 'everyone'),
    new TocItem('If you create games', 'creators'),
    new TocItem('If you are a package maintainer', 'distros'),
  ));
?>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<ul>
  <li>
    <p><a href="https://www.patreon.com/castleengine">Support us on Patreon</a>.

  <li>
    <p><a href="donate">All the ways how you can donate - Patreon, Open Collective, PayPal, crypto...</a>

  <?php /*
  Delayed for now,
  we had a page on Wikipedia but it was deleted due to low notability,
  https://en.wikipedia.org/wiki/Wikipedia:Articles_for_deletion/Castle_Game_Engine .
  I accept this -- we need to be a more popular engine before retrying it.

  <li>Create a page on <a href="http://en.wikipedia.org/">Wikipedia</a> about our
    < ?php echo a_href_page('Castle Game Engine', 'index'); ? > and / or
    < ?php echo a_href_page('view3dscene', 'view3dscene'); ? >.
    <!--
    Or maybe only about the engine, and just redirect view3dscene there?
    I don't want to create and add it myself, it would not be fair.
    So this task waits for willing user (you! :) to do it.
    Of course you will get help (you can ask, post your draft etc.
    on our <a href="< ?php echo FORUM_URL; ? >">forum</a> and such).
    -->
  */ ?>

  <li>Spread the word! Mention the engine in your company, in your online community, talk about our engine on blogs, social platforms.
</ul>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p>First of all, just <b>use our engine to make your next fantastic game!</b> And publish it everywhere you can (your website, Steam, Itch.io, Google Play, App Store, Nintendo Switch store...).

  <li><p>Show your game on our <a href="https://forum.castle-engine.io/">forum</a> or <a href="talk.php">Discord</a>. Share a screenshot, movie recording, a build, code -- anything you can/want to share .

  <li><p>Give us feedback about the engine: what works good, what could be improved, what do you miss. Let's talk on <a href="https://forum.castle-engine.io/">forum</a> or <a href="talk.php">Discord</a>.

  <li><p>Report any bug to <a href="https://github.com/castle-engine/castle-engine/issues">our issues tracker</a>.

  <li><p>Improve our documentation.
    It's easiest to improve the pages maintained in AsciiDoctor format, just edit the <code>.adoc</code> files in the <a href="https://github.com/castle-engine/cge-www/tree/master/htdocs/doc">htdocs/doc subdirectory of the cge-www repository</a>.


  <li><p>Contribute code, send pull requests.

    <p>If you're looking for a feature / fix to implement,
    it's usually best to just start with <i>"what is your itch"</i>,
    that is: address a problem (bug or missing feature) that you have in your game.
    If you're open to help with anything we need,
    <a href="roadmap">take a look at our roadmap</a>
    or just <a href="talk.php">ask us</a> for what's currently needed.

    <p>See <a href="coding_conventions">coding conventions</a> on how to contribute code.
</ul>

<?php echo $toc->html_section(); ?>

<p>Package <?php echo a_href_page('Castle Game Engine', 'index'); ?>
 and <a href="castle-model-viewer">Castle Model Viewer</a>
 for your favorite Linux distribution.

<ul>
  <li><p><a href="castle-model-viewer">Castle Model Viewer</a> is a versatile viewer for <a href="creating_data_model_formats.php">many model formats</a>.

  <li><p>Castle Game Engine is an open-source 3D and 2D cross-platform game engine, with a comfortable editor and a powerful API using modern Object Pascal.
    <a href="features">All features are listed here</a>.

  <li><p>Desktop integration files (SVG icons, .desktop files etc.)
    are already included in our archives.

  <li><p>Build-dependencies include
    <a href="http://www.freepascal.org/">Free Pascal Compiler</a>,
    but this should not be a problem &mdash; all major Linux distros already
    have FPC packaged.
</ul>

<?php castle_footer(); ?>
