<?php
require_once 'castle_engine_functions.php';
castle_header('Helping in the engine development');
?>

<!--p>We have a ton of TODOs if you're interested in helping the engine development!
Thank you!:)</p-->

<?php
$toc = new TableOfContents(
  array(
    new TocItem('For everyone', 'everyone'),
    new TocItem('For developers', 'developers'),
    new TocItem('For 3D artists', 'creators'),
    // new TocItem('For Blender users', 'blender'),
    new TocItem('For Linux package maintainers', 'distros'),
  ));
?>

<?php echo $toc->html_toc(); ?>

<!-- <p>If you'd like to help in the development of our engine, here are some -->
<!-- proposed tasks:</p> -->

<?php echo $toc->html_section(); ?>

<ul>
  <li><a href="https://www.patreon.com/castleengine">Support us on Patren</a>
  <li><a href="donate_other.php">Donate through other ways</a>
  <!--li>First of all, don't hesitate to post questions and suggestions
    about anything to our <a href="<?php echo FORUM_URL; ?>">forum</a>.-->
  <!--?php echo a_href_page('Donate', 'donate'); ?-->

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

  <li>Talk about our engine on blogs and social platforms :)
</ul>

<?php echo $toc->html_section(); ?>

<!--p>For Object Pascal (<a href="http://www.freepascal.org/">FPC</a>, <a href="http://www.lazarus.freepascal.org/">Lazarus</a>) developers:-->

<ul>
  <li><p><b>Use our engine to make your next fantastic game!</b>

  <li><p>Contribute code! Remove a bug, add a feature! (Not the other way around:)

    <p>Code changes are best submitted as
    <a href="https://github.com/castle-engine/castle-engine/pulls">pull requests on GitHub</a>.
    <i>Pull requests</i> are really easy for you to create (fork our <a href="https://github.com/castle-engine/castle-engine/">repository</a>,
    commit stuff to your fork,
    then create a pull request by clicking on GitHub),
    and for me to apply.

    <p>See <a href="coding_conventions">Coding conventions</a> for useful tips on contributing code.

    <!--
    <p>If you prefer to do things the traditional way,
    you can also just create a patch file (versus recent GIT or SVN state)
    and <a href="https://github.com/castle-engine/castle-engine/issues">create
    a new issue with the patch file attached</a>.
    -->

    <p>If you're looking for a feature to implement,
    <a href="roadmap">take a look at our roadmap</a>.

  <li><p><a href="https://github.com/castle-engine/cge-www/">Contribute to our documentation</a>.
</ul>

<?php echo $toc->html_section(); ?>

<p>If you use <i>Castle Game Engine</i> or our tools (like view3dscene) to view or play anything
(a game, or just your 3D or 2D assets):

<ul>
  <li><p>Show it on our <a href="talk.php">Discord or forum</a>
    by sharing a screenshot or movie recording.
    Michalis loves to see how his work is useful for others :)

  <li><p>Contribute models to our <?php echo a_href_page('demo models', 'demo_models'); ?>.

  <li><p>Test the <a href="view3dscene.php">view3dscene snapshots</a>. These are build automatically after every commit to GitHub.
    You can test them and catch eventual bugs
    before the release. This way you can also preview new features before they
    are released.

    <p>Bugs are best reported in the <a href="https://github.com/castle-engine/view3dscene/issues">issues tracker</a>.
</ul>

<?php echo $toc->html_section(); ?>

<p>Package <?php echo a_href_page('Castle Game Engine', 'index'); ?>
 and <?php echo a_href_page('view3dscene', 'view3dscene'); ?>
 for your favourite Linux distribution.

<ul>
  <li><p>view3dscene is a versatile and stable viewer for <a href="creating_data_model_formats.php">many model formats</a>.

  <li><p>Castle Game Engine <a href="features">features are listed here</a>.

  <li><p>Desktop integration files (SVG icons, .desktop files etc.)
    are already included in our archives.

  <li><p>The <?php echo a_href_page_hashlink('dependencies of view3dscene',
    'view3dscene', 'section_depends'); ?> and <a href="documentation.php#section_libraries">dependencies of CGE</a>
    are documented.
    There's nothing weird there.

  <li><p>Build-dependencies include
    <a href="http://www.freepascal.org/">Free Pascal Compiler</a>,
    but this should not be a problem &mdash; all major distros already
    have fpc packaged.

  <li><?php echo a_href_page('view3dscene', 'view3dscene'); ?> is GPL &gt;= 2.
    CGE may be used
    <a href="license.php">under more permissive "LGPL with static-linking exception" &gt;= 2</a>.
</ul>

<?php castle_footer(); ?>
