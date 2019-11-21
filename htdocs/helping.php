<?php
require_once 'castle_engine_functions.php';
castle_header('Helping in the engine development', array(
  'path' => array('documentation')
));
echo pretty_heading($page_title);
?>

<!--p>We have a ton of TODOs if you're interested in helping the engine development!
Thank you!:)</p-->

<?php
$toc = new TableOfContents(
  array(
    new TocItem('For everyone', 'everyone'),
    new TocItem('For developers', 'developers'),
    new TocItem('For 3D artists', 'creators'),
    new TocItem('For Blender users', 'blender'),
    new TocItem('For Linux package maintainers', 'distros'),
  ));
?>

<?php echo $toc->html_toc(); ?>

<!-- <p>If you'd like to help in the development of our engine, here are some -->
<!-- proposed tasks:</p> -->

<?php echo $toc->html_section(); ?>

<ul>
  <li><a href="https://www.patreon.com/castleengine">Support us on Patren</a>.
    Or <a href="donate_other.php">donate in other ways</a>.
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
  <li><p>Use our <?php echo a_href_page('engine', 'index'); ?>
    to make your next fantastic game, of course! :)

  <li><p>Contribute code! Remove a bug, add a feature! (Not the other way around:)

    <p>Code changes are best submitted as
    <a href="https://github.com/castle-engine/castle-engine/pulls">pull requests on GitHub</a>.
    <i>Pull requests</i> are really easy for you to create (fork our <a href="https://github.com/castle-engine/castle-engine/">repository</a>,
    commit stuff to your fork,
    then create a pull request by clicking on GitHub),
    and for me to apply.

    <p>If you prefer to do things the traditional way,
    you can also just create a patch file (versus recent GIT or SVN state)
    and <a href="https://github.com/castle-engine/castle-engine/issues">create
    a new issue with the patch file attached</a>.

    <p>If you're looking for a feature to implement,
    <a href="planned_features.php">take a look at our planned features</a>.

    <p>If you know your way around OpenGL,
    see <a href="https://github.com/castle-engine/castle-engine/wiki/OpenGL-ES,-Android-and-iOS-TODOs">the list of renderer TODOs</a> &mdash; there's a number of easy and rewarding tasks waiting!

  <li><p><a href="<?php echo WIKI_URL; ?>">Contribute
    to our wiki</a> useful tips or tutorials about using our engine.
</ul>

<?php echo $toc->html_section(); ?>

<p>If you use <i>Castle Game Engine</i> or our tools (like view3dscene) to view or play anything
(a game, or just your 3D or 2D assets):

<ul>
  <li><p>Show it on our <a href="talk.php">Discord or forum</a>
    by sharing a screenshot.
    Michalis loves to see how his work is useful for others :)

  <li><p>Contribute models to our <?php echo a_href_page('demo models', 'demo_models'); ?>.

  <li><p>Look into improving our documentation.
    Our <?php echo a_href_page('scene graph (X3D) documentation', 'vrml_x3d'); ?>
    is large, and wants to be larger. Contributions describing
    how something works, or how to do something practical, are welcome.

    <p><a href="<?php echo WIKI_URL; ?>">You can
    add your contributions directly to our wiki</a>.
    (Once we'll get some user content in our wiki,
    we'll think how to make it more visible.)<!-- &mdash; copy it to the current "static"
    documentation pages, or maybe make link to wiki more prominent,
    maybe even move whole current static documentation to wiki.)-->

  <li><p>Test the <a href="http://michalis.ii.uni.wroc.pl/view3dscene-snapshots/">view3dscene snapshots</a>. These are build automatically after every commit to GitHub.
    You can test them and catch eventual bugs
    before the release. This way you can also preview new features before they
    are released.

    <p>Bugs can be reported on the <a href="<?php echo FORUM_URL; ?>">forum</a>
    or in the <a href="https://github.com/castle-engine/view3dscene/issues">issues tracker</a>.
</ul>

<?php echo $toc->html_section(); ?>

<p>If you make free software / open-source game, you have probably already chosen
<a href="http://www.blender.org/">Blender</a> as your main 3D modeller.
We can <a href="creating_data_blender.php">export from Blender to X3D or castle-anim-frames
(which is internally a sequence of X3D models)</a>.
It would be great to improve Blender -&gt; X3D exporter:

<ul>
  <li><p>Support animations. <a href="creating_data_blender.php">Right now we support animations using castle-anim-frames</a>, but a direct support inside X3D exporter would be more efficient.

    <p>At least exporting animation
    of transformations (translation, rotation, scale of objects) would
    already be very useful.
    Exporting mesh deformation
    (from shape keys, or derived from bone animation) would be great.
    Exporting skinned mesh animation (as mesh deformation, or using X3D H-Anim nodes)
    would be great.

  <li><p>Configuring collisions (using X3D <code>Collision</code> node). <a href="manual_optimization.php">As explained in the manual about "optimization"</a>, it's often useful to configure what collides and how.

  <li><p>Export 3D sound sources from Blender.</p>
</ul>

<p>We have <?php echo a_href_page('our own fork of the Blender X3D exporter',
'creating_data_blender'); ?>, which can be used to distribute our improvements
to CGE users. But preferably, changes should be submitted and applied to
the <a href="http://www.blender.org/">Blender</a> sources as well.

<!--
Only stuff really specific to our engine (cooperation between Blender
and some specific features of our engine) should be left inside our
custom exporter.--></p>

<?php echo $toc->html_section(); ?>

<p>Please package <?php echo a_href_page('view3dscene', 'view3dscene'); ?>
 for your favourite Linux distribution :)
It's a great and stable VRML/X3D browser (and viewer for other 3D models,
like Collada and 3DS).
Some facts in favor of view3dscene, important for package maintainers:

<ul>
  <li>Stable program, with <?php echo a_href_page('long development
    history and regular releases', 'news'); ?>.
  <li>Desktop integration files (SVG icon, .desktop file, MIME xml)
    are already included in our archive.
  <li>The <?php echo a_href_page_hashlink('dependencies of view3dscene',
    'view3dscene', 'section_depends'); ?> are documented.
    There's nothing weird there (<i>ffmpeg</i> and <i>ImageMagick</i> are only
    light suggestions; <i>OpenAL</i> may also be a suggestion instead of
    a recommendation; the rest is standard for any GTK program using OpenGL).
  <li>Build-dependencies of view3dscene include
    <a href="http://www.freepascal.org/">Free Pascal Compiler</a>,
    but this should not be a problem &mdash; all major distros already
    have fpc packaged.
  <li><?php echo a_href_page('Sources of view3dscene are here',
    'all_programs_sources'); ?>, get both view3dscene and engine sources,
    and follow instructions on that page to compile.
  <li>The whole thing is GPL &gt;= 2 (most of the engine may also be used
    under more permissive "LGPL with static-linking exception" &gt;= 2,
    but this probably doesn't matter for you).
</ul>

<p>Michalis uses <a href="http://www.debian.org/">Debian</a>,
and sometimes <a href="http://www.ubuntu.com/">Ubuntu</a>,
and would love to see his software available in all Linux repositories :)

<?php castle_footer(); ?>
