<?php
require_once 'castle_engine_functions.php';
castle_header('Helping in the engine development', array(
  'path' => array('documentation')
));
echo pretty_heading($page_title);
?>

<p>We have a ton of TODOs if you're interested in helping the engine development!
Thank you!:)</p>

<?php
$toc = new TableOfContents(
  array(
    new TocItem('For everyone', 'everyone'),
    new TocItem('For 3D worlds creators', 'creators'),
    new TocItem('For ObjectPascal developers', 'developers'),
    new TocItem('For Blender experts', 'blender'),
    new TocItem('For Linux distros package maintainers', 'distros'),
  ));
?>

<?php echo $toc->html_toc(); ?>

<p>If you'd like to help in the development of our engine, here are some
proposed tasks:</p>

<?php echo $toc->html_section(); ?>

<ul>
  <!--li>First of all, don't hesitate to post questions and suggestions
    about anything to our <a href="<?php echo FORUM_URL; ?>">forum</a>.-->
  <li>Brag about our engine on blogs and social platforms :)
  <li><?php echo a_href_page('Donate', 'donate'); ?>.
  <li>Create a page on <a href="http://en.wikipedia.org/">Wikipedia</a> about our
    <?php echo a_href_page('Castle Game Engine', 'index'); ?> and/or
    <?php echo a_href_page('view3dscene', 'view3dscene'); ?>.
    Maybe only a page about view3dscene for starters?
    <!--
    Or maybe only about the engine, and just redirect view3dscene there?
    I don't want to create and add it myself, it would not be fair.
    So this task waits for willing user (you! :) to do it.
    Of course you will get help (you can ask, post your draft etc.
    on our <a href="<?php echo FORUM_URL; ?>">forum</a> and such).
    -->
</ul>

<?php echo $toc->html_section(); ?>

<p>If you use our tools to create or browse your 3D worlds, you can:

<ul>
  <li><p>Show off your works on our <a href="<?php echo FORUM_URL; ?>">forum</a>,
    in any way you like (screenshot, link to model, youtube video etc.).
    If you use some of <?php echo a_href_page('our engine special VRML/X3D extensions
    or rendering features', 'x3d_extensions'); ?>, you are strongly
    encouraged to do so (Michalis loves to see how his work is useful for others :) !
    But also post about "normal" 3D models that simply look nice in our
    <?php echo a_href_page('view3dscene', 'view3dscene'); ?> or such.
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

  <li><p>Test the <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/">nightly
    builds of our binaries</a>. These are build automatically every night
    using current SVN code. You can test them and catch eventual bugs
    before the release. This way you can also preview new features before they
    are released.

    <p>Bugs can be reported on the <a href="<?php echo FORUM_URL; ?>">forum</a>
    or in the <a href="<?php echo TICKETS_TRACKER_URL; ?>">tickets tracker</a>.
</ul>

<?php echo $toc->html_section(); ?>

<p>For ObjectPascal (<a href="http://www.freepascal.org/">FPC</a>, <a href="http://www.lazarus.freepascal.org/">Lazarus</a>) developers:

<ul>
  <li><p>Use our <?php echo a_href_page('engine', 'index'); ?>
    to make your next game, of course! :) And make it great :)

  <li><p><a href="<?php echo WIKI_URL; ?>">Contribute
    to our wiki</a> useful tips or tutorials about using our engine.

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

    <p>If you're looking for a feature to implement, <a href="planned_features.php">take
    a look at our planned features</a>.
</ul>

<?php echo $toc->html_section(); ?>

<p>I think that success of our engine is tightly coupled with
the quality of <a href="http://www.blender.org/">Blender</a> X3D exporter.
Of course, you can use anything to generate VRML/X3D for use with our engine.
But if you make FOSS game, you have probably already chosen
<a href="http://www.blender.org/">Blender</a> as your main 3D modeller.

<p>Blender is really fantastic. But the current exporter from Blender to X3D
lacks some important features. For starters, there isn't
any way to export animation to X3D. At least exporting animation
of transformations (translation, rotation, scale of objects) would
already be very useful. Exporting mesh deformation
(from shape keys, or derived from bone animation) would be great.
There are also various other small lacks.
Many features of VRML/X3D and our engine are not used intensively enough,
because there isn't any way to express them and export from Blender.

<p>Preferably, changes should be reported and applied to
the <a href="http://www.blender.org/">Blender</a> sources.
But, eventually, we can also host our custom X3D exporter.

<p>See also our <?php echo a_href_page('Blender exporting notes',
'creating_data_blender'); ?>.

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
and would love to see his software available in your repositories :)

<?php castle_footer(); ?>
