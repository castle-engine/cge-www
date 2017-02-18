<?php
require_once "castle_engine_functions.php";
castle_header("The Castle &mdash; additional notes", array(
  'path' => array('all_programs', 'castle')
));

$toc = new TableOfContents(
  array(
    new TocItem('Detailed requirements', 'requirements_detailed'),
    new TocItem('Window size', 'window_size'),
    new TocItem('Sound notes', 'sound'),
    new TocItem('Log', 'log'),
    new TocItem('Other command-line options', 'command_line'),
    new TocItem('Details about items in the game', 'items_details'),
    new TocItem('Version numbers', 'versioning'),
    new TocItem('Debug menu, debug options', 'debug_menu_options'),
  )
);
?>

<h1><?php echo a_href_page('The Castle', 'castle'); ?> &mdash;
additional notes</h1>

This page collects various additional notes about running my game
<?php echo a_href_page('"The Castle"', "castle") ?>.
If all you want is just to run the game, then <i>don't read this page</i>.
You don't need this. This page explains some details that may be needed
for advanced users and developers to understand how the game works,
but normal users will not find here anything useful.

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>In short: good graphics card, with good OpenGL drivers installed.
In case of doubt, be sure to install newest drivers from your
graphics card manufacturer.

<p>Also you should have enough free memory available when running the game.
As of version 0.6.3, game can take up to 280 MB on GeForce with Linux drivers
from nvidia.com. But this may depend on various factors, including your
OpenGL implementation. If the game seems to drain your system
of all the memory (which will most likely be observed as sudden slowdown
in the middle of "Loading creatures" sequence, as this is the main
memory-eater) you can
<ul>
  <li>Try closing all other programs before running the game
    (although a decent OS should send them to swap anyway...)</li>
  <li>Lower the setting "Creature animation smoothness" in "Video options".</li>
</ul>

<p>Library requirements:

<?php echo depends_ul(array(
  DEPENDS_OPENGL,
  DEPENDS_LIBPNG_AND_ZLIB,
  SUGGESTS_OPENAL_VORBISFILE,
  DEPENDS_MACOSX)); ?>

<?php echo $toc->html_section(); ?>

<p>The game prefers to be run in resolution 800x600.
If possible, it will resize your screen to this.
If you don't want to change your screen resolution,
you can disable it from the main menu:
"Video options -> Allow screen settings change on startup".

<p>You can use <?php echo a_href_page("standard options
understood by our OpenGL programs", "opengl_options") ?>
 like <code>--geometry</code> and <code>--fullscreen</code>
to force windowed or fullscreen mode with desired size.
For example run like this:

<pre>
castle --geometry 800x600
</pre>

<?php echo $toc->html_section(); ?>

<p>OpenAL is used to play sounds. See
<?php echo a_href_page(
  'my instructions about installing OpenAL and command-line
  options common for my programs using OpenAL', 'openal'); ?>.

<p>For best effect, be sure to try all available devices in "Sound options" &mdash;
the default device doesn't always sound the best.
This is true for both Unixes and Windows.
E.g. a standard sound card (without real 3d surround support) on Windows
may sound better with "Generic Software" device.

<p>If the game performance suffers when the sound is on,
or sound quality suffers, or there is sound latency, then you should
probably stop any other programs that play some music.

<?php echo $toc->html_section(); ?>

Look at log output.
<a href="manual_log.php">Here we describe where log output is.</a>
When sending bug reports, it may be useful to attach generated log
to your report &mdash; it will show some things about your
OpenGL, OpenAL, and what happened during the game.

<?php echo $toc->html_section(); ?>

<p>See <?php echo a_href_page('some general notes about command-line
options understood by all my programs', 'common_options'); ?>.
In particular, <code>castle --help</code> will show full list
of all available command-line options.

<?php /*
  <li><p><code>--no-shadows</code>

    <p>This disallows rendering of shadows in the game.
    Just like turning off "Video options -> Shadows" option, this will
    speed up rendering, since shadows will not be rendered.
    Moreover this command-line option will cause whole
    game to require less resources (we don't require stencil buffer
    from the OpenGL context) and loading creatures will be (very slightly)
    faster.

    <p>It's advised to use this option only if stencil buffer
    of your graphic card requires too much resources.
    Then setting "Video options -> Shadows" option will not help,
    but this command-line option will help.
    In other cases, just turning off menu option "Video options -> Shadows"
    is a sufficient way to speed up rendering.
  </li> */ ?>

<?php echo $toc->html_section(); ?>

<p>You can pick items (just walk on them), browse your items (keys: i, [, ]),
drop (key: r), equip and use (key: enter). When you have no weapon equipped,
then new picked weapon will be automatically equipped.
Unequipping happens automatically on drop.

<p>Items are "stackable", i.e. when you own two or more items
of the same kind, they are displayed like one item with quantity N.
Picking and dropping handle this appropriately (when dropping you
can drop only part of owned items).

<p>You can use the "e" key ("Interact") when looking at item lying on the level,
this tells you the name and quantity of visible object without picking it up.
(of course, if object is too far, you will not be able to tell it exactly).

<p>Dropping items may seem like a useless feature for now &mdash;
there is no limit on the amount of items you can carry, <i>right now</i>.
Dropping items will be more usefull in the the future,
when the game will get more RPG gameplay elements.
But even now dropping items may have a clever use: since collision detection
is implemented correctly, you can drop items to block some closing doors.
Beware that in the future some creatures may be able to
pick up your items and some doors may simply squish your precious items...

<?php echo $toc->html_section(); ?>

<p>For PGD competition (that ended on 2006-05),
version numbers were 0.y.z, where "y" was a stage
of competition. So e.g. "version 0.2.0" was what I submitted
at the end of stage 2 of competition. "z" was a release number.
The last version uploaded for PGD competition was 0.6.1.

<p>Now, after PGD competition ended, I continue development
using <?php echo a_href_page('my normal versioning scheme', 'versioning') ?>.

<?php echo $toc->html_section(); ?>

<p>If you want to modify game content heavily, I advice to get
familiar with our <i>debug menu</i>. The debug menu contains
many useful commands that will make your life much easier. By default,
debug menu is activated by backquote key (the one with tilde, the same
key used to bring console in many FPS games).

<p>Using the debug menu
you can turn on some special features useful for designers/debugging
(e.g. to see bounding volumes of objects) or turn off normal game features
that may be annoying when designing (e.g. stop time for creatures).

<p>You can also request a reload of particular creature/item/level VRML/XML
etc. files. This is extremely useful when you changed some data file
and you would like to quickly reload just this one file,
to see how things work now &mdash; without exiting the game.

<p>There are also some command-line debug options (but not too much &mdash;
it's always more flexible to have things available at runtime instead
of only at the start). Run the program with <code>--help</code>
to get their list.
<?php /*
<code>--debug-no-creatures</code> is one such useful option:
it avoids this lengthy "Loading creatures" loading time, which is useful
when you're not interested in testing creatures
(e.g. when you only design new level geometry).
*/ ?>

<?php
  castle_footer();
?>
