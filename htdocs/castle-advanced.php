<?php
  require_once "vrmlengine_functions.php";

  vrmlengine_header("The Castle &mdash; additional notes");

  $toc = new TableOfContents(
    array(
      new TocItem('Detailed requirements', 'requirements_detailed'),
      new TocItem('Window size', 'window_size'),
      new TocItem('Sound notes', 'sound'),
      new TocItem('Other command-line options', 'command_line'),
      new TocItem('Details about items in the game', 'items_details'),
      new TocItem('Version numbers', 'versioning')
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
Developers that want to help actually extend and modify the game
should read this, and additionally the page about
<?php echo a_href_page('"The Castle" development', 'castle-development') ?>.

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

<p>But there is a command-line option <tt>--screen-size</tt> that you
can use to change desired screen size. You can specify any screen sizes
(not necessarily matching some usual fullscreen sizes, so even something crazy
like <tt>--screen-size 600x700</tt> works). But beware that, while the
game works and is perfectly playable in any resolution, some 2D graphics and
menus really look best only in default 800x600 resolution.

<!--
<p>Reasoning: My practice shows that it's simply too difficult to really make
a game that looks perfect on any resolution, because any 2D graphics
is highly tied to resolution it was prepared for. Scaling 2D graphics,
even using really good algorithms, never results in optimal look.
My game is 3D, but still there are elements displayed in 2D,
so this still matters.
-->

<p>If your current screen resolution is 800x600 (or whatever you
requested by <tt>--screen-size</tt>), the game will
happily run in fullscreen. Otherwise the game will try to resize
your screen first (on exit, the screen size will
be restored to previous size). If this resizing will fail
(for whatever reason, e.g. you have poor graphics driver),
it will run in windowed mode.

<p>You can pass command-line option <tt>--no-screen-change</tt> (short form: <tt>-n</tt>)
to disable the automatic screen resizing. Then the game will
run in fullscreen mode only if your current screen size is appropriate,
otherwise it will run in windowed mode.
You can also set this from the main menu:
"Video options -> Allow screen settings change on startup".

<?php echo $toc->html_section(); ?>

<p>OpenAL is used to play sounds. See
<?php echo a_href_page(
  'my instructions about installing OpenAL and command-line
  options common for my programs using OpenAL',
  'openal_notes'); ?>.

<p>For best effect, be sure to try all available devices in "Sound options" &mdash;
the default device doesn't always sound the best.
This is true for both Unixes and Windows.
E.g. a standard sound card (without real 3d surround support) on Windows
may sound better with "Generic Software" device.

<p>If the game performance suffers when the sound is on,
or sound quality suffers, or there is sound latency, then you should
probably stop any other programs that play some music.

<?php echo $toc->html_section(); ?>

Other command-line options, not mentioned in this file before:
<ul>
  <li><p><tt>--debug-log</tt>

    <p>This will output a lot of info on stdout.
    When sending bug reports, it may be useful to attach generated log
    to your report &mdash; it will show some things about your
    OpenGL, OpenAL, and what happened during the game.

  <li><p>See <?php echo a_href_page('some general notes about command-line
    options understood by all my programs', 'common_options'); ?>.
    In particular, <tt>castle --help</tt> will show full list
    of all available command-line options.</li>

  <li><p><tt>--no-shadows</tt>

    <p>This disallows rendering of shadows in the game.
    Just like turning off "Video options -> Shadows" option, this will
    speed up rendering, since shadows will not be rendered.
    Moreover this command-line option will cause whole
    game to require less resources (we don't require stencil buffer
    from the OpenGL context) and loading creatures will be (very slightly)
    faster.

    <p>It's adviced to use this option only if stencil buffer
    of your graphic card requires too much resources.
    Then setting "Video options -> Shadows" option will not help,
    but this command-line option will help.
    In other cases, just turning off menu option "Video options -> Shadows"
    is a sufficient way to speed up rendering.
  </li>
</ul>

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

<?php
  if (!IS_GEN_LOCAL) {
    $counter = php_counter("castle-advanced", TRUE);
  };

  vrmlengine_footer();
?>
