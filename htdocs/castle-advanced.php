<?php
require_once "castle_engine_functions.php";
castle_header("The Castle - additional notes");

$toc = new TableOfContents(
  array(
    new TocItem('Details about items in the game', 'items_details'),
    new TocItem('Version numbers', 'versioning'),
    new TocItem('Debug menu, debug options', 'debug_menu_options'),
  )
);
?>

<h1><?php echo a_href_page('The Castle', 'castle'); ?> &mdash;
additional notes</h1>

This page collects various additional notes about running
<?php echo a_href_page('"The Castle"', "castle") ?>.
If all you want is just to run the game, then <i>don't read this page</i>.
You don't need this. This page explains some details that may be needed
for advanced users and developers to understand how the game works,
but normal users will not find here anything useful.

<p>Contents:
<?php echo $toc->html_toc(); ?>

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
