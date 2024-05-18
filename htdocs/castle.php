<?php
define('CASTLE_GITHUB_NAME', 'castle-game');

require_once 'castle_engine_functions.php';
castle_header("The Castle");

// echo flattr_button(); - Flattr not used now

echo pretty_heading('The Castle', VERSION_CASTLE);
echo castle_thumbs(array(
  array('filename' => 'castle_screen_demo_1.png', 'titlealt' => 'Image from "The Castle"'),
  array('filename' => 'castle_screen_demo_4.png', 'titlealt' => 'Image from "The Castle"'),
  array('filename' => 'castle_screen_demo_5.png', 'titlealt' => 'Image from "The Castle"'),
));

$toc = new TableOfContents(
  array(
    new TocItem('Note: This is a game from 2006 and has some programmer art :)', 'old'),
    new TocItem('Overview', 'overview'),
    new TocItem('Download (version 1.1.0)', 'download'),
    new TocItem('Installing', 'install', 1),
    new TocItem('Movie', 'movies'),
  )
);
?>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>Note that this game was done quite long time ago:
it was done for <a href="http://pascalgamedevelopment.com/">PascalGameDevelopment</a>
competition in 2006. I was tinkering with the code since then
(the <a href="https://github.com/castle-engine/castle-game/">source code from GitHub</a>
compiles with <a href="https://castle-engine.io/download">latest Castle Game Engine</a>).
But I never had time
to actually rework the 3D levels and creatures to something prettier.

<p>This is combined with the fact that the game graphics was composed by me (Michalis),
a programmer, not an artist.
I used various free 3D models, doing various adjustments and adding some simple
own models.

<p>So forgive 3D graphics that look quite dated now :)

<p>You want to see something pretty? Check out <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/fps_game">examples/fps_game</a> in CGE sources! This was done by real artists, and in 2022, and it looks amazing :)

<?php echo $toc->html_section(); ?>

<p><i>"The Castle"</i> is a first-person shooter style game in a dark
fantasy setting. Your main weapon is a sword, so the
fight is mostly short-range. 3 main levels included,
packed with creatures, items and sounds.</p>

<p>Also a couple of bonus levels are available, for fun and
to show off some engine features. In particular, there's the classic DOOM E1M1
level &mdash; I know you always wanted to have a sword and a bow in DOOM :)
Enjoy !</p>

<?php /*
<p><i>Requirements to run</i>: At least <i>512 MB
RAM</i> is recommended.<!--, although on some graphic cards even 256 MB may be enough (commented out: not on much, and sometimes 1GB is better....) -->
Fast and stable OpenGL is also needed
(read: <i>good graphic card with up-to-date drivers</i>).
The game was tested with various graphic cards and OpenGL implementations:
<a href="http://www.nvidia.com/">NVidia</a>, Radeon with closed drivers
from ATI and open-source ones,
<a href="http://www.mesa3d.org/">Mesa</a> OpenGL
also works (although pure software Mesa will usually be too slow).
*/
?>

<?php
/* Not so useful, on good graphic cards loading goes fast anyway.

<p>For a good graphic cards, you may decide to turn
<i>Conserve memory</i> to <i>No</i>. The game will eat much more
memory then (both in RAM and on graphic card), but you will have to
wait only once for "Loading creatures").
*/
?>

<?php
/* <p><i>History and plans:</i> This game is a little old now,
and it doesn't really use the capabilities of latest
<?php echo a_href_page('Castle Game Engine', 'index'); ?> versions.
This game was initially developed for
<a href="http://pascalgamedevelopment.com/">PascalGameDevelopment</a>
competition. It was at the beginning of 2006, at a time when
<?php echo a_href_page('our engine', 'index'); ?>
 didn't even have a version number yet :) <!-- (So, before 1.0.0 engine release.) -->
A new game, that actually uses the current capabilities
of our engine (we developed a lot of new features since 2006...), is planned.
*/
?>

<?php echo $toc->html_section(); ?>

<?php echo cge_download_application('1.1.0', 'snapshot',
  'castle-engine', 'castle-game', 'castle',
  array(
    //'android-debug',
    'win64-x86_64',
    'linux-x86_64',
    'darwin-x86_64',
  )); ?>

<?php echo $toc->html_section(); ?>

<ul>
  <li>
    <p><b>Windows:</b></p>

    <p>Extract the downloaded archive anywhere.
    Run the game by running <code>castle.exe</code>.</p>

    <p>Note that from the game you will be able to choose various
    audio devices (see menu <i>Sound options</i>).
    I advice to check both devices.
    Under Windows <i>Generic Software</i> device often sounds better
    than the default one. On the other hand, as far as I know, only
    <i>Generic Hardware</i> can produce sound better than stereo, if you
    have more than 2 speakers.</p>
    </li>

  <li>
    <p><b>Linux, FreeBSD:</b></p>

    <p>Installing actual game: extract the downloaded archive anywhere.
    Run the game by running the binary, like <code>./castle</code>.</p>

    <p>To hear game sounds you should
    install <a href="openal#_installing_openal">OpenAL</a> and <a href="http://xiph.org/vorbis/">VorbisFile</a> libraries using your Linux distribution package manager.

  <li>
    <p><b>macOS:</b></p>

    <p>Extract the archive, drag-and-drop the application wherever you like.

    <p>Since the application is <i>unsigned</i>, double-clicking the application for the first time will result in an error along the lines <i>"developer cannot be verified"</i>. You have to right-click on the application, choose <i>"Open"</i> from the context menu, and then you will be able to confirm that you want to open an unsigned application.

    <p>See <?php echo a_href_page(
    'macOS dependencies', 'doc/macos'); ?> for more macOS information.</p>

</ul>

<?php echo $toc->html_section(); ?>

<p>Short gameplay sequence and playing around with shadow volumes.
Note that this was done with quite old engine version,
in new version more things cast shadows.

<p><b>Spoiler alert</b>: the first half of the movie shows levels you will see
later in the game.</p>

<?php if (!HTML_VALIDATION) { ?>
<iframe width="425" height="349" src="https://www.youtube.com/embed/2XgQHo4DrGk" frameborder="0" allowfullscreen></iframe>
<?php } ?>

<?php
  castle_footer();
?>
