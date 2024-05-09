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
    new TocItem('Download', 'download'),
    new TocItem('Installing', 'install', 1),
    new TocItem('Uninstalling', 'uninstall', 1),
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

<?php
  echo_standard_program_download(
  '"The Castle", version ' . VERSION_CASTLE, 'castle',
  VERSION_CASTLE);
?>

<p>See <?php echo a_href_page('news', 'news') ?> for
the list of changes since last release.</p>

<?php echo $toc->html_section(); ?>

<ul>
  <li>
    <p><b>Linux, FreeBSD:</b></p>

    <p>To hear game sounds you should
    install <a href="openal#_installing_openal">OpenAL</a> and <a href="http://xiph.org/vorbis/">VorbisFile</a> libraries using your Linux distribution package manager.

    <p>Installing actual game: extract the downloaded archive anywhere.
    Run the game by running the binary, like <code>./castle</code>.</p>

    <p>If you don't want to always run the binary from the game's
    directory, you can also put the game binary anywhere on $PATH
    (e.g. <code>$HOME/bin</code> or <code>/usr/local/bin</code>)
    and extract the <code>data</code> to
    to <code>$HOME/.local/share/castle/</code>, or <code>/usr/local/share/castle/</code>,
    or <code>/usr/share/castle/</code>.</p>

  <li>
    <p><b>macOS:</b></p>

    <p>The game requires X11 server, libpng and (if you want to hear
    sound) OpenAL with vorbisfile. See <?php echo a_href_page(
    'macOS dependencies', 'doc/macos'); ?> for simple
    instructions how to get them.</p>

    <p>Installing actual game: extract the downloaded archive anywhere.
    Run the game by running the binary, like <code>./castle</code>.
    You can also install / symlink in special directories, see
    above Linux notes.</p>

  <li>
    <p><b>Windows:</b></p>

    <p>OpenAL and vorbis libraries are already
    included in the archive. So you don't have to install OpenAL yourself.</p>

    <p>Installing actual game: extract the downloaded archive anywhere.
    Run the game by running <code>castle.exe</code>.</p>

    <p>Note that from the game you will be able to choose various
    audio devices (see menu <i>Sound options</i>).
    I advice to check both devices.
    Under Windows <i>Generic Software</i> device often sounds better
    than the default one. On the other hand, as far as I know, only
    <i>Generic Hardware</i> can produce sound better than stereo, if you
    have more than 2 speakers.</p>
    </li>
</ul>

<?php echo $toc->html_section(); ?>

<p>Just delete the directory where you unpacked the game.
You may also want to delete configuration file:</p>

<table class="thin_borders">
  <tr><td>Unix (Linux, FreeBSD, macOS)<td><code>$HOME/.config/castle/castle.conf</code>
  <tr><td>Windows >= NT / 2000 / XP <td><code>Documents and
    Settings\&lt;UserName&gt;\Application Data\castle.conf</code>
  <tr><td>Windows 95 / 98 / ME <td><code>&lt;program's directory&gt;\castle.conf</code>
</table>

<?php echo $toc->html_section(); ?>

<p>Short gameplay sequence and playing around with shadow volumes.
Note that this was done with quite old engine version,
in new version more things cast shadows.

<p><b>Spoiler alert</b>: the first half of the movie shows levels you will see
later in the game.</p>

<?php if (!HTML_VALIDATION) { ?>
<iframe width="425" height="349" src="https://www.youtube.com/embed/2XgQHo4DrGk" frameborder="0" allowfullscreen></iframe>
<?php } ?>
</div>

<?php
  castle_footer();
?>
