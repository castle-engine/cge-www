<?php
  require_once 'vrmlengine_functions.php';

  common_header("The Castle", LANG_EN, '');
?>

<?php
  echo pretty_heading('The Castle', VERSION_CASTLE);

  echo '<table align="right">' .
    '<tr><td>' . medium_image_progs_demo("castle_screen_demo_1.png", "castle", false) .
    '<tr><td>' . medium_image_progs_demo("castle_screen_demo_4.png", "castle", false) .
    '<tr><td>' . medium_image_progs_demo("castle_screen_demo_5.png", "castle", false) .
    '</table>';
?>

<ol>
  <li><a href="#section_overview">Overview</a></li>
  <li><a href="#section_download">Download</a>
    <ol>
      <li><a href="#subsection_install">Installing</a></li>
      <li><a href="#subsection_uninstall">Uninstalling</a></li>
    </ol>
  </li>
  <li><a href="#section_freshmeat">Freshmeat entry</a></li>
  <li><?php echo a_href_page('Additional notes (troubleshooting)',
    'castle-advanced') ?></li>
  <li><?php echo a_href_page('Development', 'castle-development') ?></li>
  <li><?php echo a_href_page('Credits', 'castle-credits') ?></li>
</ol>

<h2><a name="section_overview">Overview</a></h2>

<p><i>"The Castle"</i> is a first-person shooter style game in a dark
fantasy setting. Your main weapon is a sword, so the
fight is mostly short-range. 3 main levels included,
packed with creatures, items and sounds.</p>

<p>Also a couple of bonus levels are available, for fun and
to show off some engine features. In particular, there's the classic DOOM E1M1
level &mdash; I know you always wished to had a sword and a bow in DOOM :)
Enjoy !</p>

<p><i>Requirements to run</i>: At least <i>512 MB
RAM</i> is recommended.<!--, although on some graphic cards even 256 MB may be enough (commented out: not on much, and sometimes 1GB is better....) -->
Fast and stable OpenGL is also needed
(read: <i>good graphic card with up-to-date drivers</i>).
The game was tested with various graphic cards and OpenGL implementations:
<a href="http://www.nvidia.com/">NVidia</a>, Radeon with closed drivers
from ATI and open-source ones,
<a href="http://www.mesa3d.org/">Mesa</a> OpenGL
also works (although pure software Mesa will usually be too slow).

<?php
/* Not so useful, on good graphic cards loading goes fast anyway.

<p>For a good graphic cards, you may decide to turn
<i>Conserve memory</i> to <i>No</i>. The game will eat much more
memory then (both in RAM and on graphic card), but you will have to
wait only once for "Loading creatures").
*/
?>

<p>For ambitious future plans about this game, see
<?php echo a_href_page_hashlink('my goals for this game',
'castle-development', 'section_goals'); ?>.

<h2><a name="section_download">Download</a></h2>

<p><?php
  echo_standard_program_download(
  '"The Castle", version ' . VERSION_CASTLE, 'castle',
  VERSION_CASTLE, $std_releases_post_1_2_0);
?>
 See <?php echo a_href_page('changes_log', 'changes_log') ?>
 for the list of changes since last release.

<p>This is free software, so if you're interested you're also welcome to
<?php echo sf_download('download game sources',
  'castle-' . VERSION_CASTLE . '-src.tar.gz', true); ?>.
This contains all Pascal sources, blender models,
some Makefiles, GIMP xcf files, etc.
You will also need
<?php echo a_href_page('Kambi VRML game engine', 'kambi_vrml_game_engine'); ?>
 if you want to recompile the game, see
<?php echo a_href_page_hashlink('here for more info',
'castle-development', 'section_compiling'); ?>.

<h3><a name="subsection_install">Installing</a></h3>

<ul>
  <li>
    <p><b>Linux, FreeBSD:</b></p>

    <p>To hear game sounds you should first
    <?php echo a_href_page_hashlink('install OpenAL', 'openal_notes',
    'section_install'); ?> and vorbisfile library.
    If installing the older OpenAL <i>Sample Implementation from Loki</i>
    be sure to install also <tt>libvorbis-dev</tt> package
    (i.e. with developers symlinks) too (otherwise SI reports that
    vorbis extension is present, but will fail to use it).
    This problem is not present with newer OpenAL-Soft,
    as it doesn't have vorbis extension and we will use our own
    handling then.
    <i>If you don't want to hear sounds, you don't have to install
    OpenAL.</i></p>

    <p>Installing actual game: extract the downloaded archive anywhere.
    Run the game by running the binary, like <tt>./castle</tt>.</p>

    <p>If you don't want to always run the binary from the game's
    directory, you can also extract the game (or make a symlink to extracted dir)
    to <tt>$HOME/.castle.data/</tt>, or <tt>/usr/local/share/castle/</tt>,
    or <tt>/usr/share/castle/</tt>. You can then
    move or symlink the binary <tt>castle</tt> to any place
    you like (e.g. <tt>$HOME/bin</tt> or <tt>/usr/local/bin</tt>).</p>

  <li>
    <p><b>Mac OS X:</b></p>

    <p>The game requires X11 server, libpng and (if you want to hear
    sound) OpenAL with vorbisfile. See <?php echo a_href_page(
    'Mac OS X dependencies', 'macosx_requirements'); ?> for simple
    instructions how to get them.</p>

    <p>Installing actual game: extract the downloaded archive anywhere.
    Run the game by running the binary, like <tt>./castle</tt>.
    You can also install / symlink in special dorectories, see
    above Linux notes.</p>

  <li>
    <p><b>Windows:</b></p>

    <p>OpenAL and vorbis libraries are already
    included in the archive. So you don't have to install OpenAL yourself.</p>

    <p>Installing actual game: extract the downloaded archive anywhere.
    Run the game by running <tt>castle.exe</tt>.</p>

    <p>Note that from the game you will be able to choose various
    audio devices (see menu <i>Sound options</i>).
    I advice to check both devices.
    Under Windows <i>Generic Software</i> device often sounds better
    than the default one. On the other hand, as far as I know, only
    <i>Generic Hardware</i> can produce sound better than stereo, if you
    have more than 2 speakers.</p>
    </li>
</ul>

<h3><a name="subsection_uninstall">Uninstalling</a></h3>

<p>Just delete the directory where you unpacked the game.
You may also want to delete configuration file:</p>

<table class="thin_borders">
  <tr><td>Unix (Linux, FreeBSD, Mac OS X)<td><tt>$HOME/.castle.conf</tt>
  <tr><td>Windows >= NT / 2000 / XP <td><tt>Documents and
    Settings\&lt;UserName&gt;\Application Data\castle.conf</tt>
  <tr><td>Windows 95 / 98 / ME <td><tt>&lt;program's directory&gt;\castle.conf</tt>
</table>

<h2><a name="section_freshmeat">Freshmeat entry</a></h2>

<p>Here's a link to
<a href="http://freshmeat.net/projects/castlegame/">"The Castle"
entry on freshmeat</a>. You can use this e.g. to subscribe to new
releases, so that you will be automatically notified about new
releases of "The Castle".</p>

<h2><a name="section_advanced">Additional notes (troubleshooting)</a></h2>

For some detailed notes how the game works, or what to try when things
don't work as they should (troubleshooting), see
<?php echo a_href_page('additional game notes', 'castle-advanced') ?>.

<h2><a name="section_development">Development</a></h2>

For information about how you can help, how to compile the game,
how to modify the game data &mdash; see the separate
<?php echo a_href_page('development page', 'castle-development') ?>.

<h2><a name="section_credits">Credits</a></h2>

See <?php echo a_href_page('credits page', 'castle-credits') ?>.

<?php
  if (!IS_GEN_LOCAL) {
    $counter = php_counter("castle", TRUE);
  };

  common_footer();
?>
