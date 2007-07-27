<?php
  require_once 'vrmlengine_functions.php';

  camelot_header("The Castle", LANG_EN, '');
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
packed with creatures, items and sounds.

<p>Also a bonus level, from a well-known 3D game, will be available to you
from "New Game" menu once you finish the main game
(you can also switch to this level from the debug menu, if you're impatient).

<p><i>Requirement to run</i>: fast and stable OpenGL.
<ul>
  <li>Preferably <a href="http://www.nvidia.com/">NVidia</a> graphic card
    with latest drivers. And around 300 MB of free memory will be needed.

    <p>Radeon open-source drivers under Linux are reported to
    work OK too. Radeon cards inside MacBookPro (with closed ATI drivers,
    tested both under Linux and Mac OS X) also work perfectly fine.

    <p><a href="http://www.mesa3d.org/">Mesa</a> OpenGL
    also works (although usually will be too slow).
  </li>

  <li><p>As for Radeon with drivers from <a href="http://www.ati.com/">ATI</a>
    on Windows:

    <p>Around 500 MB free memory is required. Which means that practically
    a PC with 512 MB is usually not enough (OS, not to mention any
    other programs like WWW browser, will usually eat much more than 12 MB),
    so you usually need &gt; 512 MB memory (like 1 GB or more).
    On some Radeon models this may be different &mdash; your mileage may vary,
    as they say.

    <p>Also note that "Loading creatures" may take really long
    time &mdash; a couple of minutes. I know that this sucks, but that's it
    (for now, of course),
    If you'll have enough free memory, the game should be playable after
    a couple of minutes. Fortunately, "Loading creatures" takes a long time
    only when you enter new game for the first time (so when you change levels,
    or if you die and will retry the level, you will not have to wait again).
  </li>
</ul>

<p>For ambitious future plans about this game, see
<?php echo a_href_page_hashlink('my goals for this game',
'castle-development', 'section_goals'); ?>.

<h2><a name="section_download">Download</a></h2>

<?php echo_standard_program_download(
  '"The Castle", version ' . VERSION_CASTLE, 'castle',
  VERSION_CASTLE, true); ?>
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

<p>First install OpenAL:

<ul>
  <li><b>Unix (Linux, FreeBSD, Mac OS X) users :</b>
    To hear game sounds you should first
    <?php echo a_href_page_hashlink('install OpenAL', 'openal_notes',
    'section_install'); ?> and vorbisfile library.
    Be sure to install <tt>libvorbis-dev</tt> package
    (i.e. with developers symlinks) too &mdash; that's because of
    OpenAL's dumb behavior (without <tt>libvorbis-dev</tt> package
    OpenAL reports that vorbis extension is present, but it will actually fail
    to use it).</li>
  <li><b>Windows users</b>: OpenAL and vorbis libraries are already
    included in the archive. So you don't have to do anything.</li>
</ul>

<p>Then install the game:

<dl>
  <dt>Linux, FreeBSD, Mac OS X:</dt>

  <dd>Extract downloaded archive to
    <tt>/usr/local/share/castle/</tt> or <tt>$HOME/.castle.data/</tt>.
    You can move or symlink the executable <tt>castle</tt> to any place
    you like (e.g. <tt>$HOME/bin</tt> or <tt>/usr/local/bin</tt>).
    Run the game by running <tt>castle</tt>.</dd>

  <dt>Windows:</dt>

  <dd>Extract downloaded archive to any directory.
    Run the game by running <tt>castle.exe</tt>.

    <p>Note that from the game you will be able to choose various
    audio devices (see menu <i>Sound options</i>)
    &mdash; I advice to check both devices.
    Under Windows <i>Generic Software</i> device often sounds better
    than the default one.
  </dd>
</dl>

<h3><a name="subsection_uninstall">Uninstalling</a></h3>

Just delete the directory where you unpacked the game.
You may also want to delete configuration file:
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

  camelot_footer();
?>
