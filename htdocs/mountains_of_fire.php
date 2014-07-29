<?php
  define('CASTLE_ENGINE_CUSTOM_CSS', 'dark-game-theme.css');

  require_once 'castle_engine_functions.php';
  castle_header("Mountains Of Fire", NULL,
    array('all_programs', 'mountains_of_fire'));

  /* echo '<div style="float: right; width: 400px;">'; */
  /* echo googleplus_button(); */
  /* echo facebook_button(); */
  /* echo flattr_button(); */
  /* echo '</div>'; */

  echo pretty_heading('Mountains Of Fire', VERSION_MOUNTAINS_OF_FIRE);
  echo castle_thumbs(array(
    array('filename' => 'mountains_of_fire_screen_0.png', 'titlealt' => 'Mountains Of Fire - game screen 1'),
    array('filename' => 'mountains_of_fire_screen_1.png', 'titlealt' => 'Mountains Of Fire - game screen 2'),
//    array('filename' => 'mountains_of_fire_screen_2.png', 'titlealt' => 'Mountains Of Fire - game screen 3'),
    array('filename' => 'mountains_of_fire_screen_3.png', 'titlealt' => 'Mountains Of Fire - game screen 4'),
    array('filename' => 'mountains_of_fire_screen_4.png', 'titlealt' => 'Mountains Of Fire - game screen 5'),
//    array('filename' => 'mountains_of_fire_screen_5.png', 'titlealt' => 'Mountains Of Fire - game screen 6'),
  ));
?>

<p>
<!-- <i>Mountains of Fire</i> is a small 3D game. It features -->
This split-screen game shows the 3D world from two different perspectives:
<ol>
  <li>a worm, swimming in <i>hot</i> lava, and
  <li>a human, <i>carefully</i> walking over the rocky surface.
</ol>
Together, they cooperate to reach a safe place within the mountains
flooded with lava.
The game can be played by two players, or by a single player controlling
both human and sandworm simultaneously.
The controls were designed to be reachable by a single player.

<p>The game is open-source, completely free to download and play.
Try it now!
Based on an open-source
<a href="http://castle-engine.sourceforge.net/engine.php">Castle Game Engine</a>.

<?php
  $toc = new TableOfContents(
    array(
      new TocItem('Download', 'download'),
      new TocItem('Controls and hints how to play', 'controls'),
      new TocItem('Command-line options', 'command_line'),
      new TocItem('Source code', 'source'),
      new TocItem('License', 'license'),
    )
  );
  $toc->echo_numbers = true;
  echo $toc->html_toc();
?>

<?php echo $toc->html_section(); ?>

<div class="download">
  <table><tbody><tr>
    <td><a href="http://downloads.sourceforge.net/castle-engine/mountains_of_fire-<?php echo VERSION_MOUNTAINS_OF_FIRE; ?>-linux-i386.tar.gz"><img width="64" height="64" alt=" Linux&lt;br/&gt;(32 bit)" src="images/os_icons/linux32.png"><br> Linux<br>(32 bit)</a></td>
    <td><a href="http://downloads.sourceforge.net/castle-engine/mountains_of_fire-<?php echo VERSION_MOUNTAINS_OF_FIRE; ?>-win32-i386.zip"><img width="64" height="64" alt=" Windows&lt;br/&gt;(32 bit, works on 64-bit too)" src="images/os_icons/win.png"><br> Windows<br>(32 bit, works on 64-bit too)</a></td>
  </tr></tbody></table>
</div>

<div style="margin-top: 2em;">
<div class="bottom-widget" style="display: inline-block; vertical-align: top; padding: 0em; margin: 0em 1em 0em 1em;">
<?php echo googleplus_badge(); ?>
</div>

<div class="bottom-widget" style="display: inline-block; vertical-align: top; padding: 0em; margin: 0em 1em 0em 1em;">
<?php echo facebook_button(); ?>
</div>

<div class="bottom-widget" style="display: inline-block; vertical-align: top; padding: 0em; margin: 0em 0em 0em 1em;">
<?php echo flattr_button(false); ?>
</div>
</div>

<?php echo $toc->html_section(); ?>

<ol>
  <li>
    <p>Human views the world in 1st person. It walks over the terrain.
    In right-handed mode, human view is the right one.

    <p>Human is controlled only with mouse.
    Move mouse to look around.
    Click mouse left button to start/stop moving forward (no need to hold the button pressed),
    Click mouse right button to start/stop moving backward (no need to hold the button pressed).

    <p>Human is hurt by hot lava.
    Stand on a neutral ground (rocks) to regenerate,
    or very close to the worm when it freezes the lava (on blueish fluid).

    <p>When the human dies, game ends.
  </li>

  <li>
    <p>Worm views the world from the top. It can swim in lava.
    In right-handed mode, worm view is the left one.

    <p>Move the worm with AWSD.

    <p>Worm freezes lava into a water that is not harmful for player.
    Worm is hurt when it stays stationary for too long, as the heat
    is too much even for a sandworm.
    You have to move the worm to regenerate.

    <p>When the worm dies, human can still try to continue the game.
  </li>
</ol>

<p>Hints:
<ul>
  <li>Worm is visible in both views, even when it's obscured by a wall etc.
    If you're lost, human can look around, and see where the worm is.
</ul>

<p>Miscellaneous keys:
<ul>
  <li>F5 takes a screenshot.
  <li>Escape exit.
</ul>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p><tt>--left-handed</tt>

    <p>Run the game swapping the split screen order. This is useful if the mouse
    is on the left of the keyboard, then you want the left screen part
    to show human view (as human is controlled by mouse).

  <li><p><a href="http://castle-engine.sourceforge.net/opengl_options.php">Usual window size and fullscreen options</a>.
    By default we start in fullscreen.

  <li><p><a href="http://castle-engine.sourceforge.net/openal.php#section_options">Usual sound options</a>.
    For example <tt>--no-sound</tt>.

  <li><p><tt>--debug-log</tt>, <tt>--debug-speed</tt>
</ul>

<?php echo $toc->html_section(); ?>

<p>The full source code is available in the SVN repository,
and can be downloaded like this:

<pre class="terminal small"><?php echo sf_checkout_link(true, 'mountains_of_fire'); ?></pre>

<p>See the <tt>README.txt</tt> for compilation instructions.
For a standalone compilation all you need is the
<a href="http://castle-engine.sourceforge.net/engine.php">engine</a>
and <a href="http://www.freepascal.org/">FPC</a>.

<?php echo $toc->html_section(); ?>

<p>The game code and some of the data are licensed on GNU GPL (version &gt;= 2).
The rest of the game data is licensed on Creative Commons (various
versions, see AUTHORS.txt files in the source code for details).
The underlying Castle Game Engine is licensed on more permissive
<a href="http://castle-engine.sourceforge.net/engine.php#section_license">GNU
LGPL with static linking exception</a>.

<p>The game was originally developed
during the weekend gamejam (<a href="https://www.facebook.com/tsgcompo">TSG compo</a>)
on 21-22nd of June 2014.
Design, programming, modeling by Michalis Kamburelis.

<?php
  castle_footer();
?>
