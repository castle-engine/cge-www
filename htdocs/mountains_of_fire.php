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

<p><i>Mountains of Fire</i> is a small 3D game created by Michalis Kamburelis.
It features a split-screen view showing the 3D world from two different
perspectives:
<ol>
  <li>a worm, swimming in <i>hot</i> lava, and
  <li>a human, <i>carefully</i> walking over the rocky surface.
</ol>
Together, they cooperate to reach a safe place within the mountains
flooded with lava.
The game can be played by two players, or by a single player controlling
both human and sandworm simultaneously.
The controls were designed to be reachable by a single player.

<p>The game was developed during the weekend gamejam (<a href="https://www.facebook.com/tsgcompo">TSG compo</a>)
on 21-22nd of June 2014.


<div class="download">
  <table><tbody><tr>
    <td><a href="http://downloads.sourceforge.net/castle-engine/mountains_of_fire-<?php echo VERSION_MOUNTAINS_OF_FIRE; ?>-linux-i386.tar.gz"><img width="64" height="64" alt=" Linux&lt;br/&gt;(32 bit)" src="images/os_icons/linux32.png"><br> Linux<br>(32 bit)</a></td>
    <td><a href="http://downloads.sourceforge.net/castle-engine/mountains_of_fire-<?php echo VERSION_MOUNTAINS_OF_FIRE; ?>-win32-i386.zip"><img width="64" height="64" alt=" Windows&lt;br/&gt;(32 bit, works on 64-bit too)" src="images/os_icons/win.png"><br> Windows<br>(32 bit, works on 64-bit too)</a></td>
  </tr></tbody></table>
</div>

<div style="margin-top: 2em;">
<div class="bottom-widget" style="display: inline-block; vertical-align: top; padding: 0em; margin: 0em 1em 0em 1em;">
<?php echo googleplus_button(); ?>
</div>

<div class="bottom-widget" style="display: inline-block; vertical-align: top; padding: 0em; margin: 0em 1em 0em 1em;">
<?php echo facebook_button(); ?>
</div>

<div class="bottom-widget" style="display: inline-block; vertical-align: top; padding: 0em; margin: 0em 0em 0em 1em;">
<?php echo flattr_button(false); ?>
</div>
</div>

<h2>Source code</h2>

<p>The full source code is available in the SVN repository,
and can be downloaded like this:

<pre class="terminal small"><?php echo sf_checkout_link(true, 'mountains_of_fire'); ?></pre>

<p>See the <tt>README.txt</tt> for compilation instructions.
For a standalone compilation all you need is the
<a href="http://castle-engine.sourceforge.net/engine.php">engine</a>
and <a href="http://www.freepascal.org/">FPC</a>.

<h2>License</h2>

<p>The game code and some of the data are licensed on GNU GPL (version &gt;= 2).
The rest of the game data is licensed on Creative Commons (various
versions, see AUTHORS.txt files in the source code for details).
The underlying Castle Game Engine is licensed on more permissive
<a href="http://castle-engine.sourceforge.net/engine.php#section_license">GNU
LGPL with static linking exception</a>.

<?php
  castle_footer();
?>
