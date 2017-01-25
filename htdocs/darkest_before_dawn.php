<?php
define('CASTLE_ENGINE_CUSTOM_CSS', 'dark-game-theme.css');
define('CASTLE_GITHUB_NAME', 'darkest-before-dawn');

require_once 'castle_engine_functions.php';
castle_header("Darkest Before the Dawn", array(
  'path' => array('all_programs', 'darkest_before_dawn')
));

define('VERSION_DARKEST_BEFORE_DAWN', '1.2.0');

/* echo '<div style="float: right; width: 400px;">'; */
/* echo googleplus_button(); */
/* echo facebook_button(); */
/* echo flattr_button(); */
/* echo '</div>'; */

echo pretty_heading('Darkest Before the Dawn', VERSION_DARKEST_BEFORE_DAWN);
echo castle_thumbs(array(
  array('filename' => 'darkest_before_dawn_1.png', 'titlealt' => 'Darkest Before the Dawn - game screen 1'),
  array('filename' => 'darkest_before_dawn_2.png', 'titlealt' => 'Darkest Before the Dawn - game screen 2'),
  array('filename' => 'darkest_before_dawn_ui.png', 'titlealt' => 'Darkest Before the Dawn - title screen'),
  /* array('html' => */
  /*   '<div class="social_button">' . googleplus_button() . '</div>' . */
  /*   '<div class="social_button">' . facebook_button() . '</div>' . */
  /*   '<div class="social_button" style="margin: 0">' . flattr_button() . '</div>' */
  /* ), */
));
?>

<p><i>Darkest Before the Dawn</i> is a tiny game developed
by Michalis Kamburelis during the
<a href="http://tensquaregames.com/">TenSquareGames</a> gamejam on 23-24th
of November 2013. It is the first game made using
<?php echo a_href_page('Castle Game Engine', 'index'); ?> specifically
for Android (although it can be compiled as a normal, standalone
game for Linux, Windows etc. too).

<!--
<p>The game is available as an .apk file for Android.
On your Android device, make sure to enable installation of applications
from <i>Unknown sources</i> (in <i>Settings -> Security</i>,
<a href="http://developer.android.com/distribute/open.html#unknown-sources">like shown here</a>).
Then just download the apk file, and Android should
automatically propose to install it.
-->

<div class="download jumbotron">
<a href="https://play.google.com/store/apps/details?id=net.sourceforge.castleengine.darkestbeforedawn" class="btn btn-primary btn-lg">Install "Darkest Before the Dawn" for Android through Google Play</a>

  <br><br>

  Or download:

  <table><tbody><tr>
    <td style="padding-left: 0"><a href="http://downloads.sourceforge.net/castle-engine/darkest_before_dawn-linux-i386.tar.gz" class="btn btn-primary btn-lg"><img width="64" height="64" alt=" Linux&lt;br/&gt;(32 bit)" src="images/os_icons/linux32.png"><br> Linux<br>(32 bit)</a></td>
    <td><a href="http://downloads.sourceforge.net/castle-engine/darkest_before_dawn-win32.zip" class="btn btn-primary btn-lg"><img width="64" height="64" alt=" Windows&lt;br/&gt;(32 bit, works on 64-bit too)" src="images/os_icons/win.png"><br> Windows<br>(32 bit, works on 64-bit too)</a></td>
    <td><a href="http://downloads.sourceforge.net/castle-engine/darkest_before_dawn-release.apk" class="btn btn-primary btn-lg"><img width="64" height="64" alt=" Android" src="images/os_icons/android.png"><br> &nbsp;<br>Android</a></td>
  </tr></tbody></table>

  <?php echo download_donate_footer(); ?>
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

<p>The full
<a href="https://github.com/castle-engine/darkest-before-dawn">source code and data are available on GitHub</a>.

<p>See the <code>README.md</code> for compilation instructions.
For a standalone compilation all you need is the
<a href="index.php">engine</a>
and <a href="http://www.freepascal.org/">FPC</a>.
To compile the Android version, you will need Android SDK, NDK, and FPC set
up as a cross-compiler to Android+Arm. See the <i>Castle Game Engine</i>
docs for details.

<h2>License</h2>

<p>The game code and some of the data are licensed on GNU GPL (version &gt;= 2).
The rest of the game data is licensed on Creative Commons (various
versions, see AUTHORS.txt files in the source code for details).
The underlying Castle Game Engine is licensed on more permissive
<a href="license.php">GNU LGPL with static linking exception</a>.

<?php
  castle_footer();
?>
