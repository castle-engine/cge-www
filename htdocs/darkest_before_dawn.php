<?php
  define('CASTLE_ENGINE_CUSTOM_CSS', 'darkest_before_dawn.css');

  require_once 'castle_engine_functions.php';
  castle_header("Darkest Before the Dawn", NULL,
    array('all_programs', 'darkest_before_dawn'));

  define('VERSION_DARKEST_BEFORE_DAWN', '1.0.0');

  /* echo '<div style="float: right; width: 400px;">'; */
  /* echo googleplus_button(); */
  /* echo facebook_button(); */
  /* echo flattr_button(); */
  /* echo '</div>'; */

  echo pretty_heading('Darkest Before the Dawn', VERSION_DARKEST_BEFORE_DAWN);
  echo castle_thumbs(array(
    array('filename' => 'darkest_before_dawn_1.png'),
    array('filename' => 'darkest_before_dawn_2.png'),
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
<?php echo a_href_page('Castle Game Engine', 'engine'); ?> specifically
for Android (although it can be compiled as a normal, standalone
game for Linux, Windows, Mac OS X etc. too).

<p>The game is available as an .apk file for Android.
On your Android device, make sure to enable installation of applications
from <i>Unknown sources</i> (in <i>Settings -> Security</i>,
<a href="http://developer.android.com/distribute/open.html#unknown-sources">like shown here</a>).
Then just download the apk file, and Android should
automatically propose to install it.

<div class="download">
<?php
  if (IS_GEN_LOCAL)
  {
    echo '<p><a href="' . CURRENT_URL . $this_page_name .
      '">Download the game from it\'s WWW page</a>.</p>';
  } else
  {
    ?>
    <?php echo sf_download('Download "Darkest Before the Dawn" apk for Android', 'darkest_before_dawn-' . VERSION_DARKEST_BEFORE_DAWN . '.apk'); ?>
    <?php
  }
?>
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

<pre class="terminal small"><?php echo sf_checkout_link(true, 'darkest_before_dawn'); ?></pre>

<p>See the <tt>README.txt</tt> for compilation instructions.
For a standalone compilation all you need is the
<a href="http://castle-engine.sourceforge.net/engine.php">engine</a>
and <a href="http://www.freepascal.org/">FPC</a>.
To compile the Android version, you will need Android SDK, NDK, and FPC set
up as a cross-compiler to Android+Arm.

<h2>License</h2>

<p>The game code and data are licensed on GNU GPL (version &gt; 2).
Except some game data that is licensed on various Creative Commons
versions, see AUTHORS.txt files in the source code for details.
The underlying Castle Game Engine is licensed on more permissive
<a href="http://castle-engine.sourceforge.net/engine.php#section_license">GNU
LGPL with static linking exception</a>.

<?php
  castle_footer();
?>
