<?php
define('CASTLE_GITHUB_NAME', 'darkest-before-dawn');

require_once 'castle_engine_functions.php';
castle_header("Darkest Before the Dawn");

define('VERSION_DARKEST_BEFORE_DAWN', '1.2.0');

echo pretty_heading('Darkest Before the Dawn', VERSION_DARKEST_BEFORE_DAWN);
echo castle_thumbs(array(
  array('filename' => 'darkest_before_dawn_1.png', 'titlealt' => 'Darkest Before the Dawn - game screen 1'),
  array('filename' => 'darkest_before_dawn_2.png', 'titlealt' => 'Darkest Before the Dawn - game screen 2'),
  array('filename' => 'darkest_before_dawn_ui.png', 'titlealt' => 'Darkest Before the Dawn - title screen'),
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

<?php
echo cge_download_application(
  '1.4.0',
  'snapshot',
  'castle-engine',
  'darkest-before-dawn',
  'darkest_before_dawn',
  array(
    'android',
    'win64-x86_64',
    'linux-x86_64'
  )
);
?>

<h2>Source code</h2>

<p>The full
<a href="https://github.com/castle-engine/darkest-before-dawn">source code and data are available on GitHub</a>.

<p>See the <code>README.md</code> for compilation instructions.
For a standalone compilation all you need is the
<a href="/">engine</a>
and <a href="http://www.freepascal.org/">FPC</a>.
To compile the Android version, you will need Android SDK, NDK, and FPC set
up as a cross-compiler to Android+Arm. See the <i>Castle Game Engine</i>
docs for details.

<h2>License</h2>

<p>The game code and some of the data are licensed on GNU GPL (version &gt;= 2).
The rest of the game data is licensed on Creative Commons (various
versions, see AUTHORS.txt files in the source code for details).
The underlying Castle Game Engine is licensed on more permissive
<a href="license">GNU LGPL with static linking exception</a>.

<?php
  castle_footer();
?>
