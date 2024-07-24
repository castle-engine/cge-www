<?php
require_once 'castle_engine_functions.php';
castle_header("Darkest Before the Dawn", array(
  'social_share_image' => 'darkest_before_dawn_1.png',
));

echo pretty_heading('Darkest Before the Dawn');
?>

<?php
echo cgeImg('float', array(
  array('filename' => 'darkest_before_dawn_1.png', 'titlealt' => 'Darkest Before the Dawn - game screen 1'),
  array('filename' => 'darkest_before_dawn_2.png', 'titlealt' => 'Darkest Before the Dawn - game screen 2'),
  array('filename' => 'darkest_before_dawn_ui.png', 'titlealt' => 'Darkest Before the Dawn - title screen'),
));
?>

<p>A scary 3D game. <b>You're only safe within the light.</b>

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
  '2.0.0',
  'snapshot',
  'castle-engine',
  'darkest-before-dawn',
  'darkest_before_dawn',
  array(
    'google-play=net.sourceforge.castleengine.darkestbeforedawn',
    'android',
    'win64-x86_64',
    'linux-x86_64'
  )
);
?>

<?php
  castle_footer();
?>
