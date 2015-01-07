<?php
require_once 'castle_engine_functions.php';
require_once 'news_common.php';

global $main_page;
$main_page = true;

castle_header('Castle Game Engine',
  'A free/open-source 3D (game) engine. Written in clean Object Pascal code, with an excellent support for the VRML / X3D (and other) 3D data formats. Cross-platform, using OpenGL, many advanced 3D features (shadows, mirrors) available.');
?>

<div class="row">
  <div class="col-md-4">
    <div class="main_link">
      <p><b>For developers:</b></p>
      <p><a href="engine.php"><img src="images/main_link_size/castle_game_engine_icon.png" alt="Castle Game Engine icon" /></a></p>
      <p class="program_image_link_title"><a href="engine.php"><b>Castle Game Engine</b></a></p>
      <p><!--The engine is where all the magic actually happens :)-->
      Open-source (LGPL) 3D and 2D game engine for <a href="http://www.freepascal.org/">FreePascal / Lazarus</a>. Excellent support for the&nbsp;<?php echo a_href_page('VRML / X3D', 'vrml_x3d'); ?> 3D data format, other 3D formats also supported. Many advanced graphic effects.</p>
    </div>
  </div>

  <div class="col-md-4">
    <div class="main_link">
      <p><b>For 3D modelers:</b></p>
      <p><a href="view3dscene.php"><img src="images/main_link_size/castle_sunset.png" alt="view3dscene" /></a></p>
      <p class="program_image_link_title"><a href="view3dscene.php"><b>view3dscene</b></a></p>
      <p>VRML / X3D browser, and a viewer for other 3D model formats (Collada, 3DS, MD3, Wavefront OBJ...). Explore the virtual world with collisions, gravity, animations, shadows, mirrors, shaders and more. <!--use embedded ray-tracer, --> Convert models to VRML/X3D.</p>
      <p>Don't forget to also <a href="demo_models.php">download our collection of demo models</a>.</p>
    </div>
  </div>

  <div class="col-md-4">
    <div class="main_link">
      <p><b>For players:</b></p>
      <p><a href="mountains_of_fire.php"><img src="images/main_link_size/mountains_of_fire_screen_1.png" alt="Mountains Of Fire" /></a></p>
      <p class="program_image_link_title"><a href="mountains_of_fire.php"><b>Mountains Of Fire</b></a></p>
      <p>3D game with split-screen view where human and worm cooperate to survive. For single player or 2 players.</p>
      <!-- <p><a href="darkest_before_dawn.php"><img src="images/main_link_size/darkest_before_dawn_2.png" alt="Darkest Before the Dawn" /></a></p> -->
      <!-- <p class="program_image_link_title"><a href="darkest_before_dawn.php"><b>Darkest Before the Dawn</b></a></p> -->
      <!-- <p>Small scary 3D game, for Android and standalone (Linux, Windows).</p> -->
    </div>

    <div class="main_link">
      <p class="program_image_link_title"><a href="all_programs.php"><b>More games</b></a></p>
      <p>... and other tools using our engine.</p>
    </div>
  </div>
</div>

<div class="main-widgets">
<div class="bottom-widget">
<b style="font-size: larger;"><?php echo last_news_to_html(); ?></b>
</div>

<div class="bottom-widget">
<?php echo googleplus_badge(); ?>
</div>

<div class="bottom-widget">
<?php echo facebook_button(); ?>
</div>

<!--
<div class="bottom-widget">
<div><?php echo twitter_widget(); ?></div>
</div>

<div class="bottom-widget">
<?php echo youtube_subscribe(false); ?>
</div>

<div class="bottom-widget">
<?php echo flattr_button(false); ?>
</div>
-->
</div>

<?php
  castle_footer();
?>
