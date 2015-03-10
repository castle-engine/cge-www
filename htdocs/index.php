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
      Open-source (LGPL) game engine for <a href="http://www.freepascal.org/">FreePascal and Lazarus</a>. Excellent support for many 3D and 2D data formats, portable (desktops, Android, iOS...), many advanced graphic effects, comfortable API.</p>
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
      <?php
        $featured_game_page_name = 'http://michaliskambi.itch.io/hydra-battles';
        $featured_game_screenshot_name = 'hydra_battles_screen_best';
        $featured_game_name = 'Hydra Battles';
        $featured_game_description = 'Isometric RTS game for 2 players, with some twists. <a href="https://github.com/michaliskambi/hydra-battles">Source code on GitHub</a>.';
      ?>
      <p><?php echo a_href_page('<img src="images/main_link_size/' . $featured_game_screenshot_name .'.png" alt="' . $featured_game_name . '" />', $featured_game_page_name); ?>
      <p class="program_image_link_title"><?php echo a_href_page('<b>' . $featured_game_name . '</b>', $featured_game_page_name); ?></p>
      <p><?php echo $featured_game_description; ?></p>
    </div>

    <div class="main_link">
      <p class="program_image_link_title"><a href="all_programs.php"><b>More games</b></a></p>
      <p>... and other tools using our engine.</p>
    </div>
  </div>
</div>

<div class="row">
  <div class="col-sm-4">
    <div class="main-widgets">
      <div class="bottom-widget">
      <b style="font-size: larger;"><?php echo last_news_to_html(); ?></b>
      </div>

      <div class="bottom-widget">
      <?php echo facebook_button(); ?>
      </div>

      <div class="bottom-widget">
      <?php echo googleplus_badge(); ?>
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
  </div>
  <div class="col-md-8">
    <?php echo disqus_form(); ?>
  </div>
</div>

<?php
  castle_footer();
?>
