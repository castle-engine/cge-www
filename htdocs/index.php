<?php
require_once 'castle_engine_functions.php';
require_once 'news_common.php';

$main_page = true;

castle_header('Castle Game Engine',
  'A free/open-source 3D (game) engine. Written in clean Object Pascal code, with an excellent support for the VRML / X3D (and other) 3D data formats. Cross-platform, using OpenGL, many advanced 3D features (shadows, mirrors) available.');
?>

<table class="main_links" cellspacing="20">
  <tr>
    <td class="main_link">
      <p><b>For players:</b></p>
      <p><a href="castle.php"><img src="images/main_link_size/castle_screen_demo_1.png" alt="&quot;The Castle&quot;" /></a></p>
      <p class="program_image_link_title"><a href="castle.php"><b>&quot;The Castle&quot;</b></a></p>
      <p>First-person perspective game, in a dark fantasy setting.</p>
    </td>

    <td class="main_link">
      <p><b>For 3D modelers:</b></p>
      <p><a href="view3dscene.php"><img src="images/main_link_size/castle_sunset.png" alt="view3dscene" /></a></p>
      <p class="program_image_link_title"><a href="view3dscene.php"><b>view3dscene</b></a></p>
      <p>VRML / X3D browser, and a viewer for other 3D model formats (Collada, 3DS, MD3, Wavefront OBJ, some others). Explore the virtual world, with collision-checking, gravity and interactive animations, <!--use embedded ray-tracer, --> and convert various models to VRML/X3D.</p>
      <p>Don't forget to also <a href="demo_models.php">download our collection of demo models</a>.</p>
    </td>

    <td class="main_link">
      <p><b>For developers:</b></p>
      <p><a href="engine.php"><img src="images/main_link_size/castle_game_engine_icon.png" alt="Castle Game Engine icon" /></a></p>
      <p class="program_image_link_title"><a href="engine.php"><b>Castle Game Engine</b></a></p>
      <p>Open-source (LGPL) 3D game engine for <a href="http://www.freepascal.org/">FreePascal / Lazarus</a>. We have an&nbsp;excellent support for the&nbsp;<?php echo a_href_page('VRML / X3D', 'vrml_x3d'); ?> 3D data (although other 3D model formats are also supported).</p>
      <p>The engine is where all the magic actually happens :)</p>
    </td>
  </tr>
</table>

<div style="width: 100%; text-align: center">
<div style="display: inline-block; padding: 0em; margin: 0em 1em 0em 0em;">
<?php flattr_button(false); ?>
</div>

<div style="display: inline-block; padding: 0em; margin: 0em 0em 0em 1em;">
<!-- If you have problems with +1 button,
  http://www.google.com/support/forum/p/Webmasters/thread?tid=04ed1149585fd12f&hl=en
  contains many hints.
  For me, what worked was to enable "Accept Third-Party Cookies" in FF. -->
<?php if (!HTML_VALIDATION) { ?>
<g:plusone></g:plusone>
<?php } ?>
</div>
</div>

<?php /*
<p style="margin-bottom: 0.1em;"><i><b>Author:</b>
<a href="http://michalis.ii.uni.wroc.pl/~michalis/">Michalis Kamburelis (aka Kambi)</a>
(< ?php echo michalis_mailto('email'); ? >).</i></p>
*/ ?>

<!-- Some dumb separation from footer -->
<br/>
<br/>

<?php
  castle_footer();
?>
