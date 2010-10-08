<?php
  require_once 'vrmlengine_functions.php';
  $main_page = true;
  vrmlengine_header('Kambi VRML game engine',
    'A free/open-source 3D (game) engine. Written in clean Object Pascal code, with an excellent support for the VRML / X3D (and other) 3D data formats. Cross-platform, using OpenGL, many advanced 3D features (shadows, mirrors) available.');
?>

<img src="images/kambi_vrml_game_engine_icon.png"
  alt="Kambi VRML game engine icon"
  align="right"/>

<?php /* echo pretty_heading("Kambi VRML game engine"); */  ?>

<p><b>For developers:</b> <b>Kambi VRML game engine</b>
is a free/open-source 3D (game) engine.
The engine is written in clean <a href="http://www.freepascal.org/">Object Pascal</a>
code, and we have an excellent support for the
<?php echo a_href_page('VRML / X3D', 'vrml_x3d'); ?> 3D data
(although other 3D model formats are also supported).
See here <?php echo a_href_page('for detailed documentation and downloads for developers', 'kambi_vrml_game_engine'); ?>.</p>

<p><b>For normal human beings:</b> this page is a collection of various
games and tools developed using our engine. <!--Most of them
are somewhat related to 3D graphics.-->
Two most important programs available here are
<a href="castle.php">"The Castle"</a> game and
<a href="view3dscene.php">view3dscene</a> 3D model viewer.</p>

<p><b>Latest update:</b>

<!-- To force engine icon separated from "latest update" frame on wide pages -->
<br clear="all"/>

<div class="latest_update_description" style="padding: 0px;">
<div style="padding: 0.5em">
<div class="rss_link"><a href="news_feed.php">RSS</a></div>
<?php
  require_once 'news_common.php';
  echo last_change_log_to_html(false);
?>
</div>

<p style="background: #DDD; padding: 0.2em; margin: 0px">
Comments?
Go to our <?php echo FORUM_LINK; ?> or <?php echo MAILING_LIST_LINK; ?>.<br/>
See also <?php echo
a_href_page('the news archive', 'news') ?>.
</p>

</div>

<p><b>Watch engine development</b>: To <i>really</i> watch the engine development
closely, you can watch the commits through
<a href="http://cia.vc/stats/project/vrmlengine">Kambi VRML game engine on
cia.vc</a>.</p>

<p style="margin-bottom: 0.1em;"><i><b>Author:</b>
<a href="http://michalis.ii.uni.wroc.pl/~michalis/">Michalis Kamburelis (aka Kambi)</a>
(<?php echo michalis_mailto('email'); ?>).</i></p>

<?php
  if (!IS_GEN_LOCAL) {
    $counter = php_counter("index", TRUE);
    /* echo "<p>Stronê odwiedzono " . $counter . " razy.";
       // chwilowo licznik dziala ukryty
    */
  };

  vrmlengine_footer();
?>
