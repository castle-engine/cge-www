<?php
  require_once 'castle_engine_functions.php';
  $main_page = true;
  castle_engine_header('Castle Game Engine',
    'A free/open-source 3D (game) engine. Written in clean Object Pascal code, with an excellent support for the VRML / X3D (and other) 3D data formats. Cross-platform, using OpenGL, many advanced 3D features (shadows, mirrors) available.');
?>

<img src="images/castle_game_engine_icon.png"
  alt="Castle Game Engine icon"
  align="right"/>

<div style="float: right; margin: 1em;">
<?php flattr_button(false); ?>
<!-- 1+ button doesn't work (changes into exclamation mark), don't know why -->
<!--br/><br/>
<g:plusone></g:plusone-->
</div>

<p><b>For developers:</b> <b>Castle Game Engine (previously "Kambi VRML game engine")</b>
is a free/open-source 3D (game) engine.
The engine is&nbsp;written in clean <a href="http://www.freepascal.org/">Object Pascal</a>
code, and we have an&nbsp;excellent support for the&nbsp;<?php echo a_href_page('VRML / X3D', 'vrml_x3d'); ?> 3D data
(although other 3D model formats are also supported).
See here <?php echo a_href_page('for detailed documentation and&nbsp;downloads for developers', 'engine'); ?>.</p>

<p><b>For normal human beings:</b> this page is a&nbsp;collection of&nbsp;various
games and tools developed using our engine. <!--Most of them
are somewhat related to 3D graphics.-->
Two&nbsp;most important programs available here are
<a href="view3dscene.php">view3dscene</a> 3D model viewer and
<a href="castle.php">"The Castle"</a> game.</p>

<p><b>Latest update:</b>

<!-- To force engine icon separated from "latest update" frame on wide pages -->
<br clear="all" />

<div class="latest_update_description" style="padding: 0px;">
<div style="padding: 0.5em">
<div class="rss_link"><a href="news_feed.php">RSS</a></div>
<?php
  require_once 'news_common.php';
  echo last_news_to_html(false);
?>
</div>

<table class="news_older_newer"><tr>
  <td class="news_newer">Comments?
  Go to our <?php echo FORUM_LINK; ?> or <?php echo MAILING_LIST_LINK; ?>.</td>
  <td class="news_older">
  See also <?php echo
  a_href_page('previous news &raquo;', 'news.php?id=' . $news[1]['id']) ?></td>
</tr></table>

</div>

<p><b>Watch engine development</b>: To <i>really</i> watch the engine development
closely, you can watch the commits through
<a href="http://cia.vc/stats/project/castle-engine">Castle Game Engine on
cia.vc</a>. There is also <a href="https://www.ohloh.net/p/castle-engine">our
project page on Ohloh</a> (you're welcome to rate and click on
<i>"I use this"</i> button there!).</p>

<p style="margin-bottom: 0.1em;"><i><b>Author:</b>
<a href="http://michalis.ii.uni.wroc.pl/~michalis/">Michalis Kamburelis (aka Kambi)</a>
(<?php echo michalis_mailto('email'); ?>).</i></p>


<?php
  castle_engine_footer();
?>
