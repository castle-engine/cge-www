<?php
  require_once 'castle_engine_functions.php';
  castle_header("Demo movies", NULL, array('engine'));

  echo pretty_heading("Demo movies");
?>

<p>Various videos about our engine are at</p>

<p style="font-size: larger; font-weight: bold">
<a href="http://www.youtube.com/user/michaliskambi">Michalis Kamburelis YouTube channel</a>
</p>

<p>You can download AVI versions with (slightly) better quality
for some videos below.
For newer videos, just switch YouTube to "HD quality".

<ol>
  <li><?php echo current_www_a_href_size('Demo movie from "The Castle" game (AVI)', 'movies/1.avi'); ?> (<a href="http://www.youtube.com/watch?v=qpUTK3_r7Lc">on YouTube here</a>)</li>
  <li><?php echo current_www_a_href_size('Demo movie showing GLSL shaders and steep parallax mapping', 'movies/2.avi'); ?> (<a href="http://www.youtube.com/watch?v=ag-d-JGvHfQ">on YouTube here</a>)</li>
  <li><?php echo current_www_a_href_size('Demo movie from "The Rift"', 'movies/3.avi'); ?> (<a href="http://www.youtube.com/watch?v=daIrz3ehN_I">on YouTube here</a>)</li>
  <li><?php echo current_www_a_href_size('Flames', 'movies/fireplace_demo.avi'); ?> (<a href="http://www.youtube.com/watch?v=V-EJvVbi1DQ">on YouTube here</a>)</li>
</ol>

<p>Software that I use to create videos:

<ol>
  <li><p>First three videos above were recorded on 2008-05-01.
    Production entirely on Linux by free software:
    capture thanks to <a href="https://devel.neopsis.com/projects/yukon/">Yukon
    (OpenGL video capturing framework)</a>,
    converted to editable format by <a href="http://www.mplayerhq.hu/">mencoder</a>
    (lives can't directly open seom files),
    editing (glued, fading between parts) thanks to
    <a href="http://lives.sourceforge.net/">Lives (Linux Video Editing System)</a>.</p>

  <li><p>fireplace_demo recorded using <?php echo a_href_page('view3dscene',
    'view3dscene'); ?> "Screenshot to video" option.</p></li>

  <li><p>Most newer videos recorded by <a href="https://github.com/nullkey/glc/wiki">GLC</a>
    and eventually editer using <a href="http://www.blender.org/">Video Sequencer in Blender</a>.</p></li>
</ol>

<?php
  castle_footer();
?>
