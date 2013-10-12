<?php
  require_once "castle_engine_functions.php";

  castle_header("The Castle &mdash; credits", NULL, array('castle'));

  $toc = new TableOfContents(
    array(
      new TocItem('Authors', 'authors'),
      new TocItem('Software used when developing this game', 'software_used'),
      new TocItem('Data used in this game', 'data_used'),
      new TocItem('DOOM E1M1 bonus level', 'doom', 1),
    )
  );
?>

<h1><?php echo a_href_page('The Castle', 'castle'); ?> &mdash; credits</h1>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p><a href="http://michalis.ii.uni.wroc.pl/~michalis/">Michalis Kamburelis</a>
did the whole programming and most of the 3D modelling.

<p><i>Szymon Stoma and Kaśka Zaremba</i> designed and modelled the scenario
on "The Gate" level (all models in <tt>data/levels/gate/gate_parts/</tt>),
modelled many 3D objects (keys, bow, quiver, arrows, ball_missile_s),
provided some sounds and a large amount of feedback.</p>

<p><i>Grzegorz Hermanowicz</i> implemented underwater "sick"
projection effect and started gravity for arrows implementation.</p>

<p><i>Michał Wojtkiewicz</i> provided the <tt>levels/gate/tree</tt> model.</p>

<p><i>Great thank you to everyone who contributed!</i></p>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p><?php echo a_href_page('Castle Game Engine', 'engine'); ?>.
    The base of our game.

    <p>We also used some engine tools and accompanying programs:
    <?php echo a_href_page('rayhunter', 'rayhunter'); ?>
    was used to render some items 2D images (see <tt>data/items/xxx/image.png</tt>),
    <?php echo a_href_page('view3dscene', 'view3dscene'); ?>
    was used to test and slightly edit 3D models,
    and to make many screenshots.
  </li>

  <li><a href="http://www.freepascal.org/">Free Pascal Compiler</a></li>
</ul>

<p>Graphics:
<ul>
  <li><a href="http://www.blender3d.org/">Blender</a></li>
  <li><a href="http://www.gimp.org/">GIMP</a></li>
  <li><a href="http://www.imagemagick.org/script/index.php">ImageMagick</a></li>

  <li><a href="http://www.planetside.co.uk/terragen/">Terragen</a>
    was used to create skies (see <tt>data/skies/</tt>).

  <li>Some post-processing of VRML models was done using EmacsLisp.
    See <tt>data/kambi-castle-utils.el</tt> and various Makefiles,
    like <tt>data/levels/gate/Makefile</tt>.
</ul>

<p>Sounds:
<ul>
  <li><a href="http://www.metadecks.org/software/sweep/">Sweep</a>
    and <a href="http://audacity.sourceforge.net/">Audacity</a> were used
    to edit sounds.</li>

  <li><a href="http://sox.sourceforge.net/">Sox</a> was used
    for some batch processing of sounds.
</ul>

<?php echo $toc->html_section(); ?>

<p>While most of the data used by the game was done
by "The Castle" team (see <a href="#section_authors">above</a>),
some data was obtained from the Internet,
under various "free" licenses. Unfortunately, not 100% of them are really
FOSS compatible, but we're working on it.
See <tt>castle/data/AUTHORS.txt</tt> for details.

<?php echo $toc->html_section(); ?>

<p>Inside the game I added, as a joke/experiment/tribute,
a level based on well-known
<a href="http://www.idsoftware.com/">id Software</a> game "Doom".
Level geometry and textures come from original DOOM E1M1
level, the level is just converted to VRML (and changed in many places
for my engine).</p>

<p>Thanks go to whole Id staff that produced DOOM. For a lot of technical details
how DOOM E1M1 level was done in my game, see the file
<tt>data/levels/doom/e1m1/README.txt</tt> in game data.
Music is taken from great <a href="http://www.sirgalahad.org/paul/doom/">"DOOM
I and II Music" as compiled by Paul Burdette</a>.

<div style="border:1px solid #aaaaaa; padding-left: 1em; padding-right: 1em">

  <p><i>Legal things</i>:

  <p><b>For distro packagers</b>: remember that the Doom level and it's resources
  (i.e. most of the things inside directories <tt>data/levels/doom/e1m1/</tt>,
  <tt>data/textures/doom/</tt> and <tt>data/sounds/doom/</tt>)
  are not open-source, are not
  covered by GNU GPL license, are not under my (Michalis Kamburelis)
  copyright etc. Legally, I guess that they are a heavily processed version of the
  content that was originally developed by Doom authors, and they own it.

  <p>If you want to redistribute my game as a pure open-source, you
  can simply remove these <tt>doom/</tt> subdirectories. The DOOM level
  will then disappear from the "New Game" menu.

  <p><b>For Id Software</b>: hopefully if any lawyer working for Id Software
  will ever spot this page, he will understand that the "Doom level" in my game is

  <ul>
    <li>Partially a joke and an experiment (how Doom level will look
      when rendered in modern technology, with free 3D and OpenGL and all).
    <li>Partially a tribute to Doom.
    <li>I include only the very first Doom level, E1M1,
      that is virtually known to every person who ever played any computer game,
      it was even released as a shareware after all.
    <li>The "Doom level" is not a main level of my game, it's just a bonus.
  </ul>

  <p>So don't sue me. Thanks :)

</div>

<?php
  castle_footer();
?>
