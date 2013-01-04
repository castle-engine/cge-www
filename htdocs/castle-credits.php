<?php
  require_once "castle_engine_functions.php";

  castle_header("The Castle &mdash; credits", NULL, array('castle'));

  $toc = new TableOfContents(
    array(
      new TocItem('Authors', 'authors'),
      new TocItem('Other used things', 'used_things'),
      new TocItem('Code', 'code', 1),
      new TocItem('Graphics', 'graphics', 1),
      new TocItem('Sounds', 'sounds', 1),
      new TocItem('DOOM E1M1 bonus level', 1),
    )
  );
?>

<h1><?php echo a_href_page('The Castle', 'castle'); ?> &mdash; credits</h1>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>Programming and modelling (of everything not covered below) :
<a href="http://michalis.ii.uni.wroc.pl/~michalis/">Michalis Kamburelis</a>.

<p><a href="http://stoma.bestweb.pl/">Szymon Stoma and Kaśka Zaremba</a>
designed and modelled the scenario
on "The Gate" level (all models in <tt>data/levels/gate/gate_parts/</tt>),
modelled various objects (keys, bow, quiver, arrows, ball_missile_s),
provided some sounds and a large amount of feedback. Thanks!
<!-- All their work is on GNU GPL,
contact Szymon &amp; Kaśka</a>
for other licensing.
--></p>

<p><a href="http://sourceforge.net/users/herrmannek/">Grzegorz Hermanowicz
(herrmannek)</a> implemented underwater "sick"
projection effect and started gravity for arrows implementation,
thanks ! Finally we have a 2nd guy with SVN write access,
so if something gets broken &mdash; that's totally not me :)</p>

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

<ul>
  <li><?php echo a_href_page('My "Castle Game Engine"', 'engine'); ?>.
    You can treat this engine as part of this game
    (I'm actually extending it while developing this game...),
    but it's generally useful.</li>

  <li>My font2pascal was used to generate font,
    see castlebitmapfont_bvsans_m10.pas and mk_fonts.sh</li>

  <li><a href="http://www.freepascal.org/">FreePascal</a> is a compiler
    that made this all possible.</li>
</ul>

<?php echo $toc->html_section(); ?>

<ul>
  <li><tt>data/levels/gate/cart</tt> model was
    found among the first 10 google results for "free 3d models".
    Terms are some open license (like artistic or gpl).</li>

  <li><tt>tree.blend</tt> based on <tt>tree02.3DS</tt>
    from Michał Wojtkiewicz. Thanks!</li>

  <li>Werewolf and Alien creatures' models based on free models from
    <a href="http://www.3dcafe.com/">www.3dcafe.com</a>.
    Spider's queen face also based on model from this site.</li>

  <li>Ghost's teeth are based on vampire teeth from
    <a href="http://www.katorlegaz.com/">www.katorlegaz.com</a>
    from characters/vamp.zip model.</li>

  <li><a href="http://www.blender3d.org/">Blender</a></li>
  <li><a href="http://www.gimp.org/">GIMP</a></li>
  <li><a href="http://www.imagemagick.org/script/index.php">ImageMagick</a></li>

  <li><a href="http://www.planetside.co.uk/terragen/">Terragen</a>
    was used to create sky (see data/skies/).
    This is the only non-open-source component I used... pity :(.
    But I don't know of any (open-source or not) alternative to Terragen.</li>

  <li>Many textures from <a href="http://www.wolfiesden.com/golgotha/golgotha.asp">public
    domain Golgotha textures</a></li>

  <li>Many textures from <a href="http://www.lemog.fr/lemog_textures/index.php">LeMog - 3d Textures</a>

  <li>Some post-processing of VRML models is done using EmacsLisp,
    see <tt>data/kambi-castle-utils.el</tt> and various Makefiles,
    like <tt>data/levels/gate/Makefile</tt>

  <li>stars.jpg <a href="http://www.astrooptik.com/Bildergalerie/latest_namibia/pictures.htm">grabbed
    from here</a>

  <li>Some of my other programs dealing with VRML were also used:
    <?php echo a_href_page('rayhunter', 'rayhunter'); ?>
    was used to render items 2D images (see <tt>data/items/xxx/image.png</tt>),
    <?php echo a_href_page('view3dscene', 'view3dscene'); ?>
    was often used to see how VRMLs look
    (independent of how they look when used in "The Castle"),
    and to generate some VRML code pieces, and some screenshots.</li>
</ul>

<?php echo $toc->html_section(); ?>

<ul>
  <li>I use <a href="http://www.metadecks.org/software/sweep/">Sweep</a>
    and <a href="http://audacity.sourceforge.net/">Audacity</a> to edit
    sounds.</li>
  <li><a href="http://sox.sourceforge.net/">Sox</a> is also a great program,
    suitable for batch processing of sounds.

  <li>Many freeware sounds from
    <a href="http://www.flashkit.com/soundfx/">www.flashkit.com/soundfx/</a>.

  <li>Sound loops (i.e. game music):

    <p>Found by
      <a href="http://www.flashkit.com/loops/">www.flashkit.com/loops/</a>.
      These loops are really great so I give here a link to all
      author's websites &mdash; if I'll forget some loop please remind me.

      <table border="1">
        <tr>
          <th>Place in this game</th>
          <th>Title and URL to flashkit</th>
          <th>Author</th>
        </tr>

        <tr>
          <td>Intro (initial menu)</td>
          <td>(don't remember)</td>
          <td><a href="http://www.edgen.com/">edgen</a></td>
        </tr>

        <tr>
          <td>"The Gate" level</td>
          <td>(don't remember)</td>
          <td><a href="http://calpomatt.lavista.org/">calpomatt</a></td>
        </tr>

        <tr>
          <td>Castle Hall" level</td>
          <td><a href="http://www.flashkit.com/soundfx/Ambience/TEMPLE-Adam_Goh-7394/index.php">Temple</a></td>
          <td><a href="http://www.sades.com/">Adam Goh</a></td>
        </tr>

        <tr>
          <td>"Cages" level</td>
          <td><a href="http://www.flashkit.com/loops/Ambient/Soundscapes/Cath-Mark-9479/index.php">Cathedral</a></td>
          <td><a href="http://reaction.roxr.com/">Mark</a></td>
        </tr>

        <tr>
          <td>Game finish sequence</td>
          <td><a href="http://www.flashkit.com/loops/Ambient/Ambient/Quiet-calpomat-5053/index.php">Quiet</a></td>
          <td><a href="http://calpomatt.lavista.org/">calpomatt</a></td>
        </tr>
      </table>
  </li>

  <li><tt>menu_current_item_changed.wav</tt> based on
    <tt>/usr/share/sounds/KDE_TypeWriter_Key_2.ogg</tt>
  <li><tt>werewolf_howling.wav</tt> from
    <a href="http://www.acs.ucalgary.ca/~mmusiani/">here</a>.
  <li><tt>tomb.wav</tt> from
    <a href="http://rasmus.uib.no/~st01369/filarkiv/lyder.html">here</a>
</ul>

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
  (i.e. most of the things inside directories <tt>data/levels/doom/</tt>,
  <tt>data/textures/doom/</tt> and <tt>data/sounds/doom/</tt>)
  are not open-source, are not
  covered by GNU GPL license, are not under my (Michalis Kamburelis)
  copyright etc. Legally, I guess that they are a heavily processed version of the
  content that was originally developed by Doom authors, and they own it.

  <p>If you want to redistribute my game as a pure open-source, you
  can simply remove these <tt>doom/</tt> subdirectories.
  You can also remove <tt>doom_e1m1</tt> entry inside
  <tt>data/levels/index.xml</tt>,
  so that the game doesn't crash when user tries to enter non-existing
  doom level.

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
