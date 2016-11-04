<?php
require_once 'castle_engine_functions.php';

castle_header("lets_take_a_walk", array(
  'path' => array('all_programs'),
  'meta_description' => 'A small toy demonstrating rendering 3D graphic and spatial sound with Castle Game Engine.',
));

$toc = new TableOfContents(
  array(
    new TocItem('Download', 'download'),
    new TocItem('Optional command-line options', 'options'),
    new TocItem('Requirements', 'depends'),
    new TocItem('Things used when making this game', 'credits')
  )
);
?>

<?php
  echo pretty_heading($page_title, VERSION_LETS_TAKE_A_WALK);
  echo default_program_thumbnail("lets_take_a_walk");
?>

<p><b>Let's take a walk</b> is not really a game. It's rather a toy &mdash;
just a demo of what I can achieve with my VRML + OpenGL + OpenAL code
(read: 3d graphic and spatial audio fun). Just download it, gasp in awe,
and have fun for a couple of minutes. Programmers may have fun
for a little longer investigating program's
<?php echo a_href_page('sources', 'all_programs_sources'); ?>.

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>Note that to hear game sounds you should first
<?php echo a_href_page_hashlink('install OpenAL', 'openal',
  'section_install'); ?>. Windows users don't have to do anything,
appropriate DLLs are already is the archive.

<?php
  /* potrzeba mi
     - binarki
     - cross-platform data
     - libpng&zlib
     - openal'a

     pod Windowsem:
     lets_take_a_walk_win.zip zawiera wszystkie trzy pierwsze spakowane
     w jednym zipie - dla wygody wielu ludzi którzy będą chcieli uzywac
     mojej gry tylko pod Windowsem i nie zrozumieliby dlaczego musza
     downloadowac az 3 pliki.

     pod Unixami:
     OpenAL i libpng&zlib musza sobie sami zainstalowac. Binarke i data wrzucam
     w jeden plik, lets_take_a_walk_linux.tar.gz, aby bylo analogicznie
     jak pod Windowsem, mimo ze i tak moga chciec wrzucic binarke do innego
     katalogu.

     Dorzucam jeszcze do archiwow docs skopiowane z local_html_versions.
  */
?>

<?php echo_standard_program_download(
  'lets_take_a_walk', 'lets_take_a_walk', VERSION_LETS_TAKE_A_WALK); ?>

<p><i>Installation</i>: just extract the downloaded archive anywhere.
Run the game binary (<code>./lets_take_a_walk</code> on Unix,
<code>lets_take_a_walk.exe</code> on Windows).</p>

<p>Press F1 in the game to get help.

<p>This is free/open-source software.
Sources of <code>lets_take_a_walk</code> live as an example inside
engine sources, in <code>castle_game_engine/examples/3d_sound_game/</code>.</p>

<?php echo $toc->html_section(); ?>

<p>You can use various command-line options when running lets_take_a_walk:

<ul>
  <li><?php echo a_href_page_hashlink(
    'Standard command-line options for my programs using OpenAL',
    'openal', 'section_options'); ?>
  <li><?php echo a_href_page(
    'Standard command-line options for my programs using OpenGL',
    'opengl_options'); ?>. Note that by default this program
    runs in fullscreen mode.
  <li><?php echo a_href_page(
    'Standard command-line options for all my programs', 'common_options'); ?>
</ul>

<?php echo $toc->html_section(); ?>

<?php echo depends_ul( array(
  DEPENDS_OPENGL,
  DEPENDS_LIBPNG_AND_ZLIB,
  SUGGESTS_OPENAL,
  DEPENDS_MACOSX) ); ?>

<?php echo $toc->html_section(); ?>

<p>Compiled by <a href="http://www.freepascal.org">FPC</a>.
Important libraries used: <a href="http://www.opengl.org">OpenGL</a>,
<a href="http://connect.creativelabs.com/openal">OpenAL</a>,
<a href="http://www.libpng.org">libpng</a>.

<p>Sounds from
<a href="http://www.a1freesoundeffects.com/">A1 Free Sound Effects</a>.
Textures done with <a href="http://www.gimp.org">GIMP</a>.
<code>user_help</code> and <code>mute_label_scroll</code> used a little help from
<a href="http://www.imagemagick.org/">ImageMagick</a>.
<code>base_shadowed.png</code> generated using my program <code>gen_light_map</code>
(available as part of <?php echo a_href_page('engine sources', 'index'); ?>,
see <code>examples/vrml/tools/gen_light_map.lpr</code> file).
Sky in <code>skies/</code> done using <a href="http://www.planetside.co.uk/">
Terragen</a>.
<code>base_b_proc.wrl</code> done in <a href="http://www.blender3d.org">Blender</a>
and processed using my small <code>process_base_b</code> program
(available in <?php echo a_href_page('lets_take_a_walk sources', 'index'); ?>,
see <code>lets_take_a_walk/devel/vrml/process_base_b.lpr</code> file).
<code>rat.wrl</code> and <code>tree.wrl</code> are modified (using Blender)
files from  <a href="http://www.3dcafe.com">www.3dcafe.com</a>.

<?php
  castle_footer();
?>
