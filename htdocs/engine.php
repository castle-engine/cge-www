<?php
  require_once 'castle_engine_functions.php';

  castle_header("Engine overview for developers", NULL, array('engine'));

  $toc = new TableOfContents(
    array(
      new TocItem('Features', 'features'),
      new TocItem('Download sources', 'download_src'),
      new TocItem('Engine sources', 'engine_src', 1),
      new TocItem("Other programs' sources", 'program_sources', 1),
      new TocItem("Subversion (SVN) notes", 'svn', 1),
      new TocItem('FPC (Free Pascal Compiler) versions', 'fpc_ver', 1),
      new TocItem('License', 'license'),
      new TocItem('Documentation', 'docs'),
      new TocItem('Automatic tests', 'tests')
    )
  );
  $toc->echo_numbers = true;

  flattr_button();

  echo pretty_heading($page_title, VERSION_CASTLE_GAME_ENGINE);
  echo castle_thumbs(array(
    array('filename' => 'castle_fountain_1.png', 'titlealt' => 'Fountain water'),
    array('filename' => 'multiple_viewports_dynamic_world.png', 'titlealt' => 'multiple_viewports: interactive scene, with shadows and mirror'),
    array('filename' => 'castle_siege_shadows.png', 'titlealt' => 'castle_siege model from DeleD sample models, with shadows'),
    array('filename' => 'chinchilla_diffuse_prt.png', 'titlealt' => 'Precomputed Radiance Transfer'),
    array('filename' => 'screen_effects_demo3.png', 'titlealt' => 'Demo of three ScreenEffects defined in VRML/X3D, see screen_effects.x3dv'),
    array('filename' => 'sunny_street_tree_hard.png', 'titlealt' => 'Close up shadows on the tree. Notice that leaves (modeled by alpha-test texture) also cast correct shadows.'),
    array('filename' => 'terrain1.png', 'titlealt' => 'Terrain 1'),
    array('filename' => 'water_reflections.png', 'titlealt' => 'Water reflections by optimized GeneratedCubeMapTexture'),
    array('filename' => 'tex3d_smoke.png', 'titlealt' => 'Fog from 3D noise'),
));
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>This is an open-source (<a href="#section_license">LGPL / GPL</a>)
game engine. In short:

<ul>
  <li><p>Our main 3D scene format is <?php echo a_href_page('VRML / X3D', 'vrml_x3d'); ?>, which is an open standard (you'll find you can export to it from virtually any 3D modeler), and a lot of our strength comes from it (it's a 3D scene format that can also express interactive world features, scripting etc.). Even if you don't know VRML/X3D, the whole engine is designed as a general-purpose 3D engine, and other 3D model formats are supported as well (Collada, Wavefront, MD3 and others).</p></li>

  <li><p>The engine is developed for the <a href="http://freepascal.org/">Free Pascal Compiler</a>, an open-source cross-platform compiler, and the engine is cross-platform as well (Linux, Mac OS X, Windows, and more). We have <a href="http://lazarus.freepascal.org/">Lazarus</a> components for RAD development, although the core engine doesn't depend on Lazarus LCL and you can develop full games with pure FPC (we have our own OpenGL window management unit, if you want).</p></li>
</ul>

<p>More exhaustive list of the features:</p>

<ul>
  <li><b>Optimized OpenGL rendering</b> of models in
    <b>X3D, VRML 2.0 (97) and VRML 1.0</b> formats.
    Including support for advanced VRML/X3D features like prototypes and
    events (user can interact with the 3D world).</li>

  <li><b>Collada, 3DS, MD3, Wavefront OBJ</b> file formats are also supported.
    They are internally converted into the VRML/X3D nodes graph,
    which means that they get all the optimizations for rendering,
    and 3D content from all file formats can be mixed (for 3D editing tools
    and such).</li>

  <li><b>Saving</b> the current state of VRML/X3D node graph
    to standardized XML and classic encodings.<!-- is also fully supported and tested.-->
    You can even use it to make your own 3D modeller on top of our engine.
    Various <?php echo a_href_page_hashlink('conversions between 3D model formats', 'view3dscene', 'section_converting'); ?>
    and limited editing capabilities are provided out-of-the-box by our tools.</li>

  <li><b>Animations</b> are supported,
    <a href="vrml_engine_doc/output/xsl/html/chapter.animation.html">in two flavors</a>:
    interactive animation interpolated at runtime,
    or precalculated animation for fast playback.</li>

  <li>Octrees are used for various <b>collision detection</b> tasks.
    For dynamic scenes, a hierarchy of octrees is used, allowing accurate
    and fast collision detection even when the scene constantly changes.</li>

  <li><b>Scene manager</b> for centralized 3D world handling,
    with <b>custom viewports</b> possible.</li>

  <li>Shadows by both <b>shadow volumes</b> (full implementation, with z-fail / z-pass
    switching, silhouette detection etc.) and <b>shadow maps</b>.
    <?php echo a_href_page('Our shadow maps are very comfortable to use',
    'kambi_vrml_extensions_shadow_maps'); ?>, and shadows from multiple light
    sources are correctly rendered.
    We also have experimental <i>Variance Shadow Maps</i> implementation.</li>

  <li><b><?php echo a_href_page_hashlink('Bump mapping',
    'kambi_vrml_extensions', 'section_ext_bump_mapping'); ?></b> is trivially
    easy to use. Various algorithms are available: from
    the classic bump mapping (take normal from the texture),
    through the parallax bump mapping,
    up to the steep parallax bump mapping with self-shadowing.</li>

  <li><b>Shaders</b>. There are classes to easily use ARB fragment / vertex programs
    and GLSL shaders. Most importantly, you can
    <?php echo a_href_page('add and control GLSL shaders from VRML/X3D',
    'vrml_implementation_shaders'); ?>.
    So GLSL shaders are fully available
    for model designers, programmer doesn't have to do anything.
    We have developed special extensions to
    <?php echo a_href_page('composite shader effects',
    'compositing_shaders'); ?>.

  <li><b>Screen-space effects</b> in GLSL are very easy to create (see
    <?php echo a_href_page('ScreenEffect docs',
    'kambi_vrml_extensions_screen_effects'); ?>).

  <li>Advanced texturing, following X3D standard: <b>multi-texturing</b>,
    <b>cube map texturing</b> (can be loaded from separate files,
    DDS files, or captured during runtime), <b>3D textures</b>,
    <b>S3TC compressed textures</b>, <b>anisotropic filtering</b>.

  <li>Speeding up rendering by <b>hardware occlusion query</b>,
    a <a href="http://http.developer.nvidia.com/GPUGems/gpugems_ch29.html">simple approach</a> and
    more involved <a href="http://http.developer.nvidia.com/GPUGems2/gpugems2_chapter06.html">Coherent Hierarchical Culling</a>.

  <li>GLWindow unit is available to easily <b>create windows with OpenGL
    context</b>. The intention of this unit is to be something like glut,
    but magnitudes better &mdash; using clean ObjectPascal, for start.
    Also it allows you to easily create <b>menu bars, open/save file and similar
    dialogs</b> that are implemented using native controls (GTK (1.0 or 2.0, and yes,
    GTK 2.0 version is perfectly stable and advised) or WinAPI).</li>

  <li>Reading and writing of <b>images</b> in various formats, processing them
    and using as OpenGL textures. Besides many common image formats
    (png, jpg, ppm, bmp, just for starters), included is also support for
    <b>DDS</b> (textures with compression, mipmaps, 3d, cube maps) and
    RGBE format (Radiance HDR format).</li>

  <li>Handling of <b>fonts</b>, including rendering them with OpenGL,
    as bitmap or outline (3D) fonts.</li>

  <li>Comfortable <b>3D sound engine</b>,
    using <?php echo a_href_page('OpenAL', 'openal'); ?>,
    with intelligent sound source management,
    supporting WAV and OggVorbis formats.
    Includes <?php echo a_href_page('VRML/X3D integration ("Sound" component of X3D specification)', 'vrml_implementation_sound'); ?>, so content creators
    can define sound sources themselves.</li>

  <li>Basic <b>2D controls rendered through OpenGL</b>
    (buttons, panels, tooltips, menus etc.) are available.
    Good for games, where making a custom-looking GUI (that fits with
    your game theme) is important.</li>

  <li><b>Anti-aliasing</b> (initializing OpenGL multi-sampling) is covered.</li>

  <li>Simple <b>ray-tracer</b> is implemented
    (<?php echo a_href_page("see the gallery","raytr_gallery"); ?>).</li>

  <li>Playing <b>movie files</b>. This includes loading and saving
    as image sequence or "real" movie files (<a href="http://ffmpeg.mplayerhq.hu/">ffmpeg</a>
    is needed to encode / decode movie files). While the implementation
    is limited to a small movies for now (as memory consumption is large),
    it's perfect for flame or smoke animation in games. We even have a simple
    movie editor as an example program in engine sources.

  <li>The engine is <b>portable</b>. Currently tested and used on Linux,
    FreeBSD, Mac OS X and Windows (all i386), and Linux on x86_64.
    Porters/testers for other OS/processors are welcome,
    the engine should be able to run on all modern OSes supported by FPC.</li>

  <li>There are <b>many example programs</b>, look in sources
    <tt><?php echo ENGINE_DIR_NAME; ?>/examples/</tt> subdirectory.</li>

  <li>There are ready TGLWindow descendants and Lazarus components
    to make simple VRML browser. Oh, yes, the engine may be integrated
    with Lazarus &mdash; we have some <b>Lazarus components</b>,
    including ultra-simple VRML browser component (<tt>TKamVRMLBrowser</tt>).

  <li>Engine components are independent when possible.
    For example, you can only take model loading and processing
    code, and write the rendering yourself. Or you can use our OpenGL rendering,
    but still initialize OpenGL context yourself (no requirement to do it
    by our <tt>GLWindow</tt> unit). And so on.
    Of course, ultimately you can just use everything from our engine,
    nicely integrated &mdash; but the point is that you don't have to.</li>

  <!-- <li>Evaluating mathematical expressions -->
  <!-- li>Curves handling.</li -->
  <!--
      <li>ParsingPars, unit to parse command-line options

      <li>VectorMath, unit with many vector-and-matrix operations,
        mainly for 3d graphics

      <li>KambiScript, parsing and executing KambiScript programs
        and mathematical expressions

      <li>TDynXxxArray classes, something like richer dynamic arrays,
        done like "simulated" C++ templates
  -->
</ul>

<p>The engine was used to develop all programs on these pages.
It should be compiled by <a href="http://www.freepascal.org">FreePascal</a>.</p>

<p>See also <?php echo a_href_page('demo movies of ' . ENGINE_NAME,
'movies'); ?>.

<?php echo $toc->html_section(); ?>
<?php echo $toc->html_section(); ?>

<div class="download">
<?php echo sf_download('Download "' .ENGINE_NAME. '" (version ' .
  VERSION_CASTLE_GAME_ENGINE . ')',
  ENGINE_DIR_NAME. '-' . VERSION_CASTLE_GAME_ENGINE .
  '-src.tar.gz'); ?>
</div>

<p>This archive contains the whole engine sources.
There are many simple example programs included
(see <tt>examples/</tt> subdirectory).
Compile everything with simple "<tt>make</tt>" inside the
<tt><?php echo ENGINE_DIR_NAME; ?>/</tt> directory.
Compile examples <!-- (the ones that do not require Lazarus) --> by
simple "<tt>make examples</tt>". Or you can compile and run from
<a href="http://lazarus.freepascal.org/">Lazarus</a>.

<p>For more details about trying out the examples, compiling and generally
using the engine see the "Introduction" page in our
<?php echo a_href_page('reference', 'reference') ?>.

<!--
The explanations that actually the engine
main OpenGL initialization method is <b>not</b> the Lazarus TOpenGLControl
takes too much space.

<p>There are also some Lazarus packages and examples (e.g. to extend Lazarus
<tt>TOpenGLControl</tt> component), they have to be compiled from
within Lazarus. Although note that the engine doesn't require LCL
for anything.
these are not an
essential part of the engine for now.
The main way for
initializing OpenGL for games is by GLWindow unit that doesn't depend on
any Lazarus units. -->

<p>An important strength of our engine is that you can express a lot
of stuff inside your data, that is inside
<?php echo a_href_page('VRML/X3D', 'vrml_x3d'); ?> models.
So many features of our engine
(<?php echo a_href_page('shaders','vrml_implementation_shaders'); ?>,
 <?php echo a_href_page('screen effects', 'kambi_vrml_extensions_screen_effects'); ?>,
 <?php echo a_href_page('mirrors', 'vrml_implementation_cubemaptexturing'); ?>
 and many many more) don't have any special ObjectPascal examples,
because they are simply not needed. For simple uses, you just define what you
need inside VRML/X3D file (of course, for advanced usage you can do a lot more
with ObjectPascal code, and you can always build/modify VRML/X3D graph
by ObjectPascal code).
So <i>be sure to grab <?php echo a_href_page('our demo VRML/X3D models', 'demo_models'); ?></i>
 and try opening them with any engine example program (like <tt>scene_manager_basic.lpr</tt>,
or even our <?php echo a_href_page('view3dscene', 'view3dscene'); ?>)
&mdash; you will find that everything just works,
not requiring a single line of ObjectPascal code.</p>

<p><i>Under Windows you will need some DLLs</i>
to successfully run some of the programs.
I collected all DLLs needed by my programs
<?php echo current_www_a_href_size('in this archive',
  'miscella/win32_dlls.zip'); ?>, so just download
this and put in program's exe directory (or somewhere on $PATH).
These DLLs are already included in binary archives of my programs,
but are not included in source archives (since they are only usable
for Windows users). In general, for all OSes, see section
"Requirements" in the documentation of programs and make sure that
you have appropriate libraries installed on your system.</p>

<p>Alternatively, you can get the latest (bleeding-edge) sources from Subversion by:

<pre class="terminal small"><?php echo sf_checkout_link(true, ENGINE_DIR_NAME); ?></pre>

<?php echo $toc->html_section(); ?>

<p>Below are sources for specific programs.
Each of them contains program-specific modules, main program file
and script <tt>compile.sh</tt> to simply compile whole program using FPC.
Download those that you are interested in and unpack them into
the same directory where you unpacked the core engine sources.
Then execute <tt>compile.sh</tt> scripts to compile the programs you want.

<?php
function older_engine_version($older_version)
{
  echo sf_download($older_version . ' version',
    ENGINE_DIR_NAME. '-' . $older_version . '-src.tar.gz');
}
?>

<!--
<p>Note about compatibility: Sometimes I happen to break backwards
compatibility in the engine API. The sources in SVN are always
updated, and should be totally compatible.
But if you download tar.gz sources below, you may find that they
are not compatible with current engine version... that's why
they are notes like <i>compatible with engine XXX version</i>
notes near some programs below.
-->

<ul>
<?php
  function echo_src_svnonly($name)
  {
    echo '<li><p>' . $name . ': only from Subversion by:<br><tt class="terminal small">' .
        sf_checkout_link(true, $name) . '</tt></li>
      ';
  }

  /* Internal name is both the basename of the archive and
     the subdirectory name within SVN repository. */
  function echo_src_archive_2($title, $internal_name, $engine_ver)
  {
    $version_const_name = 'VERSION_' . strtoupper($internal_name);
    if ($internal_name == 'castle')
      $version = '0.9.3'; else
      $version = constant($version_const_name);

    echo '<li><p>' .
      sf_download('sources of '.$title,
        $internal_name . '-' . $version . '-src.tar.gz');

    if ($engine_ver == VERSION_CASTLE_GAME_ENGINE)
    {
      echo '<br/>These tar.gz sources are compatible with latest engine ';
      older_engine_version($engine_ver);
      echo '.';
    } else
    {
      echo '<br/>These tar.gz sources were tested with engine ';
      older_engine_version($engine_ver);
      echo ', use SVN to get sources compatible with latest engine version.';
    }

    echo
      '<p>Download from Subversion by:</p><pre class="terminal small">' .
        sf_checkout_link(true, $internal_name) . '</pre></li>
      ';
  }

  function echo_src_archive($title_and_internal_name, $engine_ver)
  {
    echo_src_archive_2($title_and_internal_name, $title_and_internal_name,
      $engine_ver);
  }

  echo_src_archive('view3dscene', '2.5.1');
  echo_src_archive('castle', '2.5.1');
  echo_src_archive('rayhunter', '2.1.0');

  echo_src_archive('lets_take_a_walk', '2.0.0');
  echo_src_archive('malfunction', '2.0.3');
  echo_src_archive('kambi_lines', '2.0.0');

  echo_src_archive('glplotter', '2.0.0');
  echo_src_archive('gen_function', '1.5.0');

  echo_src_archive_2('glViewImage', 'glviewimage', '2.0.0');
  echo_src_archive('bezier_curves', '2.0.0');
  echo_src_archive_2('glinformation and glinformation_glut', 'glinformation', '2.0.1');

  echo_src_svnonly('sandbox');
  echo_src_svnonly('rift');
?>
</ul>

<p>Note: archives above do not contain user documentation for
these programs. For now you can just go to the page of appropriate
program and read documentation there (if you downloaded binary
version of program you will also have documentation there).

<?php echo $toc->html_section(); ?>

<p>You can get all the sources from our Subversion repository.
If you don't know about Subversion, see
<a href="http://subversion.tigris.org/">Subversion main site</a> and
<a href="http://svnbook.red-bean.com/">the <i>excellent</i>
book about the Subversion</a>.</p>

<p>To download full sources for all projects, do</p>

<pre class="terminal small"><?php echo sf_checkout_link(true, ''); ?></pre>

<p>Please note that the full <tt>trunk</tt> is quite large.
It contains everything: th core engine sources (<tt><?php echo ENGINE_DIR_NAME; ?></tt> subdirectory),
webpages stuff (in <tt>www</tt> subdirectory),
<tt>view3dscene</tt> sources, <tt>castle</tt> sources etc.
Often you want to download only specific subdirectories of it.</p>

<p>You can also <a href="https://sourceforge.net/p/castle-engine/code/">browse
the SVN repository</a>.</p>

<p>Code from SVN is always the bleeding-edge current
version of the work. That said, usually it's quite stable (I have a personal
policy to try to commit only code that is compileable and somewhat tested).
So feel free to peek, and please report eventual bugs you spot.
You can also download the code from one of
<tt class="terminal">http://svn.code.sf.net/p/castle-engine/code/tags/</tt>
subdirectories, these contain frozen code from specific versions of my programs,
so should be 100% stable.</p>

<?php echo $toc->html_section(); ?>

<p>You need the <a href="http://www.freepascal.org/">Free Pascal Compiler
(FPC)</a> to use our engine. You may also find it comfortable
to use <a href="http://lazarus.freepascal.org/">Lazarus</a>,
which is an IDE (editor, debugger etc.) built around FPC.</p>

<p><b>We always support and advice the latest stable release of FPC
(currently 2.4.4).</b>.</p>

<p>We also support a couple of older FPC releases.
With new engine versions (later than 2.5.1), <b>FPC versions &gt;= 2.2.4
are supported</b>.</p>

<!--p>I also regularly test FPC from SVN,
so it's usually painless to use even development FPC releases.</p-->

<?php echo $toc->html_section(); ?>

<p>The whole engine and all related programs' sources are licensed on terms of <a href="http://www.gnu.org/licenses/gpl.html">GNU General Public License</a>. See <a href="http://www.gnu.org/">www.gnu.org</a> for more information about this license (including translations of it to various languages) and philosophy of free software.</p>

<p>Moreover, the core of the engine is also alternatively available under the more permissive <a href="http://www.gnu.org/copyleft/lesser.html">GNU Lesser General Public License</a> with the so-called "static linking exception". The idea of this exception is to allow statically linking with the engine on the same terms as dynamically linking. (<i>Static linking</i> is what normally happens when you compile a program using my units, without wrapping them in a DLL / Delphi runtime package.)</p>

<p>All this basically means that you have to share your modifications <i>to the engine</i>, and you can use the engine in closed-source programs.</p>

<p>The precise legal text of the "static linking exception" follows (it's the same as used by <a href="http://www.freepascal.org/faq.var#general-license">FreePascal Runtime Library</a> and many other projects):</p>

<p style="margin-left: 2em; background: #EEE;">
As a special exception, the copyright holders of this library give you permission to link this library with independent modules to produce an executable, regardless of the license terms of these independent modules, and to copy and distribute the resulting executable under terms of your choice, provided that you also meet, for each linked independent module, the terms and conditions of the license of that module. An independent module is a module which is not derived from or based on this library. If you modify this library, you may extend this exception to your version of the library, but you are not obligated to do so. If you do not wish to do so, delete this exception statement from your version.
</p>

<p>Important notes:</p>
<ul>
  <li><p>If you want to use the engine on LGPL terms (as opposed to more strict GPL) you <i>must</i> compile the engine with <tt><?php echo ENGINE_LGPL_DEF; ?></tt> symbol defined in file <tt><?php echo ENGINE_DIR_NAME; ?>/base/kambiconf.inc</tt>. Just put there <tt>{$define <?php echo ENGINE_LGPL_DEF; ?>}</tt> line (or simply remove the beginning space in already prepared comment <tt>{&nbsp;$define <?php echo ENGINE_LGPL_DEF; ?>}</tt>).</p>

    <p>This is necessary to avoid pulling in GPL-only dependencies. For now, this is only the NURBS unit (uses GPL-only code from <a href="http://vrml.cip.ica.uni-stuttgart.de/dune/">White_dune</a>). This missing NURBS implementation is the only difference between LGPL and "strict GPL" engine version.</p></li>

  <li><p>Note that LGPL stuff concerns only the engine, i.e. things inside <tt><?php echo ENGINE_DIR_NAME; ?></tt> archive. The rest of the programs (<tt>view3dscene</tt>, <tt>castle</tt> etc.) are still strict GPL.</p></li>
</ul>

<?php echo $toc->html_section(); ?>

<ul>
  <li><?php echo a_href_page('VRML engine reference (generated by pasdoc)',
    'reference') ?>.</li>
  <li><?php echo a_href_page("VRML engine documentation",
    'vrml_engine_doc'); ?> (more general overview of how the engine works).</li>
</ul>

<?php echo $toc->html_section(); ?>

<?php echo castle_thumbs(array(
  array('filename' => 'test_kambi_units_screen_demo.png', 'titlealt' => 'test_kambi_units')
));
?>

<p>I'm managing a suite of automatic tests,
in the spirit of <a href="http://www.extremeprogramming.org/">Extreme Programming</a>.
On 2005-04-25 I converted my tests to use
<a href="http://camelos.sourceforge.net/fpcUnit.html">fpcunit</a>
(this is a close FPC analogy to <a href="http://www.junit.org/">JUnit for Java</a>)
and it's <a href="http://www.lazarus.freepascal.org/">Lazarus</a> GUI runner.</p>

<p>The tests are included in the engine sources,
see the subdirectory <tt>tests/</tt>. You can open and run them
from Lazarus to see a result in a nice GUI window.</p>

<p>You can also compile and run a console version, that doesn't require
Lazarus (LCL), only pure FPC is needed:</p>

<pre class="terminal">
cd tests/
./compile_console.sh
./test_kambi_units -a
</pre>

<?php
  castle_footer();
?>
