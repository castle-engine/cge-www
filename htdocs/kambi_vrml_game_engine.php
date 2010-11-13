<?php
  require_once 'vrmlengine_functions.php';

  vrmlengine_header("Engine overview for developers", NULL,
    array('kambi_vrml_game_engine'));

  $toc = new TableOfContents(
    array(
      new TocItem('Features', 'features'),
      new TocItem('Download sources', 'download_src'),
      new TocItem('Engine sources', 'engine_src', 1),
      new TocItem("Other programs' sources", 'program_sources', 1),
      new TocItem("Subversion (SVN) notes", 'svn', 1),
      new TocItem('FPC version required', 'fpc_ver', 1),
      new TocItem('License', 'license'),
      new TocItem('Documentation', 'docs'),
      new TocItem('Automatic tests', 'tests')
    )
  );
  $toc->echo_numbers = true;
?>

<div style="float: right; margin: 1em;">
<a class="FlattrButton" style="display:none;"
href="http://vrmlengine.sourceforge.net/"></a>
</div>

<?php
  echo pretty_heading($page_title, VERSION_KAMBI_VRML_GAME_ENGINE);
  echo '<table align="right" style="clear: right">' .
    '<tr><td>' . medium_image_progs_demo_core("fountain_only_materials.png", '&quot;The Fountain&quot; level with only materials') .
    '<tr><td>' . medium_image_progs_demo_core("fountain_shadows.png", '&quot;The Fountain&quot; level textured with shadows') .
    '<tr><td>' . medium_image_progs_demo_core("fountain_toon_shading.png", '&quot;The Fountain&quot; level with toon shading GLSL program') .
    '<tr><td>' . medium_image_progs_demo_core("fountain_bump_mapping_good_materials.png", '&quot;The Fountain&quot; level with bump mapping used') .
    '<tr><td>' . medium_image_progs_demo_core("gate_fill_mode_normal.png", '&quot;The Gate&quot; level with normal fill mode') .
    '<tr><td>' . medium_image_progs_demo_core("gate_fill_mode_solid_wireframe.png", '&quot;The Gate&quot; level with solid wireframe fill mode') .
    '<tr><td>' . medium_image_progs_demo_core("gate_fill_mode_silhouette.png", '&quot;The Gate&quot; level with silhouette fill mode') .
    '</table>';
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>This is an open-source (<a href="#section_license">LGPL / GPL</a>)
game engine. In short:

<ul>
  <li><p>Our main 3D scene format is <?php echo a_href_page('VRML / X3D', 'vrml_x3d'); ?>, which is an open standard (you'll find you can export to it from virtually any 3D modeller), and a lot of our strength comes from it (it's a 3D scene format that can also express interactive world features, scripting etc.). Even if you don't know VRML/X3D, the whole engine is designed as a general-purpose 3D engine, and other 3D model formats are supported as well (basic Collada, Wavefront, MD3 and others).</p></li>

  <li><p>The engine is developed for the <a href="http://freepascal.org/">Free Pascal Compiler</a>, an open-source cross-platform compiler, and the engine is cross-platform as well (Linux, Mac OS X, Windows, and more). We have <a href="http://lazarus.freepascal.org/">Lazarus</a> components for RAD development, although the core engine doesn't depend on Lazarus LCL and you can develop full games with pure FPC (we have our own OpenGL window management unit, if you want).</p></li>
</ul>

<p>More exhaustive list of the features:</p>

<ul>
  <li><b>Optimized OpenGL rendering</b> of models in
    <b>VRML 1.0, 2.0 (aka VRML 97) and X3D</b> formats.
    Including support for advanced VRML/X3D features like prototypes and
    events (user can interact with the 3D world).</li>

  <li><b>3DS, MD3, Wavefront OBJ, Collada</b> file formats are also supported.
    They are internally converted into the VRML nodes graph,
    which means that they get all the optimizations for rendering,
    and 3D content from all file formats can be mixed (for 3D editing tools
    and such).</li>

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
    switching, silhouette detection etc.) and <b>shadow maps</b>.</li>

  <li><b><?php echo a_href_page_hashlink('Bump mapping',
    'kambi_vrml_extensions', 'section_ext_bump_mapping'); ?></b> (using various
    implementations: basic multitexturing with dot3 (normalized by cube map or not),
    GLSL normal, GLSL with classic parallax mapping, GLSL with steep parallax
    mapping and self-shadowing).</li>

  <li><b>Shaders</b>. There are classes to easily use ARB fragment / vertex programs
    and GLSL shaders. Most important, you can
    <?php echo a_href_page('add and control GLSL shaders from VRML',
    'vrml_implementation_shaders'); ?>.
    So GLSL shaders are fully available
    for model designers, programmer doesn't have to do anything.

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
    GTK 2.0 version is perfectly stable and adviced) or WinAPI).</li>

  <li>Reading and writing of <b>images</b> in various formats, processing them
    and using as OpenGL textures. Besides many common image formats
    (png, jpg, ppm, bmp, just for starters), included is also support for
    <b>DDS</b> (textures with compression, mipmaps, 3d, cube maps) and
    RGBE format (Radiance HDR format).</li>

  <li>Handling of <b>fonts</b>, including rendering them with OpenGL,
    as bitmap or outline (3D) fonts.</li>

  <li><b>3D sound</b> by OpenAL helpers, including intelligent OpenAL sound manager
    and OggVorbis format handling.</li>

  <li><b>Anti-aliasing</b> (initializing OpenGL multi-sampling) is covered.</li>

  <li><b>Ray-tracer</b> based on VRML models is implemented.</li>

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
    <tt>kambi_vrml_game_engine/examples/</tt> subdirectory.</li>

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

<p>See also <?php echo a_href_page('demo movies of Kambi VRML game engine',
'movies'); ?>.

<?php echo $toc->html_section(); ?>
<?php echo $toc->html_section(); ?>

<div class="download">
<?php echo sf_download('Download "Kambi VRML game engine" (version ' .
  VERSION_KAMBI_VRML_GAME_ENGINE . ')',
  'kambi_vrml_game_engine-' . VERSION_KAMBI_VRML_GAME_ENGINE .
  '-src.tar.gz'); ?>
</div>

<p>This archive contains the whole engine sources.
There are many simple example programs included
(see <tt>examples/</tt> subdirectory).
Compile everything with simple "<tt>make</tt>" inside the
<tt>kambi_vrml_game_engine/</tt> directory.
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
intiializing OpenGL for games is by GLWindow unit that doesn't depend on
any Lazarus units. -->

<p>Note: under Windows you will need some DLLs
to successfully run some of the programs.
I collected all DLLs needed by my programs
<?php echo current_www_a_href_size('in this archive',
  'miscella/win32_dlls.zip'); ?>, so just download
this and put in program's exe directory.
These DLLs are already included in binary archives of my programs,
but are not included in source archives (since they are only usable
for Windows users). In general, for all OSes, see section
"Requirements" in the documentation of programs and make sure that
you have appropriate libraries installed on your system.</p>

<p>Alternatively, you can get the latest (bleeing-edge) sources from Subversion by:

<pre class="terminal small"><?php echo sf_checkout_link(true, 'kambi_vrml_game_engine'); ?></pre>

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
    'kambi_vrml_game_engine-' . $older_version . '-src.tar.gz');
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
    $version = constant($version_const_name);

    echo '<li><p>' .
      sf_download('sources of '.$title,
        $internal_name . '-' . $version . '-src.tar.gz');

    if ($engine_ver == VERSION_KAMBI_VRML_GAME_ENGINE)
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

  echo_src_archive('view3dscene', '2.1.0');
  echo_src_archive('castle', '1.7.0');
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
It contains everything: th core engine sources (<tt>kambi_vrml_game_engine</tt> subdirectory),
webpages stuff (in <tt>www</tt> subdirectory),
<tt>view3dscene</tt> sources, <tt>castle</tt> sources etc.
Often you want to download only specific subdirectories of it.</p>

<p>You can also <a href="http://vrmlengine.svn.sourceforge.net/viewvc/vrmlengine/">browse
the SVN repository using ViewVC</a>.</p>

<p>Code from SVN is always the bleeding-edge current
version of the work. That said, usually it's quite stable (I have a personal
policy to try to commit only code that is compileable and somewhat tested).
So feel free to peek, and please report eventual bugs you spot.
You can also download the code from one of
<tt class="terminal">https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/tags/</tt>
subdirectories, these contain frozen code from specific versions of my programs,
so should be 100% stable.</p>

<!--
Too much information:

<div class="note_box">
<p class="note_title">A note to developers familiar with SVN</p>

<p>I know that my layout is a little
non-standard: standard way is to make subdirectory for each project
at top level (like <tt>kambi_vrml_game_engine</tt>, <tt>castle</tt> etc.)
and beneath each of them make subdirectories <tt>trunk</tt>, <tt>tags</tt>.
But current layout is more comfortable for users: usually you want to
download both <tt>kambi_vrml_game_engine</tt> and one of the other directories
(like <tt>castle</tt>) and have them as siblings, since this allows you
to compile most things by using ready provided compilation scripts.</p>
</div>
-->

<?php echo $toc->html_section(); ?>

<p>The engine is meant to be compiled with
<a href="http://www.freepascal.org/">Free Pascal Compiler (FPC)</a>.
The policy is to always support the latest stable release of FPC,
and sometimes some older releases:

<ul>
  <li><p><em>FPC 2.4.0</em> : This is the simplest and best option
    &mdash; works perfectly, and you can download already
    compiled packages of it from
    <a href="http://www.freepascal.org/">www.freepascal.org</a>.</p>
  </li>

  <li><p><em>FPC 2.2.4</em> : will work OK for some time, although you're
    adviced to upgrade to latest FPC.</p>

  <li><p><em>FPC 2.2.2</em> : will work OK for some time, although you're
    adviced to upgrade to latest FPC.

    <p>Note: under GTK &gt;= 2.14 (like in Ubuntu &gt;= 8.10) you may get
    linker errors like</p>

    <p><tt>/usr/local/lib/fpc/2.2.2/units/x86_64-linux/gtk2/gtk2.o: In function `GTK2_GTK_TYPE_FILE_FOLDER$$QWORD': gtk2.pas:(.text+0x105b5): undefined reference to `gtk_file_folder_get_type'</tt></p>

    <p>These are results of GTK 2.14 removing these functions (thus breaking
    compatibility). It's <a href="http://mantis.freepascal.org/view.php?id=11837">fixed in FPC &gt; 2.2.2</a>,
    temporary workaround (if you need to stick to FPC &lt;= 2.2.2) is to add
    <tt>-k--noinhibit-exec</tt> to the fpc command-line when compiling programs.
    These errors will be then treated as mere warnings, and you should
    get a working executable.</p>
  </li>

  <li><p><em>FPC 2.2.0</em> : will work OK for some time, although you're
    adviced to upgrade to latest FPC.

    <p><i>Note only for for x86_64</i>:
    to avoid <a href="http://bugs.freepascal.org/view.php?id=10508">this bug</a>,
    you have to use fixed glext.pas unit from engine sources
    (<tt>trunk/kambi_vrml_game_engine/src/opengl/x86_64/</tt>) or
    use newer FPC.</p>
  </li>

  <li><p><em>FPC 2.0.4</em> : will work OK for some time from now
    (2007-09-20). But you have to use GL units from FPC &gt;= 2.2.0,
    for example grab them from FPC SVN:</p>

<pre class="terminal">
  svn checkout http://svn.freepascal.org/svn/fpc/tags/release_2_2_0/packages/extra/opengl/
</pre>

  </li>

  <li><p><em>Latest FPC development version,
    downloadable from FPC SVN repository</em>, usually works.
    However, you must remember that SVN version changes rapidly
    and no-one can give any guarantee about current stability of FPC from SVN
    (or stability of my programs compiled with this FPC).</p>
  </li>
</ul>

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
  <li><p>If you want to use the engine on LGPL terms (as opposed to more strict GPL) you <i>must</i> compile the engine with <tt>KAMBI_VRMLENGINE_LGPL</tt> symbol defined in file <tt>kambi_vrml_game_enggine/base/kambiconf.inc</tt>. Just put there <tt>{$define KAMBI_VRMLENGINE_LGPL}</tt> line (or simply remove the beginning space in already prepared comment <tt>{&nbsp;$define KAMBI_VRMLENGINE_LGPL}</tt>).</p>

    <p>This is necessary to avoid pulling in GPL-only dependencies. For now, this is only the NURBS unit (uses GPL-only code from <a href="http://vrml.cip.ica.uni-stuttgart.de/dune/">White_dune</a>). This missing NURBS implementation is the only difference between LGPL and "strict GPL" engine version.</p></li>

  <li><p>Note that LGPL stuff concerns only the engine, i.e. things inside <tt>kambi_vrml_game_engine</tt> archive. The rest of the programs (<tt>view3dscene</tt>, <tt>castle</tt> etc.) are still strict GPL.</p></li>
</ul>

<?php echo $toc->html_section(); ?>

<ul>
  <li><?php echo a_href_page('VRML engine reference (generated by pasdoc)',
    'reference') ?>.</li>
  <li><?php echo a_href_page("VRML engine documentation",
    'vrml_engine_doc'); ?> (more general overview of how the engine works).</li>
</ul>

<?php echo $toc->html_section(); ?>

<p>I'm managing a suite of automatic tests,
in the spirit of <a href="http://www.extremeprogramming.org/">Extreme Programming</a>.
On 2005-04-25 I converted my tests to use
<a href="http://camelos.sourceforge.net/fpcUnit.html">fpcunit</a>
(this is a close FPC analogy to <a href="http://www.junit.org/">JUnit for Java</a>)
and it's <a href="http://www.lazarus.freepascal.org/">Lazarus</a> GUI runner.

<p>The tests are included with the rest of engine sources,
see subdirectory <tt>tests/</tt>. This is a GUI program, so you can
compile it from Lazarus. You can also compile a console version
(that doesn't require any part of Lazarus LCL) by <tt>compile_console.sh</tt>
script inside.

<p>I will not give you a compiled executable of the testing program
(after all, it would have little sense, because all tests would succeed,
unless there's some problem specific to your OS configuration),
but I am generous enough to show you a snapshot of a happy test_kambi_units
program after successfully running all 86 tests:<br>
<?php echo
  medium_image_progs_demo('test_kambi_units_screen_demo.png', 'test_kambi_units', false)
?>

<?php
  vrmlengine_footer();
?>
