<?php
  require_once 'castle_engine_functions.php';

  castle_header("Castle Game Engine - Downloads and docs for developers", NULL, array('engine'));

  $toc = new TableOfContents(
    array(
      new TocItem('Introduction', 'intro'),
      new TocItem('Download', 'download_src'),
      new TocItem('Documentation', 'docs'),
      new TocItem('Features', 'features'),
      new TocItem('FPC (Free Pascal Compiler) version required', 'fpc_ver'),
      new TocItem('License', 'license'),
    )
  );
  $toc->echo_numbers = true;

/*  echo flattr_button(); */

  echo '<img src="images/castle_game_engine_icon.png"
    alt="Castle Game Engine icon"
    style="float: right; clear: right; margin-top: 1em;" />';

  echo pretty_heading('Castle Game Engine',
    VERSION_CASTLE_GAME_ENGINE, 'Downloads and docs for developers<br/>');

  echo castle_thumbs(array(
    array('filename' => 'fps_game_screen_19.png', 'titlealt' => 'FPS game screen'),
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

<p>This is a free/open-source (<a href="#section_license">LGPL / GPL</a>)
3D game engine. In short:

<ul>
  <li><p>Our main 3D scene format is <?php echo a_href_page('VRML / X3D', 'vrml_x3d'); ?>, which is an open standard (you'll find you can export to it from virtually any 3D modeler), and a lot of our strength comes from it (it's a 3D scene format that can also express interactive world features, scripting etc.). Even if you don't know VRML/X3D, the whole engine is designed as a general-purpose 3D engine, and other 3D model formats are supported as well (Collada, Wavefront, MD3 and others).</p></li>

  <li><p>We have a lot of 3D graphic features. Shaders, shadows, bump mapping, mirrors, custom viewports, screen-space effects, and much more. Just look at the screenshots on this page&nbsp;:)</p></li>

  <li><p>We have a comfortable and extensible implementation of scene manager and 3D objects. You have a ready implementation of levels, creatures (with AI), items, players and other things typical to 3D games. You can extend it in many ways. You can also make your own 3D objects (if your game 3D world doesn't fit in our idea of creatures/levels etc.) by descending from T3D and adding to TCastleSceneManager.Items directly.

  <li><p>The engine is developed for the <a href="http://freepascal.org/">Free Pascal Compiler</a>, an open-source cross-platform compiler, and the engine is cross-platform as well (Linux, Mac OS X, Windows, and more). We have <a href="http://lazarus.freepascal.org/">Lazarus</a> components for RAD development, although the core engine doesn't depend on Lazarus LCL and you can develop full games with pure FPC (we have our own OpenGL window management unit, if you want). The whole engine is 100% clean Object Pascal code.</p></li>
</ul>

<p>The <a href="#section_features">features section</a> below on this page
contains more exhaustive list of engine features, with links to detailed
information.</p>

<p>Many <a href="http://www.youtube.com/user/michaliskambi">demo movies about
Castle Game Engine are available in Michalis Kamburelis YouTube channel</a>.</p>

<?php echo $toc->html_section(); ?>

<div class="download">
<div class="download_title">Download "Castle Game Engine" (version <?php echo VERSION_CASTLE_GAME_ENGINE; ?>)</div>
<ul>
  <li><?php echo sf_download('As tar.gz archive', 'castle_game_engine-' . VERSION_CASTLE_GAME_ENGINE . '-src.tar.gz'); ?>
  <li><?php echo sf_download('As zip archive (easier to unpack on Windows)', 'castle_game_engine-' . VERSION_CASTLE_GAME_ENGINE . '-src.zip'); ?>
</ul>
</div>

<p>This archive contains the whole engine sources.
There are many simple example programs included
(see <tt>examples/</tt> subdirectory).
Compile everything with simple "<tt>make</tt>" inside the
<tt>castle_game_engine/</tt> directory.
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
initializing OpenGL for games is by CastleWindow unit that doesn't depend on
any Lazarus units. -->

<p>An important strength of our engine is that you can express a lot
of stuff inside your data, that is inside
<?php echo a_href_page('VRML/X3D', 'vrml_x3d'); ?> models.
So many features of our engine
(<?php echo a_href_page('shaders','x3d_implementation_shaders'); ?>,
 <?php echo a_href_page('screen effects', 'x3d_extensions_screen_effects'); ?>,
 <?php echo a_href_page('mirrors', 'x3d_implementation_cubemaptexturing'); ?>
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

<p><b>Remember to install required libraries</b>:
programs developed using our engine (engine examples and normal programs)
may use a couple of libraries.
The full list of libraries is at the "Requirements" section at the documentation
of each program, and the <?php echo a_href_page('engine reference',
 'reference') ?> also lists the libraries in the introduction section.
In short, you will most likely want to have <i>libpng, zlib, OpenAL, and vorbisfile</i>.</p>

<ul>
  <li><i>On Linux and FreeBSD</i> you should install these libraries
    using your favourite package manager.
    Remember to install <tt>-dev</tt> versions of these libraries too
    (if you're under Debian or similar distribution) to be able to compile
    programs that link to these libraries.

  <li><i>On Windows</i> (32-bit) you can
    find all the necessary DLL files
    <?php echo current_www_a_href_size('in this archive',
    'miscella/win32_dlls.zip'); ?>. Download
    this and copy all DLLs to program's exe directory (or put them somewhere on $PATH).
    <!--
    These DLLs are already included in binary archives of my programs,
    but are not included in source archives (since they are only useful for Windows).
    -->

  <li><i>On Mac OS X</i>: <?php echo a_href_page('Mac OS X requirements are listed here',
    'macosx_requirements'); ?>.
</ul>

<!--
In general, for all OSes, see section
 in the documentation of programs and make sure that
you have appropriate libraries installed on your system.
-->

<p>Instead of downloading the archive, you can also get the latest
(bleeding-edge) engine sources from Subversion by:</p>

<pre class="terminal small"><?php echo sf_checkout_link(true, 'castle_game_engine'); ?></pre>

<?php echo $toc->html_section(); ?>

<ul>
  <li><?php echo a_href_page('Tutorial', 'tutorial_intro') ?></li>
  <li><?php echo a_href_page('Classes overview (cheatsheet)', 'tutorial_classes_overview') ?></li>
  <li><?php echo a_href_page('Guide to creating game data', 'creating_data_intro') ?></li>
  <li><?php echo a_href_page('API reference (generated by pasdoc)', 'reference') ?></li>
  <li><?php echo a_href_page("Engine Internals Documentation",
    'engine_doc'); ?> (a little outdated documentation, but contains some
    information about how the engine works)</li>
</ul>

<?php echo $toc->html_section(); ?>

<p>Exhaustive list of our engine features:</p>

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

  <li>We allow you to use <b>any 3D modeler</b> to design your 3D data.
    <a href="http://www.blender.org/">Blender</a>? 3DS Max? Anything else?
    In part, that's because VRML and X3D are open and popular 3D
    formats, and decent exporters for them exist in practically every 3D
    authoring tool. For detecting "placeholders" on levels
    (see
    <?php echo a_href_page('creating levels documentation', 'creating_data_levels'); ?>),
    you can configure detection method.

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

  <li><b>Extensible system of 3D objects</b>. You have <b>ready,
    comfortable management of creatures, items, levels and players</b>.
    You can extend it by deriving descendants of engine classes in
    <?php api_link('CastleCreatures', 'CastleCreatures.html'); ?> or
    <?php api_link('CastleItems', 'CastleItems.html'); ?> or
    <?php api_link('CastleLevels', 'CastleLevels.html'); ?>.
    Or you can make your own management of 3D objects, by deriving descendants
    of base <?php api_link('T3D', 'Castle3D.T3D.html'); ?> and other classes.
    <?php echo a_href_page('Engine tutorial', 'tutorial_resources_extending'); ?>
    contains detailed information about this.

  <li>Shadows by both <b>shadow volumes</b> (full implementation, with z-fail / z-pass
    switching, silhouette detection etc.) and <b>shadow maps</b>.
    <?php echo a_href_page('Our shadow maps are very comfortable to use',
    'x3d_extensions_shadow_maps'); ?>, and shadows from multiple light
    sources are correctly rendered.
    We also have experimental <i>Variance Shadow Maps</i> implementation.</li>

  <li><b><?php echo a_href_page_hashlink('Bump mapping',
    'x3d_extensions', 'section_ext_bump_mapping'); ?></b> is trivially
    easy to use. Various algorithms are available: from
    the classic bump mapping (take normal from the texture),
    through the parallax bump mapping,
    up to the steep parallax bump mapping with self-shadowing.</li>

  <li><b>Shaders</b>:
    <ul>
      <li>We have classes to easily use GLSL shaders.
        But usually you don't need to use them, because...</li>
      <li>You can
        <?php echo a_href_page('design and control GLSL shaders inside VRML/X3D',
        'x3d_implementation_shaders'); ?>.
        So GLSL shaders are fully available
        for model designers, programmer doesn't have to do anything.</li>
      <li>We have developed special extensions to
        <?php echo a_href_page('composite shader effects', 'compositing_shaders'); ?>,
        to enable custom GLSL effects cooperate with each other and with
        built-in shader effects.</li>
      <li>We fully support rendering both in fixed-function
        and programmable pipelines. In the latter case,
        the whole shading is expressed through GLSL shaders
        (that you can override with <tt>ComposedShader</tt>
        or exhance with <tt>Effect</tt>, see links above).</li>
    </ul>

  <li><b>Screen-space effects</b> in GLSL are very easy to create (see
    <?php echo a_href_page('ScreenEffect docs',
    'x3d_extensions_screen_effects'); ?>).

  <li>Advanced texturing, following X3D standard: <b>multi-texturing</b>,
    <b>cube map texturing</b> (can be loaded from separate files,
    DDS files, or captured during runtime), <b>3D textures</b>,
    <b>S3TC compressed textures</b>, <b>anisotropic filtering</b>.

  <li>Speeding up rendering by <b>hardware occlusion query</b>,
    a <a href="http://http.developer.nvidia.com/GPUGems/gpugems_ch29.html">simple approach</a> and
    more involved <a href="http://http.developer.nvidia.com/GPUGems2/gpugems2_chapter06.html">Coherent Hierarchical Culling</a>.

  <li>CastleWindow unit is available to easily <b>create windows with OpenGL
    context</b>. The intention of this unit is to be something like glut,
    but magnitudes better &mdash; using clean ObjectPascal, for start.
    Also it allows you to easily create <b>menu bars, open/save file and similar
    dialogs</b> that are implemented using native controls (GTK 2 or WinAPI).</li>

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
    Includes <?php echo a_href_page('VRML/X3D integration ("Sound" component of X3D specification)', 'x3d_implementation_sound'); ?>, so content creators
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
    FreeBSD, Mac OS X and Windows, i386 or x86_64 (common 32-bit and
    64-bit processors).
    Porters/testers for other OS/processors are welcome,
    the engine should be able to run on all modern systems supported by FPC.
    Ports to Android and iOS devices (smartphones, tablets) are also within reach,
    as our 3D renderer uses modern OpenGL (VBOs, possibility to render everything
    by GLSL).
    The portability of our engine is of course in large part thanks to the great
    <a href="http://www.freepascal.org">Free Pascal Compiler</a>.

    <!--
    All Unix flavors may work out of the box.
    On big-endian processors
    (most not-x86 processors), some image loading code probably needs to be adjusted.
    -->
  </li>

  <li>There are <b>many example programs</b>, look in sources
    <tt>castle_game_engine/examples/</tt> subdirectory.</li>

  <li>There are ready window classes (<tt>TCastleWindow</tt>)
    and Lazarus components (<tt>TCastleControl</tt>) to make simple
    VRML/X3D and other 3D models browser, on a Lazarus form or independent from Lazarus LCL.
    The engine is integrated with Lazarus &mdash;
    we have various <b>Lazarus components</b>.</li>

  <li>Engine <b>components are independent</b> when possible.
    For example, you can only take model loading and processing
    code, and write the rendering yourself. Or you can use our OpenGL rendering,
    but still initialize OpenGL context yourself (no requirement to do it
    by our <tt>CastleWindow</tt> unit). And so on.
    Of course, ultimately you can just use everything from our engine,
    nicely integrated &mdash; but the point is that you don't have to.</li>

  <!-- li>NURBS, Bezier curves and surfaces.</li -->
  <!--
      <li>Parsing command-line options following modern Unix commands standards
        (- -long options, short options, arguments etc.)

      <li>CastleVectors, unit with many vector-and-matrix operations,
        mainly for 3d graphics

      <li>CastleScript, parsing and executing CastleScript programs
        and mathematical expressions

      <li>Integrated logging. Integrated user preferences loading/saving.
        Used throughout the engine components.
  -->
</ul>

<?php echo $toc->html_section(); ?>

<p>You need the <a href="http://www.freepascal.org/">Free Pascal Compiler
(FPC)</a> to use our engine. You may also find it comfortable
to use <a href="http://lazarus.freepascal.org/">Lazarus</a>,
which is an IDE (editor, debugger etc.) built around FPC.</p>

<p><b>We always support and advice the latest stable release of FPC
(currently 2.6.0).</b>.</p>

<!--
<p><i>Note only for FPC 2.6.0:</i> if you use <tt>compile.sh</tt> scripts,
you need to fix them: use <tt>${CASTLE_FPC_OPTIONS:-}</tt>
instead of <tt>"${CASTLE_FPC_OPTIONS:-}"</tt> (strip double quotes,
to avoid <a href="http://bugs.freepascal.org/view.php?id=21000">FPC #21000 bug</a>).
This isn't a problem for earlier FPC versions, or if you compile programs
with Lazarus.
-->

<p>Usually, we also support a couple of older FPC releases.
But currently (for engine 4.0.0) only <b>FPC versions &gt;= 2.6.0 are supported</b>
(because of a couple of very useful new language features).</p>

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
  <li><p>If you want to use the engine on LGPL terms (as opposed to more strict GPL) you <i>must</i> compile the engine with <tt>CASTLE_ENGINE_LGPL</tt> symbol defined in file <tt>castle_game_engine/base/castleconf.inc</tt>. Just put there <tt>{$define CASTLE_ENGINE_LGPL}</tt> line (or simply remove the beginning space in already prepared comment <tt>{&nbsp;$define CASTLE_ENGINE_LGPL}</tt>).</p>

    <p>This is necessary to avoid pulling in GPL-only dependencies. For now, this is only the NURBS unit (uses GPL-only code from <a href="http://vrml.cip.ica.uni-stuttgart.de/dune/">White_dune</a>). This missing NURBS implementation is the only difference between LGPL and "strict GPL" engine version.</p></li>

  <li><p>Note that LGPL stuff concerns only the engine, i.e. things inside <tt>castle_game_engine</tt> archive. The rest of the programs (<tt>view3dscene</tt>, <tt>castle</tt> etc.) are still strict GPL.</p></li>
</ul>

<?php
  castle_footer();
?>
