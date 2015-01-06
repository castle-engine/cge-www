<?php
  require_once 'castle_engine_functions.php';

  castle_header("Castle Game Engine - Features", NULL, array('features'));

  $toc = new TableOfContents(
    array(
      new TocItem('Main features', 'features'),
      new TocItem('Videos', 'videos'),
      new TocItem('Exhaustive list of features', 'exhaustive_features'),
    )
  );
  $toc->echo_numbers = true;

/*  echo flattr_button(); */

  echo pretty_heading('Features');
?>

<div class="download jumbotron">
<a class="btn btn-primary btn-lg" href="<?php echo FORUM_URL; ?>">We want your screenshots!</a>

<div style="margin-top: 1em;">We want to improve this page,
to show off our engine features by screenshots, not just words.
So, everyone, please submit the screenshots showing
how you work with the engine!
This is the time where you can show your cool work environment
(maybe a peek at your editor), or show us a glance of your
new game / tool you develop :)<br><br>
It would be cool if we can create this page as a community :) Thanks!
</div>
</div>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<img src="images/castle_game_engine_icon.png"
    alt="Castle Game Engine icon"
    style="float: right; clear: right; margin-top: 1em;" />

<ul>
  <li><p>We support a wide range of formats for 3D and 2D data. Our main scene format is <b><?php echo a_href_page('VRML / X3D', 'vrml_x3d'); ?></b>, which is an open standard (you'll find you can export to it from virtually any 3D modeler), and a lot of our strength comes from it (it's a scene format that can also express <b>interactive world features, scripting</b> etc.). But VRML/X3D is just our "scene graph" format, the whole engine is designed as a <b>general-purpose 3D/2D engine</b>, and many other model formats are supported as well: <b>Collada, Wavefront OBJ, MD3, <a href="https://sourceforge.net/p/castle-engine/wiki/Spine/">Spine</a></b> and many others.</p></li>

  <li><p>We have a lot of 3D graphic features. <b>Shaders, shadows, bump mapping, mirrors, custom viewports, screen-space effects</b>, and much more. Just look at the screenshots on this page&nbsp;:)</p></li>

  <li><p>We have a comfortable and extensible implementation of <b>scene manager and 3D objects</b>. You have a ready implementation of <b>levels, creatures (with AI), items, players</b> and other things typical to 3D games. You can extend it in many ways. You can also make your own 3D objects (if your game 3D world doesn't fit in our idea of creatures/levels etc.) by descending from T3D and adding to TCastleSceneManager.Items directly.

  <li><p><b>3D and 2D</b>. Not everything is about 3D. Our API is perfect <b>for 2D games too, with flexible rendering of 2D images, movies, text</b> and everything you can compose from them (like GUI controls). We also support <a href="https://sourceforge.net/p/castle-engine/wiki/Spine/">Spine</a> which is very cool for creating 2D animations.

  <li><p>The engine is developed for the <a href="http://freepascal.org/">Free Pascal Compiler</a>, an open-source cross-platform compiler, and the engine is cross-platform as well (<b>Linux, Windows, Mac OS X, Android, iOS</b>, and more). We have <a href="http://lazarus.freepascal.org/">Lazarus</a> components for RAD development, although the core engine doesn't depend on Lazarus LCL and you can develop full games with pure FPC (we have our own OpenGL window management unit, if you want). The whole engine is 100% clean Object Pascal code.</p></li>
</ul>

<?php echo $toc->html_section(); ?>

<p>Many <a href="https://www.youtube.com/channel/UCq9jJ5ivIXC5VEWiUAfxBxw">demo movies
are available on Castle Game Engine YouTube channel</a>.</p>

<?php echo $toc->html_section(); ?>


<ul>
  <li><b>Optimized OpenGL and OpenGLES2 rendering</b> of models in
    <b>X3D, VRML 2.0 (97) and VRML 1.0</b> formats.
    Including support for advanced VRML/X3D features like prototypes and
    events (user can interact with the 3D world).</li>

  <li><b>Collada, 3DS, MD3, Wavefront OBJ, Spine</b> file formats are also supported.
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
        (that you can override with <code>ComposedShader</code>
        or exhance with <code>Effect</code>, see links above).</li>
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

  <li>Handling of <b>fonts</b>. We can read fonts in many formats (like .ttf)
    using <i>FreeType</i> library, and render them at any size, with anti-aliasing
    or not. Fonts can also be embedded inside a Pascal source code,
    which allows us to provide default fonts (available as-is),
    and to use fonts even when <i>FreeType</i> library is not available.
    You can also use colorful fonts from a texture.
    Font rendering can allow international characters in UTF-8.</li>

  <li>Comfortable <b>3D sound engine</b>,
    using <?php echo a_href_page('OpenAL', 'openal'); ?>,
    with intelligent sound source management,
    supporting WAV and OggVorbis formats.
    Includes <?php echo a_href_page('VRML/X3D integration ("Sound" component of X3D specification)', 'x3d_implementation_sound'); ?>, so content creators
    can define sound sources themselves.</li>

  <li><b>2D controls</b>
    (buttons, panels, tooltips, on-screen menus etc.) are available.
    Customizing their look is very easy.
    Also creating your own 2D controls, using smartly stretched images and text,
    is very easy.
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

  <li>We support <b><a href="http://sourceforge.net/p/castle-engine/wiki/Android%20development/">Android</a></b>
    and <b><a href="http://sourceforge.net/p/castle-engine/wiki/iOS%20Development/">iOS (iPhone, iPad)</a></b>.
    You can write a single source code and compile it to a variety
    of desktop (Linux, Windows, MacOSX...) and mobile (Android, iOS) platforms.

  <li>The engine is <b>portable</b>. Currently tested and used on Linux,
    FreeBSD, Mac OS X and Windows, i386 or x86_64 (common 32-bit and
    64-bit processors), Android, iOS.
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

  <li><a href="https://sourceforge.net/p/castle-engine/wiki/Build%20tool/">Build tool</a>
    is available to easily compile and package our programs for various platforms,
    including creating Android apk packages.</li>

  <li>There are <b>many example programs</b>, look in sources
    <code>castle_game_engine/examples/</code> subdirectory.</li>

  <li>There are ready window classes (<code>TCastleWindow</code>)
    and Lazarus components (<code>TCastleControl</code>) to make simple
    VRML/X3D and other 3D models browser, on a Lazarus form or independent from Lazarus LCL.
    The engine is integrated with Lazarus &mdash;
    we have various <b>Lazarus components</b>.</li>

  <li>Engine <b>components are independent</b> when possible.
    For example, you can only take model loading and processing
    code, and write the rendering yourself. Or you can use our OpenGL rendering,
    but still initialize OpenGL context yourself (no requirement to do it
    by our <code>CastleWindow</code> unit). And so on.
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

  <li>Engine can be used to develop <b>natively-looking tools, not just OpenGL games</b>,
    since our OpenGL controls integrate with any GUI library (Lazarus LCL, GTK,
    WinAPI, Carbon...).
    You can embed the engine in a normal GUI program.
    You can use multiple OpenGL controls and windows visualizing (the same
    or different) game world.
    There are also various classes for processing 3D data
    without any rendering (like TCastleSceneCore), these are of course useful too
    (e.g. to write ray-tracers).
</ul>

<?php
  castle_footer();
?>
