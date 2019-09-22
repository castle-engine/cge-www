<?php
require_once 'castle_engine_functions.php';
castle_header("Features", array(
  'path' => array('features'),
  'social_share_image' => page_url('images/castle_game_engine_icon.png'),
));

$toc = new TableOfContents(
  array(
    new TocItem('Summary', 'summary'),
    new TocItem('Demo reel', 'demo'),
    new TocItem('Many 3D and 2D formats supported (X3D, glTF, Collada, Spine...)', 'data'),
    new TocItem('Portable (standalone, mobile, console, web browser plugin)', 'portable'),
    new TocItem('Graphic features and effects (shadows, mirrors, bump mapping...)', 'graphic'),
    new TocItem('Scene manager and comfortable API', 'api'),
    new TocItem('And much more!', 'more'),
  )
);

/*  echo flattr_button(); */
?>

<div class="fixed-width-content features-page">

<!-- <img src="images/castle_game_engine_icon.png" -->
<!--   alt="Castle Game Engine icon" -->
<!--   class="engine-icon" /> -->

<?php echo pretty_heading('Castle Game Engine Features'); ?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<ul>
  <li>Use <b>any 3D or 2D software</b> to create your models in any format: X3D, glTF, VRML<!--?php echo a_href_page('VRML / X3D', 'vrml_x3d'); ?-->, Collada, OBJ, MD3,
    <!--a href="https://github.com/castle-engine/castle-engine/wiki/Spine"-->Spine...
  <li>Develop <b>cross-platform</b> applications, for desktop (<b>Windows, Linux, macOS, FreeBSD...</b>), mobile (<b>Android, iOS</b>), consoles (<b>Nintendo Switch</b>) and other devices (<b>Raspberry Pi</b>).
  <li><b>Visual editor</b> to design games UI and to build applications, powerful command-line <b>build tool</b> under the hood.
  <li>Optimized rendering with a lot of graphic effects (<b>shadows, mirrors, bump mapping, shader effects</b>...).
  <li><b>Build and edit</b> the scene graph (X3D) <b>at runtime</b>.
    <!--Load and save images and X3D graph as needed.-->
    Create 3D processing, visualization tools and CAD applications.
  <li>Extensible system for game objects, with <b>physics, creatures with AI and navmesh</b>, and more.
  <li>Access numerous <b>services, like in-app purchases and game services</b> on mobile devices.
  <li>Create <b>cross-platform user-interface with anchors and automatic scaling</b>.
</ul>

<p>Read on for details!

<?php echo $toc->html_section(); ?>

<iframe class="media-responsive" width="853" height="480" src="https://www.youtube.com/embed/o5q7guVkYVo" frameborder="0" allowfullscreen></iframe>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p>We support a wide range of formats for 3D and 2D data.

    <ul>
      <li><p>Our main scene format is <b><?php echo a_href_page('X3D', 'vrml_x3d'); ?></b>, which is an open standard and you can export to it from virtually any 3D modeler. X3D can express 3D data with <b>interactive features, scripting, prototypes</b> etc.
      <li><p>While X3D is our <i>"scene graph"</i> format, the whole engine is designed as a <b>general-purpose 3D / 2D engine</b>, and many other model formats are supported as well: <a href="creating_data_model_formats.php">glTF 2.0, Spine JSON, VRML, Collada, 3DS, Wavefront OBJ, MD3, STL</a> and others.
      </li>
    </ul>

    <p>Our <?php echo a_href_page('manual', 'manual_intro'); ?> describes a lot of ways to use 3D and 2D objects,
    for example see <?php echo a_href_page('simple loading of 3D models', 'manual_load_3d'); ?>.

  <li>We have an optimized <b>renderer for OpenGL and OpenGLES2</b>.

    <?php
    echo castle_thumbs(array(
      array('filename' => 'fps_game_screen_18.png', 'titlealt' => 'FPS game demo'),
      array('filename' => 'fps_game_blender.png', 'titlealt' => 'FPS game demo - design of level in Blender'),
      array('filename' => 'castle_spine_screen_9.png', 'titlealt' => '2D game with animations done in Spine'),
    ), 'auto', 'left');
    ?>

  <li><p>You can use <b>any 3D modeler</b> to design your 3D and 2D data.
    <a href="http://www.blender.org/">Blender</a>? 3DS Max? Anything else?
    In part, that's because X3D (and it's predecessor, VRML) is an open and popular 3D format,
    and decent exporters for it exist in practically every 3D authoring tool.
    For detecting
    <?php echo a_href_page('"placeholders" on levels', 'creating_data_levels'); ?>
    you can configure the detection method to match your authoring tool.

    <p>In case of Blender,
    we have a <a href="creating_data_blender.php">description of how to create and export data</a>
    along with some custom exporters (but to export the static 3D data from Blender,
    you can just use the standard X3D exporter!).

  <li><p><b>Animations</b> are supported,
    <a href="vrml_engine_doc/output/xsl/html/chapter.animation.html">in two flavors</a>:
    interactive animation interpolated at runtime,
    or a "precalculated" animation (for fastest playback, but at the cost of using more memory).

    <?php
    echo castle_thumbs(array(
      //array('filename' => 'dragon_new_game.png', 'titlealt' => '&quot;Dragon Squash&quot; game'),
      //array('filename' => 'dragon_new_spine.png', 'titlealt' => '&quot;Dragon Squash&quot; animation in Spine'),
      array('filename' => 'dragon_old_spine.png', 'titlealt' => 'Old &quot;Dragon Squash&quot; animation in Spine'),
      array('filename' => 'dragon_old_view3dscene.png', 'titlealt' => 'Old &quot;Dragon Squash&quot; animation in view3dscene'),
    ), 'auto', 'left');
    ?>

  <li><p>We can <b>save (aka "serialize")</b> the current state of the world
    to an X3D file, which can then be read back by our engine.
    <!--
    file (standardized XML and classic encodings).-->
    <!-- is also fully supported and tested.-->
    You can use it e.g. to make a full-featured 3D editor on top of our engine!
    See <?php api_link('TCastleSceneCore.Save', 'CastleSceneCore.TCastleSceneCore.html#Save'); ?>
    and <?php api_link('Save3D', 'X3DNodes.html#Save3D'); ?> for basic methods
    to do this.

    <p>Various <?php echo a_href_page_hashlink('conversions between 3D model
    formats', 'view3dscene', 'section_converting'); ?>
    and even some editing capabilities are provided out-of-the-box by our tools.
    </li>

  <li><p>Reading and writing of <b>images</b> in various formats, processing them
    and using as OpenGL textures. Besides many common image formats
    (<b>PNG, JPG, PPM, BMP, and much more</b>), included is also support for
    <b>Khronos KTX</b> and <b>DDS</b> (textures
    with compression, mipmaps, 3d, cube maps) and
    RGBE format (Radiance HDR format).
    See:
    <ul>
      <li><?php echo a_href_page('manual about standard 2D controls', 'manual_2d_user_interface'); ?>,
      <li><?php echo a_href_page('manual about drawing custom 2D stuff', 'manual_player_2d_controls'); ?>,
      <li><?php api_link('CastleImages', 'CastleImages.html'); ?> (reading, writing, processing images on CPU),
      <li><?php api_link('CastleGLImages', 'CastleGLImages.html'); ?> (rendering and other processing of images on GPU).
    </ul>
    <!--,
    < ?php api_link('CastleDDS', 'CastleDDS.html'); ? > -->
    </li>

  <li><p><p>Handling of <b>fonts</b>. We can read fonts in many formats (like .ttf)
    using <i>FreeType</i> library, and render them at any size, with anti-aliasing
    or not. Fonts can also be embedded inside a Pascal source code,
    which allows us to provide default fonts (available as-is),
    and to use fonts even when <i>FreeType</i> library is not available.
    You can also use colorful fonts from a texture.
    Font rendering allows international characters in UTF-8.
    See <?php echo a_href_page('manual about text and fonts', 'manual_text'); ?> and unit
    <?php api_link('CastleFonts', 'CastleFonts.html'); ?>.
    </li>
</ul>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p>You can compile and package your games for various platforms:
    <ul>
      <li><b>Any modern desktop operating system</b> (Linux, Windows, macOS, FreeBSD, more...), with any CPU supported by FPC (like i386, x86_64, ARM, Aarch64...).
      <li><b>Mobile platforms</b> (<a href="https://github.com/castle-engine/castle-engine/wiki/Android">Android</a>, <a href="https://github.com/castle-engine/castle-engine/wiki/iOS">iOS</a>),
      <li><a href="https://castle-engine.io/wp/2019/03/23/castle-game-engine-supports-nintendo-switch/"><b>Nintendo Switch</b></a>.
      <li><b>Web browser plugin</b> (Linux, Windows, <a href="https://github.com/castle-engine/castle-engine/wiki/Web-Plugin">for browsers supporting NPAPI</a>). This is deprecated now, we're looking closely at WebAssembly target in FPC or pas2js to enable us compilation to modern web.
    </ul>

    <?php
    echo castle_thumbs(array(
      array('filename' => 'android12glued.png', 'titlealt' => 'Various Android applications developed using Castle Game Engine'),
      array('filename' => 'plugin_4.png', 'titlealt' => 'A couple of web browser plugin instances'),
    ), 'auto', 'left');
    ?>

  <li><p>It's easy to <?php echo a_href_page('compile the same game to many platforms', 'manual_cross_platform'); ?>.

  <li><p>We have a <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">build tool</a> to make it easy to compile and package your games. Creating complete, ready-to-be-released Android and iOS applications is trivial. We automatically generate an Android apk or iOS XCode project.

  <li><p>Trivial to use <b>integration with many services on Android</b>: games (achievements, leaderboards), in-app purchases, ads, sound, vibrations, and much more. See <a href="https://github.com/castle-engine/castle-engine/wiki/Android-Project-Services-Integrated-with-Castle-Game-Engine">Android Project Services</a> documentation.

  <li><p>Trivial to use <b><a href="https://github.com/castle-engine/castle-engine/wiki/iOS-Services">integration with many services on iOS</a></b>, like Apple Game Center, in-app purchases, Facebook SDK, Google Analytics and Game Analytics.

    <?php
    echo castle_thumbs(array(
      array('filename' => 'dragon_squash_title.png', 'titlealt' => 'Dragon Squash with Googe Games integration'),
      array('filename' => 'dragon_squash_achievements.png', 'titlealt' => 'Dragon Squash with Googe Games achievements'),
      array('filename' => 'android-components-icons.png', 'titlealt' => 'Integrations on Android available in Castle Game Engine - in-app purchases, ads, google games and more'),
    ), 'auto', 'left');
    ?>

  <li><p>The engine can be compiled into a <b>library useful from other programming languages</b>.
    This does not expose the complete engine API (for now), but it does expose a simple 3D model loading,
    rendering, animation, interaction. So you can e.g. make your own VRML / X3D browser.
    We have an example in engine sources <code>examples/library/qt_library_tester/</code>
    that shows how the engine can be <b>embedded in a C++ Qt application</b>.
</ul>

<?php echo $toc->html_section(); ?>

<ul>
  <li><b>Scene manager</b> is used for centralized 3D world handling,
    with <b>custom viewports</b> possible.

    <?php
    echo castle_thumbs(array(
      array('filename' => 'multiple_viewports_dynamic_world.png', 'titlealt' => 'Multiple viewports, interactive scene, shadow volumes and cube-map reflections'),
      array('filename' => 'view3dscene_viewports.png', 'titlealt' => 'Multiple viewports with a DOOM level in view3dscene'),
      //array('filename' => 'rhan_shrine_solid_wireframe.png', 'titlealt' => 'Solid wireframe rendering mode'),
    ), 'auto', 'left');
    ?>

  <li>Shadows by <b>shadow maps</b>.
    <?php echo a_href_page('Our shadow maps are very comfortable to use',
    'x3d_extensions_shadow_maps'); ?>, and shadows from multiple light
    sources are correctly rendered.
    We also have experimental <i>Variance Shadow Maps</i> implementation.

    <?php
    echo castle_thumbs(array(
      array('filename' => 'rhan_shrine_5_everything.png', 'titlealt' => 'Bump mapping and shadow maps from multiple light sources'),
      array('filename' => 'lights_editor_shadow_maps.png', 'titlealt' => 'Lights editor on a scene with shadow maps'),
      array('filename' => 'sunny_street_tree_hard.png', 'titlealt' => 'Close up shadows on the tree. Notice that leaves (modeled by alpha-test texture) also cast correct shadows.'),
    ), 'auto', 'left');
    ?>

  <li>Shadows by <b>shadow volumes</b> (full implementation,
    with z-fail / z-pass switching, silhouette detection etc. &mdash;
    but for now limited only to a single light).
    See <?php echo a_href_page_hashlink('shadow volumes documentation', 'x3d_extensions',
    'section_ext_shadows'); ?>.

  <li><b><?php echo a_href_page_hashlink('Bump mapping (normal maps), specular maps, shininess maps and more',
    'x3d_implementation_texturing_extensions', 'section_ext_common_surface_shader'); ?></b>
    are avaiable.
    Our <a href="creating_data_blender.php">custom Blender X3D exporter</a>
    can automatically generate the necessary information.
    Various <i>bump mapping algorithms</i>
    are implemented: from the classic bump mapping (take normal from the texture),
    up to the <i>steep parallax bump mapping with self-shadowing</i>.

  <li><b>Shaders</b>:
    <ul>
      <li>We have classes to easily use GLSL shaders
        (see <?php api_link('CastleGLShaders', 'CastleGLShaders.html'); ?> unit).
        But usually you don't need to use them, because...</li>
      <li>You can
        <?php echo a_href_page('design and control GLSL shaders inside X3D',
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
        or enhance with <code>Effect</code>, see links above).</li>
    </ul>

    <?php
    echo castle_thumbs(array(
      array('filename' => 'castle_fountain_1.png', 'titlealt' => 'Fountain with water reflecting environment using cubemap'),
      array('filename' => 'barna29_nice_shadows.png', 'titlealt' => 'Real-time water with caustics, reflections, shadows'),
      array('filename' => 'volumetric_animated_fog_all.png', 'titlealt' => 'Volumetric fog'),
    ), 'auto', 'left');
    ?>

  <li><b><?php echo a_href_page('Screen-space effects', 'x3d_extensions_screen_effects'); ?></b> in GLSL are
    very easy to define and use, in pure X3D or in Pascal code.

  <li>Many texturing features:

    <ul>
      <li><b>multi-texturing</b> (see <?php echo a_href_page('X3D texturing component docs', 'x3d_implementation_texturing'); ?>),
     <li><b>cube map texturing</b> (can be loaded from separate files,
        DDS files, or captured during runtime,
        see <?php echo a_href_page('X3D cubemap texturing component docs', 'x3d_implementation_cubemaptexturing'); ?>),
      <li><b>3D textures</b> (see <?php echo a_href_page('X3D 3D texturing component docs', 'x3d_implementation_texturing3d'); ?>),
      <li><b>compressed textures</b> (supporting both desktop and mobile
        compression formats, like S3TC, ATITC, PVRTC, ETC).<br>
        <b>The compressed and/or downscaled texture versions may be automatically generated and used</b>,
        you simply declare them in <a href="creating_data_material_properties.php">the material_properties.xml file</a>
        and use <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">our build tool</a> to regenerate all texures at building.
        <!-- ?php api_link('Alternatively, you can replace image URLs at runtime,
          to switch uncompressed texture files with compressed depending on GPU',
          'CastleImages.html#LoadImagePreprocess'); ?-->
      <li><b>anisotropic filtering</b> (just use <a href="<?php echo x3d_spec_latest_url('texturing'); ?>#TextureProperties">TextureProperties.anisotropicDegree</a> in X3D),
      <li><?php api_link('GPU texture memory profiler', 'CastleGLImages.html#TextureMemoryProfiler'); ?>,
        extremely valuable to optimize your texture memory usage (important
        on mobile platforms).
    </ul>

    <?php
    echo castle_thumbs(array(
      array('filename' => 'water_reflections.png', 'titlealt' => 'Water reflections by optimized GeneratedCubeMapTexture'),
    //  array('filename' => 'tex3d_smoke.png', 'titlealt' => 'Fog from 3D noise'),
    //  array('filename' => 'rendered_texture_mirror_2.png', 'titlealt' => 'Mirrors by RenderedTexture, by Victor Amat'),
      array('filename' => 'rendered_texture_with_background.png', 'titlealt' => 'RenderedTexture'),
    ), 'auto', 'left');
    ?>

  <li>Speeding up rendering by <b>hardware occlusion query</b>,
    a <a href="http://http.developer.nvidia.com/GPUGems/gpugems_ch29.html">simple approach</a> and
    more involved <a href="http://http.developer.nvidia.com/GPUGems2/gpugems2_chapter06.html">Coherent Hierarchical Culling</a>.
    See <?php api_link('UseOcclusionQuery', 'CastleScene.TSceneRenderingAttributes.html#UseOcclusionQuery'); ?>,
    <?php api_link('UseHierarchicalOcclusionQuery', 'CastleScene.TSceneRenderingAttributes.html#UseHierarchicalOcclusionQuery'); ?>
    properties.

  <li><b>Anti-aliasing</b> (by OpenGL multi-sampling),
    see <?php api_link('AntiAliasing', 'CastleWindow.TCastleWindowCustom.html#AntiAliasing'); ?>
    property.
    </li>
</ul>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p>We have a comfortable and extensible implementation of <b>scene manager and 3D objects</b>. You have a ready implementation of <b>levels, creatures (with AI), items, players</b> and other things typical to 3D games.
    <ul>
      <li>You can extend it in many ways.
      <li>You can also make your own 3D objects (if your game 3D world doesn't fit in our idea of creatures/levels etc.) by descending from <?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?>.
    </ul>
    <p><?php echo a_href_page('Engine manual', 'manual_resources_extending'); ?> contains detailed information about this.

  <li><p><b>3D and 2D</b>. Not everything is about 3D. Our API is perfect <b>for 2D games too, with flexible (and pixel-perfect) rendering of 2D images, movies, text</b> and everything you can compose from them (like GUI controls). We also support <a href="https://github.com/castle-engine/castle-engine/wiki/Spine">Spine</a> which is very cool for creating 2D animations.

  <li><p><b>Rigid body physics</b>. We are integrated with a powerful <i>Kraft Physics Engine</i> and you can <a href="manual_physics.php">easily use dynamic physics in your games</a>.

    <div class="thumbnails">
      <iframe width="300" height="168" src="https://www.youtube.com/embed/8k9zX6dPQEU" frameborder="0" allowfullscreen></iframe>
      <iframe width="300" height="168" src="https://www.youtube.com/embed/hIuEGnRm-yM" frameborder="0" allowfullscreen></iframe>
    </div>

  <li><p>Octrees are used for various <b>collision detection</b> tasks.
    For dynamic scenes, a hierarchy of octrees is used, allowing accurate
    and fast collision detection even when the scene constantly changes.
    There are many ways to use collision detection,
    for example you can query for collision between a specific
    <?php api_link('TCastleTransform instance', 'CastleTransform.TCastleTransform.html'); ?> and the rest of the world
    (by methods like
    <?php api_link('TCastleTransform.Move', 'CastleTransform.TCastleTransform.html#Move'); ?>,
    <?php api_link('TCastleTransform.Ray', 'CastleTransform.TCastleTransform.html#Ray'); ?>
    <?php api_link('TCastleTransform.Height', 'CastleTransform.TCastleTransform.html#Height'); ?>)
    or you can ask the whole world whether it collides with some geometric shape
    (calling methods like
    <?php api_link('TCastleTransform.BoxCollision', 'CastleTransform.TCastleTransform.html#BoxCollision'); ?>,
    <?php api_link('TCastleTransform.SphereCollision', 'CastleTransform.TCastleTransform.html#SphereCollision'); ?>
    <?php api_link('TCastleTransform.RayCollision', 'CastleTransform.TCastleTransform.html#RayCollision'); ?>
    on the main 3D world instance: <?php api_link('CastleSceneManager.Items', 'CastleSceneManager.TCastleSceneManager.html#Items'); ?>).
    </li>
</ul>

<?php echo $toc->html_section(); ?>

<ul>
  <li>Comfortable <b><?php echo a_href_page('3D sound engine', 'manual_sound'); ?></b>,
    using <?php echo a_href_page('OpenAL', 'openal'); ?>,
    with intelligent sound source management,
    supporting WAV and OggVorbis formats.
    Includes <?php echo a_href_page('X3D integration ("Sound" component of X3D specification)', 'x3d_implementation_sound'); ?>, so content creators
    can define sound sources themselves.</li>

  <li><?php echo a_href_page('view3dscene', 'view3dscene'); ?> tool to view
    and inspect your 3D models, before loading them in your game.

  <li><b>2D controls</b>
    (buttons, panels, tooltips, on-screen menus etc.) are available.
    Customizing their look is very easy.
    Also creating your own 2D controls, using smartly stretched images and text,
    is very easy.
    Good for games, where making a custom-looking GUI (that fits with
    your game theme) is important.
    See <?php api_link('CastleControls', 'CastleControls.html'); ?> unit.</li>

  <li>Although the main focus of the engine is real-time rendering,
    we have also implemented a <b>software ray-tracer</b>, just to show that it's
    possible! Two ray-tracing algorithms may be used:
    deterministic (classic Whitted-style ray-tracer)
    or Monte Carlo <i>path tracing</i>.
    All the engine 3D formats are supported, with smooth normals, textures,
    reflections, shadows...
    See <?php echo a_href_page("the gallery","raytr_gallery"); ?>
    and the
    <?php api_link('CastleRayTracer', 'CastleRayTracer.html'); ?> unit.
  </li>

  <li>Playing <b>movie files</b>. This includes loading and saving
    as image sequence or "real" movie files (<a href="http://ffmpeg.mplayerhq.hu/">ffmpeg</a>
    is needed to encode / decode movie files). While the implementation
    is limited to a small movies for now (as memory consumption is large),
    it's perfect for flame or smoke animation in games. We even have a simple
    movie editor as an example program in engine sources.
    See <?php api_link('TVideo', 'CastleVideos.TVideo.html'); ?>,
    <?php api_link('TGLVideo2D', 'CastleGLImages.TGLVideo2D.html'); ?>
    and related classes.

  <li>You can read <b>maps</b> designed using
    <a href="http://www.mapeditor.org/">Tiled Map Editor</a>.
    A default visualization (as a 2D control) is included in the engine,
    but you can also read the map information and display it on your own,
    in 2D or 3D. See the <code>examples/tiled/</code> in the engine code.</li>

  <li>You can convert a spritesheet from Cocos2D or Starling formats into X3D.
    See the program <code>tools/sprite-sheet-to-x3d</code> distributed
    as part of the engine.</li>

  <li>We have <b>many example programs</b>. Browse the engine
    <code>castle_game_engine/examples/</code> subdirectory.
    For even more examples, see
    <?php echo a_href_page('some larger
    games we have developed using Castle Game Engine', 'all_programs'); ?> and their
    sources (for example <a href="https://github.com/castle-engine/">on GitHub
    Castle Game Engine organization</a>).

    <?php
    echo castle_thumbs(array(
      array('filename' => 'terrain1.png', 'titlealt' => 'Terrain 1'),
      array('filename' => 'rift_2.png', 'titlealt' => 'Fixed-camera game'),
      array('filename' => 'mountains_of_fire_screen_1.png', 'titlealt' => 'Mountains Of Fire - split-screen coop game'),
    ), 'auto', 'left');
    ?>

  <li>We have ready window classes (<code>TCastleWindow</code>)
    and Lazarus components (<code>TCastleControl</code>) to make simple
    X3D and other 3D models browser, on a Lazarus form or independent from Lazarus LCL.
    The engine is integrated with Lazarus &mdash;
    we have various <b>Lazarus components</b>.</li>

  <li>Engine <b>components are independent</b> when possible.
    For example, you can only take model loading and processing
    code, and write the rendering yourself. Or you can use our OpenGL rendering,
    but still initialize OpenGL context yourself (no requirement to do it
    by our <code>CastleWindow</code> unit). And so on.
    Of course, ultimately you can just use everything from our engine,
    nicely integrated &mdash; but the point is that you don't have to.</li>

  <li>Engine can be used to develop <b>natively-looking tools, not just OpenGL games</b>,
    since our OpenGL controls integrate with any GUI library (Lazarus LCL, GTK,
    WinAPI, Carbon...).
    You can embed the engine in a normal GUI program.
    You can use multiple OpenGL controls and windows visualizing (the same
    or different) game world.
    There are also various classes for processing 3D data
    without any rendering (like TCastleSceneCore), these are of course useful too
    (e.g. to write ray-tracers).

  <li><p>The engine is developed for the <a href="http://freepascal.org/">Free Pascal Compiler</a>, an open-source cross-platform compiler. We have <a href="http://lazarus.freepascal.org/">Lazarus</a> components for RAD development, although the core engine doesn't depend on Lazarus LCL and you can develop full games with pure FPC (we have our own OpenGL window management unit, if you want). The whole engine is 100% clean Object Pascal code.</p>

    <?php
    echo castle_thumbs(array(
      array('filename' => 'model_3d_viewer.png', 'titlealt' => 'Lazarus model_3d_viewer example'),
      array('filename' => 'little_things_screen_7.png', 'titlealt' => '&quot;Little Things&quot; game with screen effects'),
      array('filename' => 'chinchilla_diffuse_prt.png', 'titlealt' => 'Precomputed Radiance Transfer'),
    ), 'auto', 'left');
    ?>
</ul>

<!--
Make this link more prominent when we have more movies?

<p>Many <a href="https://www.youtube.com/channel/UCq9jJ5ivIXC5VEWiUAfxBxw">demo movies
are available on Castle Game Engine YouTube channel</a>.</p>
-->

<div class="download jumbotron">
<a class="btn btn-primary btn-lg" href="<?php echo FORUM_URL; ?>">We want your screenshots!</a>

<div style="margin-top: 1em;">Do you use our engine or view3dscene?
Please make us happy and tell us about it!
For example, post a a screenshot to our
<?php echo a_href_page('forum', 'forum'); ?>.
We will happily use the best screenshots to showcase our engine here.
This is the time where you can show your cool work environment
(maybe a peek at your editor), or show us a glance of your
new game / tool you develop&nbsp;:)
</div>
</div>

</div> <!-- class="fixed-width-content" -->

<?php
  castle_footer();
?>

<?php

/*
------------------------------------------------------------------------------
UNUSED:
  <li>CastleWindow unit is available to easily <b>create windows with OpenGL
    context</b>. <!-- The intention of this unit is to be something like glut,
    but magnitudes better &mdash; using clean ObjectPascal, for start.-->
    It allows you to easily create <b>menu bars, open/save file and similar
    dialogs</b> that are implemented using native controls (GTK 2 or WinAPI).</li>

  <li><b>Collada, 3DS, MD3, Wavefront OBJ, Spine</b> file formats are also supported.
    They are internally converted into the X3D nodes graph,
    which means that they get all the optimizations for rendering,
    and 3D content from all file formats can be mixed (for 3D editing tools
    and such).</li>

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
*/
?>
