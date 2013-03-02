<?php

/* Next news:
* There was a silent 4.0.1 release to fix compilation of CastleGLWindowsFonts unit, and do other minor fixes/improvements around font-related units and examples.
* Improvements to 2D rendering (as it happens, they also mostly revolve around bitmap fonts):
  - use SetWindowPos instead of glRasterPos almost everywhere (cleaner, better for future strict OpenGL >= 3, and avoids crashes with Mesa 9 --- at least with "OpenGL renderer string: Gallium 0.4 on AMD RV710" and "OpenGL version string: 2.1 Mesa 9.0.2" (on Ubuntu 12.10)).
  - use PrintAndMove instead of Print, it's much more optimal.
* Support for 8 and 16 samples for anti-aliasing, there are (at least NVidia) GPUs supprting it. By "support" I mean that you can see new options now in view3dscene Preferences->Anti-Aliasing, and that TCastleWindow.AntiAliasing allows this, and that our screen effects library cooperates with them (so make screen effects combined with anti-aliasing work). Everything else (like basic TCastleWindow.MultiSampling and TCastleControl.MultiSampling) was already working.
*/

array_push($news,
    array('title' => 'Castle Game Engine 4.0.0 release! And view3dscene 3.12.0, and castle 1.0.0, and more',
          'year' => 2013,
          'month' => 1,
          'day' => 26,
          'short_description' => '',
          'guid' => 'release-4.0.0',
          'description' =>
castle_thumbs(array(
//  array('filename' => 'fps_game_screen_19.png', 'titlealt' => 'FPS game screen'),
  array('filename' => 'fps_game_screen_18.png', 'titlealt' => 'FPS game screen'),
  array('filename' => 'fps_game_screen_17.png', 'titlealt' => 'FPS game screen'),
  array('filename' => 'control_lazarus_shared_resources.png', 'titlealt' => 'Lazarus form with 2 castle controls sharing textures'),
  array('filename' => 'resource_animations_1.png', 'titlealt' => 'resource_animations: Knight default idle animation'),
  array('filename' => 'missile_stuck_in_wall_4.png', 'titlealt' => 'Missile (arrow) stuck in a wall'),
  array('filename' => 'castle_items_tower_screen_2_ring.png', 'titlealt' => 'Testing dropping items on level'),
  array('filename' => 'barna29_water_0.png', 'titlealt' => 'German Pavillion in the Universal Expo of Barcelona of 1929 (by Victor Amat)'),
  array('filename' => 'room_arranger_3_ssao.png', 'titlealt' => 'Room Arranger with SSAO demo, shown by view3dscene'),
  array('filename' => 'room_arranger_viewer_2.png', 'titlealt' => 'Final stages of RoomArranger viewer using our engine, with more controls and SSAO'),
  array('filename' => 'lights_editor_shadow_maps.png', 'titlealt' => 'Lights Editor playing with shadow maps'),
  array('filename' => 'lights_editor_fountain_0.png', 'titlealt' => 'Lights Editor in view3dscene - fountain, shadow volumes settings'),
)) .
'<p>We proudly present the <a href="' . CURRENT_URL . 'engine.php">Castle Game Engine</a> 4.0.0 release! This is the greatest release of our engine <i>ever</i> for <a href="http://www.freepascal.org/">ObjectPascal</a> developers interested in making their own games:</p>

<ol>
  <li><p>We introduce a <b>high-level game API, to construct a fully-working 3D game really easy</b>. This includes integrated units to manage levels, creatures (with smart AI), items, player, with inventory, management of 3D resources, easy 3D sound, flexible game data layout (you can use XML files to define a lot of things without touching game code), and so on.</p>

    <p>Of course, we use <a href="' . CURRENT_URL . 'vrml_x3d.php">VRML/X3D</a> for all the 3D models, so everything can be animated and interactive, and may use features like shadows, mirrors, shaders and such. See <a href="' . CURRENT_URL . 'demo_models.php">our demo 3D models</a> (in particular, check new <tt>water/caustics/</tt> demo inside, by Victor Amat, <a href="http://youtu.be/1mUU8prDi9k">movie showing it is here</a>).</p>

    <p>There are a couple new examples in engine sources. First of all, <tt>examples/fps_game/</tt> is a must-see: it shows a complete working FPS game, with a lot of comments in the source code and data files.</p>

    ' . (!HTML_VALIDATION ? '<iframe width="640" height="480" src="http://www.youtube.com/embed/S0bA3mJ8lZc" frameborder="0" allowfullscreen></iframe>' : '') . '

    <p>The engine is very flexible, you can derive new classes and override a lot of methods to customize behavior of everything in ObjectPascal. You can define new level logic, new creatures, new items, and finally you can also use basic 3D classes to easily create any 3D entities suitable for your game. You can also easily create your own viewports, scene managers, 2D controls, use 3D sound and much more. Everything is explained in the <a href="' . CURRENT_URL . 'tutorial_intro.php">new tutorial</a>.</p>
    <!--  Of course, you can also define animations and even use scripting inside VRML/X3D files, but for a non-trivial game -->

    <!--
    <p>Another new example program is the <tt>examples/resource_animations</tt> to view creature/item animations. This is accompanied by <a href="' . CURRENT_URL . 'creating_data_resources.php">creating data guide about resources</a>, that explains new methods to define 3D animations.</p>
    -->
  </li>

  <li><p>We have <b>a lot of new documentation</b> to show you how to use the engine:</p>

    <ul>
      <li><a href="' . CURRENT_URL . 'tutorial_intro.php">Tutorial</a> - introduces most concepts of the engine, with example code snippets.</li>
      <li><a href="' . CURRENT_URL . 'tutorial_classes_overview.php">Classes overview (cheatsheet)</a> - a quick ride through most important engine classes and ideas.</li>
      <li><a href="' . CURRENT_URL . 'creating_data_intro.php">Guide to creating game data</a> - how to create your 3D models using any 3D modeling software (like open-source <a href="http://www.blender.org/">Blender</a>), and how to write various configuration files like <tt>resource.xml</tt> and <tt>level.xml</tt>.</li>
      <li><a href="' . CURRENT_URL . 'apidoc/html/index.html">As always, there is also a complete API reference.</a></li>
    </ul>

  <li><p>The full list of changes is definitely too large to fit into a normal news item. In the past I usually tried to list all the important changes in a release announcement, but there\'s just too many things this time :) New engine units include
    <a href="' . CURRENT_URL . 'apidoc/html/CastleCreatures.html">CastleCreatures</a>,
    <a href="' . CURRENT_URL . 'apidoc/html/CastleItems.html">CastleItems</a>,
    <a href="' . CURRENT_URL . 'apidoc/html/CastleLevels.html">CastleLevels</a>,
    <a href="' . CURRENT_URL . 'apidoc/html/CastlePlayer.html">CastlePlayer</a>,
    <a href="' . CURRENT_URL . 'apidoc/html/CastleMaterialProperties.html">CastleMaterialProperties</a>,
    <a href="' . CURRENT_URL . 'apidoc/html/CastleResources.html">CastleResources</a>.
    There are also countless changes in the rest of the engine to make it better and more integrated. See the <a href="' . CURRENT_URL . 'news.php">news</a> from the whole 2012 year for a complete list of details.</p>

    <!--p>This was probably the longest delay between releases of our engine, but I hope it was worth it :)</p-->

    <p><i>For developers upgrading from engine 3 version</i>: all of our unit names are now prefixed with <tt>CastleXxx</tt>, see <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/castle_game_engine/doc/naming_engine_4.0.txt">doc/naming_engine_4.0.txt</a> document. So you will almost definitely need to fix your "uses" clauses. Aside from that, the engine is quite compatible with previous version. <!-- (as the basis for controls and scene manager and 3D objects was already there; now it\'s just used much more extensively). --> Of course please <a href="' . CURRENT_URL . 'forum.php">ask of the forum</a> if you have any questions about upgrading code from engine 3 to 4 (or about anything else, for that matter :)</p>
</ol>

<p>We also release a <b>new 3.12.0 version of <a href="' . CURRENT_URL . 'view3dscene.php">view3dscene</a>, our VRML/X3D browser and viewer for other 3D model formats</b>. The most important improvements are listed below. Note that improvements 1-8 are actually in the engine, instantly available for all the games/applications using our engine. But most users will probably observe them in <a href="' . CURRENT_URL . 'view3dscene.php">view3dscene</a> for the 1st time.</p>

<ol>
  <li>Navigating in <i>Walk / Fly</i> modes by mouse dragging.</li>
  <li><a href="http://youtu.be/lsUztfdike8">Using 3D mouse devices</a>.</li>
  <li>Screen-space ambient occlusion (see menu <i>"View -> Screen Effects"</i> menu item, developers: try the ultra-simple <a href="' . CURRENT_URL . 'apidoc/html/CastleSceneManager.TCastleAbstractViewport.html#ScreenSpaceAmbientOcclusion">TCastleAbstractViewport.ScreenSpaceAmbientOcclusion</a> boolean property).</li>
  <li>All screen effects cooperate now with multi-sampling (anti-aliasing).</li>
  <li>UNIT statement from X3D 3.3 is implemented.</li>
  <li>VisibilitySensor node is supported.</li>
  <li>Many fixes to triangulating concave polygons.</li>
  <li><a href="http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_shading">X3D extension to force Phong shading</a>.</li>
  <li>New <i>"Edit -> Lights Editor"</i> feature.</li>
</ol>

<p>Many thanks go to Jan Adamec for implementing the 1-3 features above (and more)! Our engine is used as VRML viewer for shareware <a href="http://www.roomarranger.com/">Room Arranger</a>.</p>

<p>All the other programs and data on our pages were updated to use/show new <a href="http://castle-engine.sourceforge.net/engine.php">engine 4.0.0 version</a>:
  <a href="' . CURRENT_URL . 'castle.php">The Castle 1.0.0 (finally!)</a>,
  <a href="' . CURRENT_URL . 'malfunction.php">malfunction 1.2.8</a>,
  <a href="' . CURRENT_URL . 'kambi_lines.php">kambi_lines 1.1.7</a>,
  <a href="' . CURRENT_URL . 'view3dscene.php">view3dscene 3.12.0</a>,
  <a href="' . CURRENT_URL . 'rayhunter.php">rayhunter 1.3.4</a>,
  <a href="' . CURRENT_URL . 'glviewimage.php">glViewImage 1.4.1</a>,
  <a href="' . CURRENT_URL . 'glplotter_and_gen_function.php">glplotter 1.2.5</a>,
  <a href="' . CURRENT_URL . 'bezier_curves.php">bezier_curves 1.1.9</a>,
  <a href="' . CURRENT_URL . 'glinformation.php">glinformation 1.2.2</a>,
  <a href="' . CURRENT_URL . 'glplotter_and_gen_function.php">gen_function 1.0.5</a>,
  <a href="' . CURRENT_URL . 'demo_models.php">demo models 3.2.0</a>.
</p>

<!--
- kambiShadows, kambiShadowsMain fields renamed to shadowVolumes, shadowVolumesMain &mdash; much more sensible names. But the old names will remain to be available, for compatibility (probably for a long time). TCastleAbstractViewport.ShadowVolumes is true by default now.
-->
'),

    array('title' => 'Development: engine 4.0.0 release in a few days, many improvements this month',
          'year' => 2013,
          'month' => 1,
          'day' => 17,
          'short_description' => '',
          'guid' => '2013-01-17',
          'description' =>
castle_thumbs(array(
  array('filename' => 'fps_game_screen_19.png', 'titlealt' => 'FPS game screen'),
  array('filename' => 'fps_game_screen_18.png', 'titlealt' => 'FPS game screen'),
  array('filename' => 'fps_game_screen_17.png', 'titlealt' => 'FPS game screen'),
  array('filename' => 'fps_game_screen_16.png', 'titlealt' => 'FPS game screen'),
  array('filename' => 'fps_game_screen_15.png', 'titlealt' => 'FPS game screen'),
  array('filename' => 'fps_game_screen_14.png', 'titlealt' => 'FPS game screen'),
  array('filename' => 'fps_game_screen_13.png', 'titlealt' => 'FPS game screen'),
  array('filename' => 'fps_game_screen_12.png', 'titlealt' => 'FPS game screen'),
  array('filename' => 'fps_game_screen_11.png', 'titlealt' => 'FPS game screen'),
//  array('filename' => 'fps_game_screen_10.png', 'titlealt' => 'FPS game screen'),
//  array('filename' => 'fps_game_screen_03.png', 'titlealt' => 'FPS game screen'),
  array('filename' => 'fps_game_screen_02.png', 'titlealt' => 'FPS game screen'),
//  array('filename' => 'fps_game_screen_01.png', 'titlealt' => 'FPS game screen'),
//  array('filename' => 'fps_game_screen_00.png', 'titlealt' => 'FPS game screen'),
  array('filename' => 'debug_castle_screen_1.png', 'titlealt' => 'Debug 3D information'),
//  array('filename' => 'debug_castle_screen_0.png', 'titlealt' => 'Debug 3D information'),
)) .
'<p>Many final improvements done to the engine, to make the next 4.0.0 release really polished for developers. Below is a list of new SVN features (implemented since last news, mostly within the last 3 weeks).</p>

<p>Engine 4.0.0 release is almost ready, what remains is finishing the new tutorials. BTW, I got into habit of posting a little smaller (and more frequent) notes about engine development on <a href="https://plus.google.com/101185352355602218697" rel="publisher">our Google+ page</a>, so you\'re welcome to follow us there.</p>

<p>Latest improvements to the engine:</p>

<ol>
  <li>
    <p><b>New very important example program is available in engine source code: <tt>fps_game</tt></b>.</p>
    <p>It is a simple demo, with a lot of comments in code, of a fully-working FPS game. We have level, creatures, items (medkit and weapon), inventory, player can be hurt and die, player can shoot and kill the creatures, bump mapping, move limit, sectors/waypoints (for AI), buttons, notifications, custom viewport, sounds, sky, water, footsteps and a <b>lot</b> of comments.</p>
    <p>P.S. Various game data (knight, textures, sounds) are from <a href="http://opengameart.org/">opengameart.org</a>, countless thanks go to it\'s many contributors. Details what is from where are inside AUTHORS.txt files in sources. Only level and items were modeled by Michalis from scratch.</p>
  </li>

  <li><p><b>Improvements to game logic:</b></p>
    <ul>
      <li>Creature enemy is now configurable by overriding <tt>TWalkAttackCreature.Enemy</tt> method. The default implementation chooses the central player as the enemy (if the player is not dead yet), but you can override it to implement something more sophisticated (like creature vs creature fighting, teams etc. &mdash; not everything is tested there yet, but it all should be doable now, so go wild :).</li>
      <li>New methods to define creature/items (all resources) animations are available. You can load animation from <a href="http://castle-engine.sourceforge.net/kanim_format.php">kanim</a>, or from multiple VRML/X3D files (one VRML/X3D for each animation), or from a single VRML/X3D file (in which case the file will have to contain different TimeSensor to activate each animation). Description how it works is inside DRAFT.modeling_tutorial.txt for now. Example of all 3 methods to define animations are inside <tt>castle_game_engine/examples/resource_animations/</tt> in sources.</li>
      <li>Many improvements to our <tt>RenderDebug3D</tt> and <tt>RenderDebugCaptions</tt> mechanisms (you can see them e.g. in fps_game demo): respect Orientation (work with games with either +Y or +Z up), sphere is displayed better,  caption is multi-line and contains "enemy distance" info.</li>
      <li><tt>TItem.Picked</tt> method can be overridden to configure what happens when you pick item. Example code in fps_game shows how to make an item that is consumed on pickup (aka "power-up").</li>
      <li><tt>TextureProperties</tt> mechanism was enhanced into <tt>MaterialProperties</tt>, that can describe much more things. In the future, we can add there more stuff about material/texture that cannot be comfortably expressed in Blender, but that is conceptually tied to material/texture. For now, the main addition is that you can define bump mapping by normal_map properties there. The hack in <a href="http://castle-engine.sourceforge.net/blender.php">our Blender exporter</a> (to autodetect xxx_normalmap textures) will possibly be removed at some point (as it has no future, it\'s dirty).</li>
      <li><tt>player.xml</tt> must be now loaded explicitly by <tt>Player.LoadFromFile</tt> (we try to never load the file from some hardcoded location without you requesting it). <!--; previous implementation was always loading <tt>data/player.xml</tt> in <tt>TPlayer</tt> constructor). You can give your own location of <tt>player.xml</tt>. --> Also, all player properties have now sensible defaults, so in simple cases there\'s no need to use this file at all.</li>
    </ul>
  </li>

  <li><p><b>Final code renames/refactoring:</b></p>
    <ul>
      <li>A lot of unit renames, to bring all units to <tt>CastleXxx</tt> names. I admit it was a mistake trying to have non-prefixed unit names in engine 3.0.0. After some time, many unit names had to be prefixed anyway, and this created inconsistent naming &mdash; some unit names had <tt>Castle</tt> prefix, some not. Now all unit names have <tt>Castle</tt> prefix (the only exception being <tt>X3DXxx</tt> units).
        <p>It means that when upgrading your project from engine 3 to 4, you\'ll have to do appropriate renames in the "uses" clause of your units. Compiler will tell you where :) I also prepared a list of renames in <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/castle_game_engine/doc/naming_engine_4.0.txt">doc/naming_engine_4.0.txt</a> file.</li>
      <li>Many <tt>DefaultXxx</tt> global constants moved to constants inside classes, to not pollute namespaces with a lot of constants that are usually useful only as default value for corresponding property Xxx.</li>
      <li>All creature/items "kinds" renamed to "resources". Previously, terms "kinds" and "resources" were used to refer to the same thing, which could create confusion. Now it\'s simple: <tt>resource.xml</tt> file refers to the resource class (maybe it\'s creature resource, maybe item resource and so on), and creates resource instance. <tt>TCreatureResource</tt> is derived from <tt>T3DResource</tt> (as <tt>TCreature</tt> derives from <tt>T3D</tt>). <tt>TItemResource</tt> is derived from <tt>T3DResource</tt>, like <tt>TItemOnLevel</tt> derives from <tt>T3D</tt>.</li>
      <li><tt>CastleKeysMouse</tt> (formerly <tt>KeysMouse</tt>) types are no longer aliased in <tt>CastleWindow</tt> unit. This was unmaintainable, and wasn\'t available in Lazarus <tt>CastleControl</tt>. Simply add <tt>CastleKeysMouse</tt> to your uses clause if you use any identifier from it.</li>
      <li>Removed units: <tt>VideosCache</tt> (merged into <tt>CastleVideos</tt>), <tt>Rectangles</tt> (merged to <tt>CastleOnScreenMenu</tt>), <tt>BezierCurve</tt> (merged to <tt>CastleCurves</tt>), <tt>NormalizationCubeMap</tt> (moved to <tt>bump_mapping</tt> example dir, not useful for engine; and <tt>bump_mapping</tt> was actually removed from engine core, as not useful anymore, it\'s now in <a href="http://michalis.ii.uni.wroc.pl/wsvn/michalis/obscure_castle_engine_demos_and_tools/">obscure_castle_engine_demos_and_tools repo</a>).</li>
    </ul>
  </li>

  <li><p><b>Other engine improvements:</b></p>
    <ul>
      <li><tt>CastleImages</tt> supports resizing images with bilinear interpolation. <tt>TCastleImage.Resize</tt> and <tt>TCastleImage.MakeResized</tt> take <tt>Interpolation</tt> parameter, <tt>riNearest</tt> or <tt>riBilinear</tt>. Default is still <tt>riNearest</tt>, so fast (and ugly) like before. <tt>CastleWindowProgress</tt>, when it needs to scale progress background image, requests <tt>riBilinear</tt> interpolation. <a href="http://castle-engine.sourceforge.net/glviewimage.php">glViewImage</a> allows to test both resizing methods.</li>
      <li>Fonts: Huge refactoring of font units and types.
        <ul>
          <li>We now have 4 units: <tt>CastleOutlineFonts</tt>, <tt>CastleBitmapFonts</tt>, <tt>CastleGLOutlineFonts</tt>, <tt>CastleGLBitmapFonts</tt>.
          <li><tt>TBitmapFont</tt> and <tt>TOutlineFont</tt> are classes, with Data field and some methods. This way utilities from <tt>CastleOutlineFonts</tt> become methods.
          <li><tt>font2pascal</tt> utility added, this is a very old Windows-only utility to convert fonts into our bitmap/outline font structures. It awaits eagerly to be reimplemented using freetype, contributions for this are most welcome!
        </ul>
      </li>
      <li>Memory optimization for models with many nodes (esp. using heavily prototypes), by about 10%, and another 10% possible if you define <tt>CONSERVE_TRIANGLE_MEMORY</tt> symbol.</li>
      <li>Improvements to <tt>CastleWindowProgress</tt>: <tt>Opacity</tt> property, configurable colors, using <tt>UIFontSmall</tt> when necessary, fixed to work with non-standard glViewport (like after typical TCastleViewport usage).</li>
      <li>"Home" key in Examine mode goes to initial viewpoint (configurable by VRML/X3D Viewpoint or SetInitialView in code), instead of always going to viewpoint with +Y up. This should be universally more useful and more natural than previous behavior.</li>
      <li>Fixes to viewports for ATI on Linux (one bug of Mesa, one bug of fglrx, see BuggyDepth32 and BuggySwapNonStandardViewport in CastleGLVersion).</li>
      <li>Examine camera has <tt>ExclusiveEvents</tt> = true, consistent with other TUIControl.</li>
      <li><tt>CastleLog</tt> output stream is configurable.</li>
    </ul>
  </li>

  <li><p><b>The caustics demo</b> (you seen a movie of this <a href="http://castle-engine.sourceforge.net/news.php?id=2012-07-10">in earlier news</a>) is now committed to our <a href="http://castle-engine.sourceforge.net/demo_models.php">demo models</a>. Many thanks to Victor Amat! I hope to use these ideas to make such water available out-of-the-box for all games using our engine (right now you will have to adjust shaders for your specific case).</p></li>
</ol>')
);
