<?php

/*

Simple fractals demo using our engine utilities (TCastleWindow and such): https://github.com/michaliskambi/fractals-demo-cge . GPL >= 2, by Michalis. I setup this on github mostly to play with github :)

Room Arranger with SSAO demo scene:
images/original_size/room_arranger_*_ssao.png

For final release news, add these images as triangulation demo:
images/original_size/polygon_0.png
images/original_size/polygon_1.png

*/

array_push($news,
    array('title' => 'Development: 3D mouse, drag to walk, SSAO, screen effects with anti-aliasing, triangulation, X3D 3.3, soft shadows demo, VisibilitySensor, 3D world improvements for games',
//          'short_title' =>
          'year' => 2012,
          'month' => 5,
          'day' => 12,
          'short_description' => '',
          'guid' => '2012-05-11-new-features',
          'description' =>
castle_thumbs(array(
  array('filename' => 'roomarranger_manor.png', 'titlealt' => 'Manor designed using RoomArranger, shown by view3dscene'),
  array('filename' => 'room_arranger_1.png', 'titlealt' => 'House floor designed using RoomArranger, shown by view3dscene'),
  array('filename' => 'room_arranger_viewer.png', 'titlealt' => 'First stages of RoomArranger viewer using our engine'),
  array('filename' => 'room_arranger_viewer_2.png', 'titlealt' => 'Final stages of RoomArranger viewer using our engine, with more controls and SSAO'),

  array('filename' => 'model_3d_viewer.png', 'titlealt' => 'New GUI of examples/lazarus/model_3d_viewer, with screenshot and navigation types buttons, and engine logo'),
  array('filename' => 'avatar_climb_stairs.png', 'titlealt' => 'Demo of avatarSize[2] (configure the tallest object that you can climb, e.g. which stairs you can climb on)'),

  array('filename' => 'ssao_no_aa.png', 'titlealt' => 'SSAO (Screen-Space Ambient Occlusion) without Anti-Aliasing'),
  array('filename' => 'ssao_aa.png', 'titlealt' => 'SSAO (Screen-Space Ambient Occlusion) with Anti-Aliasing'),

  array('filename' => 'fountain_0_no_ssao.png', 'titlealt' => 'Fountain model; no SSAO'),
  array('filename' => 'fountain_1_ssao_near-far-hardcoded.png', 'titlealt' => 'Fountain model; SSAO'),

  array('filename' => 'fountain_textures_0_no_ssao.png', 'titlealt' => 'Fountain model with textures; no SSAO'),
  array('filename' => 'fountain_textures_1_ssao.png', 'titlealt' => 'Fountain model with textures; SSAO'),

  array('filename' => 'barna29_water_0.png', 'titlealt' => 'German Pavillion in the Universal Expo of Barcelona of 1929 (by Victor Amat)'),
  array('filename' => 'barna29_water_1.png', 'titlealt' => 'German Pavillion in the Universal Expo of Barcelona of 1929 (by Victor Amat)'),
  array('filename' => 'soft_shadow_poisson_sampling.png', 'titlealt' => 'Soft shadows, adapted from nVidia SDK 9, but using a PoissonDisk sampler (by Victor Amat)'),

  array('filename' => 'visibility_sensor_test.png', 'titlealt' => 'VisibilitySensor demo'),

)) .
'<p>Hi everyone! We have a <i>lot</i> of news about new features and improvements in our <a href="http://castle-engine.sourceforge.net/engine.php">engine</a> and <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a>. No new release yet, we just announce what is available in SVN (and can be tested by <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/">downloading view3dscene from snapshots</a>). I really have to start making these news posts more often, otherwise they get ridiculously long, well.. as long as this one :)</p>

<p>Oh, and before we start: if you like seeing the engine grow so much, please consider <a href="' . CURRENT_URL . 'donate.php">donating</a>. I really appreciate it, especially as I plan to spend next year without a regular job, focusing mostly on the development of "Castle 2", a new great open-source game using our engine.</p>

<p>Features and improvements implemented in the last months:</p>

<ol>
  <li><p>First of all, great news &mdash; <b>we have new developer working on our engine: Jan Adamec</b>. He\'s also the author of <a href="http://www.roomarranger.com/">Room Arranger</a>, a shareware program to design your room, apartment, house and more. The next version will use our engine for the internal viewer of generated VRML models :) Here\' a movie of our engine used to render a house model from RoomArranger:</p>

    <iframe width="420" height="315" src="http://www.youtube.com/embed/rsuF9f3tio8" frameborder="0" allowfullscreen></iframe>
  </li>

  <li><p>Thanks to Jan Adamec, we have a lot of new features and improvements to navigation in Walk/Fly modes:</p>

    <ul>
      <li><p>We <b>support <a href="http://www.3dconnexion.com/">3D mouse devices</a></b> in all our tools (like <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a> and all engine examples, games etc.). Only under Windows for now &mdash; contributions for other OSes are welcome, of course.</p>

        <p>The video below shows how 3D mouse works in various navigation modes:</p>

        <iframe width="420" height="315" src="http://www.youtube.com/embed/lsUztfdike8" frameborder="0" allowfullscreen></iframe>
      </li>

      <li><p><b>Navigation by mouse dragging</b> is now possible in Walk/Fly modes (similar to other VRML/X3D viewers). Drag with left mouse button to move forward, backward and rotate. Drag with right mouse button to move sideways (strafe) and fly up/down.</p></li>

      <li><p>Holding <b>Shift during movement allows to run</b> (speed increases * 2).</p>

      <li><p><b>Mouse wheel (look up / down) works for cameras inside Lazarus control</b> (just like in TCastleWindow).</p></li>
    </ul>

    <p>This nice screen summarizes most new controls used in Walk/Fly modes:
    <img src="' . CURRENT_URL . 'images/original_size/navigation_controls.png" alt="Walk/Fly navigation controls" />
  </li>

  <li><p>Also thanks to Jan Adamec, we have <b>Screen Space Ambient Occlusion</b> implemented inside our engine. Available as comfortable <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a> menu item "View -&gt; Screen Effects -&gt; Screen Space Ambient Occlusion" (for developers: <tt>TCastleAbstractViewport.ScreenSpaceAmbientOcclusion</tt>.) Works on arbitrary models out-of-the-box :) Uses <a href="http://castle-engine.sourceforge.net/x3d_extensions_screen_effects.php">our screen effects framework</a>.</p></li>

  <li><p><b>Screen effects cooperate now wit multi-sampling (anti-aliasing)</b>. Previously, any screen effect (like SSAO, or <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a> effects, or effects in VRML/X3D files like <tt>demo_models/screen_effects/</tt> from <a href="http://castle-engine.sourceforge.net/demo_models.php">demo models</a>) was disabling anti-aliasing.</p>

    <p>Now we can use <a href="http://www.opengl.org/registry/specs/ARB/texture_multisample.txt">OpenGL ARB_texture_multisample</a> to have multi-sample textures used with screen effects. The GLSL code of every screen effect is linked with some helper <tt>screen_xxx</tt> functions. If you use them, your effects will work both with and without multi-sampling. The new functions are documented in <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/docs/x3d_extensions_screen_effects.html">future Screen Effects docs</a> (this will replace <a href="http://castle-engine.sourceforge.net/x3d_extensions_screen_effects.php">the current Screen Effects docs</a> at the next release). All existing engine effects, like SSAO and other view3dscene effects in "View-&gt;Screen Effects", are already ported to use scene_xxx functions and cooperate with anti-aliasing.</p></li>

  <li><p><b>Triangulator fixes for complicated concave polygons</b>. Various fixes to account for duplicated vertexes and (more general) collinear triangles inside polygon. Previously, they could cause problems, with normal vectors of polygon and ears determined incorrectly, and allowing invalid non-empty ear triangles. The video below visualizes how our simple triangulation algorithm (by ear clipping) works:

    <p><iframe width="420" height="315" src="http://www.youtube.com/embed/RMTXqTu4tKc" frameborder="0" allowfullscreen></iframe>

  <li><p><b>X3D 3.3</b> handling. See <a href="http://www.web3d.org/files/specifications/19775-1/V3.3/index.html">X3D 3.3 specification</a> for details.

    <ul>
      <li><p><tt>UNIT</tt> statement handled (in both classic and XML encoding), see <tt>demo_models/x3d/units*</tt>. Angle and length conversion is actually done.

        <p>For example, you can now express angles in degrees by simple declaration at the beginning of X3D file. This affects interpretation of these fields:

        <ul>
          <li>all SFRotation, MFRotation
          <li>all creaseAngle
          <li>TextureTransform.rotation
          <li>Background.skyAngle,groundAngle
          <li>Arc2D.startAngle,endAngle
          <li>ArcClose2D.startAngle,endAngle
        </ul>

        <p>Length conversion is also possible. This is less useful IMHO &mdash; you can as well just wrap your model in a Transform with some scale. Actually, that\'s exactly our implementation of "UNIT length" for now &mdash; we simply add appropriate scale (calculated looking at length unit of inlined model (inner), and looking at length unit of inlining model (outer)).
      </li>

      <li><p><tt>MetadataBoolean</tt> node added.

      <li><p><tt>GravityPhysicsModelNode</tt> renamed to <tt>ForcePhysicsModelNode</tt> (spec was mistakenly confusing these two names). (Only parsing.)

      <li><p>Added <tt>SliderJoint.sliderForce</tt> (Only parsing.)

      <li><p><tt>DISEntityTypeMapping</tt> has X3DUrlObject as ancestor (Only parsing.)

      <li><p>The only relevant thing still missing from full X3D 3.3 implementation is the new, very exciting for me, <a href="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/components/volume.html">Volume rendering component</a>. I would like to tackle it at some point, as it may be quite fun, impressive and useful. (But no deadline yet, so if you\'re really interested in seeing it implemented &mdash; contact me, patches are welcome :)
    </ul>
  </li>

  <li><p>Victor Amat send us some very nice 3D model of <b><a href="http://en.wikipedia.org/wiki/Barcelona_Pavilion">German Pavillion in the Universal Expo of Barcelona of 1929</a></b>, see the screenshots on the right, with very nice water and reflections.

    <p>And, also from Victor Amat, a nice <b>soft shadows demo based on shadow maps</b>. It\'s an adaptation from nVidia SDK 9, but using a PoissonDisk sampler instead of a jittered grid pattern. You can see a screenshot from it on the side. The source X3D file is inside <a href="http://castle-engine.sourceforge.net/demo_models.php">demo models</a> SVN, see subdirectory <tt>demo_models/shadow_maps/soft_shadow_poisson_sampling/</tt>. Thousand thanks!

  <li><p>The X3D feature to specify <b>height of the tallest object you can climb</b> inside <tt>NavigationInfo.avatarSize[2]</tt> field is now correctly supported. The demo model is inside <a href="http://castle-engine.sourceforge.net/demo_models.php">demo models</a> (open <tt>demo_models/navigation/avatar_climb_stairs.x3dv</tt>; only in SVN for now, until next release).</p></li>

  <li><p><b><tt>VisibilitySensor</tt> node</b> is implemented (looking only at frustum for now). See the testcase sensors_environmental/visibility_sensor.x3dv in <a href="http://castle-engine.sourceforge.net/demo_models.php">demo models</a> SVN.

  <li><p>The <b>speed of Examine rotation by keys is now capped at a maximum constant</b>, to prevent accidentally making wildly fast (and, as such, useless and confusing) rotations.</p></li>

  <li><p><a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a> gets the <b>"Screenshot" button on the toolbar</b> (as it\'s an often used feature).

    <p><b>Making a screenshot in view3dscene doesn\'t capture GUI controls</b> (like toolbar buttons). Previously, it was inconsistent, e.g. "movie screenshot" was not capturing toolbar but "single image screenshot" was capturing them (unless it was initiated from command-line...). The general rule now is: screenshot avoids the buttons, visualizations (bounding box), and such. You can always capture a screenshot using external application, with window frames and all features you want. view3dscene screenshot\'s purpose is to capture <i>your scene</i> &mdash; not any GUI cruft.

  <li><p><b><a href="http://castle-engine.sourceforge.net/blender.php">Our Blender X3D exporter</a> updated to be compatible to Blender 2.62</b> (and as close as possible to distributed original Blender exporter).

  <li><p><b>Memory and speed optimizations</b> for scenes with many shapes (iteration by "is nested" callbacks instead of temporary lists, better opaque/transparent detection and more). Also, Lazarus package settings are now more suitable for production use (no assertions, no range/overflow checks, optimization level = 2).</p>

    <p>This also causes an upgrade of our requirements for FPC version: we require now FPC &gt;= 2.6.0. We need the <a href="http://wiki.freepascal.org/FPC_New_Features_2.6.0#Support_for_nested_procedure_variables">"is nested"</a> feature, which I like a lot &mdash; it allows to utilize callbacks in more places, making things like traversing various trees more optimized and at the same time comfortable.</p></li>

  <li><p><i>Developers:</i> A lot of T3D class improvements to <b>unify collision detection for all 3D objects</b>. We now make all collision checks (every 3D object vs all others) go through exactly the same methods and look at the same properties. Of course you can customize it (e.g. player may not want to collide with items, to allow picking up items by stepping on them), but it\'s all uniform in the base T3D class. Previously, various collisions in <a href="http://castle-engine.sourceforge.net/castle.php">"The Castle"</a> (player vs level/creatures/items, creature vs level/other creatures/player etc.) had specialized hacky code.</p>

    <p>Also <b>more useful base 3D objects, transformed in more ways, aware of the world around them</b>. The idea is to move "Castle 1" creatures AI to the engine, to make it trivial to use creature AI in your own games. I want to allow both using existing AI (like our walk-attack state AI), and easy designing of your own AI for specific game needs. Details:</p>

    <ul>
      <li>New <b>T3DMoving, T3DLinearMoving classes, and T3D.Pushable property</b>: perfect for implementing elevators, moving platforms, and anything else than can move and push other players/creatures/items.</li>
      <li>New <b>T3DOrient</b>, to orient any 3D models to given position/direction/up. Good for things that have a sense of up and direction, like creatures and player avatar.
      <li>Since player descends from T3DOrient, it\'s now trivial to <b>position 3D items relatively to player</b>. Both "Castle 1" weapons are now always displayed in 3D (when attacking or not), this simplifies a lot previous code.
      <li>New <b>T3DAlive</b> class, for keeping track of things that have life/maxlife and can be hurt (with a possible knockback effect).
      <li>Methods like <b>MyMove, MyHeight and MyMoveAllowed</b>, to easily check your own collision vs the rest of the world. The idea is that you can trivially derive new classes from T3DTransform or T3DOrient (T3DAlive any many others descend from T3DOrient too), and use MyHeight and MyMoveAllowed to implement your own artificial intelligence. It\'s very easy now.
      <li><b>TCastleSceneManager.Player</b> property, to guide camera (for 1st perspective view) and to be a target for hostile creatures (if you use default AI in CastleCreatures).
      <li>Both walk-attack and missile creatures get a <b>uniform FallingDownSpeed</b> (units per second) property.
      <li>Many other unifications and simplifications to creature, players, items handling.
    </ul>
  </li>

  <li><p><i>Developers:</i> a lot of <b>improvements to game data handling</b>, to make developing new games (like "Castle 2") more comfortable.</p>

    <ul>
      <li>XML sounds/index.xml file improvements: now <b>you can add new sounds to the game (castle, and future castle2) merely by adding them to necessary XML files</b>. No need to reference the sound from ObjectPascal code.</li>
      <li><a href="http://castle-engine.sourceforge.net/kanim_format.php">KAnim usage in "The Castle"</a> improved: <b>all castle animations are now separate kanim files</b>, and can be browsed e.g. by <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a>. "Castle 2" will not use kanim at all.</li>
      <li><b>T3DResource class</b>, which can be loaded with reference-counting. This is a much generalized and reworked previous castle TObjectKind class.
      <li><b>XML files for each creature, item and level and now separate</b>. The idea is to allow you to add things such as a new creature, new item, and new level to the game data without the need to recompile or to edit any central "index" file. You just add some subdirectories with index.xml files inside, and the game automatically picks them up. See <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/castle/data/README_about_index_xml_files.txt">documentation of index.xml files</a> in castle data.
    </ul>
  </li>

  <li><p><i>Developers:</i> <b>picking improvements with T3D hierarchy</b>: unified <a href="http://castle-engine.sourceforge.net/castle.php">"The Castle"</a> style of picking, common to FPS games (pressing the "e" button when looking at item, stateless interaction with 3D items) with <a href="http://castle-engine.sourceforge.net/x3d_implementation_pointingdevicesensor.php">VRML/X3D pointer sensors</a> (operating with TouchSensors and drag sensors, that is stateful, tracks isOver/isActive and generally more powerful). Now it all goes through <tt>T3D.PointingDeviceActivate/Move</tt>, that can be overridden at each T3D.</p>

    <p>The user-visible benefit from this is that you can now pick creatures to see their name and health status. (Press "e" (interact key) to see info about creature or item at the center of the screen.) The real benefit is that new games can use picking easily, for more purposes.</p>
  </li>

  <li><p><i>Developers:</i> <b>camera classes improvements</b>: their input shortcuts are now published (making it possible to customize them from Lazarus object inspector), they do not descend from TUIControl (as you should only use cameras on <tt>SceneManager.Camera</tt>), TCastleSceneManager.DefaultVisibilityLimit, better <tt>Camera.Radius</tt> treatment.</p></li>

  <li><p><i>Developers:</i>Various improvements to example <tt>examples/lazarus/model_3d_viewer/</tt>, based on Jan Adamec patches: <b>buttons to change navigation mode, to make a screenshot, and nice engine logo</b>.

  <li><p>Website stuff:

    <ol>
      <li><p>As you may have seen at our header, our small website joined the protests against the bad legislations being pushed lately &mdash; pretending to be about anti-piracy, in fact being about censorship and anti-privacy. <a href="https://www.eff.org/deeplinks/2012/01/how-pipa-and-sopa-violate-white-house-principles-supporting-free-speech">Stop PIPA and SOPA</a> in USA, <a href="http://panoptykon.org/">Stop ACTA (in Polish)</a> (<a href="http://www.stopacta.info/">list of ACTA issues in English here</a>.)</p></li>
      <li><p><i>Fundry.com</i> (crowdfunding for software) has shut down in March/April. It was one of the ways to <a href="' . CURRENT_URL . 'donate.php">donate to our engine</a>, where you could donate to a development of a particular feature (suggested by you or someone else). It\'s sad that the site bankrupted, as I liked the idea very much (even if my particular project didn\'t yet earn anything this way). Fortunately, I found quite a few alternatives for crowdfunding specifically for FOSS projects:</p>
        <ol>
          <li><a href="http://www.fossfactory.org/">fossfactory.org</a>
          <li><a href="http://gun.io/">gun.io</a>
          <li><a href="https://elveos.org/en/">elveos.org</a> (it seems the guys are looking for someone to take over the website, or to close it down... So it\'s probably not a good idea to jump on it now.)
        </ol>
        <p>If you have any experience with these sites and would like to suggest some of them (or others), please share on <a href="http://sourceforge.net/p/castle-engine/discussion/">our forum</a> :)
      </li>
    </ol>
  </li>
</ol>
'),

    array('title' => 'Development: virtual trackball, URLs, BitCoin, T3DTransform, Win64, and more',
//          'short_title' =>
          'year' => 2012,
          'month' => 1,
          'day' => 13,
          'short_description' => '',
          'guid' => '2012-01-13-lots-of-improvements',
          'description' =>
castle_thumbs(array(
  array('filename' => 'new_scene_manager_demos.png', 'titlealt' => 'Screen from new scene_manager_demos example program'),
  array('filename' => 'anchor_www.png', 'titlealt' => 'You can use VRML/X3D Anchor node to refer to URL of a webpage'),
)) .
'<p>We have a really great start this year :) A lot of work has been done in the past 2 weeks since last release. Many improvements implemented in
<a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a> and
<a href="http://castle-engine.sourceforge.net/engine.php">the engine</a>.
In somewhat random order:</p>

<ol>
  <li><p>The <i>Examine</i> rotation with mouse was much improved:
    dragging the mouse
    near the border of the window will now cause rotation around the Z axis.
    (More precisely, we interpolate between rotation around Z axis and traditional XY rotation,
    by looking at how close the mouse position is to the middle of the window.)
    This makes rotations with mouse much more flexible (previously, you had to use keys
    for Z rotation), and also intuitive. This is called
    <i>"virtual trackball"</i>, mentioned on
    <a href="http://audilab.bme.mcgill.ca/~funnell/graphics/graphics3dview.html">Robert J. Funnell\'s "3-D viewers" page</a>.
    </p></li>

  <li><p>New view3dscene menu item <i>"Help -&gt; Visit view3dscene website"</i>,
    new castle menu item <i>"Visit our website"</i>,
    and <a href="http://castle-engine.sourceforge.net/x3d_implementation_networking.php">Anchor
    node can now open URLs in a browser (for documents that are not recognized as 3D models)</a>.

    <p>Engine contains a unit <tt>CastleOpenDocument</tt>, using code
    adapted from Lazarus LCL, to open URLs and documents on all platforms.

  <li><p>To our
    <a href="http://castle-engine.sourceforge.net/forum.php"><i>"Helping
    in the engine development"</i> section below the forum link</a>
    I added notes <i>"For Linux distros package maintainers"</i>.
    Please help creating a view3dscene package for popular Linux distributions!

  <li><p>We accept BitCoin for <a href="http://castle-engine.sourceforge.net/donate.php">donations</a>.
    If you wish to donate this way,
    simply send some bitcoins to this address: <tt>1FuJkCsKpHLL3E5nCQ4Y99bFprYPytd9HN</tt></p>

    <p>If you like view3dscene, please consider <a href="http://castle-engine.sourceforge.net/donate.php">donating
    using any of the listed options</a> :) Thanks!

  <li><p>For developers using our <a href="http://castle-engine.sourceforge.net/engine.php">engine</a>:
    <tt>T3DTransform</tt> class is available, to comfortably transform 3D scenes
    (translate, rotate around specified center, scale around specified center
    with specified scaleOrientation). The demo how to use it is inside
    <tt>castle_game_engine/examples/3d_rendering_processing/scene_manager_demos.lpr</tt> in SVN,
    and also in new "The Castle" sources.

  <li><p>Various work on simplifying <a href="http://castle-engine.sourceforge.net/castle.php">"The Castle"</a> sources, and merging
    the useful features into the engine core. For users, this mostly
    results in shadow volumes improvements on "The Castle":

    <ul>
      <li>Shadow volumes are now enabled by default</li>
      <li>Comfortable T3D.ReceiveShadowVolumes property</li>
      <li>Teleport (on gate level), and spiders sliding down (on cages level) are now done by <tt>T3DTransform</tt> descendants. This means that spiders sliding down cast shadows too.</li>
    </ul>

  <li><p>Notes about recently released FPC 2.6.0: Yes, it works perfectly
    fine with our engine 3.0.0.

    <p>The only small problem is
    <a href="http://bugs.freepascal.org/view.php?id=21000">FPC issue #21000</a>,
    which is actually a bug in my compilation scripts.
    This concerns you only if you compile final programs (not just the engine),
    and only if you use scripts (as opposed to Lazarus) to compile.
    In such case, make sure you use <tt>${CASTLE_FPC_OPTIONS:-}</tt>
    instead of <tt>"${CASTLE_FPC_OPTIONS:-}"</tt> (strip double quotes).

  <li><p>Cooperation between <tt>Anchor</tt> and other pointing-device
    sensors improved in constructions like

<pre>
Anchor {
  children [
    TouchSensor { ... }
    Shape { ... }
  ]
}
</pre>

    <p>Previously such <tt>Anchor</tt> was ignored (hidden by
    <tt>TouchSensor</tt>), now it\'s treated like sibling to <tt>TouchSensor</tt>.
    So it can be activated, it\'s description is shown etc.
    Compatible with at least InstantReality.

  <li><p>Engine works fully under 64-bit Windows (Win64, Windows on x86_64).
    <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/">Snapshots
    are build for win-x86_64</a> too. If there\'s interest (please report on
    <a href="http://castle-engine.sourceforge.net/forum.php">forum</a>)
    we may release binaries for this system on next view3dscene release.
    (I don\'t think it\'s terribly important, because our 32-bit Windows
    binaries actually work on Win64 flawlessly already.)

  <li><p>Tear-off menus are removed from the engine and view3dscene.
    This means a little functionality lost if you used view3dscene on Unix
    (Linux, Mac OS X). Sadly, tear-off menus are deprecated in GTK 3.1,
    and they are in fact already buggy in existing GTK 2 versions.
    See <A href="https://sourceforge.net/p/castle-engine/tickets/3/">ticket #3</a>
    for links and references about this.

  <li><p>Obsolete CastleWindow <i>GTK 1 backend removed</i>.
    Even it\'s compilation was broken since a long time.
    We use GTK 2 since many years.
</ol>

<p>Remember that you can grab <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/">view3dscene binary from snapshots</a> to try the new features immediately. For developers, you can download source code of engine and other programs from SVN.</p>
')
);
