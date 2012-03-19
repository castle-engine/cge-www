<?php

/* Next news:

Examine rotation speed by keys limited, to not make wild rotations.

Developers: picking improvements with T3D hierarchy: unified "The Castle" style of picking (pressing the "e" button when looking at item, stateless interaction with 3D items, like in many FPS games) with VRML/X3D pointer sensors (operating with TouchSensors and drag sensors, that is stateful, tracks isOver/isActive and generally is more powerful). Now it all goes through T3D.PointingDeviceActivate/Move, that can be overridden at each T3D.

"The Castle" code improvements (to move some stuff to engine core). User-visible:
- You can now pick creatures to see their name and health status.
  (Press "e" (interact key) to see info about creature or item under the screen middle.)

Developers: camera classes improvements:
- publish their input shortcuts, making it possible to customize them from Lazarus object inspector.
- do not descend from TUIControl, you should only use cameras on SceneManager.Camera
- TCastleSceneManager.DefaultVisibilityLimit, and looks more as Camera.CameraRadius.

X3D 3.3 handling:
- UNIT statement handled (in both classic and XML encoding), see demo_models/x3d/units*. Angle and length convertion is actually done.
  - For example, you can now express angles in degress by simple declaration at the beginning of X3D file. This affects interpretation of these fields:
    - all SFRotation, MFRotation
    - creaseAngle
    - TextureTransform.rotation
    - Background.skyAngle,groundAngle
    - Arc2D.startAngle,endAngle
    - ArcClose2D.startAngle,endAngle
  - Length conversion is also done. This is less useful IMHO --- you can equivalently wrap your model in a Transform with scale. Actually, that's exactly our implementation of "UNIT length" for now --- we simply add appropriate scale (calculated looking at length unit of inlined model (inner), and looking at length unit of inlining model (outer)).
- MetadataBoolean node added.
- GravityPhysicsModelNode renamed to ForcePhysicsModelNode (spec was mistakenly confusing these two names).
- Missing SliderJoint.sliderForce
- DISEntityTypeMapping has X3DUrlObject as ancestor
- The only revelant stuff missing from X3D 3.3 now is the new, very exciting for me, "Volume rendering component". I would like to tackle it at some point, as it may be quite fun, impressive and useful. (But no deadline yet, so if you're really interested in seeing it implemented --- contact me, patches are welcome :)

In other news, as you probably seen at our header, our small website joined the protests against bad laws &mdash; pretending to be anti-piracy, in fact being about censorship and anti-privacy.
<li><div style="float: right; padding: 1em; background: black; border: thick inset black; margin-right: 0.5em; margin-top: 0.5em;"><a style="color: white; font-weight: bold;" href="https://www.eff.org/deeplinks/2012/01/how-pipa-and-sopa-violate-white-house-principles-supporting-free-speech">Stop PIPA and SOPA</a> in USA.
<li><div style="float: right; padding: 1em; background: black; border: thick inset black; margin-right: 0.5em; margin-top: 0.5em;"><a style="color: white; font-weight: bold;" href="http://panoptykon.org/">Stop ACTA (description in Polish)</a>, http://www.stopacta.info/.

sounds/index.xml file improvements: now you can add new sounds to the game (castle, and future castle2) merely by adding them to necessary XML files. No need to reference the sound from ObjectPascal code.

kanim improvements: all castle animations are now normal kanim files, and can be browsed e.g. by view3dscene. castle2 will not use kanim.

Add to news: a lot of T3D improvements. This unifies approach to collision detection for all 3D objects, to make checking for all collisions (player vs level+creatures+items, creature vs level+other creatures+player and such) go through exactly the same methods and look at the same methods (except where clearly justified and documented exceptions). Sounds like a mothfull... Ok, the idea is to move "castle 1" creatures AI to the engine, to make it trivial to use creature AI in your own games :) I want to allow both using existing AI (like our walk-attack state AI), and easy designing of your own AI for specific game needs.
- New T3DMoving, T3DLinearMoving classes, along with T3D.Pushable: perfect for implementing elevators, moving platforms, and anything else than can move and push other players/creatures/items.
- New T3DOrient, to orient any 3D models to given position/direction/up.
- Since player descends from T3DOrient, it's now trivial to position 3D items relatively to player. Both castle weapons are now always displayed in 3D (both when attacking or not), simplifies a lot previous specialized code.
- New T3DAlive, for keeping track of things that have life/maxlife and that can be hurt (with a possible knockback effect).
- Methods like MyMove, MyHeight and MyMoveAllowed, to easily check your own collision vs the rest of the world. The idea is that you can trivially derive new classes from T3DTransform or T3DOrient (T3DAlive any many others descend from T3DOrient too), and use MyHeight and MyMoveAllowed to implement your own artificial intellgence. It's very easy now.
- TCastleSceneManager.Player property, to guide camera (for 1st perspective view) and to be a target for hostile creatures (if you use default AI in CastleCreatures).
- Both walk-attack and missile creatures get a uniform FallingDownSpeed (units per second) treatment now. Many other unifications and simplifications to creature, players, items handling.

- T3DResource class, which can be loaded with reference-counting. This is a much generalized and reworked previous castle TObjectKind class.
- XML files of creatures, items, and now more separate. The idea is to allow you to add things such as a new creature, new item, and new level to the game data without the need to recompile or to edit any central "index" file. You just add some subdirectories with index.xml files inside, and the game automatically picks them up. See <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/castle/data/README_about_index_xml_files.txt">documentation of index.xml files</a> in castle data.

- http://www.roomarranger.com/ may use a 3D viewer based on our engine to visualize rooms and houses designs in 3D :) Jan Adames has prepared a specialized version of 3D viewer (based on example <tt>examples/lazarus/model_3d_viewer/</tt> in our engine sources) for this purpose. You can see the screenshots of it on the right :)
img:room_arranger_viewer
img:room_arranger_1

- Updated <>our Blender X3D exporter</a> to be compatible to Blender 2.62 (and as close as possible to distributed original Blender exporter)

- "Screenshot" button in model_3d_viewer (add screenshot of it), "Screenshot" button in view3dscene toolbar (add screenshot of it).
  Screenshot in view3dscene doesn't capture GUI controls (like toolbar buttons) --- previously, it was inconsistent, e.g. "movie" screenshot was not capturing them but "single image" was capturing them. The general rule now is: screenshot avoids the buttons, visualizations (bounding box), and such. You can always capture a screenshot using external application, with window frames and all features you want. view3dscene screenshot's purpose is to capture *your scene* --- not any GUI cruft.

- Victor Amat send us a nice 3D model of (from title), see screenshots in barna29_water_0.png, barna29_water_1.png, with nice water. Thanks!

- VisibilitySensor is implemented (looking only at frustum for now). See the testcase sensors_environmental/visibility_sensor.x3dv. (add screen visibility_sensor_test.png)
*/

array_push($news,
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
