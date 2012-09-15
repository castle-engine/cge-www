<?php

/* Next news:
*/

array_push($news,
    array('title' => 'Development: engine 4.0.0 - almost there: levels, items, player improvements',
          'year' => 2012,
          'month' => 9,
          'day' => 15,
          'short_description' => '',
          'guid' => '2012-09-15',
          'description' =>
castle_thumbs(array(
  array('filename' => 'castle_items_tower_screen_2_ring.png', 'titlealt' => 'Testing dropping items on level. These items were dropped when standing still and looking around --- it\'s good that they arrange in a nice ring.'),
  array('filename' => 'castle_items_tower_screen_0', 'titlealt' => 'Testing many items dropped on level.'),
  array('filename' => 'alien_invasion_castle_screen_0', 'titlealt' => 'Testing behavior (AI) of a group of creatures.'),
  array('filename' => 'alien_invasion_castle_screen_1', 'titlealt' => 'Testing behavior (AI) of a group of creatures, with debug bounding volume visualization.'),
  array('filename' => 'alien_invasion_castle_screen_2', 'titlealt' => 'Getting shot with a fireball.'),
  array('filename' => 'alien_invasion_castle_screen_3', 'titlealt' => 'Blackout (well, redout) after getting shot with a fireball.'),
  array('filename' => 'castle_screen_items_gravity.png', 'titlealt' => 'Dropping items, testing gravity on items.'),
  array('filename' => 'castle_screen_debug_alt_target.png', 'titlealt' => 'The blue crosses show the &quot;random position&quot; that creatures use when they cannot seem to reach the target (player) directly.'),
)) .
'<p>Work on the new <a href="http://castle-engine.sourceforge.net/engine.php">Castle Game Engine</a> 4.0.0 version continues :) Many improvements for the programmers that want to develop their own games using our engine:</p>

<ol>
  <li><p>Many improvements to
    <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/castle_game_engine/doc/DRAFT.engine_tutorial.txt">engine tutorial</a>,
    <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/castle_game_engine/doc/DRAFT.engine_classes_diagram.txt">classes diagram</a> and
    <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/castle_game_engine/doc/README_about_index_xml_files.txt">level.xml and resource.xml files documentation</a>.
    It\'s still a lot of raw, unformatted text, but the content pretty much covers now everything I want, and it describes the latest engine workings in SVN.</p>

  <li><p>Previous <tt>index.xml</tt> are now named <tt>level.xml</tt> (for levels), or <tt>resource.xml</tt> (for creatures, items, and other heavy 3D stuff that may be shared by levels). This makes things cleaner, and LoadFromFiles calls easier (you can just let it search whole ProgramDataPath).</p></li>

  <li><p>Placeholder 3D objects have now consistent naming. "Placeholders" are 3D objects that have special meaning when you load your level through TGameSceneManager.LoadLevel &mdash; objects with some special names are removed from normal level geometry, and they indicate... well, various things. See <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/docs/reference/html/CastleLevels.TGameSceneManager.html#LoadLevel">TGameSceneManager.LoadLevel docs</a> (from <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/docs/reference/html/">engine SVN reference</a>) for full reference. Short:</p>
    <ul>
      <li>Resources (creatures and items initial positions) placeholders and are now named <tt>CasRes...</tt>.</li>
      <li>Movement limit box is named <tt>CasMoveLimit</tt> (previously "LevelBox"; see <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/docs/reference/html/CastleSceneManager.TCastleSceneManager.html#MoveLimit">TCastleceneManager.MoveLimit</a> for docs).</li>
      <li>Water volume is <tt>CasWater</tt> (see <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/docs/reference/html/CastleSceneManager.TCastleSceneManager.html#Water">TCastleSceneManager.Water</a> docs).</li>
      <li>Sectors and waypoints are <tt>CasSector...</tt>, <tt>CasWaypoint...</tt> (for creature AI; see <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/docs/reference/html/CastleSceneManager.TCastleSceneManager.html#CreateSectors">TCastleSceneManager.CreateSectors</a>).</li>
    </ul>
  </li>

  <li><p>Fix a lot of code to honour the "up" world vector to be +Y as well as +Z. Gravity is decided looking at Viewpoint gravity in VRML/X3D, and creature orientation is decided looking at <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/docs/reference/html/Base3D.T3DOrient.html#DefaultOrientation">T3DOrient.DefaultOrientation</a>. See also  <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/castle_game_engine/doc/DRAFT.engine_tutorial.txt">engine tutorial</a> section <i>Which way is up</i>. And see <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/docs/reference/html/Base3D.html#TOrientationType">TOrientationType</a> type values.</p></li>

  <li><p>CastleLevels improvements:</p>
    <ul>
      <li>LoadLevel interface much simpler.
      <li>Much more docs around TGameSceneManager and TLevelAvailable.
      <li>Ugly MenuBackground parameter removed, it\'s not needed at all.
      <li>Oh, and TCastleWindow.SceneManager and TCastleControl.SceneManager is now of TGameSceneManager class. So if you use TCastleWindow / TCastleControl, you can use the levels system designed in CastleLevels really trivially:
<pre class="sourcecode">
  LevelsAvailable.LoadFromFiles;
  Window.SceneManager.LoadLevel(\'my_level_name\');
</pre>
        <p>Congratulations, you just wrote a game :) All that remains is to prepare a game 3D data, and <tt>level.xml</tt> file with <tt>name="my_level_name"</tt>. You already have all the code you need :) See <tt>castle_game_engine/examples/3d_sound_game/</tt> for a larger working example using this.</p>
      </li>
    </ul>
  </li>

  <li><p>An easy way to debug AI information, to see how AI "thinks". Just set global boolean RenderDebug3D (formerly RenderDebugBoundingVolumes) to true, and you will see:
    <ul>
      <li><p><i>Yellow axis is the middle (eyes) position</i> for each creature and item. This determines the line of sight, and helps with other collision decisions when "legs" position would be uncomfortable.</p></li>
      <li><p><i>Red axis, for each creature, is the last seen target (usually enemy, which is usually player) position</i>. You often don\'t see it, because you\'re actually standing on it. But it can be seen when you dodge behind a corner, and you look at your previous position.</p></li>
      <li><p><i>Blue axis, for each creature, is the alternative walk target</i>. Used when creature cannot seem to reach it\'s normal target. Basically, when straight line and sectors/waypoints fail, the creature wanders randomly hoping to get on the right track (or at least to avoid player shooting them easy).</p>
        <p>Of course, this is the last resort. If your creatures seem to be blocked too often, you may want to divide your level into more sectors and use more waypoints to "guide" the creatures. Unless you want your creatures to be dumb, of course, which is quite sensible for many games.</p>
      </li>
    </ul>
  </li>

  <li><p>Sound: engine units and classes simpler, no AL prefix everywhere. Also fixes to AudioClip X3D node (it wasn\'t always releasing reference when it should, causing crashes in special situations).</p></li>

  <li><p>Limit amount of logging by default. Our InitializeLog (see CastleLog unit docs, try <tt>--debug-log</tt> options of various programs) was producing way too much information by default, and important things were difficult to spot. Now by default it\'s shorter, showing only seldom happening things or important warnings.</p></li>

  <li><p>Much cleanup in <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/docs/reference/html/CastleInputs.html">CastleInputs unit</a>. TInputShortcut and TInputConfiguration merged. Idea of "global" and "local" key shortcuts clearly defined and documented.</p></li>

  <li><p>TCastleSceneManager automatically handles now key combinations related to player inventory.</p></li>

  <li><p>Many renames in the new API. We want to get the new engine API as good as possible, before it\'s released and breaking compatibility will not be as easy.</p>
    <ul>
      <li>T3D.Pushable to T3D.CollidesWithMoving (it means that doors/elevators push or avoid crushing this object)
      <li>ItemOnLevel, PutOnLevel etc. to ItemOnWorld, PutOnWorld etc. (we try to consistently use the term "World" to refer to your 3D world, and reserve the word "level" for levels recognized by CastleLevels.TLevelAvailable)
      <li>TItem to TInventoryItem, and some Items properties renamed to Inventory. TItem was too generic, and in ObjectPascal "Items" has too generic meaning (many generic lists have "Items" properties referring to their contents; our "Inventory" is a now a list of TInventoryItem, which are things that can be used by player).
      <li>MoveAllowed, Height renamed to MoveCollision, HeightCollision, consistent with SegmentCollision, BoxCollision and such. These check collision of <i>other things with current 3D object</i> (current T3D instance).
      <li>Rename MyMoveAllowed, MyHeight to just MoveAllowed, Height etc. These check collision of point belonging to <i>current 3D object with everything else</i> (with the whole World, except current 3D object).
    </ul>
  </li>
</ol>'),

    array('title' => 'Development: new engine: levels, items, player, tutorial, class diagram, more',
          'year' => 2012,
          'month' => 8,
          'day' => 16,
          'short_description' => '',
          'guid' => '2012-08-16',
          'description' =>
castle_thumbs(array(
  array('html' => (!HTML_VALIDATION ? '<iframe width="420" height="315" src="http://www.youtube.com/embed/1mUU8prDi9k" frameborder="0" allowfullscreen></iframe>' : '')),
  array('html' => (!HTML_VALIDATION ? '<iframe width="420" height="315" src="http://www.youtube.com/embed/rsuF9f3tio8" frameborder="0" allowfullscreen></iframe>' : '')),
  array('html' => (!HTML_VALIDATION ? '<iframe width="420" height="315" src="http://www.youtube.com/embed/lsUztfdike8" frameborder="0" allowfullscreen></iframe>' : '')),
)) .
'<p>The work on the next <a href="http://castle-engine.sourceforge.net/engine.php">Castle Game Engine</a> version continues :) Looking at the size of the improvements, next version will deserve the new major number 4.0.0 (even though last version was 3.0.0). We have almost finished new engine features planned for the next version, and we have a lot of new documentation. <!-- (as the point of next engine version is to be better documented for developers). -->

<ol>
  <li><p><a href="http://svn.code.sf.net/p/castle-engine/code/trunk/castle_game_engine/doc/DRAFT.engine_classes_diagram.txt">Diagram (as a text file...) of the most important "Castle Game Engine" classes</a> is available.</p>

    <p>I may try to squeeze this into a real image (maybe just the important boxes, and add the text on a normal HTML page). If you have experience with some diagram-drawing tools (like <a href="https://live.gnome.org/Dia/">Dia</a> or other open-source tool) I would appreciate contributions here (<a href="http://castle-engine.sourceforge.net/forum.php">send them through forum or e-mail to Michalis</a>).</p>
  </li>

  <li><p><a href="http://svn.code.sf.net/p/castle-engine/code/trunk/castle_game_engine/doc/DRAFT.engine_tutorial.txt">"Castle Game Engine" draft tutorial is getting larger and larger (15 chapters now)</a>.</p></li>

  <li><p><a href="http://svn.code.sf.net/p/castle-engine/code/trunk/castle_game_engine/doc/README_about_index_xml_files.txt">Short documentation for index.xml files used by "Castle Game Engine" to define levels, creatures and items is also available</a>. Tutorial and classes diagram describe in details how these index.xml files work. People interested in creating MODs for your games will appreciate them, as they allow to add levels, creatures and items by simply adding subdirectories to game data.</p></li>
</ol>

<p>New engine units since last news:</p>

<ol>
  <li>New <tt>CastleLevel</tt> unit to find game levels (<tt>index.xml</tt> files), load them, scanning 3D level file for placeholders defining initial creatures/items, and integrating with player.</li>
  <li>New <tt>CastlePlayer</tt> unit to manage a central player object, with an inventory, footsteps sound, swimming and so on. This unit will probably shrink a little in the future, as most of it\'s functionality should also be available for creatures.</li>
  <li>New <tt>CastleItems</tt> unit to define items (things that you can use in a game, like a sword or a life potion or a key). Item is a <tt>TItem</tt> instance, that refers to a corresponding <tt>TItemKind</tt> instance, referenced by <tt>TItem.Kind</tt>. This is similar to how you define creatures (see <a href="http://castle-engine.sourceforge.net/news.php?id=2012-06-10">previous news post about <tt>CastleCreatures</tt> unit</a>), in that we have a separate class for an item and for item kind. A little complication here is that <tt>TItem</tt> is not a 3D object (it cannot be directly added to the level), instead you have to use <tt>TItem.PutOnLevel</tt> to get <tt>TItemOnLevel</tt> instance.</li>
  <li>New <tt>CastleInputs</tt> unit, centralized keymap handling, detecting key conflicts and such.</li>
</ol>

<!--
The engine draft tutorial, see DRAFT.engine_tutorial.txt, is also being extended. To finish next engine version, we still want to
<ul>
  <li>polish various things in the above units, and
  <li>finish the draft tutorial on DRAFT.engine_tutorial.txt, and
  <li>also make a nice diagram explaining most important engine classes. The text is ready on DRAFT.engine_classes_diagram.txt.
  <li>make a simple example fps_game. For now, you can look at examples/3d_sound_game/ and castle1 sources in SVN for examples how to use new units.
</ul>
-->

<p>Other changes, noticeable in <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a> or next <a href="http://castle-engine.sourceforge.net/castle.php">"The Castle"</a> data:</p>

<ol>
  <li>In <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a> <i>"Help -&gt; Selected Object Information"</i>, show Blender object/mesh name.</li>
  <li>Handle beamWidth --&gt; GL_SPOT_EXPONENT translation similar to Xj3D: OpenGL spot exponent is <i>0.5 / max(beamWidth, epsilon)</i> (unless <i>beamWidth &gt;= cutOffAngle</i>, then spot exponent is always zero). This allows to at least influence drop off rate with <tt>beamWidth</tt>, and gives precise conversion from VRML 1.0 <tt>dropOffRate</tt> -&gt; VRML 2/X3D <tt>beamWidth</tt>. See <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/vrml_engine_doc/chapter_opengl_rendering.xml">engine internals doc about OpenGL rendering (sorry, just DocBook source now)</a> for details.</li>
  <li><a href="http://castle-engine.sourceforge.net/castle_script.php">CastleScript</a> <tt>writeln()</tt> can be used in games to make notifications. Also CastleScript <tt>shortcut()</tt> is now available, to show user the value of some key shortcut. See <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/docs/castle_script.html">CastleScript SVN docs</a> for details. This allows <a href="http://castle-engine.sourceforge.net/castle.php">"The Castle"</a> levels (and future Castle2) levels to make notifications using more standard X3D mechanisms: they now use <tt>ProximitySensor</tt> + CastleScript to show hints, instead of our custom &lt;area&gt; invention. See e.g. <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/castle/data/levels/cages/cages_final.x3dv">cages level X3D source</a> to see how it looks like.</li>
  <li>Our "thunder" effect (blinking light and some sound), used previously in "cages" level of castle1 and lets_take_a_walk, is now remade as pure X3D prototype. This means it\'s simpler, more configurable, and doesn\'t use a single line of ObjectPascal code :) See it\'s implementation here: <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/castle/data/levels/cages/thunder.x3dv">thunder.x3dv</a>, examples how it\'s used are at the bottom of <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/castle_game_engine/examples/3d_sound_game/data/levels/base/base.x3dv">base.wrl</a> and <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/castle/data/levels/cages/cages_final.x3dv">cages_final.x3dv</a>.</li>
  <li><tt>examples/3d_sound_game/lets_take_a_walk</tt> uses <tt>CastleLevel</tt>, as a first demo (besides the castle1 game).</li>
  <li>New <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a> menu item <i>"Remove placeholder nodes from "Castle Game Engine" levels"</i>, a general way to remove some specially-named nodes (see <a href="http://castle-engine.sourceforge.net/castle-development.php">castle development doc</a> for reference of most of them, see sources for all). Removed previous castle-process-3d-model.lpr, it was uncomfortable and didn\'t really prove useful.</li>
  <li>Implement TCastleWindowBase.RedBits, GreenBits, BlueBits, in addition to making ColorBits cross-platform and sensible (reads/writes RGB properties). glinformation gets --red-bits etc. options.</li>
  <li>Comfortable TCastleWindowBase.AntiAliasing property, instead of previous GLAntiAliasing unit. You can now simply use TCastleWindowBase.AntiAliasing instead of TCastleWindowBase.MultiSampling for a little higher-level approach for MSAA.</li>
</ol>

<p>Oh, and since I just don\'t have any interesting screenshots for this news item (mostly code changes, interesting for developers...), the side of this news post contains just some best movies from the previous news in this year, possible thanks to Jan Adamec (RoomArranger, 3D mouse, SSAO) and Victor Amat (water with caustics). Thanks again!</p>
'),
    array('title' => 'Development: water with caustics, lights editor, force Phong shading, shadow volumes improvements, more',
          'year' => 2012,
          'month' => 7,
          'day' => 10,
          'short_description' => '',
          'guid' => '2012-07-10',
          'description' =>
castle_thumbs(array(
  array('filename' => 'lights_editor_fountain_0.png', 'titlealt' => 'Lights Editor in view3dscene - fountain, shadow volumes settings'),
  array('filename' => 'lights_editor_fountain_1.png', 'titlealt' => 'Lights Editor in view3dscene - fountain, shadow volumes settings 2'),
  array('filename' => 'lights_editor_rhan_shrine_0.png', 'titlealt' => 'Lights Editor in view3dscene - switching shadow maps settings'),
  array('filename' => 'lights_editor_rhan_shrine_1.png', 'titlealt' => 'Lights Editor in view3dscene - switching shadow maps settings 2'),
  array('filename' => 'lights_editor_light_street_lights_radius_0.png', 'titlealt' => 'Lights Editor in view3dscene - local lights'),
  array('filename' => 'lights_editor_light_street_lights_radius_1.png', 'titlealt' => 'Lights Editor in view3dscene - headlight'),
)) .
'<p>Hi! Here are the latest news from the development of our <a href="http://castle-engine.sourceforge.net/engine.php">engine</a> and <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a>:</p>

<ol>
  <li>
    <p>Let\'s start with something shiny :) Victor Amat send me a demo of <b>water with very nice caustics</b> using our shadows and shaders and other VRML/X3D features. Be sure to switch to HD and fullscreen, for best quality:</p>

    <iframe width="640" height="480" src="http://www.youtube.com/embed/1mUU8prDi9k" frameborder="0" allowfullscreen></iframe>

    <p>This model shows a German Pavillion in the Universal Expo of Barcelona of 1929. The video shows a quick walk around the model in <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a>, showing reflections and caustics visible above and under the water. We also show that all screen effects, like <i>Screen Space Ambient Occlusion</i>, work now with shadow volumes without problems :)</p>
  </li>

  <li>
    <p><b>Lights editor</b> is available in <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a>. Use the menu <i>"Edit -&gt; Lights Editor"</i> to bring it on. It allows to interactively adjust a lot of lights settings. The scene is not paused &mdash; you can still trigger animations, walk around and generally do everything while you tweak lights settings. Of course you can try it right now by using <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/">view3dscene from snapshots</a>.</p>

    <p>A lot of light parameters can be adjusted by comfortable sliders. My main motivation for implementing this was that light settings are usually hard to get perfect using an external modeller, like <a href="http://www.blender.org/">Blender</a>, where light parameters do not precisely match the VRML/X3D concepts.</p>

    <p>Basic shadows settings (by <a href="http://castle-engine.sourceforge.net/x3d_extensions_shadow_maps.php">shadow maps</a> or <a href="http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_shadows">shadow volumes</a>) are also configurable using this. In particular try out the <i>"Shadows (Easy: Shadow Maps)"</i> setting (not for PointLight), it just works on an arbitrary scene. I hope that this will also make our dynamic shadows algorithms more discoverable :)</p>

    <p>Note: turn on <a href="http://castle-engine.sourceforge.net/x3d_implementation_lighting.php#section_per_pixel_lighting">per-pixel lighting by using "Shaders-&gt;Enable For Everything" menu item</a> to see perfect lights results, this may be necessary to make local light effects really stand out. (See also the <tt>Shape.shading="PHONG"</tt> extension below to mark inside your X3D file that some shapes should use this look by default.)</p>

    <p>This is a tool for tweaking lights, not for designing them from the start. There\'s no option to add a new light, you can only change existing light sources. There\'s also no option to delete a light, although you can always disable light by setting "On" value to "No".</p>

    <p>This is a <i>much</i> cleaned up and improved lights editor that was previously available in our <a href="http://castle-engine.sourceforge.net/castle.php">"The Castle"</a> debug menu. It allowed us to make some nice light effects in "The Castle" levels, now it\'s available for arbitrary 3D models :) Of course you can save the edited VRML/X3D file back on disk by <i>"File -&gt; Save..."</i> menu items from view3dscene.</p>
  </li>

  <li><p>We have implemented a simple
    <a href="http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_shading">Shape.shading extension</a>,
    allows you to <b>force Phong shading for a particular shape</b>.</p>
  </li>

  <li><p><b>Problems with cube maps on Intel GPU on Windows workarounded</b>:
    for Intel HD 4000, generating cube maps is broken, so don\'t do it.
    For other models, use glCopyTexSubImage instead of FBO.
    Also, glGenerateMipmap is either crashing or broken (makes black/random
    contents) on this GPU, so don\'t use mipmaps for cube maps.</p></li>

  <li><p>view3dscene menu item <i>"Display -&gt; Show Controls on Screenshots"</i>.</p></li>

  <li><p><b>Shadow volumes cooperate now with screen effects that read depth</b>
    (like SSAO).</p>

    <p>Previous implementation had problems because on GPUs with packed depth+stencil
    (pretty much all existing GPUs? I didn\'t see a GPU without it...)
    you cannot just create depth texture and separate stencil buffer for FBO,
    instead you have to create special texture that contains both depth+stencil.
    And that\'s what we do now :)</p>
  </li>

  <li><p>For <b>shadow volumes to work, your 3D models must now be perfect 2-manifold</b>.
    No border edges (view3dscene <i>"Help -&gt; Manifold Edges Information"</i>
    must say <i>"0 border edges"</i>, and <i>"View -&gt; Fill Mode -&gt; Silhouette And Border Edges"</i>
    must show only yellow edges &mdash; no blue edges).
    Previously, allowing models with some border edges to be used as shadow volumes
    casters was an endless source of artifacts.
    And to avoid these artifacts,
    some parts of the castle1 were using CastShadowVolumes = false by default,
    effectively disallowing engine to automatically detect should shadow volumes
    be used (are border edges 0).</p>

    <p>I think it\'s cleaner to just require 2-manifold models (as they were
    in practice required already for bug-free shadow volumes,
    it just wasn\'t enforced by the engine),
    and do something safe (instead of weird rendering artifacts) for other models.</p>

    <p>Yes, this may force you to fix
    your 3D models. If you really really miss old
    non-perfect-manifold rendering, please report, we can make an option re-enable
    it. But we would really like to encourage people to instead fix models
    to be perfect manifolds. Remember that you can <a href="http://castle-engine.sourceforge.net/x3d_extensions_shadow_maps.php#section_shadow_caster">mark some shapes as not shadow casters</a>
    to avoid considering them at all in the manifold/border edges
    set. Only the shapes that sum into a 2-manifold should have <tt>shadowCaster = true</tt>.</p>
  </li>

  <li><p><b>Engine improvements</b> for programmers:</p>
    <ul>
      <li>SoundEngine is now always TXmlSoundEngine, and some engine components
        (like OnScreenMenu) use sounds out of the box.
        You only have to initialize SoundEngine.SoundsXmlFileName at some point,
        and create appropriate XML file and sound files.</li>
      <li>New approach to saving user preferences, see new CastleConfig unit,
        used throughout the engine components.</li>
      <li>New CastleTextureProperties unit, to assign footsteps
        sound and damage to textures in games.</li>
    </ul>
  </li>
</ol>
'),

    array('title' => 'Development: creature AI, start of architecture mode, fractals demo, more',
          'year' => 2012,
          'month' => 6,
          'day' => 10,
          'short_description' => '',
          'guid' => '2012-06-10',
          'description' =>
castle_thumbs(array(
  array('filename' => 'room_arranger_2_ssao.png', 'titlealt' => 'Room Arranger with SSAO demo, shown by view3dscene'),
  array('filename' => 'room_arranger_3_ssao.png', 'titlealt' => 'Room Arranger with SSAO demo, shown by view3dscene'),
  array('filename' => 'room_arranger_4_ssao.png', 'titlealt' => 'Room Arranger with SSAO demo, shown by view3dscene'),

  array('filename' => 'polygon_0.png', 'titlealt' => 'Difficult polygon triangulation'),
  array('filename' => 'polygon_1.png', 'titlealt' => 'Difficult polygon triangulation'),

  array('filename' => 'fractal_1.png', 'titlealt' => 'Fractals 1'),
  array('filename' => 'fractal_2.png', 'titlealt' => 'Fractals 2'),
  array('filename' => 'fractal_3.png', 'titlealt' => 'Fractals 3'),
)) .
'<p>Hi!</p>

<p>First of all, we had a plan to release new shiny engine version at the end of May &mdash; sorry everyone, as you can see, we failed to make it on time. The work is progressing nicely (more details below), but there\'s just never enough time to finish it. So, sorry, everything has to be delayed a little further. (And if you would like to help: <a href="http://castle-engine.sourceforge.net/forum.php">we have a list of things you can do to help (not all of them require experience with programming)</a>, and <a href="http://castle-engine.sourceforge.net/donate.php">donating is very appreciated (as it allows me to spend more time on the engine development, instead of on various freelance jobs)</a>. Thanks in advance!)</p>

<p>New features done lately:</p>

<ol>
  <li><p>Engine contains now the promised <tt>CastleCreatures</tt> unit (see inside <tt>src/game/</tt> subdirectory in SVN), that allows you to <b>easily create creatures with artificial intelligence</b>. (For details see <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/docs/reference/html/CastleCreatures.html">initial docs</a> and <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/castle_game_engine/src/game/castlecreatures.pas">castlecreatures.pas source</a>.) There are various ways to use it:</p>

    <ol>
      <li><p>For starters, you can just use the walk-attack-state intelligence, by the <tt>TWalkAttackCreatureKind</tt> class. Such creature tracks the player (remembers last seen player 3D position, walks/flies to it, possibly through sectors/waypoints &mdash; so it can pass through narrow doors in a labyrinth or walk over a narrow bridge), attacks the player from the right distance (this can be either a melee attack, or shooting a missile &mdash; which adds a missile to the 3D world), eventually runs from the player (when he\'s too close and/or our health is low). For now, the player is always the main enemy, and there\'s no "creature vs creature" fighting, although it can be added easily in the future.</p>

        <p>For basic usage, there\'s no need to even derive new classes. All you need to do is to create a directory holding creature 3D data and <tt>index.xml</tt> describing it (for example, see various subdirectories of <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/castle/data/creatures/">castle/data/creatures/ in SVN</a>).</p>

        <p>There are a lot of settings to achieve particular behavior, e.g. cowardly/brave, offensive/defensive, melee/ranged, etc.</p>

        <p>There is also a "missile" intelligence (<tt>TMissileCreatureKind</tt>), that causes the creature to blindly go in given direction (possibly with gravity and/or homing (close-on-target) features). On impact, missile may explode, hurting player and/or other creatures. Any kind of missile, <!-- (that doesn\'t immediately hurt the enemy), --> like arrow or lighting bolt, is represented as such "missile creature", that flies independently of the shooter.</p>
      </li>

      <li><p>You can also derive simple descendants of above classes, to customize their behavior. For example, <a href="http://castle-engine.sourceforge.net/castle.php">"The Castle"</a> customizes walk-attack creature to give both ranged and melee attacks to the <i>Spider Queen</i>, to make <i>werewolves</i> howl from time to time, and such. See <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/castle/source/gamecreatures.pas">castle1 GameCreatures.pas unit sources</a>. We plan to add a simple FPS game to the engine examples, to illustrate this nicer.</p></li>

      <li><p>Finally, nothing stops you from taking basic <tt>TCreatureKind</tt>, or even the more basic <tt>T3DAlive</tt> class, and add an intelligence there. Methods <tt>MyMove</tt>, <tt>MyMoveAllowed</tt>, <tt>MyHeight</tt>, <tt>MyLineOfSight</tt> allow you to add any kind of movement/intelligence to any 3D object. See <tt>Base3D</tt> unit and classes there.</p></li>
    </ol>

    <p>We also have <tt>CastleResources</tt> unit (for reference-counted, heavy 3D resources) and other new units in the <tt>src/game/</tt> subdirectory.</p>

    <p>This achieves the major goal for the next engine version :) But there\'s still more work &mdash; we want to have there also (pickable) items, advanced player management, and advanced level loading. This is all already implemented in castle1 source, but we still need to generalize and clean up the code.</p>
  </li>

  <li><p>Jan Adamec started implementation of a new camera navigation method, called <i>Architecture</i> now. The idea is to create a more constrained Examine mode, suitable for viewing models with a ground/floor, where you don\'t want to accidentally flip the model upside-down.</p>

    <p>The new navigation mode is already available on <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a> header (take <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/">view3dscene binary from snapshots</a>), it\'s also available for VRML/X3D models by <tt>NavigationInfo.type="ARCHITECTURE"</tt>. Remember it\'s a work in progress for now, the behavior will change.</p></li>

  <li><p>Some more screenshots of <a href="http://www.roomarranger.com/">Room Arranger scenes</a> with <i>Screen Space Ambient Occlusion</i>, and more screenshots of triangulating of non-trivial polygons, are visible on the side of this post. See <a href="http://castle-engine.sourceforge.net/news.php?item=2012-05-11-new-features">previous news</a> for details about these features.</p></li>

  <li><p>Fractals demo using our engine (<tt>TCastleWindow</tt> and such) is available on github: <a href="https://github.com/michaliskambi/fractals-demo-cge">github.com/michaliskambi/fractals-demo-cge</a>. The code is GPL >= 2, by Michalis. Partially, I set this up to play with github :) But it also shows one way to distribute a code of a game/utility using our engine. You can look at README.txt, Lazarus lpi, and compilation scripts there as an example.</p></li>

  <li><p><a href="http://castle-engine.sourceforge.net/lets_take_a_walk.php">lets_take_a_walk, demo of 3D sounds inside our engine</a>, was refreshed (to use T3D classes) and added as another example to our engine sources (under <tt>castle_game_engine/examples/3d_sound_game/</tt>).</p></li>

  <li><p>Added comments about sponsoring, and links to <a href="http://www.fossfactory.org/">FOSS Factory</a> and <a href="http://gun.io/">Gun.io</a> to our <a href="http://castle-engine.sourceforge.net/donate.php">donations</a> page.</p></li>

  <li><p><a href="http://www.lazarus.freepascal.org/">Lazarus</a> TOpenGLControl component got quite a few improvements in Lazarus SVN, thanks to the work of Michalis, Jan Adamec and Andrey Zubarev: <a href="http://bugs.freepascal.org/view.php?id=22026">MultiSampling</a>, <a href="http://bugs.freepascal.org/view.php?id=22170">AlphaBits, DepthBits, StencilBits</a>, <a href="http://bugs.freepascal.org/view.php?id=18046">AUX buffers</a>. This is an ancestor of our <tt>TCastleControl</tt> component, so all these improvements are directly useful also inside our engine.</p></li>

  <li><p>Other small fixes/updates:</p>

    <ul>
      <li>Workaround for NVidia GLSL bugs on "const in gl_XxxParameters".</li>
      <li>RenderedTexture.viewpoint fixed to send <a href="http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_viewpoint_camera_matrix">camera*Matrix events</a>.</li>
      <li>Updated Windows DLLs to latest versions from <a href="http://gnuwin32.sourceforge.net/">gnuwin32.sourceforge.net</a>: <a href="http://gnuwin32.sourceforge.net/packages/zlib.htm">zlib upgraded to 1.2.3 (last change 20 July 2005)</a>, <a href="http://gnuwin32.sourceforge.net/packages/libpng.htm">libpng upgraded to(1.2.37 (last change 4 June 2009)</a>.</li>
    </ul>
  </li>
</ol>
'),

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
      <li><b>XML files for each creature, item and level and now separate</b>. The idea is to allow you to add things such as a new creature, new item, and new level to the game data without the need to recompile or to edit any central "index" file. You just add some subdirectories with index.xml files inside, and the game automatically picks them up. See <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/castle_game_engine/doc/README_about_index_xml_files.txt">documentation of index.xml files</a> in castle data.
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
