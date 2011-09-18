<?php

array_push($news,
    array('title' => 'Development news: animating skinned H-Anim humanoids',
          'year' => 2010,
          'month' => 12,
          'day' => 22,
          'short_description' => '',
          'description' =>
vrmlengine_thumbs(array(
  array('filename' => 'lucy_test.png', 'titlealt' => 'Lucy (from Seamless3d test page)'),
  array('filename' => 'lucy_joints_visualization.png', 'titlealt' => 'Lucy with our joints visualization'),
  array('filename' => 'hanim_0.png', 'titlealt' => 'BoxMan with joints visualized'),
  array('filename' => 'billboards_with_shadow.png', 'titlealt' => 'Billboards casting shadows'),
), 1) .
'<p>I just implemented animating skinned humanoids, following the H-Anim specification. This is implemented in our engine, and in particular can be used by our ' . news_a_href_page('view3dscene', 'view3dscene') . '.</p>

<p><a href="http://castle-engine.sourceforge.net/vrml_implementation_hanim.php">Documentation of current H-Anim support is here</a> (when the view3dscene with this will be officially released, it will be <a href="http://castle-engine.sourceforge.net/vrml_implementation_hanim.php">moved to stable H-Anim support docs</a>).</p>

<p>As usual, you can test the latest development version by downloading binary from our <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/">nightly builds</a>. Sample models are <a href="http://www.seamless3d.com/browser_test/index.html">"Lucy" examples</a> from Seamless3D, also "The famous boxman" linked from the bottom of <a href="http://doc.instantreality.org/tutorial/humanoid-animation/">InstantReality H-Anim overview</a>.</p>

<p>Check out also the new view3dscene menu item <i>"Edit -&gt; Add Humanoids Joints Visualization"</i>.</p>

<p>Other improvements in our engine and ' . news_a_href_page('view3dscene', 'view3dscene') . ':</p>

<ul>
  <li><tt>MultiGeneratedTextureCoordinate</tt> node introduced, to better define the <a href="http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#section_ext_tex_coord">Box/Cone/Cylinder/Sphere.texCoord</a>.</li>
  <li><a href="http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#section_ext_tex_coord_bounds">Texture coord generation dependent on bounding box (TextureCoordinateGenerator.mode = BOUNDS*)</a>. This allowed fixing shadow maps implementation for the case when shape has a texture but no explicit texture coordinate node.</li>
  <li>Fix Collada-&gt;VRML conversion (thanks to Simon from <a href="http://apps.sourceforge.net/phpbb/vrmlengine/viewforum.php?f=3">forum</a>).</li>
  <li>Zoom improved, to prevent going too far away from object by zoom-in.</li>
  <li>Help wanted: if you\'re familiar with Mac OS X (and FreePascal), I outlined <a href="http://castle-engine.sourceforge.net/macosx_requirements.php">here how you can help</a>.</li>
  <li>Fixed <tt>view3dscene --screenshot</tt> modal box on invalid filename.</li>
  <li>When local geometry (like coordinates) is often changed, shape changes into "dynamic" state: it\'s octree will be very simple, so we will not waste time on rebuilding it.</li>
  <li>Make a nice warning when more than one value specified for SFNode field value in X3D encoding.</li>

  <li>Engine terrain demo works also on GPUs without GLSL support.</li>
  <li>Fixes for rendering Walk/Fly tooltips in view3dscene on some GPUs.</li>
  <li>Engine documentation improved a lot, I talked about this in details in <a href="http://castle-engine.sourceforge.net/news.php?item=2010-12-2-development_news__major_improvements_to_engine_api_reference__future_plans">previous news post</a>.</li>
  <!--li>Memory leaks when reading invalid XML files fixed.</li-->

</ul>
'),

    array('title' => 'Development news: major improvements to engine API reference, future plans',
          'year' => 2010,
          'month' => 12,
          'day' => 2,
          'short_description' => '',
          'description' =>
'<p>In the last few days, I was working hard on making our
<i>engine API reference</i> perfect.
<a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/">The new and improved reference is here</a>, while
' . news_a_href_page('the stable (old) reference is here', 'reference') . '.
Of course at the next engine release, the new one will replace the old one :)</p>

<ul>
  <li><p><b>All units have now a nice documentation</b>, in English, suitable for <a href="http://pasdoc.sipsolutions.net/">PasDoc</a>, with nice formatting, abstracts etc.</p></li>

  <li><p>While doing the above, I also had to revisit some of the really ancient code of the engine (as this is where most of the bad docs, that needed fixing / translating, were). This caused me to <b>cleanup and even remove some of the old cruft from the engine</b> &mdash; total of 7 units are gone (some removed completely, some were trimmed down to a tiny utilities that were integrated into another units), some miscellanaus old hacks (like opengltypes.inc stuff, or TSkyCube, some FPC 1.0.x hacks etc.) are also removed.</p>

    <p>Out of curiosity, I did a line count on *.pas and *inc files in our engine (omitting auto-generated code). The total number is that since 2.2.0, the engine has 3863 lines <b>less</b>, making for code that much cleaner! Yeah!</p></li>

  <li><p><b>Future plans</b>:
    <ul>
      <li><p><i>' . news_a_href_page('castle', 'castle') . ' 1.0.0 release</i> is planned very soon. This will not include any new user-visible new features, but will incorporate all the engine bugfixes and speed improvements from last engine versions. The idea is to signal that "castle" is mostly finished now, and we\'re ready for new challenges :) A new large game using our engine is planned (since quite some time already :)</p></li>

      <li><p><i>' . news_a_href_page('view3dscene', 'view3dscene') . ' 3.8.0</i> is planned to include <a href="http://www.web3d.org/x3d/specifications/ISO-IEC-19775-1.2-X3D-AbstractSpecification/Part01/components/sound.html">X3D Sound component</a> implementation. At least the basic stuff, that works easily using our current OpenAL framework and allocator.</p></li>

      <li><p><i>' . news_a_href_page('view3dscene', 'view3dscene') . ' 3.9.0</i> is planned to have a more modern renderer, where <i>everything</i> rendered in 3D goes through VBO. This should make a performance boost for newer GPUs, also making some dynamic scenes work much faster. For older GPUs, the old rendering method (using locked vertex arrays and display lists) will be kept (and hopefully auto-selected).</p>

        <p>This will also bring many GLSL shaders improvements. Full GLSL-only pipeline should be done in view3dscene 3.9.0 or 3.10.0.</p></li>
    </ul>
  </li>
</ul>'),

    array('title' => 'view3dscene 3.7.0 release: Screen effects, drag sensors, ClipPlane, Billboard, toolbar and much more',
          'year' => 2010,
          'month' => 11,
          'day' => 18,
          'short_description' => '',
          'description' =>
vrmlengine_thumbs(array(
  array('filename' => 'screen_effect_blood_in_the_eyes_1.png', 'titlealt' => 'Screen effect &quot;blood in the eyes&quot;: modulate with reddish watery texture'),
  array('filename' => 'screen_effect_trees.png', 'titlealt' => 'Another screen effect example'),
  array('filename' => 'screen_effects_demo3.png', 'titlealt' => 'Demo of three ScreenEffects defined in VRML/X3D, see screen_effects.x3dv'),
  array('filename' => 'screen_effect_headlight_and_gamma.png', 'titlealt' => 'Screen effect: headlight, gamma brightness (on DOOM E1M1 level remade for our Castle)'),
  array('filename' => 'screen_effect_film_grain.png', 'titlealt' => 'Film grain effect'),
  array('filename' => 'screen_effect_grayscale_negative.png', 'titlealt' => 'Screen effect: grayscale, negative (on Tremulous ATCS level)'),
  array('filename' => 'tooltip_examine.png', 'titlealt' => 'Examine navigation tooltip'),
  array('filename' => 'billboards_0.png', 'titlealt' => 'Billboard demo'),
  array('filename' => 'bridge_final_0.png', 'titlealt' => 'Bridge model in engine examples'),
), 2) .
'<p>After 3 months of work, I\'m proud to present a new release
of our VRML/X3D browser:
' . news_a_href_page('view3dscene 3.7.0', 'view3dscene') . '.</p>

<!--p>There are many new features and improvements, some of which were already
announced in more details on our ' . news_a_href_page('news', 'news') . '.</p-->

<ul>
  <li><p><b>Screen effects</b> is a new eye-candy feature in our engine. Try the <i>View -&gt; Screen Effects</i> menu in ' . news_a_href_page('view3dscene', 'view3dscene') . ' for various effects that can be applied on any 3D scene.</p>

    <p>For people who know a little <a href="http://www.opengl.org/documentation/glsl/">GLSL (OpenGL Shading Language)</a>, this is quite powerful toy for designing your own screen effects. You can define a simple GLSL shader in VRML/X3D file, that processes the screen in any way you like, given the color and depth buffer. <a href="http://castle-engine.sourceforge.net/kambi_vrml_extensions_screen_effects.php">Documentation and examples of defining your own screen effects are here.</a></p>

    <p><small>Developers: Screen effects may also be defined (and controlled) directly from the Object Pascal source code. You only have to override <a href="http://castle-engine.sourceforge.net/apidoc/html/KambiSceneManager.TKamAbstractViewport.html#ScreenEffects">TKamAbstractViewport.GetScreenEffects and TKamAbstractViewport.ScreenEffectsCount</a> and return your own effects there. See multiple_viewports example source code (' . news_a_href_page('in engine sources in examples/vrml/', 'kambi_vrml_game_engine') . ') for a simple example. And see <a href="http://vrmlengine.svn.sourceforge.net/viewvc/vrmlengine/trunk/view3dscene/v3dscenescreeneffects.pas">v3dscenescreeneffects.pas</a> for more involved example straight from the ' . news_a_href_page('view3dscene', 'view3dscene') . ' sources.</small></p>

    <!--p>Also, the old <i>Change Scene Colors</i> (color modulators inside engine sources) are removed. This was a poor idea, with ugly implementation and little use. New Screen Effects allow much more effects, with a modern implementation.</p-->
  </li>

  <li><p><b>New nodes</b> implemented: <a href="http://castle-engine.sourceforge.net/vrml_implementation_pointingdevicesensor.php">drag sensors (<tt>PlaneSensor, SphereSensor, CylinderSensor</tt>)</a>,
    <a href="http://castle-engine.sourceforge.net/vrml_implementation_rendering.php"><tt>ClipPlane</tt>, <tt>ColorRGBA</tt></a>,
    <a href="http://castle-engine.sourceforge.net/vrml_implementation_navigation.php"><tt>Billboard</tt>, <tt>ViewpointGroup</tt></a>,
    <a href="http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#section_ext_toggler">Toggler</a>.</p>
  </li>

  <li><p>Major <b>improvements and fixes to existing nodes</b>:
    <tt>Transform</tt> (and similar nodes from H-Anim) animation is greatly optimized.
    Also changing <tt>Transform</tt> node containing light sources works fast now.
    Many <a href="http://castle-engine.sourceforge.net/vrml_implementation_time.php"><tt>TimeSensor</tt></a>,
    <a href="http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#section_ext_rendered_texture">RenderedTexture</a>,
    <tt>OrthoViewpoint</tt> improvements.
    <!--Events mechanism is optimized for many fields.-->
    See ' . news_a_href_page('news archive for details', 'news') . '.</p></li>

  <li><p><b>Camera improvements</b>: Examine camera now honors <tt>Viewpoint</tt>
    nodes. Switching navigation mode preserves camera view.
    Smooth transitions (following <tt>NavigationInfo.transitionType, NavigationInfo.transitionTime</tt> fields)
    are done. <a href="http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#section_ext_head_bobbing">headBobbingDistance
    is renamed into much more suitable headBobbingTime and expressed in seconds</a>.
    Mouse wheel is supported for zooming in Examine mode.</p>
  </li>

  <li><p><b>User interface improvements</b>: nice toolbar at the top of the window,
    with most important buttons. Navigation mode buttons have tooltips (hover
    mouse over them to see) describing camera controls.
    Nice "%s warnings" button.</p></li>

  <li><p>Primitives (<tt>Box, Cone, Cylinder, Sphere</tt>) have the
    <a href="http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#section_ext_tex_coord">texCoord</a>
    field and work with <a href="http://castle-engine.sourceforge.net/kambi_vrml_extensions_shadow_maps.php">shadow maps</a>,
    multi-texturing, <a href="http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#section_ext_bump_mapping">bump mapping</a> etc.</p>
  </li>

  <li><p>New functions for <a href="http://castle-engine.sourceforge.net/kambi_script.php#section_functions_rotation"><b>KambiScript to handle rotations</b></a>.</p></li>
</ul>

<p>Our website and documentation also got a lot of improvements.
Most of them were already announced in previous news items, and will not
be repeated now. Some new improvements:</p>

<ul>
  <li><p>' . news_a_href_page('view3dscene', 'view3dscene') . ' download links,
    and easy instructions to get GNOME integration and thumbnailer,
    are now more visible on view3dscene webpage.</p></li>

  <li><p>The pages describing our ' . news_a_href_page('VRML/X3D implementation status', 'vrml_implementation_status') . ' for each X3D component are much improved. Each component page starts with a very short introduction, describing what the component is and how it\'s used in the most typical cases. Also, the node names are links to actual X3D specification pages.</p>

    <p>The idea behind these improvements is to give interested developers (in particular, the ones not familiar with VRML/X3D yet) a way to orient themselves in the large number of VRML/X3D nodes. We give an easy overview of the component and the links to X3D specification to learn more details.</p>

    <p>And everything is of course interspersed with the details about our engine implementation, it\'s strength and current limitations.</p>
  </li>

  <li><p>Finally, I added ' . news_a_href_page('a section about donating', 'donate') . ' and a button to <a href="https://flattr.com/thing/82694/Kambi-VRML-game-engine">donate through Flattr</a> to a couple pages.</p></li>
</ul>

<p>As usual, view3dscene release is accompanied by ' . news_a_href_page('new engine release (2.2.0)', 'kambi_vrml_game_engine') . ' (this is where the magic actually happens :), and ' . news_a_href_page('new Kambi VRML test suite release (2.9.0)', 'kambi_vrml_test_suite') . ' (which contains tests and demos of all the new features).</p>
'),

    array('title' => 'Development news: Billboards, transform optimizations, UI: toolbars and hints, more',
          'year' => 2010,
          'month' => 11,
          'day' => 10,
          'short_description' => '',
          'description' =>
vrmlengine_thumbs(array(
  array('filename' => 'tooltip_examine.png', 'titlealt' => 'Examine navigation tooltip'),
  array('filename' => 'tooltip_walkfly.png', 'titlealt' => 'Walk/Fly navigation tooltip'),
  array('filename' => 'billboards_0.png', 'titlealt' => 'Billboard demo'),
  array('filename' => 'bridge_final_0.png', 'titlealt' => 'Bridge model in engine examples'),
  array('filename' => 'bridge_final_1.png', 'titlealt' => 'Bridge model in engine examples, another view'),
)) .
'<ul>
  <li><p>A major <b>Transform optimization</b> is implemented. This makes Transform animation really working at instant speeds.</p></li>
  <li><p><b>Billboard node</b> is implemented. Useful for sprites and such.</p></li>
  <li><p>At the top of view3dscene window you will now see a nice <b>toolbar</b>. This provides the most important buttons &mdash; open, change navigation mode, change collisions, and view warnings (if any).</p>

    <p>We also clearly visualize now separate "Walk" and "Fly" navigation methods (and "None" in the menu).</p>

    <!--
    The way we allow navigation mode changing is an improvement in itself. Previously we had only Examine, and Walk modes explicitly shown. The "Fly" was not explicitly shown, as it was just "Walk mode with Gravity off (and some prefer... settings different)". Now we have a separate button for "Fly" mode. You can also just change explicitly "Gravity", this will toggle you between "Walk" and "Fly". We also have a separate "None" navigation type.</p

    <p>So the options to change navigation mode should be now cleaner, both in the menu and on the toolbar.</p-->

    <p>Status text (at the bottom) is also shorter now.</p></li>

  <li><p><b>Tooltips ("hints")</b> are implemented for our OpenGL controls.</p>

    <p>They are used by view3dscene to display nice <b>description of key/mouse controls for given navigation mode</b> &mdash; just mouse over the "Examine", "Walk", "Fly" buttons. I really hope that this is useful (for both new and advanced users), comments about how you like it are most welcome. Hopefully, this will make the navigation controls more obvious.</p>

    <p><small>Developers: you may be interested that tooltips are implemented for everything, and you can render a toolbar both in 2D and 3D. So you can e.g. position a text in 3D coordinates, over an 3D object, as a tooltip. See <tt>TUIControl.TooltipStyle</tt>, <tt>TUIControl.DrawTooltip</tt>, <tt>TKamGLButton.Tooltip</tt>.</small></p></li>

  <li><p>Shadow maps PCF methods (in particular "PCF bilinear") look now better, because they know the correct shadow map size.</p></li>

  <li><p>Headlight behavior improved, e.g. you can <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/kambi_vrml_test_suite/x3d/headlight_anim.x3dv">animate headlight spot size</a>, and view3dscene "Headlight" menu item cooperates with VRML/X3D state better.</p>

  <li><p><b>Engine documentation and examples</b>:</p>
    <ul>
      <li>' . news_a_href_page('Documentation', 'vrml_engine_doc') . ': many updates, first of all a <a href="http://castle-engine.sourceforge.net/vrml_engine_doc/output/xsl/html/chapter.scene_manager.html">new short chapter about scene manager</a> (based on my old news post).</li>
      <li>' . news_a_href_page('API reference', 'reference') . ': many improvements, a lot of documentation improved for English and PasDoc, and regenerated with <a href="http://pasdoc.sipsolutions.net/">PasDoc 0.12.1</a></li>
      <li>Nice bridge model for <tt>kambi_vrml_game_engine/examples/vrml/</tt></li>
    </ul></li>
</ul>

<p>As usual, you can test the new features by trying our <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/">nightly builds</a>.</p>
'),

    array('title' => 'Development news: drag sensors, KambiScript rotations, mouse wheel, more',
          'year' => 2010,
          'month' => 10,
          'day' => 17,
          'short_description' => '',
          'description' =>

vrmlengine_thumbs(array(
  array('filename' => 'fish.png', 'titlealt' => 'Fish and animals, by Elenka Besedova'),
  array('filename' => 'fz.png', 'titlealt' => 'Fz Quake2 player model, converted to VRML/X3D by Stephen H. France, originally by Phillip T. Wheeler'),
  array('filename' => 'projected_spotlight.png', 'titlealt' => 'Animated projector with beam and shadow, by Victor Amat'),
)) .
'Welcome to the weekly news :) As usual, remember you can try out all the improvements right now by using our <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/">nightly builds</a>.

<ul>
  <li><p>All <b>X3D dragging sensors (PlaneSensor, SphereSensor, CylinderSensor)</b> are implemented now. Very nice tools to allow user to edit portions of your 3D scenes &mdash; PlaneSensor allows moving, SphereSensor allows free rotation, CylinderSensor allows rotations constrained around an axis.</p>

    <p>I encourage you to try them &mdash; test e.g. X3D conformance models
    (<a href="http://www.web3d.org/x3d/content/examples/Conformance/Sensors/PlaneSensor/">PlaneSensor</a>,
     <a href="http://www.web3d.org/x3d/content/examples/Conformance/Sensors/SphereSensor/">SphereSensor</a>,
     <a href="http://www.web3d.org/x3d/content/examples/Conformance/Sensors/CylinderSensor/">CylinderSensor</a>)
     or VRML 97 annotated reference examples
    (<a href="http://accad.osu.edu/~pgerstma/class/vnv/resources/info/AnnotatedVrmlRef/ch3-334.htm">PlaneSensor</a>,
     <a href="http://accad.osu.edu/~pgerstma/class/vnv/resources/info/AnnotatedVrmlRef/ch3-344.htm">SphereSensor</a>,
     <a href="http://accad.osu.edu/~pgerstma/class/vnv/resources/info/AnnotatedVrmlRef/ch3-315.htm">CylinderSensor</a>)
    or our Kambi VRML test suite (see SVN, files <tt>x3d/xxx_sensor*.x3dv</tt>). They allow you to really easily add great interactivity to your VRML/X3D scenes.</p>

    <p>Also related to sensors: fixed behavior when multiple pointing-device sensors are siblings (and so should be simultaneously activated).</p></li>

  <li><p>New functions for <a href="http://castle-engine.sourceforge.net/kambi_script.php#section_functions_rotation"><b>KambiScript to handle rotations</b></a>.</p></li>

  <li><p>Changing <b><tt>Transform</tt> node containing light sources</b> is greatly optimized now.</p>

    <p>This also causes a regression: if you instantiate this light source (through DEF/USE), and then try to animate it by changing it\'s Transform node &mdash; too many light instances will be updated. This is a regression (something that used to work correctly, but now doesn\'t work), but I feel it\'s justified &mdash; while previous behavior was correct, it was also awfully slow (bringing even trivial scenes to a speed of a few FPS), so the new behavior is at least usable in common cases.</p>

    <p>Various fixes along the way. Including shadow map regeneration when light changes.</p></li>

  <li><p><b>Mouse wheel</b> is supported. It is used for zoom (in camera Examine mode), scrolling text (in various message boxes), it can also be used as a (configurable) shortcut for ' . news_a_href_page('castle', 'castle') . ' actions (default is to scroll through inventory). <small>Developers: see <tt>TGLWindow.OnMouseWheel</tt> for using this in your own programs.</small></p></li>

  <li><p>Warnings after loading a model are signaled by a <b>"%d warnings" button</b> by ' . news_a_href_page('view3dscene', 'view3dscene') . '. This way warnings are still clearly visible (the button only disappears after you use it on this model), but don\'t make an "obstacle" (modal box) to viewing the model.</p></li>

  <li><p>I added to NURBS implementation status page notes about <a href="http://castle-engine.sourceforge.net/vrml_implementation_nurbs.php#section_homogeneous_coordinates">control points in homogeneous coordinates</a>, thanks to Stephen H. France for noticing the problem and Joerg Scheurich (from White Dune) for offering an explanation.</p></li>

  <li><p>Smooth camera transitions under Windows are fixed.</p></li>

  <li><p>OrthoViewpoint improvements, to internally adjust fieldOfView to keep aspect ratio. (<a href="http://svn.code.sf.net/p/castle-engine/code/trunk/kambi_vrml_test_suite/x3d/ortho_viewpoint.x3dv">OrthoViewpoint.fieldOfView demo</a>)</p></li>
</ul>'),

    array('title' => 'Website facelift',
          'year' => 2010,
          'month' => 10,
          'day' => 10,
          'short_description' => '',
          'description' =>

vrmlengine_thumbs(array(
  array('filename' => 'www_facelift_index.png', 'titlealt' => 'Snapshot comparing old and new index page look'),
  array('filename' => 'www_facelift_vrml_status.png', 'titlealt' => 'Snapshot comparing old and new vrml_implementation_status page look'),
)) .
'<p>As you can see, this week brings many improvements to our website. I hope it\'s now easier to navigate, and things look a little better :)</p>

<ol>
  <li><p>We have a nice header, visible at the top of every page, with most important links / sections clearly emphasized. Among other things, this avoids the previous looong index page. And makes the important but previously too-easy-to-miss links ' . news_a_href_page('"Forum"', 'forum') . ' and ' . news_a_href_page('"Engine" (for developers)', 'kambi_vrml_game_engine') . ' more visible.</p></li>

  <li><p>Some sections get a sidebar on the right for easier navigation. This is especially useful with ' . news_a_href_page('VRML/X3D', 'vrml_x3d') . ' section, which has a huge number of useful content especially under ' . news_a_href_page('Implementation status', 'vrml_implementation_status') . '.</p></li>

  <li><p>We also have "breadcrumbs" visible on pages deeper in the hierarchy, like ' . news_a_href_page('Shaders implementation status', 'vrml_implementation_shaders') . '. Together with header and sidebar they (hopefully) clearly show you where you are in the website.</p></li>

  <li><p>New ' . news_a_href_page('VRML/X3D', 'vrml_x3d') . ' page, an introduction to the whole VRML/X3D section, explains <i>"What is VRML / X3D"</i> in a short and friendly way. This will hopefully explain newcomers (to our engine, and/or X3D) why this 3D format is so great that I based my whole engine on it :)</p></li>

  <li>Our news are nicer now, with each ' . news_a_href_page('news', 'news') . ' post displayed on a separate page (previous "one page with all the news" was getting awfully long to load).  You get nice <i>Newer / Older</i> links and a sidebar to navigate among our news posts easily.</li>
</ol>'),

    array('title' => 'Development news: Examine improvements, smooth transitions, PlaneSensor and more',
          'year' => 2010,
          'month' => 9,
          'day' => 30,
          'short_description' => '',
          'description' =>

vrmlengine_thumbs(array(
  array('filename' => 'rendered_texture_mirror_2.png', 'titlealt' => 'Mirrors by RenderedTexture, by Victor Amat'),
  array('filename' => 'castle_siege_1.png', 'titlealt' => 'castle_siege model from DeleD sample models, converted to VRML by Stephen H. France'),
  array('filename' => 'castle_siege_shadows.png', 'titlealt' => 'castle_siege model from DeleD sample models, with shadows'),
)) .
'<p>The quest to "cleanup and optimize" all around the engine continues :) New features are listed below. As usual, you\'re welcome to test them by trying our <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/">nightly builds</a>.</p>

<ol>
  <li><p><b>Camera improvements</b>:</p>

    <ul>
      <li><p><i>Examine camera got a functionality boost</i>, and as a result some long-time troubles with switching camera modes are fixed now. Examine camera correctly honors now Viewpoint nodes, and switching camera modes preserves the current view, and switching viewpoints preserves camera mode. Thanks to Jens van Schelve for reporting this and pushing me to fix this :)</p>

        <p><small>Developers: engine has a new camera class, TUniversalCamera, that is created by default for VRML/X3D scenes and offers a functionality of both Examine and Walk navigation methods. If you previously used something like <tt>"(SceneManager.Camera as TWalkCamera)"</tt> to access Walk-specific properties, you may need to use now <tt>"(SceneManager as TUniversalCamera).Walk"</tt> to use them. Or just try to use the basic <tt>TCamera</tt> features, without downcasting to specific camera descendants.</small></p></li>

      <li><p><i>Smooth transitions</i> between viewpoints are implemented. They also follow X3D <tt>NavigationInfo.transitionType</tt>, <tt>NavigationInfo.transitionTime</tt> fields (<a href="http://www.web3d.org/x3d/specifications/ISO-IEC-19775-1.2-X3D-AbstractSpecification/Part01/components/navigation.html#NavigationInfo">X3D spec</a>).</p></li>

      <li><p><i>All camera moving and rotating speeds are now expressed inside the engine in nice units/per second</i>.</p>

        <p>Also, <a href="http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#section_ext_head_bobbing">headBobbingDistance is renamed into much more suitable headBobbingTime</a>, and is also expressed in seconds now (divide by 50 to get the same behavior with old values).</p></li>
    </ul></li>

  <li><p>New sensors implemented:<br/>
    <b><tt>PlaneSensor</tt></b>
      (<a href="http://web3d.org/x3d/specifications/ISO-IEC-19775-1.2-X3D-AbstractSpecification/Part01/components/pointingsensor.html#PlaneSensor">X3D spec</a>,
       <a href="http://castle-engine.sourceforge.net/vrml_implementation_pointingdevicesensor.php">support details</a>,
       demos in <a href="http://castle-engine.sourceforge.net/kambi_vrml_game_engine.php#section_svn">SVN</a> kambi_vrml_test_suite/x3d/plane_sensor*.x3dv),<br/>
    <b><tt>StringSensor</tt></b> (<a href="http://web3d.org/x3d/specifications/ISO-IEC-19775-1.2-X3D-AbstractSpecification/Part01/components/keyboard.html#StringSensor">X3D spec</a>, <a href="http://castle-engine.sourceforge.net/vrml_implementation_keydevicesensor.php">support details</a>, <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/kambi_vrml_test_suite/x3d/string_sensor.x3dv">demo</a>).</p></li>

  <li><p><b>Shadow maps</b> (<a href="http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#section_ext_shadow_maps">receiveShadows, shadows fields</a>) <b>for primitives</b> (<tt>Box</tt>, <tt>Sphere</tt> etc.) are fixed now (<a href="http://svn.code.sf.net/p/castle-engine/code/trunk/kambi_vrml_test_suite/shadow_maps/primitives.x3dv">demo</a>).</p></li>

  <li><p>Victor Amat updated the demo using our <a href="http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#section_ext_rendered_texture">RenderedTexture</a> to get <b>mirrors on a flat surface</b>. See kambi_vrml_test_suite/x3d/rendered_texture/chess.x3dv in <a href="http://castle-engine.sourceforge.net/kambi_vrml_game_engine.php#section_svn">SVN kambi_vrml_test_suite</a>.</p></li>

  <li><p>Various <b>fixes to <tt>TimeSensor</tt></b> and other stuff, thanks to Stephen H. France for reporting!</p></li>
</ol>
'),

    array('title' => 'Development news: ClipPlane, CHM docs, optimizations and more',
          'year' => 2010,
          'month' => 9,
          'day' => 18,
          'short_description' => '',
          'description' =>

vrmlengine_thumbs(array(
  array('filename' => 'jarjar.png', 'titlealt' => 'JarJar animation, made in X3D by Stephen H. France'),
  array('filename' => 'rendered_texture_output_events.png', 'titlealt' => 'RenderedTexture.rendering and ClipPlane demo: the teapot is sliced in half when rendering to the texture'),
)) .
'A lot of work happened last month, as I\'m sure <a href="http://cia.vc/stats/project/vrmlengine">SVN statistics</a> confirm :) Some highlights:

<ol>
  <li><p>VRML/X3D features implemented:

    <ul>
      <li><b>ClipPlane</b> node is handled.
      <li><b>ColorRGBA</b> node is handled. Also related VRML 1.0 Material-per-vertex/face is now much faster.<!-- (uses <tt>glColorMaterial</tt>).-->
      <li><a href="http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#section_ext_rendered_texture"><b>RenderedTexture.rendering, viewing, projection</b></a> output events are implemented.
      <li><b>TimeSensor.enabled, cycleTime</b> are now handled correctly.
      <li><a href="http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#section_ext_toggler"><b>Toggler</b></a> node (simple event utility) from InstantReality is handled.
    </ul>

  <li><p>Stephen H. France prepared <b><a href="http://castle-engine.sourceforge.net/abbreviated_x3d_specs_for_users.chm">X3D specification including Kambi extensions</a> and <a href="http://castle-engine.sourceforge.net/kambiscript_language.chm">KambiScript reference</a></b> in the CHM format. The CHM format makes them easy to browse and search. Thanks!</p>

  <li><p><b>Primitives</b>: more nodes (boxes, spheres, cones, cylinders) are now processed by converting them to <tt>IndexedFaceSet</tt> or similar low-level geometry. (This is called the <i>"proxy mechanism"</i> in sources.) And the whole mechanism is now much more efficient, so e.g. <tt>Extrusion</tt>, <tt>Teapot</tt>, NURBS curves and surfaces are processed now faster.</p>

    <p>The immediate gain from it is that Box, Cone, Cylinder, Sphere <a href="http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#section_ext_tex_coord">get the "texCoord" field</a>. In particular they can use our bump mapping features, they work with multi-texturing and 3D textures fully correctly, and they can be shadow map receivers (although this last thing still needs a little work).</p>

    <!--p>Developers: this makes a little incompatible change. TVRMLShape.Geometry/State now may return something more temporary. Most code should work out-of-the-box without changes (and work faster!), but if you e.g. played with removing the geometry nodes &mdash; you should consider using TVRMLShape.OriginalGeometry node instead, see also TVRMLScene.RemoveShapeGeometry.</p-->
  </li>

  <li><p><b>Events</b>: the code responsible for changing the VRML/X3D graph (in particular, through the events) got a few refreshments. Some events work better or faster now (e.g. <tt>RenderedTexture.dimensions</tt> and <tt>depthMap</tt> can be changed through events.)</p>

    <p>A couple of large optimizations for <tt>Transform</tt> animation were implemented.</p>

  <!--li><p>Next view3dscene release will include scripts to easier setup desktop (GNOME) integration.</li-->
</ol>

<p>As always, you can test the new features before the next release by trying our <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/">nightly builds</a>.'),


    array('title' => 'view3dscene 3.6.0 release: Many shadow maps improvements',
          'year' => 2010,
          'month' => 8,
          'day' => 8,
          'short_description' => '',
          'description' =>

vrmlengine_thumbs(array(
  array('filename' => 'sunny_street_above_view.png', 'titlealt' => 'Just a screenshot with nice shadow maps'),
  array('filename' => 'sunny_street_tree_hard.png', 'titlealt' => 'Close up shadows on the tree. Notice that leaves (modeled by alpha-test texture) also cast correct shadows.'),
  array('filename' => 'rendered_texture_mirror.png', 'titlealt'=> 'Flat mirrors by RenderedTexture'),
)) .
'<p>New ' . news_a_href_page('view3dscene 3.6.0', 'view3dscene') . ' release focuses on the improvements to our <a href="http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#section_ext_shadow_maps">Shadow Maps extensions</a>:</p>

<ul>
  <li><a href="http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#section_ext_light_shadows_on_everything">X3DLightNode.shadows</a> field, to easily activate shadows on everything.</li>
  <li><a href="http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#section_ext_receive_shadows">Apperance.receiveShadows</a> field, to easily activate shadows on specific shadow receivers.</li>
  <li>Light sources\' <tt>projectionNear</tt>, <tt>projectionFar</tt> and such are automatically calculated now to suitable values, as long as you use high-level <tt>X3DLightNode.shadows</tt> or <tt>Apperance.receiveShadows</tt> fields.</li>
  <li>Incompatible changes: <tt>DirectionalLight.projectionRectangle</tt> order changed, to match standard <tt>OrthoViewpoint.fieldOfView</tt> order. Also, <tt>projection*</tt> parameters are zero by default (which indicates that they should be automatically calculated).</li>
  <li>Easy menu items to control shadow maps, see the new <i>View -&gt; Shadow Maps -&gt; ...</i> submenu.</li>
  <li>New <a href="http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#section_ext_texture_gen_projective">ProjectedTextureCoordinate</a> node for projective texturing. Can project a texture also from a viewpoint now.</li>
  <li>Extensions to <a href="http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#section_ext_head_bobbing">control head-bobbing in VRML/X3D worlds</a>.</li>
  <li>Picking and ray-tracing with orthogonal projection fixed. (See also new <a href="http://castle-engine.sourceforge.net/rayhunter.php">rayhunter (version 1.3.2)</a> with <tt>--ortho</tt> option).</li>
  <li>See also <a href="http://castle-engine.sourceforge.net/news.php#2010-7-9-development_news__many_shadow_maps_improvements__castle_fountain__more">previous news item</a> for some more details about new stuff implemented.
</ul>

<p>Also, <a href="http://castle-engine.sourceforge.net/shadow_maps_x3d_slides.pdf">the slides from my Web3D 2010 talk about Shadow Maps</a> (and the <a href="http://castle-engine.sourceforge.net/shadow_maps_x3d.pdf">paper</a>) are available now.</p>

<p>In other news: Victor Amat just send me a very nice demo that uses our <a href="http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#section_ext_rendered_texture">RenderedTexture</a> to implement beautiful mirrors on a flat surface. See the models inside <tt>x3d/rendered_texture</tt> in <a href="http://castle-engine.sourceforge.net/kambi_vrml_test_suite.php">Kambi VRML test suite (new version 2.8.0)</a>.</p>

<p>All the shadow maps improvements are actually implemented inside our <a href="http://castle-engine.sourceforge.net/kambi_vrml_game_engine.php">engine (new version 2.1.0)</a>.</p>
'),

    array('title' => 'Development news: Many shadow maps improvements, castle fountain, more',
          'year' => 2010,
          'month' => 7,
          'day' => 9,
          'short_description' => '',
          'description' =>

vrmlengine_thumbs(array(
  array('filename' => 'sunny_street_above_view.png', 'titlealt' => 'Just a screenshot with nice shadow maps'),
  array('filename' => 'sunny_street_tree_hard.png', 'titlealt' => 'Close up shadows on the tree. Notice that leaves (modeled by alpha-test texture) also cast correct shadows.'),
  array('filename' => 'sunny_street_tree_pcf16.png', 'titlealt' => 'Close up shadows on the tree, with Percentage Closer Filtering.'),
  array('filename' => 'depths_camera_mapped.png', 'titlealt' => 'Shadow map mapped over the scene'),
  array('filename' => 'castle_fountain_1.png', 'titlealt' => 'Fountain water'),
  array('filename' => 'castle_fountain_2.png', 'titlealt' => 'Fountain close-up view'),
), 2) .
'<p>First of all, my paper <a href="http://castle-engine.sourceforge.net/shadow_maps_x3d.pdf">Shadow maps and projective texturing in X3D</a> got accepted for the <a href="http://conferences.web3d.org/web3d2010/">Web3D 2010 Conference</a>. Wee, I\'m going to Los Angeles :) This paper presents our new shadow mapping extensions, with many improvements over the old ones previously implemented in our engine. You can read the paper online, you can also <a href="http://castle-engine.sourceforge.net/kambi_vrml_extensions_shadow_maps.php">read the new shadow mapping extensions documentation</a>.</p>

<p>The improvements already implemented are:</p>

<ul>
  <li>First of all, <a href="http://castle-engine.sourceforge.net/kambi_vrml_extensions_shadow_maps.php#section_receive_shadows"><tt>Apperance.receiveShadows</tt> field for nice and comfortable shadows usage</a>. This very simple extension is what I hope to be ultimately used in 90% of the simple cases when you "just want shadows".</li>
  <li>Easy menu items to activate <i>Percentage Closer Filtering</i> (4, 16, 4 bilinear) and visualize shadow maps for scenes using the <tt>receiveShadows</tt> field. Look at the new <i>View -&gt; Shadow Maps -&gt; ...</i> menu items.</li>
  <li>New <tt>ProjectedTextureCoordinate</tt> node, that replaces deprecated now <tt>TextureCoordinateGenerator.mode = "PROJECTION"</tt>.</li>
  <li><a href="http://www.punkuser.net/vsm/">Variance Shadow Maps</a> are also implemented. Although their implementation is not optimal yet, and should be treated as experimental. You can easily turn them on by <i>View -&gt; Shadow Maps -&gt; Variance Shadow Maps</i> menu.</li>
</ul>

<p>For now, you can test these features by using <tt>view3dscene</tt> from our <a href="http://michalis.ii.uni.wroc.pl/~michalis/vrmlengine-snapshots/">nightly builds</a>.</p>

<p>You may also be interested in our shadow maps testing scene "sunny_street", you can checkout it from SVN url <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/vrml_engine_doc/shadow_maps_x3d/sunny_street/">http://svn.code.sf.net/p/castle-engine/code/trunk/vrml_engine_doc/shadow_maps_x3d/sunny_street/</a>.</p>

<!-- teaser -->

<p>Other features implemented:</p>

<ul>
  <li>Extensions to <a href="http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#section_ext_head_bobbing">control head-bobbing in VRML/X3D worlds (docs from nightly builds)</a>.</li>
  <li>view3dscene <i>Edit -&gt; Merge Close Vertexes</i> menu item, that makes close vertexes to be perfectly equal.</li>
  <li><a href="http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#section_ext_teapot">Teapot</a> mesh is much improved, thanks go to Victor Amat.</li>
  <li>Picking and ray-tracer in orthogonal projection (<a href="http://www.web3d.org/x3d/specifications/ISO-IEC-19775-1.2-X3D-AbstractSpecification/Part01/components/navigation.html#OrthoViewpoint">like by OrthoViewpoint</a>) fixed.</li>
  <li>Workaround nasty <a href="http://ati.cchtml.com/show_bug.cgi?id=1815">fglrx bug</a>, thanks Simon for <a href="https://sourceforge.net/apps/phpbb/vrmlengine/viewtopic.php?f=3&amp;t=14">reporting</a>.</li>
  <li>Better menu behavior with GTK2 backend.</li>
  <li>Our procedural terrain demo (<tt>examples/vrml/terrain/</tt> in sources) can export the terrain to X3D (<tt>ElevationGrid</tt>) now.</li>
  <li>Support IMPORT/EXPORT for VRML 2.0 (97) too. Although it\'s defined only in X3D spec, it\'s so useful that I enable it also for VRML 2.0.</li>
</ul>

<p>In unrelated news, the quest to release castle 1.0.0 is ongoing (even if terribly delayed). Remember, I wanted to add some eye-candy to "Fountain" level for this? Well, part of the job is done, see the screenshot on the right for a nice water pouring from the fountain.</p>
'),

    array('title' => 'view3dscene 3.5.2 release: IMPORT/EXPORT and bugfixes',
          'year' => 2010,
          'month' => 4,
          'day' => 18,
          'short_description' => '',
          'description' =>

vrmlengine_thumbs(array(
  array('filename' => 'ddracer_scene85.png', 'titlealt' => 'scene85'),
  array('filename' => 'ddracer_t128.png', 'titlealt' => 't128'),
  array('filename' => 'ddracer_t603coupe.png', 'titlealt' => 't603coupe'),
)) .
'<p>New ' . news_a_href_page('view3dscene 3.5.2', 'view3dscene') . ' is released today:</p>

<ul>
  <li><p>New feature in this release is the support for <a href="http://www.web3d.org/x3d/specifications/ISO-IEC-19775-1.2-X3D-AbstractSpecification/Part01/components/networking.html#IMPORTStatement">X3D IMPORT and EXPORT statements</a>.

    <p>This allows to attach routes to VRML/X3D nodes inside inline files, thus allowing communication between the nodes inside and outside of the inlined file. Available in both classic and XML encodings. Some more <a href="http://castle-engine.sourceforge.net/vrml_implementation_networking.php">support details is here</a>.

  <li><p>Particular setup with nested PROTOs was expanded incorrectly, fixed now.

    <p>Thanks to David Rehor for reporting, <a href="http://tatraportal.com/drracer/">check out his VRML models here</a> ! (And on the screenshots to the right :) )

  <li><p>Crashes on menu commands <i>Edit -&gt; Remove Geometry Node / Face</i> are now fixed.
</ul>

<p>Accompaying ' . news_a_href_page('engine 2.0.3', 'kambi_vrml_game_engine') . ' is also released. All above view3dscene features/fixes are actually implemented mostly in the engine. Also, for the programmers: you will find useful .lpi files to compile every engine example from Lazarus. Also program names now use standard Lazarus extension .lpr. (These last improvements were actually already "silently" released in engine version 2.0.2 shortly before 2.0.3.)

<p>Also ' . news_a_href_page('malfunction 1.2.6', 'malfunction') . ' is released, this fixes a crash when opening some levels (caused by the same problem as view3dscene\'s <i>Edit -&gt; Remove Geometry Node / Face</i> crashes).'),


    array('title' => 'view3dscene 3.5.1 bugfix release, glinformation 1.2.0',
          'year' => 2010,
          'month' => 4,
          'day' => 5,
          'short_description' => '',
          'description' =>

vrmlengine_thumbs(array(
  array('filename' => 'view3dscene_bsuit.png', 'titlealt' => 'view3dscene showing tremulous battlesuit from MD3'),
)) .
'<p>A bug crawled into view3dscene 3.5.0 release: opening kanim and MD3 files usually failed with <i>"Invalid floating point operation"</i>. Therefore, we quickly release a fix in ' . news_a_href_page('view3dscene 3.5.1', 'view3dscene') . '. By the way, <i>View-&gt;Blending...</i> menu options are rearranged and <i>Help-&gt;OpenGL information</i> looks better now.</p>

<p>Also ' . news_a_href_page('glinformation 1.2.0', 'glinformation') . ' (previously known as <tt>glcaps</tt>) is released: various improvements to the output (it\'s the same text as <i>Help->OpenGL information</i> in view3dscene) and packaging.</p>

<p>' . news_a_href_page('Engine 2.0.1', 'kambi_vrml_game_engine') . ' is also released with these fixes.</p>'),

    array('title' => 'Release: view3dscene 3.5.0, engine 2.0.0, others',
          'year' => 2010,
          'month' => 3,
          'day' => 30,
          'short_description' => '',
          'description' =>

vrmlengine_thumbs(array(
  array('filename' => 'nurbs_curve_interpolators.png', 'titlealt' => 'Animating along the NURBS curve (NurbsPositionInterpolator and NurbsOrientationInterpolator)'),
  array('filename' => 'nurbs_surface_interpolator.png', 'titlealt' => 'Animating along the NURBS surface (NurbsSurfaceInterpolator)'),
  array('filename' => 'multiple_viewports_dynamic_world.png', 'titlealt' => 'multiple_viewports: interactive scene, with shadows and mirror'),
  array('filename' => 'terrain_nice_fog.png', 'titlealt' => 'Terrain - valley view with a hint of fog'),
), 2) .

'<p>Today we release a ' . news_a_href_page('grand new 2.0.0 version of the Kambi VRML game engine', 'kambi_vrml_game_engine') . ' and a new ' . news_a_href_page('version 3.5.0 of our main tool, view3dscene', 'view3dscene') . '. Other minor programs here are also updated, to bring bugfixes to them. Changes:</p>

<p><b>User-visible features</b>:</p>

<ul>
  <li>' . news_a_href_page('NURBS support', 'vrml_implementation_nurbs') . '. Most of the X3D NURBS component (level&nbsp;1) is implemented, this includes curves, surfaces and interpolators. VRML 97 NURBS nodes are also handled.</li>
  <li>Major bugfixes to the GTK 2 (Unix) backend and shadow maps handling.</li>
  <li>Countless small bugfixes and improvements.</li>
</ul>

<p><b>Programmer-visible engine features</b>:</p>

<ul>
  <li>Scene manager (<tt>TKamSceneManager</tt>), a manager of the 3D world.</li>
  <li>Custom viewports (<tt>TKamViewport</tt>) easily usable with our scene manager.</li>
  <li>2D controls framework: <tt>TKamGLButton</tt>, <tt>TKamGLImage</tt>, better <tt>TGLMenu</tt> and more. Viewports are also 2D controls.</li>
  <li>Engine sources reorganized into more intuitive <tt>src/</tt>, <tt>examples/</tt> etc. directories.</li>
  <li>Much more components registered on the Lazarus palette. (This will be extended in next releases.)</li>
  <li>Engine is licensed now on the terms of <a href="http://castle-engine.sourceforge.net/kambi_vrml_game_engine.php#section_license">the GNU Lesser General Public License (with "static linking exception")</a>.</li>
</ul>

<p>For more details about the changes, see ' . news_a_href_page('the news archive', 'news'). '.</p>

<p>For people waiting for new ' . news_a_href_page('castle 1.0.0', 'castle') . ' release: not yet, but should happen very soon.</p>'),

    array('title' => 'Custom viewports, engine 2.0.0 release very soon',
          'year' => 2010,
          'month' => 3,
          'day' => 27,
          'short_description' => '',
          'description' =>

vrmlengine_thumbs(array(
  array('filename' => 'multiple_viewports_teapot.png', 'titlealt' => 'multiple_viewports: simple teapot scene'),
  array('filename' => 'multiple_viewports_tower_mirror_raptor.png', 'titlealt' => 'multiple_viewports: scene with raptor animation and mirror by GeneratedCubeMapTexture'),
  array('filename' => 'multiple_viewports_shadows.png', 'titlealt' => 'multiple_viewports: animated shadows by shadow volumes'),
  array('filename' => 'multiple_viewports_dynamic_world.png', 'titlealt' => 'multiple_viewports: interactive scene, with shadows and mirror'),
), 2) .

'<p>You can now have many viewports on the 2D window to observe your 3D world from various cameras. You can make e.g. split-screen games (each view displays different player), 3D modeling programs (where you usually like to see the scene from various angles at once), or just show a view from some special world place (like a security camera).</p>

<p>Your viewports may be placed in any way you like on the screen, they can even be overlapping (one viewport partially obscures another). Each viewport has it\'s own dimensions, own camera, but all viewports share the same 3D world. Each viewport has also it\'s own rendering methods, so you can derive e.g. a specialized viewport that always shows wireframe view of the 3D world.</p>

<p>This very nice feature is implemented thanks to the scene manager framework. The scene manager itself also acts as a viewport (if <tt>DefaultViewport</tt> is true), which is comfortable for simple programs where one viewport is enough. When <tt>DefaultViewport</tt> is false, scene manager is merely a container for your 3D world, referenced by custom viewports (<tt>TKamViewport</tt> classes).</p>

<p>See the screenshots on the right and <tt>kambi_vrml_game_engine/examples/vrml/multiple_viewports.lpr</tt> example program in the SVN for demo.</p>

<!-- teaser -->

<p>Other improvements include new button rendered in the OpenGL (<tt>TKamGLButton</tt> in <tt>kambi_vrml_game_engine/src/ui/opengl/glcontrols.pas</tt> unit), you can see it on the screenshots too. This is the start of a promised 2D controls library for the engine. The idea is that such button may be easily themed for your OpenGL game, to match game mood and graphics.</p>

<p>Also there\'s a <tt>TKamGLImage</tt> control, and lot\'s of bugfixes stabilizing engine 2.0.0. It\'s pretty much finished now &mdash; expect engine 2.0.0 and view3dscene 3.5 releases very shortly :)</p>'),

    array('title' => 'Terrain demo much extended',
          'year' => 2010,
          'month' => 3,
          'day' => 11,
          'short_description' => '',
          'description' =>

vrmlengine_thumbs(array(
  array('filename' => 'terrain1.png', 'titlealt' => 'Terrain 1'),
  array('filename' => 'terrain2.png', 'titlealt' => 'Terrain 2'),
  array('filename' => 'terrain_wire_lod.png', 'titlealt' => 'Terrain - wireframe view showing our simple LOD approach'),
  array('filename' => 'terrain_nice_fog.png', 'titlealt' => 'Terrain - valley view with a hint of fog'),
  array('colspan' => 2,
    'html' => (!HTML_VALIDATION ? '<object width="370" height="227"><param name="movie" value="http://www.youtube.com/v/9qx-Ry2PRWM&amp;hl=pl_PL&amp;fs=1&amp;"></param><param name="allowFullScreen" value="true"></param><param name="allowscriptaccess" value="always"></param><embed src="http://www.youtube.com/v/9qx-Ry2PRWM&amp;hl=pl_PL&amp;fs=1&amp;" type="application/x-shockwave-flash" allowscriptaccess="always" allowfullscreen="true" width="370" height="227"></embed></object>' : '')),
), 2) .

'<p>Our procedural terrain demo (see <tt>kambi_vrml_game_engine/examples/vrml/terrain</tt> in <a href="http://castle-engine.sourceforge.net/kambi_vrml_game_engine.php#section_svn">SVN</a>) got a lot of improvements this week:</p>

<ul>
  <li><i>Heterogeneous</i> terrain (idea from <a href="http://www.kenmusgrave.com/dissertation.html">Ken Musgrave</a>) implemented, this makes more realistic terrain (smooth valleys, noisy mountains).</li>
  <li>Terrain is rendered with nice blended texture layers, normals are calculated for fake lighting, fog may be used, all by simple GLSL shader.</li>
  <li>Simple LOD approach for rendering is used, this is an ultra-dumbed-down version of <a href="http://research.microsoft.com/en-us/um/people/hoppe/geomclipmap.pdf">geometry clipmaps</a>. The way I simplified this is unfortunately painfully visible as LOD "popping" artifacts. But, hey, at least I can view really large terrain.</li>
  <li>You can test various noise interpolation methods, including <i>Catmull-Rom splines</i>.</li>
  <li>2D noise can be blurred, which (may) improve the terrain look.</li>
  <li>You can switch camera to <i>Walk</i> mode.</li>
  <li>You can add a heighmap from grayscale image to generated terrain.</li>
  <li>Rendering uses VBOs for speed.</li>
</ul>

<p>Finally, the programmers may be interested in my <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/kambi_vrml_game_engine/examples/vrml/terrain/TERRAIN_GENERATION_NOTES.txt">my notes and links about basic terrain generation methods</a>.</p>'),

    array('title' => 'Cloth animation with bump mapping',
          'year' => 2010,
          'month' => 2,
          'day' => 26,
          'short_description' => '',
          'description' =>

(!HTML_VALIDATION ?
'<table align="right"><tr><td>
  <object width="425" height="344"><param name="movie" value="http://www.youtube.com/v/5DTu9tEn44g&amp;hl=pl_PL&amp;fs=1"></param><param name="allowFullScreen" value="true"></param><param name="allowscriptaccess" value="always"></param><embed src="http://www.youtube.com/v/5DTu9tEn44g&amp;hl=pl_PL&amp;fs=1" type="application/x-shockwave-flash" allowscriptaccess="always" allowfullscreen="true" width="425" height="344"></embed></object>
 </td></tr></table>' : '') .
'<p>To sweeten the wait for engine 2.0.0 (and castle 1.0.0) releases: <a href="http://www.youtube.com/watch?v=5DTu9tEn44g">here\'s a short demo movie</a> with a cloth animation (cloth simulated and animated in <a href="http://www.blender.org/">Blender</a>) rendered using our engine with bump mapping. The point here is that both 3D meshes and lights may be animated in real-time and bump mapping works perfectly.</p>

<p>This was always possible, but some recent improvements made this much easier. Namely:</p>

<ol>
  <li>Our <tt>examples/vrml/bump_mapping</tt> demo now works with animated objects without any trouble.</li>
  <li>Our <tt>examples/vrml/tools/kanim_to_interpolators</tt> is now slightly more general converter from ' . news_a_href_page('KAnim format', 'kanim_format') . ' to VRML/X3D. This means it\'s possible to make "normal" animated VRML/X3D models by ' . news_a_href_page('exporting from Blender to kanim', 'blender') . ', then converting kanim to VRML/X3D. Convertion kanim-&gt;VRML/X3D is totally lossless, so the whole setup works quite flawlessly &mdash; at least for this simple cloth demo.</li>
</ol>

<p>The source model is in SVN, in <tt>kambi_vrml_test_suite/bump_mapping/cloth/</tt>. You can open it with the bump_mapping example (from our engine sources) or view3dscene.</p>'),

    array('title' => 'More engine 2.0 news: all examples and &quot;The Castle&quot; use scene manager',
          'year' => 2010,
          'month' => 2,
          'day' => 4,
          'short_description' => '',
          'description' =>

'<p>Nearly all existing programs (examples and normal games) are now happily converted to the new <i>scene manager</i> approach. Scene manager interface got a lot of improvements by the way, <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/KambiSceneManager.TKamSceneManager.html">check the docs</a>. This means that engine 2.0.0 is coming along nicely and will soon be released :)</p>

<p>In particular, ' . news_a_href_page('The Castle', 'castle') . ' source code is converted to use the scene manager. This means that finally you can construct <i>"The Castle"</i> interactive levels by VRML/X3D events, using VRML/X3D time, touch sensors, key sensors, proximity sensors (see <a href="http://www.web3d.org/x3d/specifications/ISO-IEC-19775-1.2-X3D-AbstractSpecification/index.html">X3D spec</a> for sensor nodes docs), scripts in ' . news_a_href_page('KambiScript', 'kambi_script') . ' and such. To celebrate this, I plan to add a little eye-candy to castle\'s "Fountain" level, and then officially release the final <b>"The Castle 1.0.0"</b> along with <b>engine 2.0.0</b>. After that, work on "The Castle 2" may begin :)</p>

<p>More teasers about the <i>Castle 1.0.0</i> release and plans for <i>Castle 2</i> in later post hopefully next week.</p>

<p>Getting back to the engine work, one important news for developers:  <!-- teaser --> I renamed one of the most important new classes: what was <tt>TBase3D</tt> is now called "<a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/Base3D.T3D.html">T3D</a>". (Reasoning: this is a shorter and better name. "Base" prefix was just uncomfortable, esp. since descendants were no longer "base" 3D objects but they were have to be called "TBase3D" in many places.)</p>

<p>Also, some other classes (<tt>T[Custom]Translated/List3D</tt>) renamed to follow <tt>T3DXxx</tt> pattern, like <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/Base3D.T3DList.html">T3DList</a> and <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/GL3D.T3DTranslated.html">T3DTranslated</a>. That is, I moved the "3D" part of the name to the beginning. (Reasoning: looks just better, and is consequent with usage of other common prefixes like TKamXxx and TVRMLXxx classes. General rule is that "common part should be the prefix, not suffix", as this is easier to see (and presents itself better when identifiers are sorted alphabetically in API docs etc.).)</p>

<p>These renames concern only new classes, not released anyway yet (and we know anyway that engine 2.0.0 will break some compatibility, so let\'s take this opportunity and "get" the important classes names right, Ok? :) ).</p>

<p>Note: I edited the previous news post (the one ~2 weeks ago) to reflect these renames. I don\'t usually edit my news posts after publishing :), but in this case it was important to fix it. Otherwise some classes and code mentioned in the previous news article would not exist.</p>'),

    array('title' => 'Development news: engine 2.0',
          'year' => 2010,
          'month' => 1,
          'day' => 26,
          'short_description' => '',
          'description' =>

vrmlengine_thumbs(array(
  array('filename' => 'scene_manager_demos_1.png', 'titlealt' => 'Screenshot from scene_manager_demos &mdash; two VRML scenes and one precalculated animation at once'),
  array('filename' => 'scene_manager_demos_2.png', 'titlealt' => 'Another screenshot from scene_manager_demos'),
)) . '

<p>During the last weeks I did a lot of work on the engine API. Especially the new <b>Scene Manager</b> approach makes quite a revolutionary change, and there\'s also <i>2D Controls manager</i>, better <i>Lazarus components</i> and many more. Together, I feel these are so important for developers using my engine that the next engine version will be bumped to proud <i>2.0.0</i> :) To be released next month.</p>

<p>Not much noticeable for a normal user (sorry; although there are various fixes and improvements here and there). Below news post turned out quite long, and I\'ll keep passing you links to the <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/">current API reference of SVN code</a>, so... developers: keep reading if you\'re interested, and others: well, this may get dirty, so you can stop reading now :)</p>

<!-- teaser -->

<ol>
  <li>
    <p>The number one reason behind all these changes is that it was too difficult to add new stuff to your window. This concerned 3D stuff, mainly the two most important classes of our engine: <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/VRMLGLScene.TVRMLGLScene.html">TVRMLGLScene</a> and <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/VRMLGLAnimation.TVRMLGLAnimation.html">TVRMLGLAnimation</a>. And it also concerned 2D controls, for example our menu with fancy GUI sliders: <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/GLMenu.TGLMenu.html">TGLMenu</a> (this was used in ' . news_a_href_page('castle', 'castle') . ' and recent <tt>examples/vrml/terrain</tt> demo).</p>

    <p><b>The goal</b>: the goal (already achieved :) ) is to allow you to make a full 3D model viewer / VRML browser (with collisions, fully optimized rendering etc.) by this simple code:</p>

<pre style="background: #DDD">
var
  Window: TGLUIWindow;
  SceneManager: TKamSceneManager;
  Scene: TVRMLGLScene;
begin
  Scene := TVRMLGLScene.Create(Application { Owner that will free the Scene });
  Scene.Load(\'my_scene.x3d\');
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;

  SceneManager := TKamSceneManager.Create(Application);
  SceneManager.Items.Add(Scene);
  SceneManager.MainScene := Scene;

  Window := TGLUIWindow.Create(Application);
  Window.Controls.Add(SceneManager);
  Window.InitAndRun;
end.
</pre>

    <p>(The source code of this is in <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/kambi_vrml_game_engine/examples/vrml/scene_manager_basic.lpr">scene_manager_basic</a> demo inside engine examples. There\'s also more extensive demo in the <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/kambi_vrml_game_engine/examples/vrml/scene_manager_demos.lpr">scene_manager_demos</a> sample.)</p>

    <p>This looks nice a relatively straighforward, right? You create 3D object (<tt>Scene</tt>), you create 3D world (<tt>SceneManager</tt>), and a window to display the 3D world (<tt>Window</tt>). What is really the point here is that you immediately know how to add a second 3D object: just create <tt>Scene2</tt>, and add it to <tt>SceneManager.Items</tt>.</p>

    <p>The idea of <i>scene manager</i> will soon be explained in more detail, for now let\'s go a litle back in time and see what was wrong without the scene manager:</p>

    <p><b>The trouble</b>: our existing classes were nicely encapsulating some functionality (showing menu, rendering VRML etc.) but they <i>were a pain to add to your window / 3D world</i>.</p>

    <p>You could use something like <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/GLWindowVRMLBrowser.TGLWindowVRMLBrowser.html">TGLWindowVRMLBrowser</a> (or equivalent Lazarus component, <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/KambiVRMLBrowser.TKamVRMLBrowser.html">TKamVRMLBrowser</a>) but these offered too little flexibility. They were made to allow easily loading <i>only one scene</i>.</p>

    <p>For anything more complicated, you had to directly create your <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/VRMLGLScene.TVRMLGLScene.html">TVRMLGLScene</a> etc. classes, and (this is the key point) to handle various window / camera / callbacks to connect the 3D scene with a camera and with a window. For example, you had to register window <tt>OnIdle</tt> callback to increment VRML scene time (to keep animations running etc.) You had to register <tt>OnKeyDown</tt>, <tt>OnKeyUp</tt> and pass key events to VRML scene (to make VRML sensors work), <tt>OnMouseXxx</tt> callbacks had to be passed to handle VRML touch sensors, <tt>OnDraw</tt> to handle scene rendering. You also had to register camera callbacks (<a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/Cameras.TWalkCamera.html#OnMoveAllowed"><tt>OnMoveAllowed</tt></a>, <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/Cameras.TWalkCamera.html#OnGetCameraHeight"><tt>OnGetCameraHeight</tt></a>) to connect collision detection with your 3D scene. And the list goes on...</p>

    <p>As a witness to all these troubles, have a look at implementation of <tt>GLWindowVRMLBrowser</tt> or (nearly identical) implementation of <tt>KambiVRMLBrowser</tt> is last released engine version. They are long and clumsy. In current SVN code, they are nice, short and clean, and also more flexible (can handle many 3D objects, not just 1 VRML scene).</p>

    <p><b>The solution(s):</b> There are actually two solutions, one directed at 3D objects (living in the 3D world), the other at 2D controls (merely taking some space on the 2D window) . They are quite similar, and nicely playing with each other:</p>

    <ol>
      <li><p><i>For 3D objects</i>: we have a base class <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/Base3D.T3D.html">T3D</a> from which <i>all</i> other 3D objects are derived. For example, <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/VRMLGLScene.TVRMLGLScene.html">TVRMLGLScene</a> and <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/VRMLGLAnimation.TVRMLGLAnimation.html">TVRMLGLAnimation</a> are both descendants of <tt>T3D</tt> now. There are some other helper 3D objects (<a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/Base3D.T3DList.html">T3DList</a> - list of other 3D objects, and <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/GL3D.T3DTranslated.html">T3DTranslated</a> - translated other 3D object). And the real beauty is that you can easily derive your own <tt>T3D</tt> descendants, just override a couple methods and you get 3D objects that can be visible, can collide etc. in 3D world.</p>

        <p>Now, what to do with your 3D objects? Add them to your 3D world, of course. The new great class that is fully implemented now is the <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/KambiSceneManager.TKamSceneManager.html">TKamSceneManager</a>. In every program you create an instance of this class (or your own dencendant of <tt>TKamSceneManager</tt>), and you add your 3D objects to the scene manager. Scene manager keeps the whole knowledge about your 3D world, as a tree of <tt>T3D</tt> objects. After adding your whole 3D world to the scene manager, you can add the scene manager to <tt>Controls</tt> (more on this next), and then scene manager will receive all necessary events from your window, and will pass them to all interested 3D objects. Also scene manager connects your camera, and defines your viewport where 3D world is rendered through this camera.</p>
      </li>

      <li><p><i>For 2D controls</i>: quite similar solution is used, although with some details different. All stuff that had to receive window events must derive from base <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/UIControls.TUIControl.html">TUIControl</a> class. This means that <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/GLMenu.TGLMenu.html">TGLMenu</a> is now a descendant of <tt>TUIControl</tt>, and in the future I would like to have a small library of generally-usable simple 2D controls available here. Also note that <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/KambiSceneManager.TKamSceneManager.html">TKamSceneManager</a> is <tt>TUIControl</tt> descendant, since scene manager by default acts as a viewport (2D rectangle) through which you can see your 3D world. (Possibility to easily add custom viewports from the same scene manager is the next thing on my to-do list.)</p>

        <p>To actually use the <tt>TUIControl</tt>, you add it to the window\'s <tt>Controls</tt> list. If you use Lazarus component, then you\'re interested in <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/KambiGLControl.TKamOpenGLControl.html#Controls">TKamOpenGLControl.Controls</a> list. If you use our own window library, you\'re intersted in the <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/GLWindow.TGLUIWindow.html#Controls">TGLUIWindow.Controls</a>. Once control is added to the controls list, it will automatically receive all interesting events from our window.</p>
      </li>
    </ol>

    <p>That\'s it, actually. Thank you for reading this far, and I hope you\'ll like the changes &mdash; you can try SVN source code already and let me know how it works for you in practice.</p>

  </li>

  <li><p>Other changes:</p>

    <ul>
      <li>
        <p>The <i>Lazarus integration of the engine is better</i>, with many old classes reworked as components. This is also tied to the previous change, because both <tt>T3D</tt> and <tt>TUIControl</tt> are descendants of <tt>TComponent</tt> &mdash; which means that important classes of our engine, like <tt>TVRMLGLScene</tt>, and also the cameras are now components that can be registered on Lazarus palette.</p>

        <p>This is not yet finished: some important properties of these classes are not published, or not possible to design from the IDE. For example, you cannot add items to the <tt>TKamOpenGLControl.Controls</tt> list yet from the IDE. This doesn\'t limit your possibilities, it only means that you\'ll have to do some work by writing source code as opposed to just clicking in Lazarus. Things for sure are already a lot better than in previous engine release.</p>

        <p>Mouse look is now also available in Lazarus <tt>TKamOpenGLControl</tt> component.</p>
      </li>

      <li><p><tt>T3D/TUIControl</tt> give various improvements to all 2D/3D objects. For examples, <tt>TVRMLGLScene</tt> automatically sets cursor to "hand" when it\'s over a touch sensor. In fact, every <tt>T3D</tt> and <tt>TUIControl</tt> can affect cursor by <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/UIControls.TUIControl.html#Cursor">Cursor</a> property.</p></li>

      <li><p>If you do not explicitly create a camera for the scene manager, a suitable one is automatically created, see <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/KambiSceneManager.TKamSceneManager.html#Camera">TKamSceneManager.Camera</a>.</p></li>

      <li><p>All "navigator" classes, fields etc. are renamed to "camera". So e.g. <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/Cameras.TExamineCamera.html">TExamineCamera</a> is just a new name for our good old <tt>TExamineNavigator</tt> and such.</p>

        <p>Old names <i>navigator</i> were coined to emphasize the fact that they do not have to represent "real" camera that is used to display the 3D scene on the screen, they can be used to manipulate any 3D object of your scene. For example <tt>shadow_fields</tt> demo uses 4 examine navigators to manipulate various scene elements, <tt>rift</tt> uses special walk navigator to represent position of the player in the room, etc. But I decided that the name is just confusing, as most of the time you use this as a "normal camera". Advanced users can probably grasp the fact that in fact "camera" doesn\'t have to be used to display whole scene from the screen.</p></li>

      <li><p>GTK 2 backend of our <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/GLWindow.TGLWindow.html">TGLWindow</a> class was much reworked. Namely, 1. we no longer use GTK\'s <tt>gtk_idle_add</tt> to implement our <tt>OnIdle</tt> callbacks and 2. GTK\'s "expose" signal no longer calls directly <tt>OnDraw</tt> callback . These caused an endless stream of troubles with stability, related to GTK idle events priorities. Our new solution is much simpler, solving some recent problems and removing ugly workarounds for the old ones.

        <p>For more in-depth discussion of past problems, reasonings and solutions, see the document <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/kambi_vrml_game_engine/src/glwindow/gtk/why_not_using_gtk_idle.txt">why_not_gtk_idle.txt</a>.
      </li>

      <li><p>A minor improvements to the <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/GLWindow.TGLWindow.html">TGLWindow</a> are the <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/GLWindow.TGLWindow.html#MessageOK">MessageOK</a> and the <a href="http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/docs/reference/html/GLWindow.TGLWindow.html#MessageYesNo">MessageYesNo</a> methods, proving native-looking (GTK, WinAPI etc.) message boxes.</p></li>

      <li><p>Oh, and f you\'re trying to compile the engine with newest FPC 2.4.0, please use engine SVN source for now. Current released tar.gz sources compile only with &lt;= 2.2.4. This problem will of course disappear after new engine is released.</p></li>
    </ul>

  </li>
</ol>')
);
