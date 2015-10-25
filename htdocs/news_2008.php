<?php

array_push($news,
    array('title' => 'view3dscene 3.2, engine 1.6 release: collisions in dynamic worlds',
          'year' => 2008,
          'month' => 12,
          'day' => 18,
          'guid' => '2008-12-18',
          'short_description' => '',
          'description' => "

<p>" . news_a_href_page('view3dscene 3.2', 'view3dscene') . " is released,
with a lot of improvements and optimizations for dynamic VRML/X3D worlds.
As usual, this is accompanied by " . news_a_href_page('underlying
Kambi VRML game engine 1.6.0', 'kambi_vrml_game_engine') . " release.
Major changes:

<ul>
  <li><p>Our spatial data structure has undergone serious
    rework, which in English means that <b>your whole scene
    can be now much more dynamic</b>. Everything can move and transform,
    and things will work smoothly
    and fast. All collision detection routines will \"see\"
    the most current scene state, which means that you
    can e.g. click on moving targets, and you will fall down if a hole
    opens under your feet, and generally you can interact with every
    dynamic part of your scene without problems.</p>

    " . (!HTML_VALIDATION ?
    '<table align="right"><tr><td>
       <object width="300" height="243"><param name="movie" value="http://www.youtube.com/v/qtrSIisc6do"></param><param name="allowFullScreen" value="true"></param><param name="allowscriptaccess" value="always"></param><embed src="http://www.youtube.com/v/qtrSIisc6do" type="application/x-shockwave-flash" allowscriptaccess="always" allowfullscreen="true" width="300" height="243"></embed></object>
     </td></tr></table>' : '')
    . "


    <p>I prepared a video showing a simple dynamic world written in X3D and played with view3dscene, see it on the right. The video is only a poor substitute for actually running and playing with this yourself, feeling the smoothness of all editing (the poor framerate of the video is only because of the capturing process...). So after downloading view3dscene, you're welcome to also download this <a href=\"http://svn.code.sf.net/p/castle-engine/code/trunk/kambi_vrml_test_suite/x3d/dynamic_world.x3dv\">demo dynamic_world.x3dv</a> (or just grab it along with the rest of " . news_a_href_page('Kambi VRML test suite', 'kambi_vrml_test_suite') .
    ") and open it. It shows how you can edit the world by KambiScript, how changing transformations works fast, and how it all cooperates with collision detection &mdash; whatever scene you will build, your avatar will move honoring collision detection.

  <li><p>Changing <b><code>Switch.whichChoice</code> is greatly optimized</b>.
    Previously this was very costly operation, now it's instantaneous
    (almost no work is required).
    Many other optimizations were done for dynamic worlds, including
    <b>faster <code>Transform</code> traversing</b>, faster sensor's enabled switching,
    more reliable starting of time-dependent nodes and many more.

    <p>Thanks to <a href=\"http://www.de-panther.com/\">De-Panther</a> for
    pushing me to implement a lot of these features!


" .
castle_thumbs(array(
  array('filename' => 'shadows_dynamic_2.png', 'titlealt' => 'Dynamic shadows screenshot'),
))
. "

  <li>
    <p><b>Dynamic shadows support is greatly improved</b>, finally
    " . news_a_href_page('view3dscene', 'view3dscene') . " can render
    with shadows, honoring our <a href=\"http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_shadows\">shadow's extensions</a>.
    We also have new <a href=\"http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_shadow_caster\">shadowCaster</a> extension.
    Oh, and shadows rendering with
    transparent objects is fixed. Just try the file
    <code>x3d/kambi_extensions/shadows_dynamic.x3dv</code> from
    " . news_a_href_page('Kambi VRML test suite', 'kambi_vrml_test_suite') . "
    in new view3dscene.

    <p>view3dscene has now view mode <i>Fill Mode -&gt;
    Silhouette and Border Edges</i>, this is very handy for checking
    whether your models are correctly manifold (this is important for
    shadow volumes).

  <li><p><code>ProximitySensor.orientation_changed</code>,
    <code>X3DSequencerNode</code>,
    <code>BooleanSequencer</code>, <code>IntegerSequencer</code> implemented.

    <p><a href=\"http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_alpha_channel_detection\">alphaChannel extension field</a> added to all texture nodes.

  <li><p>Bugfix for open file dialog under GTK 2.14 (like Ubuntu 8.10).
    Thanks to Graham Seed for reporting.
</ul>
"),

    array('title' => 'Precomputed Radiance Transfer using our engine',
          'year' => 2008,
          'month' => 11,
          'day' => 9,
          'guid' => '2008-11-09',
          'short_description' => '',
          'description' =>

castle_thumbs(array(
  array('filename' => 'chinchilla_normal.png', 'titlealt' => 'Normal OpenGL lighting'),
  array('filename' => 'chinchilla_simple_occlusion.png', 'titlealt' => 'Rendering with simple ambient occlusion'),
  array('filename' => 'chinchilla_diffuse_prt.png', 'titlealt' => 'Precomputed Radiance Transfer'),
))
. "

<p>I implemented a demo of <a href=\"http://en.wikipedia.org/wiki/Precomputed_Radiance_Transfer\">Precomputed Radiance Transfer</a> using our engine.</p>

<p>In a few words, this is a technique to make very cool self-shadowing by soft shadows under dynamic lighting. (Actually it's possible to go much further, see the papers about PRT <a href=\"http://svn.code.sf.net/p/castle-engine/code/trunk/castle_game_engine/examples/radiance_transfer/README.txt\">linked from my README</a>). You can see the screenshots on the right: 1st shows normal OpenGL lighting (without PRT), 2nd shows the simple ambient occlusion per-vertex (this is, in some sense, a special case of PRT), and the 3rd screenshot shows PRT technique in all it's glory.</p>

<p>The full source code is available, naturally. Simple instructions:</p>

<pre>
$ svn checkout http://svn.code.sf.net/p/castle-engine/code/trunk/castle_game_engine/
$ cd castle_game_engine/examples/radiance_transfer
$ ./radiance_transfer_compile.sh
$ ./radiance_transfer models/chinchilla_with_prt.wrl.gz
</pre>

<p>(Update in 2013: Usually, instead of using these commands,
you should just go and download
the <a href=\"http://castle-engine.sourceforge.net/engine.php\">latest
Castle Game Engine release</a> and compile / run the radiance_transfer
example there.)

<p>Inside that directory there are also other models ready to test with
PRT. There's also <code>precompute_radiance_transfer</code> to process
any 3D model (readable by my engine &mdash; VRML, X3D, 3DS, Wavefront,
Collada...) into a VRML model that can be displayed using
<code>radiance_transfer</code> with PRT effects. There's also <code>show_sh</code>
program to view 25 first <a href=\"http://en.wikipedia.org/wiki/Spherical_harmonics\">spherical harmonics</a> (this will be useful if you'll want to understand how PRT works :) )."),

    array('title' => 'view3dscene 3.1, engine 1.5 release: Scripting, VRML browser components, and more',
          'year' => 2008,
          'month' => 10,
          'day' => 15,
          'guid' => '2008-10-15',
          'short_description' => '',
          'description' =>
news_a_href_page('view3dscene 3.1.0', 'view3dscene') . " release,
along with " . news_a_href_page('underlying
Kambi VRML game engine 1.5.0', 'kambi_vrml_game_engine') . " release.
Most notable improvements are:

" .
castle_thumbs(array(
  array('filename' => 'kambi_script_ball_game.png', 'titlealt' => 'Simple game implemented in pure X3D with KambiScript'),
  array('filename' => 'kambi_script_particles.png', 'titlealt' => 'Particle engine programmed in pure X3D with KambiScript'),
  array('filename' => 'kambi_script_edit_texture.png', 'titlealt' => 'Texture editor (pure X3D with KambiScript)'),
))
. "

<ul>
  <li><p><b>Scripting in " . news_a_href_page('KambiScript language',
    'kambi_script') . "</b>. KambiScript is a simple scripting language,
    invented specially for our engine. It's powerful
    enough for many tasks, you can process all VRML data types
    with it (including vectors, matrices, arrays, images).</p>

    <p>Screenshots on the right show example uses of KambiScript.
    Endless possibilities are available now for VRML authors, you can
    write complete interactive 3D games and run them with view3dscene
    (or any other VRML browser using our engine).
    " . news_a_href_page('Kambi VRML test suite 2.3.0', 'kambi_vrml_test_suite') .
    " contains source VRML files with KambiScript tests (see <code>kambi_script/</code>
    in there, like
    <a href=\"http://svn.code.sf.net/p/castle-engine/code/trunk/kambi_vrml_test_suite/kambi_script/ball_game.x3dv\">kambi_script_ball_game.x3dv</a>
    or
    <a href=\"http://svn.code.sf.net/p/castle-engine/code/trunk/kambi_vrml_test_suite/kambi_script/particles.x3dv\">kambi_script_particles.x3dv</a>),
    you can simply open them in view3dscene.</p></li>

  <li><p><b>Animating camera by animating Viewpoint position</b> (or it's transformation)
    works.</p></li>

  <li><p>Various <b>navigation improvements for scripted worlds</b>:
    <code>NavigationInfo.type = \"NONE\"</code> and
    <code>NavigationInfo.speed = 0</code> cases are supported.
    They are useful when you implement whole navigation yourself, by <code>KeySensor</code>
    and scripting.</p>

    <p>Also view3dscene key shortcuts changed, to allow easily avoiding
    collisions with keys that you handle through <code>KeySensor</code> and scripting.
    All menu shortcuts are now with <i>Ctrl</i> modifier
    (for example, previously you switched collision detection with <i>C</i>,
    now you have to press <i>Ctrl+C</i>).</li>

<!--
   Some minor improvements were made to
    to generate correct events on both key down and key up, with both TGLWindow
    and Lazarus component.

    simple nodes <code>Circle2D</code>,
    <code>TextureTransformMatrix3D</code>, <code>TextureTransform3D</code>,
    <code>MultiTextureTransform</code>.</p>
-->


    <!--
    Changing LineSet and PointSet_2 through VRML events fixed.
    KeySensor fixes:
    - send lowercase letters (for Lazarus component) when shift not pressed
    -->

  <li><p>For programmers using our engine, we have <b>VRML browser
    components</b>. Two flavors: <code>TGLWindowVRMLBrowser</code> (a descendant
    of our <code>TGLWindow</code>) and, for Lazarus LCL, <code>TKamVRMLBrowser</code>
    (a descendant of <code>TOpenGLControl</code>). Using them is trivial,
    just drop <code>TKamVRMLBrowser</code> on the form
    and call it's <code>Load</code> method &mdash; whole rendering and navigation
    will automatically work. Other Lazarus packages
    fixes were made, to make them more easily usable. Thanks to De-Panther for
    pushing me to implement this :)</p></li>

  <li><p><b><a href=\"http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_script_compiled\">Script
    protocol \"<code>compiled:</code>\"</a></b> is implemented, to easily link
    VRML scripts with compiled-in (written in ObjectPascal) handlers.</p></li>

  <li><p>Other improvements: using quaternions for <code>EXAMINE</code> navigation,
    simple nodes <code>Circle2D</code>, <code>TextureTransformMatrix3D</code>,
    <code>TextureTransform3D</code>, <code>MultiTextureTransform</code>.

  <!--

    glplotter 1.2.2 and gen_function 1.0.3 released, to update
    kambiscript expressions handling inside.
  -->
</ul>

<p><i>Plans for the next release</i>: first of all octree updating problems
will be solved.
Right now they are a weak point of our engine &mdash; animating geometry
by VRML events is unnecessarily time-consuming when collision detection has to be up-to-date.
Also more rendering optimizations for animating by VRML events
will be done (optimize case when you often change <code>Switch</code>
choice, automatically detect when roSeparateShapeStates /
...NoTransform / roNone is suitable).</p>
"),

    array('title' => 'view3dscene 3.0 release: X3D, events, MovieTexture, and more',
          'year' => 2008,
          'month' => 9,
          'day' => 12,
          'guid' => '2008-09-12',
          'short_description' => '',
          'description' =>
"<p>I'm pleased to present the new, shiny
" . news_a_href_page('view3dscene 3.0', 'view3dscene') . " release,
with a lot of new <a href=\"http://www.web3d.org/\">VRML/X3D</a> features
implemented. Also " . news_a_href_page('underlying Kambi VRML game engine 1.4.0', 'kambi_vrml_game_engine') . "
is released and some other programs here get minor updates.</p>

<p>New features of the engine and view3dscene:</p>

" . castle_thumbs(array(
  array('filename' => 'deranged_house_final_0.png', 'titlealt' => 'ProximitySensor in action'),
  array('filename' => 'ikea_bead_toy.png', 'titlealt' => 'Animation (IkeaBeadToy model from www.web3d examples)'),
))
. "

<ul>
  <li><p><b>X3D support</b> (both XML and classic encoding).
    Our " . news_a_href_page('VRML implementation status', 'x3d_implementation_status') . "
    page has detailed information about supported features.</p></li>

  <li><p><b>Events mechanism</b> (routes, exposed events, sensors, interpolators etc.)
    is implemented. This allows you to define interactions and animations
    of 3D world within a single VRML/X3D file, as envisioned in
    the specifications.</p>

    <p>Four basic sensors are implemented now: <code>TimeSensor</code>,
    <code>TouchSensor</code>, <code>KeySensor</code> and <code>ProximitySensor</code>.
    Also <code>Anchor</code> is \"clickable\" now. For now you have to be in
    <code>Walk</code> mode with <i>Collision Checking</i>
    enabled to have picking (<code>TouchSensor</code>, <code>Anchor</code>) working.</p>

    <p>Linear interpolators are also implemented.
    Some \"event utilities\" nodes are implemented
    (including useful <a href=\"http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_avalon\"><code>Logger</code>
    node from Avalon extensions</a>).
    Events to work with bindable nodes (Background, Fog and such) work.
    <a href=\"http://castle-engine.sourceforge.net/x3d_implementation_shaders.php\">Routing
    events to GLSL shaders uniform variables works perfectly.</a>.
    Events to control behavior of <code>Inline</code> (and <code>InlineLoadControl</code>
    for VRML 97) work too.
    Prototypes and external prototypes also work 100% with events according
    to specification, so you can pass events to/from prototypes.
    New " . news_a_href_page('Kambi VRML test suite 2.2.0', 'kambi_vrml_test_suite') . "
    has some simple demos of our events implementation.</p></li>

  <li><p><b>MovieTexture</b> is handled, with very useful <a href=\"http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_movie_from_image_sequence\">extension
    to load movie from a sequence of images (with possible alpha
    channel)</a>, this is great for pre-rendered animations of flames, smoke etc.
    Normal movie formats are also handled if <a href=\"http://ffmpeg.mplayerhq.hu/\">ffmpeg</a>
    is installed and available on \$PATH.</p></li>

  <li><p><b>Recording movies</b> from view3dscene is possible.
    This allows recording 3D animations to a movie file
    (with perfect quality, as opposed to using independent
    programs that capture OpenGL output).</p></li>

  <li>
" . castle_thumbs(array(
  array('filename' => 'view3dscene_thumbnailer_demo.png', 'titlealt' => '&quot;view3dscene&quot; as nautilus thumbnailer'),
))
. "
    <p><a href=\"http://www.gnome.org/\">GNOME</a> users will be happy to
    hear that view3dscene can be easily used as nautilus thumbnailer,
    so you can see thumbnails of your VRML / X3D and other 3D model files.</p></li>

  <li><p>Many other features, including
    <ul>
      <li><code>Extrusion</code> node handling,</li>
      <li><a href=\"http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_blending\"><code>BlendMode</code> extension</a>,
      <li><a href=\"http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_kambi_inline\"><code>KambiInline</code> extension
        to automatically replace nodes within inlined content</a>,</li>
      <li><a href=\"http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_time_origin_at_load\">extension
        to force VRML time-origin to start at loading time</a>, because
        <a href=\"http://castle-engine.sourceforge.net/x3d_time_origin_considered_uncomfortable.php\">standard
        VRML time origin is uncomfortable in my opinion,</a></li>
      <li>new X3D Indexed Triangles/Quads primitives (thanks to completely
        reorganized mesh renderer code),</li>
      <li>HAnim nodes support.</li>
    </ul>
  </li>

  <li><p>" . news_a_href_page('"The Castle" 0.8.2', 'castle') . " release also deserves
    mention, as it fixes <b>support for OpenAL Soft</b> implementation
    (available on newer Linux distros).
    It also allows creators of future levels and creatures to use X3D,
    animating with standard VRML/X3D interpolators,
    defining level interactions by VRML events etc.</li>
</ul>

<p>Have fun! You may also enjoy reading <a href=\"http://news.hiperia3d.com/2008/09/interview-michalis-kamburelis-developer.html\">an
interview with me about our VRML engine on Hiperia3D News (regards go to
Jordi R. Cardona!)</a>.</p>"),

    array('title' => 'VRML / X3D events and routes implemented',
          'year' => 2008,
          'month' => 8,
          'day' => 15,
          'guid' => '2008-08-15',
          'short_description' => '',
          'description' =>

castle_thumbs(array(
  array('filename' => 'laetitia_sprints_demo.png', 'titlealt' => 'Laetitia Sprints by X3D TimeSensor + CoordinateInterpolator', 'linktarget' => CURRENT_URL .  'movies/laetitia_sprints.avi'),
))
. "

<p>An extremely important feature of VRML / X3D is finally implemented:
routes and events mechanism works. This means that you can express animations and interactions
within single VRML / X3D file, like envisioned in the specifications.</p>

<p>We have 2 sensor nodes working already (<code>TimeSensor</code>
and <code>KeySensor</code>), 7 linear interpolator nodes, and 5 event utilities
nodes (including <a href=\"http://instant-reality.com/documentation/nodetype/Logger/\">Avalon
<code>Logger</code> node</a>, a useful debugger for events).
All exposed fields of other nodes also work, obviously.
This is all available only in SVN for now. When I get back from vacation
(at the end of August) this work will be continued (many other sensors
are easy to implement now, and some existing code should be cleaned and
optimized) and it will all be released as <i>view3dscene 3.0</i>.</p>

<p>As a demo, see the 6-second movie on the right. It shows
animation in X3D done by routing
<code>TimeSensor</code> to <code>CoordinateInterpolator</code> to <code>IndexedFaceSet</code>.
The model is <a href=\"http://www.web3d.org/x3d/content/examples/Basic/StudentProjects/\">\"Laetitia Sprints\" from
web3d.org examples</a>.</p>"),

    array('title' => 'News - white_dune, X3D, movie textures, engine icon',
          'year' => 2008,
          'month' => 7,
          'day' => 15,
          'guid' => '2008-07-15',
          'short_description' => '',
          'description' =>
'Various exciting news about development of our engine:

<ul>
  <li><p><a href="http://wdune.ourproject.org/">White dune</a>,
    free software VRML 97 modeler,
    can export normal VRML animations
    (expressed in terms of VRML interpolators) to our ' .
    news_a_href_page('Kanim (Kambi animations) file format', 'kanim_format') .
    ' and it supports our ' .
    news_a_href_page('extension nodes and fields', 'x3d_extensions') .
    ' (run with <code>-kambi</code> command-line option, or use <i>"Start next time
    with kambi support"</i> menu item). Thousand thanks for
    Joerg "MUFTI" Scheurich!</p>
  </li>

  <li><p>Among the many new features already implemented in SVN are:</p>

    <ul>
      <li><p>Reading X3D files, with all 40 X3D components,
        in both XML and classic VRML encodings,
        is implemented.</p>

        <p>Besides all features from VRML 2.0, many X3D-specific
        features are already supported, like
        geometric primitives <code>[Indexed][Triangle/Quad][Fan/Strip]Set</code> (8 nodes total).
        Rendering internals were reorganized into much smarter hierarchy, to handle
        these new X3D nodes as well as <code>IndexedFaceSet</code> and other VRML 97 and 1.0
        nodes implemented since a long time.</p>
      </li>

      <li><p><code>Extrusion</code> node handling.</p></li>

      <li><p>New extensions, like <code>BlendMode</code> node (a subset of
        <a href="http://www.instantreality.org/documentation/nodetype/BlendMode/">Avalon BlendMode node</a>)
        and <code>KambiInline</code> (an Inline that can somewhat
        process the inlined content).</p></li>

      <li>

        ' . (!HTML_VALIDATION ?
        '<table align="right"><tr><td>
           <object width="200" height="167"><param name="movie" value="http://www.youtube.com/v/V-EJvVbi1DQ"> </param> <embed src="http://www.youtube.com/v/V-EJvVbi1DQ" type="application/x-shockwave-flash" width="200" height="167"> </embed> </object>
         </td></tr></table>' : '')
        . '

        <p>Texture department:
        Textures with full alpha channel are now nicely rendered with blending
        (and textures will simple alpha channel are still detected and rendered
        faster by alpha_test). Moreover, <code>MovieTexture</code> node is now
        handled (movie can be read from image sequences, like <code>image%d.png</code>,
        and from normal movie formats thanks to <a href="http://ffmpeg.mplayerhq.hu/">ffmpeg</a>).
        As a demo, see the flames animation on the right.</p>

      <li><p>Flames movie above was not only played in our ' .
        news_a_href_page('view3dscene', 'view3dscene') . ', it was also
        recorded directly by view3dscene. That\'s right: Screenshot options
        were much improved, it\'s now possible to capture animation
        as a movie file (with perfect quality, as opposed to using independent
        programs that capture OpenGL output).</p>

' . castle_thumbs(array(
  array('filename' => 'view3dscene_thumbnailer_demo.png', 'titlealt' => '&quot;view3dscene&quot; as nautilus thumbnailer'),
))
. '
        <p><a href="http://www.gnome.org/">GNOME</a> users will be happy to
        hear that view3dscene can be easily used as nautilus thumbnailer,
        so you can see thumbnails of your VRML / X3D and other 3D model files
        (see the screenshot).
        </p>
      </li>
    </ul>
  </li>

  <li><p>We have an icon for our engine and view3dscene.
    Next view3dscene release will be nicely integrated with GNOME
    (and other desktops that support relevant freedesktop specs).
    You can already appreciate engine icon at the top corner of our main page.
    Thanks to Kasia Obrycka for icon improvements!</p>
</ul>'),

    array('title' => 'Demo movies',
          'year' => 2008,
          'month' => 5,
          'day' => 9,
          'guid' => '2008-05-09',
          'short_description' => '',
          'description' =>
"I present " . news_a_href_page('three demo movies', 'movies') . "
showing off my engine. Feast your eyes on!

<p>In related news, development of the engine goes on.
Some of the latest improvements include
<ul>
  <li><i>X3D XML handling</i>. Next release
    will include support for X3D (both XML and classic encoding) for all
    programs.
  <li><i>File filters in open/save dialogs</i>.
    In GTK 2 (by GtkFileChooserDialog) and Windows (WinAPI) backends.
  <li>Passing kambi_time to GLSL shaders, allowing <i>shaders to perform
    various beautiful animations</i>.
</ul>"),

    array('title' => 'Engine 1.3.1 release (Lazarus packages fixed)',
          'year' => 2008,
          'month' => 2,
          'day' => 25,
          'guid' => '2008-02-25',
          'short_description' => '',
          'description' =>
"Released " . news_a_href_page('engine version 1.3.1', 'kambi_vrml_game_engine') . ":
fixed Lazarus packages compilation, for developers that want to use our
engine with Lazarus."),

    array('title' => 'Engine 1.3.0 release, view3dscene 2.4.0, castle 0.8.1, many other releases',
          'year' => 2008,
          'month' => 2,
          'day' => 19,
          'guid' => '2008-02-19',
          'short_description' =>
"<p>Many long-awaited graphic features implemented in our engine.
Released " . news_a_href_page('engine version 1.3.0', 'kambi_vrml_game_engine') . ",
" . news_a_href_page('view3dscene 2.4.0', 'view3dscene') . " and
" . news_a_href_page('castle 0.8.1', 'castle') . ".
Below is only a shortcut of the most important changes
(see " . news_a_href_page('changes_log', 'news') . " for a full list of changes) :</p>

<ul>
  <li><a href=\"http://castle-engine.sourceforge.net/x3d_extensions.php#ext_bump_mapping\">Bump
    mapping</a>. Various bump mapping methods are implemented,
    the most advanced being steep parallax mapping with self-shadowing.</li>

  <li>Shaders support, including <a href=\"http://castle-engine.sourceforge.net/x3d_extensions.php#ext_shaders\">specifying GLSL
    shaders in VRML</a>. Programmers may easily initialize
    GLSL and ARB assembly shaders.</li>

  <li>Anti-aliasing available in both "
    . news_a_href_page('view3dscene', 'view3dscene') . "
    and " . news_a_href_page('castle', 'castle') . ".</li>

  <li>Collada model format basic support (1.3.x and 1.4.x)
    added to the engine, you can also convert Collada files to VRML 2.0.</li>

  <li><i>Examine</i> mode allows to rotate and move the scene by mouse
    dragging.</li>

  <li><code>--screenshot</code> command-line option for
    " . news_a_href_page('view3dscene', 'view3dscene') . ",
    to take screenshots of the scene in batch mode.</li>

  <li>" . news_a_href_page('Our Blender VRML 97 exporter script', 'blender') . "
    improved: <i>set solid / set smooth / autosmooth / autosmooth degrees</i>
    settings from Blender are correctly exported to VRML.</li>
</ul>

<p>Other releases:
" . news_a_href_page('Kambi VRML test suite 2.1.0', 'kambi_vrml_test_suite') . "
has many new tests/demos for new features (bump mapping, GLSL,
Collada format). Also released most other programs,
to bring them up-to-date with current engine state.</p>
",
          'description' =>

"<p>Released " . news_a_href_page('engine version 1.3.0', 'kambi_vrml_game_engine') . ",
" . news_a_href_page('view3dscene 2.4.0', 'view3dscene') . " and
" . news_a_href_page('castle 0.8.1', 'castle') . ".
Many long-awaited graphic features implemented:</p>

<ul>
  <li><p><b>Bump mapping</b>: <a href=\"http://castle-engine.sourceforge.net/x3d_extensions.php#ext_bump_mapping\">VRML
    renderer allows bump mapping</a>. Various bump mapping methods
    are implemented (best method is auto-detected and used at runtime):
    dot by multitexturing (not normalized and normalized by cube map),
    dot by GLSL (optionally with parallax mapping, optionally with
    steep parallax mapping and self-shadowing).</p>

    <p>" . news_a_href_page('view3dscene', 'view3dscene') . " allows to easily
    turn on bump mapping, assuming model specifies normal maps.<br/>
    " . news_a_href_page('castle', 'castle') . " uses bump mapping, for now only
    on the \"fountain\" level.</p>

    <p><i>For programmers</i>: see also <code>kambi_vrml_game_engine/examples/vrml/bump_mapping</code> demo in engine sources,
    it demonstrates emboss, dot and all other bump mapping
    methods built in VRML engine. Also my notes about emboss and dot
    (by multitexturing) bump mapping methods may be interesting.</p>

    <p>(Update in 2013: bump_mapping demo is no longer available,
    the whole bump mapping support is now integrated in Castle Game Engine,
    can be tested using many of it's demos, e.g. try
    " . news_a_href_page('view3dscene', 'view3dscene') . "  on
    " . news_a_href_page('demo models', 'demo_models') . ".)
  </li>

  <li><p><b>GLSL shaders support:</b> engine supports easily using
    ARB vertex / fragment programs (assembly shaders) and&nbsp;GLSL.</p>

    <p>You can also <a href=\"http://castle-engine.sourceforge.net/x3d_extensions.php#ext_shaders\">directly
    specify GLSL shaders inside VRML file</a>, which is a great feature
    for VRML authors. Syntax of shaders in VRML follows X3D specification.</p>

    <p>" . news_a_href_page('view3dscene', 'view3dscene') . " allows to control
    GLSL shaders and even simply assign GLSL shaders
    (check out <i>Edit -&gt; Simply assign GLSL shader to all objects</i>
    menu item), so you can test your shaders with any 3D model.</p>

    <p><i>For programmers</i>: you may find useful my notes about shading languages in
    <a href=\"http://michalis.ii.uni.wroc.pl/wsvn/michalis/obscure_castle_engine_demos_and_tools/shading_langs/README.txt\">shading_langs_demo/README</a>.</p>

  <li><p><b>Anti-aliasing</b> available (if multisampling is supported by
    graphic card). " . news_a_href_page('view3dscene', 'view3dscene') . "
    has comfortable menu <i>File -&gt; Startup Preferences -&gt; Anti aliasing</i>
    and also a command-line option <code>--anti-alias</code> to control this,
    " . news_a_href_page('castle', 'castle') . " has comfortable menu item
    in <i>Video options</i>.</p>

  <li><p><b>Collada model format</b> basic support (1.3.x and 1.4.x)
    added to the engine (" . news_a_href_page('view3dscene', 'view3dscene') . "
    and everything else open Collada files, just like any other model format;
    you can also convert Collada files to VRML 2.0).</p>
  </li>

  <li><p>Wavefront OBJ format handling improved (we handle normal vectors,
    materials, textures).</p>
  </li>

  <li><p><code>Examine</code> mode allows to rotate and move the scene by mouse
    dragging. This is more intuitive and sometimes comfortable.
    This feature is mostly noticeable in
    " . news_a_href_page('view3dscene', 'view3dscene') . ", although
    some example programs in engine demos also use such examine mode
    and also benefit from this.</p>
  </li>
</ul>

<p>Besides improvements above, some improvements specific to
" . news_a_href_page('view3dscene', 'view3dscene') . ":</p>

<ul>
  <li><code>--screenshot</code> command-line option, requested a few times,
    to take screenshots of the scene in batch mode.</li>
  <li>Various <i>fill modes</i>. Previously only <i>normal</i> and
    <i>wireframe</i> were available, now we have a couple more,
    like solid wireframe and silhouette. Keys changed (\"w\" key no longer works,
    \"f\" key changes fill mode instead of fog state).</li>
</ul>

<p>Also " . news_a_href_page('our Blender VRML 97 exporter script', 'blender') . "
improved: <i>set solid / set smooth / autosmooth / autosmooth degrees</i>
settings from Blender are correctly exported to VRML file (as creasteAngle field).</p>

<p>Other most notable internal engine changes:</p>

<ul>
  <li>I dropped my custom OpenGLh binding in favor of using GL, GLU, GLExt units
    from FPC. <i>Full story:</i> OpenGLh was developed when FPC had no usable OpenGL binding unit
    (neither had Delphi)... Times have changed, and current GL, GLU, GLExt
    units are usable, and (partially thanks
    <a href=\"http://www.mail-archive.com/fpc-devel@lists.freepascal.org/msg02328.html\">to</a>
    <a href=\"http://www.freepascal.org/mantis/view.php?id=7570\">my</a>
    <a href=\"http://www.freepascal.org/mantis/view.php?id=7600\">patches</a>,
    <a href=\"http://bugs.freepascal.org/view.php?id=10460\">quite</a>
    <a href=\"http://bugs.freepascal.org/view.php?id=10507\">a</a>
    <a href=\"http://bugs.freepascal.org/view.php?id=10508\">few</a> :) )
    they work good and support OpenGL 2.0 functions.
    (While OpenGLh was on the level of GL 1.2 + many extensions).</li>

  <li>Among many new demo programs, there's also
    <code>kambi_vrml_game_engine/examples/vrml/plane_mirror_and_shadow.lpr</code>
    to test plane-projected shadows and plane mirrors. Plane-projected shadows
    is only for simple demo (we have implemented shadow volumes, thousand times better
    algorithm, after all), but plane mirrors will be implemented
    in the future in the VRML engine (using \"mirror\" <code>Material</code>
    field automatically).</li>
</ul>

<p>Other releases:
" . news_a_href_page('Kambi VRML test suite 2.1.0', 'kambi_vrml_test_suite') . "
has many new tests/demos for new features (bump mapping, GLSL,
Collada format). Also released:
" . news_a_href_page('rayhunter 1.2.2', 'rayhunter') . ",
" . news_a_href_page('lets_take_a_walk 1.2.1', 'lets_take_a_walk') . ",
" . news_a_href_page('malfunction 1.2.4', 'malfunction') . ",
" . news_a_href_page('kambi_lines 1.1.4', 'kambi_lines') . ",
" . news_a_href_page('glplotter 1.2.1', 'glplotter_and_gen_function') . ",
" . news_a_href_page('glViewImage 1.2.2', 'glviewimage') . ",
" . news_a_href_page('bezier_curves 1.1.6', 'bezier_curves') . ",
" . news_a_href_page('glcaps 1.1.2', 'glcaps') . ",
mainly to bring them up-to-date with current engine state.</p>
")
);
