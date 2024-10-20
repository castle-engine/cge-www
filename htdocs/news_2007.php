<?php

array_push($news,
    array('title' => 'castle 0.8.0, view3dscene 2.3.0 released',
          'year' => 2007,
          'month' => 11,
          'day' => 17,
          'guid' => '2007-11-17',
          'short_description' =>
"<p>A lot of updates today. Here's a shortcut of only the most important changes
(see " . news_a_href_page('news', 'news') . " for a full list of changes) :
<ul>
  <li>" . news_a_href_page('"The Castle" 0.8.0', 'castle') . " released:
    new demo level <i>the fountain</i> (VRML 2.0, dynamic shadows),
    many shadows improvements (z-fail, proper detection z-pass/z-fail, shadow
    culling etc.), conserve memory feature (all Radeon issues should be fixed
    now).</li>
  <li>" . news_a_href_page('view3dscene 2.3.0', 'view3dscene') . " released:
    prototypes (both <code>PROTO</code> and <code>EXTERNPROTO</code>),
    VRML 2.0 lights are correctly handled,
    handling of colors for <code>IndexedFaceSet</code> and <code>IndexedLineSet</code>,
    <a href=\"https://castle-engine.io/x3d_extensions.php#ext_text3d\">Text3D extension</a>.</li>
  <li>" . news_a_href_page('Kambi VRML game engine 1.2.0', 'kambi_vrml_game_engine') . "
    released: most things mentioned above were actually implemented in the base
    engine units, also: x86-64 port.</li>
  <li>" . news_a_href_page('Kambi VRML test suite 2.0.0', 'kambi_vrml_test_suite') . "
    released: many new tests for new features.</li>
  <li>" . news_a_href_page('Blender VRML stuff page added, with improved VRML 2.0
    exporter and kanim exporter', 'blender') . ".</li>
  <li>Updated version of " . news_a_href_page('VRML engine documentation',
    'engine_doc') . " is available, with a
    <a href=\"https://castle-engine.io/vrml_engine_doc/output/xsl/html/chapter.shadows.html\">chapter
    about shadows implementation.</a></li>
</ul>",
          'description' =>

"<p>" . news_a_href_page('"The Castle" 0.8.0', 'castle') . " released:

<ul>
  <li><p>New demo level: <i>the fountain</i>, done in pure VRML 2.0
    format (no more VRML 1.0). Shadows for whole level are generated dynamically.
    In the next release, this level is supposed to be augmented with some
    eye candy graphical effects, for now enjoy VRML 2.0 and shadows :)</p></li>

  <li><p>Shadows improvements (see also
    <a href=\"https://castle-engine.io/vrml_engine_doc/output/xsl/html/chapter.shadows.html\">new chapter
    in documentation about shadows</a>) :</p>

    <ul>
      <li>First of all, z-fail implemented and proper detection when z-fail
        is needed implemented, so faster z-pass is used when possible.
        \"The Castle\" shows (toggle with Tab, just like for FPS) number
        of shadows qualified as z-pass, z-fail, z-fail with light cap needed etc.
      <li>Shadow volumes silhouette optimization improved: now models don't have
        to be perfect manifold to use this. See
        <code>kambi_vrml_game_engine/examples/vrml/shadow_volume_test/</code>
        demo, in particular the <code>shadow_volume_test_ball_with_tentacles.sh</code>
        example.</li>
      <li>Much better frustum culling for shadows.</li>
    </ul>
  </li>

  <li><p>Arrows are affected by gravity, and underwater \"sick\" projection
    effect, thanks to Grzegorz Hermanowicz (herrmannek).</li>

  <li><p>Numerous memory and speed optimizations to load VRML models and
    animations faster and better (thanks to valgrind (callgrind, massif)).
    Also in \"The Castle\" there's new <i>Conserve memory</i> feature
    (this basically means that only creature animations needed for current
    level are kept in memory), turned on by default.</p>

    <p>So \"Loading creatures\" is much less resource consuming.
    And finally pretty much all Radeon issues are fixed now.</li>

  <li>Fixed hang (actually, a really really long delay) when closing sound device
    on Linux (actually, with OpenAL sample implementation).</li>

  <li>Demo levels are available directly from \"New game\" menu now.</li>
  <li>Nicer credits screen.</li>
</ul>

<p>" . news_a_href_page('view3dscene 2.3.0', 'view3dscene') . " released:

<ul>
  <li>Prototypes (both <code>PROTO</code> and <code>EXTERNPROTO</code>)
    VRML 2.0 feature is fully implemented now !</li>
  <li>VRML 2.0 lights are correctly handled (<code>DirectionalLight</code>
    affects every sibling, positional lights affect whole scene taking
    <code>radius</code> into account).</li>
  <li><code>ROUTE</code> constructs of VRML 2.0 are parsed now.
    Although they still don't actually <b>do</b> anything.
    So at least scenes using routes are partially handled (routes are simply
    ignored), instead of just producing an error.</li>
  <li>Default blending dest factor for view3dscene is
    <code>GL_ONE_MINUS_SRC_ALPHA</code>, since this is expected by most VRML
    authors.</li>
  <li>VRML files compressed by gzip are handled OK even if they have
    normal <code>.wrl</code> extension.</li>
  <li>--write-to-vrml fixed</li>
  <li>Handling of colors (<code>color</code>, <code>colorPerVertex</code>,
    <code>colorIndex</code>) for <code>IndexedFaceSet</code> and <code>IndexedLineSet</code>
    done.</li>
  <li>NavigationInfo.speed is now handled correctly (it sets speed per second)</li>
  <li><a
    href=\"https://castle-engine.io/x3d_extensions.php#ext_text3d\">Text3D extension</a>.</li>
</ul>

<p>" . news_a_href_page('Kambi VRML game engine 1.2.0', 'kambi_vrml_game_engine') . "
released. Most features mentioned above for view3dscene and castle
(shadows, optimizations, all VRML 2.0 features) are actually implemented
in the engine, and other programs only use them. Additionally, some
more internal features not mentioned above:</p>

<ul>
  <li><p>Engine is ported and works flawlessly on x86-64 on Linux.
    No more only 32-bit :) Also, it's partially ported to Windows x84-64
    (tested compilation with cross compiler, no actual run tests).</p>

    <p>This also results in the change of archive binary names:
    they all get <code>i386</code> after their name, eventually I may release
    precompiled versions for <code>x86-64</code> too.</p></li>

  <li><p>GLWindow allows to change cursor shape.</p></li>

  <li><p>Everything is compiled using new FPC 2.2.0.</p></li>
</ul>

<p>" . news_a_href_page('Kambi VRML test suite 2.0.0', 'kambi_vrml_test_suite') . "
released: many new tests to test new features (protos, external protos,
colors, light scope, running path to test NavigationInfo.speed, 3d text),
some important VRML 1.0 tests ported to VRML 2.0 too (castle,
relative_names, texture_test, house behind the glass).</p>

<p>" . news_a_href_page('Blender VRML stuff page added, with improved VRML 2.0
exporter and kanim exporter', 'blender') . ".</p>

<p>Updated version of " . news_a_href_page('VRML engine documentation',
'engine_doc') . " is available, with a chapter about shadows
implementation.</p>
"),

/* --------------------------------------------------------------------------- */

    array('title' => 'glplotter 1.2.0 and view3dscene 2.2.1 released',
          'year' => 2007,
          'month' => 9,
          'day' => 6,
          'guid' => '2007-09-06',
          'short_description' => '',
          'description' =>

"<ul>
  <li>" . news_a_href_page('glplotter 1.2.0 and gen_function 1.0.2',
    'glplotter_and_gen_function') . " released: glplotter GUI greatly improved:
    Open/Add menu items to open graphs from files
    and to generate graphs from function expressions.
    This means that now you don't have to specify graphs at command-line,
    and now you don't have to write pipes with gen_function.
    Also documentation and some options translated finally to English.</li>
  <li>" . news_a_href_page('view3dscene 2.2.1', 'view3dscene') . " released:
    bug fix release. Fixed crash when removing geometry node from
    VRML 2.0 hierarchy. Fixed jagged animation when world time was
    really large (usually occurs when \"on display\" time pass was
    really large for some time). Fixed messing the gravity
    \"up\" vector when reopening the scene.</li>
  <li>" . news_a_href_page('Kambi VRML game engine 1.1.1',
    'kambi_vrml_game_engine') . " released: changes needed by
    view3dscene and glplotter above.</li>
  <li><a href=\"https://castle-engine.io/news_feed.php\">RSS
    feed</a> listing all changes is available now.
    <small>SouceForge already made RSS feeds for our project,
    but they didn't allow me HTML code there, and HTML links
    are simply useful for my news messages.</small></li>
</ul>"),

/* --------------------------------------------------------------------------- */

    array('title' => 'view3dscene 2.2.0 and related releases',
          'year' => 2007,
          'month' => 8,
          'day' => 25,
          'guid' => '2007-08-25',
          'short_description' => '',
          'description' =>

"<ul>
  <li>" . news_a_href_page('view3dscene 2.2.0', 'view3dscene') . " release:
    view3dscene can display animations now (for now in " .
     news_a_href_page(
    "Kanim (Kambi VRML engine animations) format", 'kanim_format') . " and
    MD3).</li>
  <li>" . news_a_href_page('Kambi VRML test suite 1.1.0',
    'kambi_vrml_test_suite') . " release: many kanim demos added.</li>
  <li>" . news_a_href_page('Kambi VRML game engine 1.1.0',
    'kambi_vrml_game_engine') . " release: many changes, for animations
    in view3dscene, also GLMenu and GameSoundEngine units added
    (some \"The Castle\" code improved and moved to a generally-usefull
    units area), bugfixes to MD3 texture handling.</li>
</ul>"),

/* --------------------------------------------------------------------------- */

    array('title' => 'Move to SourceForge finished',
          'year' => 2007,
          'month' => 7,
          'day' => 25,
          'guid' => '2007-07-25',
          'short_description' => '',
          'description' =>

"<p>The move of <i>Kambi VRML game engine</i> project to SourceForge is finished !
In fact, if you're reading this text, then you already view our page
as hosted on SourceForge.</p>

<p>Being on SourceForge gives us many new features, most important ones:
<a href=\"http://sourceforge.net/project/showfiles.php?group_id=200653\">file
downloads</a> use all the power and speed of SF mirrors,
development is done inside
publicly
visible SVN repository, we have a public mailing list,
we have trackers for bugs,
feature requests,
patches,
there's <a href=\"http://sourceforge.net/export/rss2_projfiles.php?group_id=200653\">RSS
feed to monitor new releases</a>.</p>"),

/* --------------------------------------------------------------------------- */

    array('title' => 'Moving to SourceForge: using SF download system',
          'year' => 2007,
          'month' => 7,
          'day' => 23,
          'guid' => '2007-07-23',
          'short_description' => '',
          'description' =>

"<p>Download links for most VRML stuff on this page direct to SourceForge
file release system now. This is another step in moving to
SourceForge.

<p>Also, some things now get version numbers:
" . news_a_href_page('Kambi VRML game engine', 'kambi_vrml_game_engine') . " (1.0.0),
" . news_a_href_page("Kambi VRML test suite", "kambi_vrml_test_suite") . " (1.0.0).
</p>"),

/* --------------------------------------------------------------------------- */

    array('title' => 'Older news',
          'year' => 2007,
          'month' => 7,
          'day' => 19,
          'short_description' => '',
          'description' =>
  '<!-- Older logs are available only in HTML, they were not converted
       to $news format. -->

  <div class="old_news_item"><p id="older_news"><span class="old_news_date">July 19, 2007:</span></p>

    <p>Just to let you know that my whole VRML stuff is on the move
    to <a href="http://sourceforge.net">SourceForge.net</a>.
    See project page on SourceForge.</p>

    <p>I already use their SVN repository to host my code.
    Most of my whole private repository was imported there,
    so even though the repository on SF exists for less than a week, it already
    has 1800+ commits :). If you look close enough, you\'ll notice two games
    visible in the repository that were not released yet on these pages.
    They are available only from SVN sources for now:
    sandbox (a demo of isometric rendering) and "The Rift" (my current small
    project &mdash; a demo in the style of adventure games, with still background
    and 3d players; just a start for now).</p>

    <p>See ' . news_a_href_page('sources', 'sources') . ' and many other
    pages for detailed instructions how to get code out of SVN repository.</p></div>

  <div class="old_news_item"><p><span class="old_news_date">June 12, 2007:</span>
    <p>Finally, the great update happens ! Most important are
    ' . news_a_href_page('"The Castle" 0.7.0', 'castle') . ' and
    ' . news_a_href_page('view3dscene 2.1.0', 'view3dscene') . ' releases,
    and (for programmers) the underlying changes to
    ' . news_a_href_page('Kambi VRML game engine', 'kambi_vrml_game_engine') . '.
    Actually almost all programs on these pages are updated too
    (packaged in a different way, ported to Mac OS X, minor fixes).</p>

    <p>As for the question: <i>why didn\'t you update anything on these pages
    within the last 6 months ?</i>. My answer is: I was imprisoned in a cave.
    By a legion of elves. Really, hundreds of nasty bastards with
    pointy ears kidnapped me and threw me into a dark cave. Just today my cat
    managed to rescue me. :) Seriously, recently my life was just
    pretty occupied &mdash; I\'m on the 1st year of
    <a href="http://www.ii.uni.wroc.pl/cms/pl/teaching/phd_studies.html">Ph.D.
    Studies at the University of Wrocław</a> (in case you didn\'t notice the
    link to "Teaching" stuff at the top of the main page).
    Besides, well, life has been really good lately &mdash; thanks to K.O.
    and the mountains. :)

    <p>"The Castle" user-visible features:
    <ul>
      <li><b>New DOOM E1M1 level.</b>
      <li><b>Ported to Mac OS X.</b>
      <li><b>Shadows much improved.</b> Not totally finished yet
        (still only depth-pass),
        but much corrected and optimized. With good graphic card, the game is
        playable with shadows on. <i>Thanks go to Olgierd Humeńczuk for setting
        me on the right track.</i>
      <li>New controls features:
        <ul>
          <li>You can assign up to two keys and one mouse button for
            each action in "The Castle".
          <li>Default shortcuts changed to more resemble modern FPS games:
            use AWSD moving, left/right arrows rotate,
            E interacts (HalfLife2-like), R drops etc.
          <li>New keybinding to use "potion of life" if owned,
            <!-- (analogy of "m" for "medkit" in tremulous)-->
            default is "l" (lower "L").
            Very handy in the middle of the fight, when you really don\'t have time
            to search your inventory by the "[", "]" keys .
          <li>"Invert vertical mouse look" option.
        </ul>
      <li><code>--screen-size</code> command-line option.
      <li>On "Castle Hall" level: werewolves fight totally changed, to be much more
        interesting.
      <li>On "The Gate" level: teleport made closer, to reduce a little the need
        for jumping, scroll of flying easier to get with sword, doors at the end,
        minor fixes.
      <li>Footsteps sound now changes within level, depending on whether you
        walk on grass or concrete ground.
      <li>Much better demo (background behing main menu). Enjoy.
      <li>Alien model improved (better animation, manifold (for shadows),
        skin texture with ambient occlusion).
    </ul>

    view3dscene user-visible features:
    <ul>
      <li><b>MD3 (Quake3 engine) model format fully supported.</b>
      <li>Radio menu items in view3dscene.
      <li>Ability to select an item (point and it\'s triangle).
      <li>Edit menu items to remove selected geometry node and selected face.
      <li>Recently opened files menu.
      <li>In MouseLook mode, right/left keys work like strafe
        and comma/dot are for rotations.
      <li>Changing blending source and dest factors at runtime.
      <li>Menu item to show OpenGL capabilities.
      <li>Also ported to Mac OS X, actually the whole engine is ported to Mac OS X.
    </ul>

    <p>Most notable bugfixes:
    <ul>
      <li>Various problems specific to particular OpenGL implementations fixed:
        <ul>
          <li>Radeon: wire box is now drawn correctly.
          <li>Mesa, Radeon: volumetric fog rendering fixed.
          <li>Other small fixes for Mesa.
          <li>Some NVidia cards: life indicator alpha rendering fixed.
          <!-- li><b>Not all Radeon bugs are fixed yet, but they are planned to be
            fixed for the next release.</b -->
        </ul>
      <li>Libpng under Unixes loading fix (no longer need to install
        libpng*-dev packages).
    </ul>

    <p>Most notable engine internal improvements for programmers
    (of course not counting features and fixes that already got mentioned above... ) :
    <ul>
      <li>VRML camera is now correctly read, transformation of <code>Viewpoint</code>
        indicates gravity direction. This way you can set gravity up vector
        and initial camera up vector to different things.
      <li>New examples in "Kambi VRML game engine":<br />
        <code>vrml/opengl/examples/shadow_volume_test</code>,<br />
        <code>opengl/examples/demo_matrix_navigation</code>,<br />
        <code>3d/examples/draw_space_filling_curve</code>,<br />
        <code>opengl/examples/fog_coord</code>,<br />
        <code>audio/examples/algets</code> and<br />
        <code>audio/examples/alplay</code>.
      <li>Everything is in FPC objfpc mode, no longer Delphi compat mode anywhere.
      <li>Integration with FPC Matrix unit started (VectorMath now reuses
        non-object types from Matrix, many units use Matrix object types
        and overloaded operators).
      <li><code>VRMLRayTracer</code> interface changed to much cleaner object-oriented.
      <li>Detailed GL_VERSION and GLU_VERSION parsing and reporting,
        including the ability to detect Mesa and Mesa version.
        See GLVersion and GLUVersion objects.
      <li>"The Castle" excellent level objects framework: things that move and animate
        on the level are now much easier to add and design. This was already heavily
        used by DOOM E1M1 level, also see the nice elevator demo on "Tower" level.
      <li>TVRMLGLAnimation updated to work with VRML 2.0 SFNode and MFNode fields.
      <li>OggVorbis loading and playing (through OpenAL extension
        or vorbisfile library).
    </ul>

    <p>"The Castle" improvements for content (e.g. 3D level) designers.
    Many "The Castle" debug menu improvements and greatly improved game
    confugurability by editing game XML files:
    <ul>
      <!--li>"Render for level screenshot"-->
      <li>Configure sounds by <code>sounds/index.xml</code> file,
        debug menu option "Reload sounds.xml".
      <li>Configure items by <code>items/kinds.xml</code> file.
      <li>Blending type configurable for all items and creatures.
      <li>All animations are now expressed in external files
        (in *.kanim files or in XML nodes inside kinds.xml files).
      <!--li>"Fly" debug option.-->
      <li>Configure levels by <code>levels/index.xml</code> file.
        Many level properties, also hint boxes are configurable there.
      <li>New level "hello world".
      <li><code>KambiHeadLight</code> node to configure headlight from VRML.
      <li>No longer any need for "Transparent" properties. All creatures/items/levels
        can now freely mix transparent and opaque parts, and everything will
        be rendered always OK.
      <li>Octree params configurable from debug menu.
    </ul>

    <p>Also packaging changes: <code>units-src</code> renamed to
    <code>kambi_vrml_game_engine-src</code>,
    <code>kambi.cfg</code> file is included inside, <code>test_kambi_units</code>
    is included inside. Most programs package names include their version numbers.

    <p>Minor programs releases:
    ' . news_a_href_page('rayhunter 1.2.1', 'rayhunter') . ',
    ' . news_a_href_page('lets_take_a_walk 1.2.0', 'lets_take_a_walk') . ',
    ' . news_a_href_page('glViewImage 1.2.1', 'glviewimage') . ',
    ' . news_a_href_page('glplotter 1.1.6', 'glplotter_and_gen_function') . ',
    ' . news_a_href_page('glcaps 1.1.1', 'glcaps') . ',
    ' . news_a_href_page('gen_funkcja 1.0.1', 'glplotter_and_gen_function') . ',
    ' . news_a_href_page('bezier_curves 1.1.5', 'bezier_curves') . ',
    ' . news_a_href_page('malfunction 1.2.3', 'malfunction') . ',
    ' . news_a_href_page('kambi_lines 1.1.2', 'kambi_lines') . '.
    </div>

  <div class="old_news_item"><p><span class="old_news_date">February 28, 2007:</span>
    <p>Hello! It\'s been a while without any significant update on this page &mdash;
    so I thought that I just let you all know that the work on
     ' . news_a_href_page('"The Castle"', 'castle') . ' and
     ' . news_a_href_page('Kambi VRML game engine', 'kambi_vrml_game_engine') . '
     was ongoing in these last months.
    0.7.0 release of "The Castle", 2.1.0 release of view3dscene along
    with releases of most other programs on this page are scheduled
    within a week or two. A lot of internal features (usable for programmers
    wanting to use my engine, or 3D content designers for "The Castle") were done,
    along with a lot of bugfixes and many small feature additions.</p></div>

  <div class="old_news_item"><p><span class="old_news_date">October 7, 2006:</span>
    <p>Good news for FreeBSD users: I finally upgraded my FreeBSD to 6.1,
    and got NVidia OpenGL working smoothly there, along with OpenAL.
    So I updated all FreeBSD binaries on these pages to their latest version.
    I also confirmed that ' . news_a_href_page('"The Castle"', 'castle') . '
     compiles and works perfectly under FreeBSD (although the FreeBSD binary
    is not included in the game archive yet).</div>

  <div class="old_news_item"><p><span class="old_news_date">October 1, 2006:</span>
    <p>A made a new page about my
    ' . news_a_href_page('Kambi VRML game engine', 'kambi_vrml_game_engine') . '.
     Most of the content of this page was already said here and there,
    but now I want to say it more explicitly: <em>I\'m making a reusable game
    engine</em>. Also the engine sources are updated now, three new example
    programs are added: <code>images/examples/image_convert</code>,
    <code>opengl/examples/test_font_break</code> and
    <code>opengl/examples/multi_glwindow</code>.</div>

  <div class="old_news_item"><p><span class="old_news_date">September 27, 2006:</span>
    <p>Final version of
    ' . news_a_href_page("my master's thesis about my VRML engine",
      'engine_doc') . ' is available now.</p></div>

  <div class="old_news_item"><p><span class="old_news_date">September 21, 2006:</span>

    <p>Newest version of
    ' . news_a_href_page("my master's thesis about my VRML engine",
      'engine_doc') . ' is available. Only the 7th chapter remains
    undone. <em>Later update the same day: all chapters done!</em></p>

    <p>Units ' . news_a_href_page('sources', 'sources') . ' updated:
    included is an example how to do fog culling (to the fog visibility range),
    see the file <code>units/vrml/opengl/examples/fog_culling.dpr</code>.
    Also blending source and dest factors are now configurable.
    Also behavior on incorrect <code>Background</code> nodes is now better
    (reports warning and proceeds).</p></div>

  <div class="old_news_item"><p><span class="old_news_date">September 13, 2006:</span>

    <p>First of all, a draft and unfinished version of
    ' . news_a_href_page("my master's thesis about my VRML engine",
    'engine_doc') . ' is available.</p>

    <p>' . news_a_href_page('view3dscene 2.0.1', 'view3dscene') . '
     released &mdash; small updates and fixes. New menu items
    were added to display the whole octree and to change the
    point size of <code>PointSet</code>.
    The quadric stacks value (for the command-line option
    <code>--detail-quadric-stacks</code>
    and <code>KambiTriangulation</code> node) can be 1 now.
    The recently released FPC 2.0.4 is used to compile view3dscene now.</p>

    <p>Also, <a href="http://freshmeat.net/projects/view3dscene/">view3dscene
    entry was added to freshmeat</a>. You can use this e.g. to subscribe to new
    releases, so that you will be automatically notified about new
    releases of view3dscene.</p>

    <p>In ' . news_a_href_page('VRML test suite',
      'kambi_vrml_test_suite') . '
     <code>vrml_2/kambi_extensions/fog_linear_with_immune.wrl</code> test fixed.</div>

  <div class="old_news_item"><p><span class="old_news_date">August 24, 2006:</span>

    <p>First of all, I\'m proud to announce that
    <b>VRML 2.0 (aka VRML 97) support is implemented now</b>.
    It\'s by no means complete yet, but it\'s definitely usable
    already &mdash; see ' . news_a_href_page('VRML implementation status',
    'x3d_implementation_status') . ' for details and results of
    various test suites. Almost all of my
    ' . news_a_href_page("non-standard VRML extensions",
      "x3d_extensions") . ' work in VRML 2.0 too, and actually
    you can even ' . news_a_href_page_hashlink(
      "mix VRML 1.0 and 2.0 features in your files",
      "x3d_extensions", 'section_ext_mix_vrml_1_2') . '.

    <ul>
      <li>' . news_a_href_page('view3dscene 2.0.0', 'view3dscene') . '
        released &mdash; VRML 2.0 support,
        various other improvements: "Jump to viewpoint" menu added
        (this is useful both for VRML 2.0 Viewpoint nodes and VRML 1.0
        cameras too), --camera-pos, --camera-dir, --camera-up,
        --camera-up-z, --camera-kind, --view-angle-x command-line options
        removed (all these properties (and much more) can be set now
        by appropriate Viewpoint/camera nodes in the file; I decided that
        keeping these options was an unnecessary complication of implementation),
        menu disabling implemented, warnings while loading VRML file are
        stored and can be later viewed from the GUI using "View warnings"
        menu item, added "Reopen" menu item, added "Edit" menu
        (to perform interactively all the things previously
        controlled by <code>--scene-change-*</code> command-line options;
        <code>--scene-change-*</code> command-line options remain to work
        but only for the first loaded scene, so they are mostly useful
        when combined with <code>--write-to-vrml</code>).
      <li>' . news_a_href_page('Kambi VRML test suite',
        'kambi_vrml_test_suite') . ' &mdash; this was previously
        known on these pages as "kambi_vrml_examples.tar.gz", or "Example VRMLs".
        Many test cases were added
        for VRML 2.0, some of which were translated from VRML 1.0,
        some are new, some are created with Blender\'s VRML 97
        exporter. These VRML files are now officially licensed on GNU GPL.
      <li>' . news_a_href_page('rayhunter 1.2.0', 'rayhunter') . '
        released &mdash; VRML 2.0 support.
      <li>' . news_a_href_page('rayhunter gallery', 'raytr_gallery') . ' &mdash;
        added <i>mirror fun</i> rendering, demonstrating mirror effect
        in one of the first rayhunter renderings of VRML 2.0 model.
      <li>' . news_a_href_page('"The Castle" 0.6.6', 'castle') . '
        released &mdash; in 0.6.5 sky on the "Gate" level
        was not visible, fixed now. Also support for designing levels
        in VRML 2.0 added, but not finished yet, see TODO item on
        "The Castle" &mdash; development page.
      <li>' . news_a_href_page('glViewImage 1.2.0', 'glviewimage') . '
        released,
        ' . news_a_href_page('glplotter 1.1.5', 'glplotter_and_gen_function') . ' released,
        ' . news_a_href_page('bezier_curves 1.1.4', 'bezier_curves') . ' released
         &mdash; updated to inherit many improvements in
        OpenGL and images units: menu disabling,
        and GIF images reading (by running ImageMagick under the hood),
        fixed handling of PNG files with alpha channel recorded in tRNS chunk.
      <li>' . news_a_href_page('lets_take_a_walk 1.1.5', 'lets_take_a_walk') . '
        released,
        ' . news_a_href_page('malfunction 1.2.2', 'malfunction') . '
        released
         &mdash; small fixes and generally updated
        to compile with latest version of VRML units.
      <li>' . news_a_href_page('Sources', 'sources') . ' and
        ' . news_a_href_page('sources documentation', 'reference') . '
        updated with all improvements mentioned above.
      <li><code>edytorek</code> is removed from these pages.
        Reasoning: I was not using it, not developing it, and I lost my interest
        in it long time ago. Since a long time I use Emacs as my only
        text editor, under all OSes. So there were a couple of embarassing
        issues with <code>edytorek</code> : it was Windows-only, it was
        compiled with Delphi Personal, and I didn\'t publish here it\'s source code...
        All these issues are quite embarassing for someone who
        uses Linux and FreePascal as his main work tools, and develops
        open-source programs... Of course I intended to clean edytorek
        code, porting it to Lazarus and publish it\'s sources some day,
        but, honestly, I don\'t think that it will ever happen.
        So, goodbye <code>edytorek</code>.
    </ul>
    </div>

  <div class="old_news_item"><p><span class="old_news_date">August 1, 2006:</span>
    <p>New version of ' . news_a_href_page('"The Castle"', "castle") . '
     (0.6.5) released: whole documentation is in HTML (available
    both here online and offline inside <code>documentation/</code>
    subdirectory, README file inside archive doesn\'t contain much now),
    <code>--debug-log</code> option will print lots of debug info,
    lifeloss when falling down lowered (to avoid getting hurt too easily
    when jumping), removed one scroll of flying from "Castle Hall"
    (to make the trick with flying over creatures harder),
    many other small changes and fixes.

    <p><i>My nearest development plans</i>:
    <ul>
      <li>At the end of this week I plan to finally
        upload here basic (static, without any PROTOs) VRML 97 support
        for ' . news_a_href_page('view3dscene', 'view3dscene') . ' !

      <li>Unfortunately further "The Castle" development is going to be suspended
        until the end of September. At the end of September I should do to
        "The Castle" two things: 1. add a small joke/experiment level
        (it\'s already partially done &mdash; you\'ll see what is this :) and
        2. finally fix these Radeon issues. This will result in 0.7.0 release.
        So stay tuned.
    </ul>
    </div>

  <div class="old_news_item"><span class="old_news_date">July 12, 2006:</span>
    <ul>
      <li>New program is available:
        <a href="https://michalis.xyz/grammar_compression.php">grammar_compression</a>
        &mdash; implementation of Sequitur and Sequential compression algorithms
        in ObjectPascal.

      <li>' . news_a_href_page('"The Castle"', "castle") . ' page
        is reworked, I created separate page for "developer" stuff.
        I want to move most information from Castle\'s README file to these WWW pages,
        and then replace README file with offline version of these pages.
    </ul>
    </div>

  <div class="old_news_item"><span class="old_news_date">July 3, 2006:</span>
    <ul>
      <li>
        ' . news_a_href_page('view3dscene 1.2.5', 'view3dscene') . '
        released &mdash; more usable behavior
        on errors while loading the scene: previously loaded state is preserved,
        errors when loading command-line scene are shown in the GUI,
        various other small usability improvements, some new/changed menu items.</li>

      <li>' . news_a_href_page('Base units', 'sources') . '
        updated with many internal changes &mdash;
        Added view3dscene_mini_by_lazarus: example that you can use all my VRML
        rendering code within "normal" Lazarus program, using Lazarus
        TOpenGLControl. test_kambi_units updated.
        Many "var" parameters changed to "out" to get more sensible FPC hints.</li>

      <li>' . news_a_href_page('glplotter 1.1.4', 'glplotter_and_gen_function') . '
        released &mdash; separate X and Y scaling available.</li>
    </ul>
    </div>

  <div class="old_news_item"><p><span class="old_news_date">June 8, 2006:</span>
    <p>New version of ' . news_a_href_page('"The Castle"', 'castle') . ' (0.6.4)
    released. Various small improvements and one important fix:
    open-source Radeon drivers
    under Linux are reported to work correctly right now. Unfortunately, issues
    with proprietary Radeon drivers (under Windows, and probably under Linux too)
    are not fixed yet &mdash; so stay tuned :)

    <p>In an unrelated news: For those of you who know my old alternative
    email address <code>mkambi@poczta.onet.pl</code>: don\'t use this address anymore.
    I will not receive mail send to this address.
    If you recently (in May 2006 or later) send a mail to this adress,
    then I probably didn\'t get it.
    <b>My only vaild email address is now
    ' . href_mailto(MICHALIS_EMAIL) . '.</b>

    <!--
      Updates not deserving mention:
      - Also castle page improved a little, added text from my PGD forum
        post (1st "overview" sentence + "requirements to run.
      - Also version numbers added on every www page for program.
      - Uploaded src: view3dscene, malfunction, lets_take_a_walk,
        to keep them in compileable state.
    -->
    </div>

  <div class="old_news_item"><span class="old_news_date">May 19, 2006:</span>
    <p>New version of ' . news_a_href_page('"The Castle"', 'castle') . ' (0.6.3)
    released. Various "cleaning" changes and fixes:
    <ul>
      <li>Memory use reduced, this also reduced loading time.

        <p><i>Comparison:</i>
        Times and memory use below were measured on Linux with release build,
        with NVidia drivers. Note that the actual times and memory use may vary
        wildly from one graphic driver to the other, as the most time and memory
        consuming tasks are in preparing OpenGL things, like display lists
        and textures. For example, on Windows, memory consumption is slightly
        lower, which indicates that NVidia drivers are slightly better optimized
        for memory use on Windows.
        But hopefully the proportions will be around the same.
        Times below were measured for entering "New Game" -> "The Gate" level.

    <pre>
    0.6.2 version times:
      Loading level: 10 sec
      Loading creatures: 41 sec
      Loading items: ~ 4 sec
      Memory use: 496 MB
    0.6.3 version times:
      Loading level: 10 sec (nothing optimized here for time)
      Loading creatures: 24 sec
      Loading items: ~ 2 sec
      Memory use: 278 MB
    </pre>

        A lot of improvements to how we store and generate TVRMLGLAnimation
        instances was done for this.

      <li>"Debug menu" is now separated from normal game menu,
        is invoked by ~ key, default FPS toggle key is Tab now.
        This is all to make debug menu (a little) more hidden.
        Also debug menu has now submenus for commands related to:
        player, creatures and items. Added commands "Set Player.MaxLife"
        and "Reload animations/models of specific item".

      <li>Error messages fixed on Linux, previously various errors
        (e.g. in level or creatures VRML models) produced
        "ModeGLEnter cannot be called on a closed GLWindow" message
        instead of the right message.

      <li>--display command-line option for XWindows (but is sometimes
        unstable, probably because of NVidia OpenGL unstabilities)

      <li>Applied patches from Szymon Stoma &amp; Kaśka Zaremba to improve
        texturing of some places on "The Gate".

      <li>MouseLook can be turned off.

      <li>Proper texture filtering is used for items, creatures and all level
        parts. Previously items, creatures and StairsBlocker on castle_hall
        used bad filtering (always GL_LINEAR, while e.g. GL_LINEAR_MIPMAP_LINEAR
        was noticeably better; in general, "Video options -> Texture quality"
        setting should be used).
    </ul>

    <p>Also ' . news_a_href_page('view3dscene', 'view3dscene') . ' (version 1.2.4)
    released: mouse look available (use "m" key). Just like in "The Castle".

    <!-- Silent new version of malfunction (1.2.1),
         precompiled only for Linux,
         to bring some fixes and source updates (for compat with my units). -->
    </div>

  <div class="old_news_item"><span class="old_news_date">May 9, 2006:</span>
    <p>New version of ' . news_a_href_page('"The Castle"', 'castle') . ' (0.6.2)
    released. Changes from version 0.6.0 include:
    right mouse button now does jumping, sound of SpiderQueen hurt fixed,
    SpiderQueen adjusted &mdash; overall it\'s a little easier to defeat now,
    although some details make it also harder (life decreased,
    the trick with jumping/flying on SpiderQueen is now much harder),
    bow + arrows added, Werewolf has higher life now.
    This was all done for version 0.6.1, that was final version for PGD
    competition. Version 0.6.2 brings only minor corrections to README and
    "Credits" text.</div>

  <div class="old_news_item"><span class="old_news_date">May 8, 2006:</span>
    <p>New version of ' . news_a_href_page('"The Castle"', 'castle') . ' (0.6.0)
    released. This is final (or almost-final) version for the PGD competition.
    List of changes since 0.5.9 version is huge, among the most important
    features are: new level "Cages" with new creatures,
    much reworked level "The Gate" (thanks to Szymon Stoma and Kaśka Zaremba),
    new features making creatures harder to beat
    (homing missiles, knockback for player, now so easy to interrupt boss
    attack, aliens try to always stay away from you,
    most creatures are generally faster), life indicator for bosses,
    and an ending sequence is done.

    <p>Also ' . news_a_href_page('view3dscene', 'view3dscene') . ' (version 1.2.3)
    released with some improvements:
    <ul>
      <li>removed FreeWalk navigation method, instead now you can
        freely control PreferHomeUpForRotations/Moving from menu.
      <li>ambientIntensity from VRML 97 implemented for lights.
    </ul>
    </div>

  <div class="old_news_item"><span class="old_news_date">May 4, 2006:</span>
    <p>New version of ' . news_a_href_page('"The Castle"', 'castle') . ' (0.5.9)
    released: creatures are now configurable by <code>kinds.xml</code> file,
    <code>--debug-no-creatures</code> command-line option,
    you can set color bit depth and display frequency (the last feature is actually
    honoured only on Windows for now &mdash; yeah, I\'m under the pressure :) ).
    "Official" downloadable version is still 0.5.6,
    <a href="http://stoma.name/michalis/castle-with-sources-0.5.9.tar.gz">version 0.5.9
    compiled only for Linux is here</a>.
    </div>

  <div class="old_news_item"><span class="old_news_date">May 3, 2006:</span>
    <p>New version of ' . news_a_href_page('"The Castle"', 'castle') . ' (0.5.8)
    released: debug menu for lights improved (ambientIntensity for lights,
    "Edit headlight", "Global Ambient Light"), some other small things.
    There\'s also a new level, but it\'s hidden &mdash; don\'t look at it now,
    should be finished tomorrow.
    "Official" downloadable version is still 0.5.6,
    <a href="http://stoma.name/michalis/castle-0.5.8.tar.gz">version 0.5.8
    compiled only for Linux is here</a>.
    </div>

  <div class="old_news_item"><span class="old_news_date">May 1, 2006:</span>
    <p>New version of ' . news_a_href_page('"The Castle"', 'castle') . ' (0.5.7)
    released: debug menu item to change jump properties,
    debug menu item to edit level lights, some other small fixes.
    "Official" downloadable version is still 0.5.6,
    <a href="http://stoma.name/michalis/castle-0.5.7.tar.gz">version 0.5.7
    compiled only for Linux is here</a>.
    </div>

  <div class="old_news_item"><span class="old_news_date">April 30, 2006:</span>
    <p>New version of
    ' . news_a_href_page('"The Castle"', 'castle') . ' (0.5.6)
    released: trying to nail down display bugs on Radeon:
    checking display lists availability,
    "Creature animation smoothness" and "Restore to defaults" in "Video options".
    </div>

  <div class="old_news_item"><span class="old_news_date">April 29, 2006:</span>
    <p>New version of
    ' . news_a_href_page('"The Castle"', 'castle') . ' (0.5.5)
    released: many small pending features/bugfixes done: you can restart
    "New Game" from any level that you once managed to get to,
    when changing keys assignment and the conflict is found you can just clear
    the assignment of the old key (Eric Grange idea), fixes when floating
    just above the water, player moving speeds adjusted better,
    better navigation when flying/swimming (you can go up/down just
    by looking up/down), fixed walking down from slight hills,
    fixed accidentaly moving adjacent menu items sliders,
    left/right keys are by default assigned to left/right strafes now,
    some others.</div>

  <div class="old_news_item"><span class="old_news_date">April 27, 2006:</span>
    <p>New version of
    ' . news_a_href_page('"The Castle"', 'castle') . ' (0.5.4)
    released: mouse looking implemented. Also "The Castle" archives
    are now hosted on much faster server provided by
    <a href="http://stoma.bestweb.pl/">Szymon</a> (thanks!).</div>

  <div class="old_news_item"><span class="old_news_date">April 26, 2006:</span>
    <ul>
      <li>New preview version of
        ' . news_a_href_page('"The Castle"', 'castle') . ' (0.5.3)
        released: <b>sounds and music are done !</b>,
        and various "Gate" level improvements (like swimming).
      <li>' . news_a_href_page('lets_take_a_walk', 'lets_take_a_walk') . '
        (ver 1.1.4) release &mdash; bugfix for newer OpenAL
        under Linux that don\'t include alut functions in the same SO file.
      <li>Inside units sources, there\'s new unit
        <code>ALSourceAllocator</code> and it\'s demo in <code>audio/examples/</code>.
        This is an intelligent manager of OpenAL sounds,
        used in ' . news_a_href_page('The Castle', 'castle') . '.
    </ul>
    </div>

  <div class="old_news_item"><span class="old_news_date">April 17, 2006:</span>

    <ul>
      <li>First of all, ' . news_a_href_page('a preview of my new game "The Castle" is available',
        'castle') . '. This is the project that I am working on since February
        this year. Everyone is most welcome to download and try it !

      <li><p>Updated ' . news_a_href_page('view3dscene', 'view3dscene') . '
        (ver 1.2.2):

        <p>VRML extensions:
        <ul>
          <li>' . news_a_href_page_hashlink(
            'Field <code>separate</code> for <code>WWWInline</code> node',
            'x3d_extensions', 'section_ext_wwwinline_separate') . '.
          <li>' . news_a_href_page_hashlink(
            '<code>Fog</code> node extensions to define volumetric fog',
            'x3d_extensions', 'section_ext_fog_volumetric') . '
          <li>' . news_a_href_page_hashlink(
            '<code>fogImmune</code> field for <code>Material</code> node',
            'x3d_extensions', 'section_ext_fog_immune') . '
        </ul>

        <p>Also head bobbing much better, and various other improvements.

      <li><p>Important updates to demo_animation
        (see <code>units/vrml/opengl/examples/</code>) in the
        ' . news_a_href_page('sources', 'sources') . ':
        <ul>
          <li>New demo (gus) showing how to use Blender "armature" animation
            to export animation to VRMLs such that demo_animation is able to
            render it. This is quite great, because this allows you to very
            comfortably design animations in Blender and then use them with my engine.
          <li>Important fix for animating models with textures, demo (cube_opening)
            added.
          <li>Animating class <code>TVRMLGLAnimation</code> extended to be able
            to animate / morph between an atritrary number of models (>= 2),
            not only 2. Each model has an associated point of time in the
            animation. Demo (gus_3_final, to be used together with gus_1_final
            and gus_2_final) added.
          <li>Automatic looping and going backwards ability for TVRMLGLAnimation.
            See --loop, --no-loop, --backwards and --no-backwards command-line
            options for demo_animation.
        </ul>

      <li><p>Updated glViewImage (ver 1.1.5) (various small fixes).

      <li><p>Updated many other programs sources to keep them compileable,
        because of many changes in units, and some other various
        small fixes.

      <li><p>Oh, and I put here my
        <a href="https://michalis.xyz/michalis-gpg-public-key.asc">public GPG key</a>.
    </ul>
    </div>

  <div class="old_news_item"><span class="old_news_date">March 9, 2006</span><br>
    Many ' . news_a_href_page('view3dscene', 'view3dscene') . ' (ver 1.2.1)
    updates:
    <ul>
      <li>VRML 97 nodes
        ' . news_a_href_page_hashlink('NavigationInfo',
        'x3d_extensions', 'section_ext_navigationinfo') . ' and
        ' . news_a_href_page_hashlink('WorldInfo',
        'x3d_extensions', 'section_ext_worldinfo') . ' handling,
        ' . news_a_href_page('kambi_vrml_test_suite',
        'kambi_vrml_test_suite') . ' has test VRMLs for this.

      <li>More work on gravity stuff: growing up to camera height
        (allows climbing stairs etc.), nice effect when falling down from high,
        jumping ("A" key), crouching ("Z" key), head bobbing.
        Strafe move keys changed (to not collide with "Z" and to be
        more standard): "Comma" / "Period". Also horizontal moving
        in Walk mode is now better (moving dir is not affected by PageUp/PageDown
        keys operations, i.e. current vertical rotation). And vertical rotations
        (PageUp/PageDown keys) are bounded, so that you\'re no longer able to "stand
        on your own head".

      <li><i>Console</i> -> <i>Print scene bounding box as VRML node</i> menu
        command.
    </ul>

    <p>See also screenshots of my game "The Castle" (<i>link not available
    anymore</i>).
    This is the main thing that I\'m working on right now, it\'s for the
    <a href="http://pascalgamedevelopment.com/">PascalGameDevelopment</a>
    competition.

    <!-- Silently updated
      lets_take_a_walk src.
      rayhunter src.
    just to keep them in compileable state -->
    </div>

  <div class="old_news_item"><span class="old_news_date">February 24, 2006</span><br>
    Many updates. Every OpenGL based program updated,
    ' . news_a_href_page('all units and programs sources', 'sources') . '
    updated (along with ' . news_a_href_page('their documentation', 'reference') . ').
    Most important things are
    <ul>
      <li>Timing bug fixed in every OpenGL program
      <li>Compilation of many examples fixed
      <li>"Sunny day" level of
        ' . news_a_href_page('malfunction', 'malfunction') . '
        completely reworked and improved
      <li>' . news_a_href_page('view3dscene', 'view3dscene') . '
        up/down navigation improved and "gravity" setting added
    </ul>

    <p>Detailed changes log follows:

    <p>General updates:
    <ul>
      <li>Timing bug fixed (timing was sometimes incorrect, which
        caused some bad artifacts when moving camera or doing some
        animations &mdash; this was particularly observed with newest
        NVidia Linux OpenGL drivers).
      <li>Compilation of many examples fixed; sorry, I wasn\'t compiling examples
        too often and recently I broke many of them (bacause of changes to
        <a href="' . CURRENT_URL . 'apidoc/html/KambiUtils.html#Parameters">Parameters</a>
        stuff). It\'s fixed now. I also added the automatic test of compilation
        to the script I use to create <code>units-src.tar.gz</code> archive, so
        this Will Not Happen Again.
      <li>Fullscreen toggle shortcut is F11 (following (GNOME) standards
        &mdash; epiphany, GIMP, gthumb and firefox).
      <li><a href="http://www.freepascal.org/bugs/showrec.php3?ID=4831">FPC 2.0.2 bug #4831</a>
        workarounded (this caused some rare problems when displaying dialog boxes).
    </ul>

    <p>' . news_a_href_page('malfunction', 'malfunction') . ' (ver 1.2.0)
    specific updates:
    <ul>
      <li>"Sunny day" level completely reworked and improved.
      <li>malfunction sources contain now Blender files used to create all
        objects and levels, see devel_data/ subdirectory.
    </ul>

    <p>' . news_a_href_page('view3dscene', 'view3dscene') . ' (ver 1.2.0)
    specific updates:
    <ul>
      <li>Small interface improvements: +/- keys (move speed change) work
        now better (time-based), progress bar is shown in OpenGL window
        when opening scene using "Open" menu item,
        Cancel key for raytracer dialog, "Navigation" submenu.
      <li>Fixed bug that occured for specific models with empty Coordinate3 node
        followed by empty IndexedFaceSet node &mdash; this triggered OpenGL
        error "invalid value" in some cases, now it doesn\'t.
      <li>Better Insert/Delete navigation (vertical moving with respect
        to home camera up, instead of current camera up) in Walk mode.
        ' . news_a_href_page('lets_take_a_walk', 'lets_take_a_walk') . '
        also benefits from this.
      <li>Gravity setting: you can now turn gravity on, and fall down.
        Basic implementation committed, expect more work on this soon
        (including "jump" and "duck" keys and head "bobbing") &mdash; I\'m porting
        stuff from some very old game of mine into
        <a href="' . CURRENT_URL . 'apidoc/html/MatrixNavigation.TMatrixWalker.html">TMatrixWalker</a>
        class.
    </ul>

    <p>' . news_a_href_page('glViewImage', 'glviewimage') . ' (ver 1.1.4)
    small improvement (accepts dir name on command-line).

    <p>Other OpenGL programs updated:
    ' . news_a_href_page('lets_take_a_walk', 'lets_take_a_walk') . ' (ver 1.1.3),
    ' . news_a_href_page('glplotter', 'glplotter_and_gen_function') . ' (ver 1.1.3),
    ' . news_a_href_page('bezier_curves', 'bezier_curves') . ' (ver 1.1.3),
    ' . news_a_href_page('kambi_lines', 'kambi_lines') . ' (ver 1.1.1).
    Also ' . news_a_href_page('sources docs page', 'reference') . '
    shortened, ' . news_a_href_page('kambi_vrml_test_suite',
    'kambi_vrml_test_suite') . ' repackaged and fixed
    some links to www.web3d.org VRML specification.

    <p><i>Last-minute note about FreeBSD</i>:
    compiled programs for FreeBSD <i>will not be updated</i> today.
    I\'m sorry, but currently I have terrible problems with OpenGL on
    FreeBSD &mdash; current NVidia drivers (8178) cause
    kernel crashes (it seems that they didn\'t really update their
    drivers to FreeBSD 6 ?), and Mesa is terribly unstable.
    I checked various OpenGL programs, including Mesa demos, and
    they all just fail in various mysterious ways (segfaults, hangs, etc.).
    So it\'s not a problem specific to my programs &mdash; it\'s some
    problem with my FreeBSD setup, but I don\'t have time to fight
    with it now. Anyway, after compiling, I was unable to actually
    test my programs on FreeBSD,
    so I will not upload here completely untested binaries.
    If you use FreeBSD, feel free to just compile them yourself.</div>

  <div class="old_news_item"><span class="old_news_date">February 13, 2006</span><br>
    Many modifications to my general units done, so
    ' . news_a_href_page('all units and programs sources', 'sources') . '
    are updated. As usual,
    ' . news_a_href_page('documentation generated by pasdoc',
    'reference') . ' is also updated.

    <p>New unit VRMLGLAnimation was created, to easily produce animations
    from still scenes.
    See extensive demo in <code>vrml/opengl/examples/demo_animation.dpr</code>,
    with raptor and sphere sample models.

    <p>I\'m also glad to add that I\'m starting in
    <a href="http://pascalgamedevelopment.com/">Pascal Game Development</a>
    game competition. This should result in a new game available on these
    pages around April 2006, and between February and April 2006 I will
    constantly update my units on these pages.</div>

  <div class="old_news_item"><span class="old_news_date">January 16, 2006</span><br>
    And once again, ' . news_a_href_page('documentation generated by pasdoc',
      'reference') . ' and units
      ' . news_a_href_page('sources', 'sources') . ' updated again:
    a lot of content translated to English.</div>

  <div class="old_news_item"><p><span class="old_news_date">December 11, 2005</span><br>
    ' . news_a_href_page('Documentation generated by pasdoc', 'reference') . '
    and units ' . news_a_href_page('sources', 'sources') . ' updated again:
    much more impressive introduction page, old README_GLOBAL file removed,
    many things translated to English. Some issues with compilation
    with FPC 2.0.2 fixed.</div>

  <div class="old_news_item"><p><span class="old_news_date">November 27, 2005</span><br>
    ' . news_a_href_page('Documentation generated by pasdoc', 'reference') . '
     and units ' . news_a_href_page('sources', 'sources') . ' updated
    to reflect many new features and improvements in
    <a href="http://pasdoc.sourceforge.net/">PasDoc 0.10.0</a> released yesterday.</div>

  <div class="old_news_item"><p><span class="old_news_date">November 12, 2005</span><br>
    ' . news_a_href_page("lets_take_a_walk", "lets_take_a_walk") . '
    1.1.2 released (only for Linux) &mdash; fixed problem with linking to
    current Debian-testing openal version.</div>

  <div class="old_news_item"><p><span class="old_news_date">October 2, 2005</span><br>
    A lot of content on these pages finally translated to English.
    New versions of most programs released, with updated documentation
    and often other improvements. Full list of changes:
    <ul>
      <li>
      ' . news_a_href_page("Specification of my extensions to VRML",
        "x3d_extensions") . ',
        ' . news_a_href_page(
        "standard command-line options understood by all my OpenGL programs",
        "opengl_options") . '
        pages completely translated to English. Polish versions removed.
      <li>' . news_a_href_page("malfunction", "malfunction") . ' 1.1.0
        released &mdash; help text is now in English,
        complete English documentation. Polish docs removed.
      <li>' . news_a_href_page("view3dscene", "view3dscene") . ' 1.1.3
        released &mdash; now English documentation is complete
        (docs about <code>--detail-...</code> options added).
        Also handling of some Inventor models improved &mdash;
        RotationXYZ is handled, some other Inventor fields are parsed
        (and then ignored).
      <li>' . news_a_href_page("kambi_lines", "kambi_lines") . '
        (previously known as <b>kulki</b>) 1.1.0 released &mdash;
        new program name, complete English documentation,
        English help text inside the game. Polish docs removed.
      <li>' . news_a_href_page("lets_take_a_walk", "lets_take_a_walk") . ' 1.1.0
        released &mdash; complete English documentation, F1 shows help text,
        sources contain <i>really</i> all source files &mdash; including
        <code>devel</code> subdir with some scripts and Blender, GIMP and Terragen
        data files. Polish docs removed.
      <li>' . news_a_href_page("glcaps", "glcaps") . ' 1.1.0
        released &mdash; complete English documentation. Polish docs removed.
      <li>' . news_a_href_page("glViewImage", "glviewimage") . ' 1.1.3
        released &mdash; complete English documentation, small changes.
      <li>' . news_a_href_page("bezier_curves", "bezier_curves") . ' 1.1.2
        released &mdash; complete English documentation, small changes.
    <li>' . news_a_href_page("rayhunter", "rayhunter") . ' 1.1.0
        released &mdash; complete English documentation, greatly extended
        abilities of <code>--write-partial-rows</code> option by <code>&lt;log-rows-file&gt;</code>.
        Polish docs removed.
      <li><code>various_notes_begin.pasdoc</code> and <code>gen_light_map.dpr</code>
        are contained in units sources.
    </ul>

    <p><b>Second update on the same day, October 2, 2005:</b>
    <ul>
      <li>' . news_a_href_page("lets_take_a_walk", "lets_take_a_walk") . ' 1.1.1
        released (only for Linux, other binaries stay 1.1.0) &mdash;
        when using OpenAL sound, sometimes <code>lets_take_a_walk</code>
        hanged on exit (i.e. when you pressed Escape or Alt+F4 etc.). Fixed now.
    </ul>

    <p><b>Third update on the same day, October 2, 2005:</b><br>
    (busy day, eh ? :)
    <ul>
      <li>' . news_a_href_page("lets_take_a_walk", "lets_take_a_walk") . '
        updated. Accidentaly 1.1.0 and 1.1.1 packages (binary,
        for all OSes, and source) were uploaded without one texture
        correct, and you will not see cool shadows on the floor.
        They are repackaged now, and all is fixed.
        In other words: if you happened to download
        ' . news_a_href_page("lets_take_a_walk", "lets_take_a_walk") . '
        today, between the hours 0.00 &ndash; 7.00, please download it and install
        once again.
    </ul>
    </div>

  <div class="old_news_item"><p><span class="old_news_date">September 12, 2005</span>
    <ul>
      <li>Units ' . news_a_href_page('sources', 'sources') . ' and
        ' . news_a_href_page('documentation generated by pasdoc', 'reference') . '
        updated (recently implemented
        <a href="http://pasdoc.sourceforge.net/">pasdoc</a> features used
        (e.g. @xxxList tags),
        <a href="' . CURRENT_URL . 'apidoc/html/introduction.html#OpenGLOptimization">
        OpenGL optimization notes</a> are now part of
        documentation parsed by pasdoc and are available for viewing in
        output HTML / pdf docs, some things translated to English,
        various small improvements in sources).
      <li>Small other improvements: better layout of
        ' . news_a_href_page('sources', 'sources') . ' page,
        DBGridExporter has fixed XML export.
      <li>' . news_a_href_page('lets_take_a_walk', 'lets_take_a_walk') . '
        version 1.0.4 release for Linux, fixes a bug (under Linux you had
        to install openal-dev lib, but installing only openal should be sufficient)
    </ul>
    </div>

  <div class="old_news_item"><p><span class="old_news_date">June 07, 2005:</span>
    <ul>
      <li>' . news_a_href_page('Documentation of my sources generated by pasdoc',
        'reference') . ' updated, to reflect many recent improvements done to
        <a href="http://pasdoc.sourceforge.net/">pasdoc</a>.
      <li>' . news_a_href_page('Sources', 'sources') . ' of units
        updated &mdash; small fixes, KambiClassUtils.TTextReader improved
        (removed this "latency" in Readln), it\'s used in pasdoc code now.
    </ul>
    </div>

  <div class="old_news_item"><p><span class="old_news_date">May 21, 2005:</span>
    <ul>
      <li>' . news_a_href_page('Demo of documentation of my sources',
        'reference') . '  added, as generated by
        <a href="http://pasdoc.sourceforge.net/">pasdoc</a>.
        I\'m also proud to announce that I\'m now one of pasdoc\'s developers.

      <li>' . news_a_href_page('My sources', 'sources') . '
        contain now Lazarus package files to easier use my units
        inside <a href="http://www.lazarus.freepascal.org/">Lazarus</a>
        programs.

      <li>Automatic tests of my units (implemented using fpcunit) are published
        and downloadable from ' . news_a_href_page_hashlink('sources page',
        'sources', 'section_sources_test') . '

      <li>KambiClassUtils unit: some important generally-useful
        stream classes implemented:
        TPeekCharStream, TSimplePeekCharStream, TBufferedReadStream.

        <p>All VRML loading code now loads using TPeekCharStream,
        you can always wrap any other TStream inside
        TSimplePeekCharStream or TBufferedReadStream.
        This means that loading VRML from file is both more flexible in source code
        and less memory-consuming at runtime.

        <p>Also ' . news_a_href_page_hashlink(
          'all VRML reading code can read VRML files compressed by gzip.',
          'x3d_extensions', 'section_ext_gzip') . '

        <p>' . news_a_href_page('view3dscene', 'view3dscene') . '
        updated to version 1.1.2,
        ' . news_a_href_page('rayhunter', 'rayhunter') . '
        updated to version 1.0.1.
        ' . news_a_href_page("Example VRMLs",
        "kambi_vrml_test_suite") . ' updated.

      <li>Some fixes, including serious bigfix to VRMLFlatSceneGL unit
        for SeparateShapeStates optimization (although this accidentaly
        didn\'t affect programs compiled with FPC 1.9.8).
        ' . news_a_href_page('view3dscene', 'view3dscene') . '
        updated to version 1.1.2,
        ' . news_a_href_page('rayhunter', 'rayhunter') . '
        updated to version 1.0.1,
        ' . news_a_href_page('malfunction', 'malfunction') . '
        updated to version 1.0.3,
        ' . news_a_href_page('lets_take_a_walk', 'lets_take_a_walk') . '
        updated to version 1.0.3.

      <li>All new versions are compiled using FPC 2.0.0 (Whohoo !
        Finally new stable version of FPC !).
        Comments on ' . news_a_href_page('sources', 'sources') . '
        page updated.

      <li>You can now use FPC OpenGL bindings (gl, glu, glext units)
        (slightly fixed, patches will be submitted to FPC team)
        instead of my OpenGLh binding.
        Just define USE_GL_GLU_UNITS.
        In the future my OpenGLh unit may be removed, and I\'ll switch to
        always using FPC OpenGL bindings.

      <li>' . news_a_href_page('glplotter', 'glplotter_and_gen_function') . ' updated to
        version 1.1.2: small bugfix: plots with "_" in names
      <li><a href="https://michalis.xyz/mandaty.php">mandaty</a> page added.
    </ul>

    <!-- glViewImage sources only updated to 1.1.2, small fixes to sources -->
    </div>

  <div class="old_news_item"><p><span class="old_news_date">March 14, 2005:</span>

  <!--
    /*
    A lot of important things were implemented. Below is only a summary,
    to read full log of changes see < ? php echo
    a_href_page(\'log of changes to these pages\', \'news\') ? >.
    New versions of
     < ?php echo a_href_page(\'view3dscene\', \'view3dscene\'); ? >  (1.1.1),
     < ?php echo a_href_page(\'lets_take_a_walk\', \'lets_take_a_walk\'); ? >  (1.0.2),
     < ?php echo a_href_page(\'malfunction\', \'malfunction\'); ? >  (1.0.2),
     < ?php echo a_href_page(\'glViewImage\', \'glviewimage\'); ? >  (1.1.1),
     < ?php echo a_href_page(\'glplotter\', \'glplotter\'); ? >  (1.1.1),
     < ?php echo a_href_page(\'bezier_curves\', \'bezier_curves\'); ? >  (1.1.1) uploaded.
    <ul>
      <li><p>Many optimizations of OpenGL display (frustum culling,
        with and without the help of octree,
        and many other speed improvements here and there).
        All VRML programs work now faster.

        <p>Added to sources file <code>units/vrml/opengl/README.optimization_notes</code>
        that describes how current optimization works, what are the possible
        drawbacks and what are the possible alternatives (and what
        drawbacks are hidden in those alternatives :).
        In case you\'re interested how it works but you don\'t want to download
        my whole sources, you can read this document
        <a href=\'src/pascal/README.optimization_notes\'>online</a>.
      <li>Smoother reaction to collision in
        < ?php echo a_href_page(\'view3dscene\', \'view3dscene\'); ? >  and
        < ?php echo a_href_page(\'lets_take_a_walk\', \'lets_take_a_walk\'); ? >.
      <li>Mnemonics for GLWindow menus implemented.
    </ul>
    -->

    <ul>
      <li>New versions of ' . news_a_href_page('view3dscene', 'view3dscene') . '
        (1.1.1) and ' . news_a_href_page('lets_take_a_walk', 'lets_take_a_walk') . '
        (1.0.2) allow somewhat more smooth camera moving:
        when you try to step into the wall (or floor, or ceil, whatever),
        your move is not completely blocked. Instead you are allowed to slowly
        move alongside the wall.

        <p><small>This is implemented by new interface to
        TMatrixWalker.OnMoveAllowed and new method TVRMLOctree.MoveAllowed.</small>

      <li>I removed units gtkglext and gdkglext from my sources,
        they are now incorporated into FPC source tree.
        Also some of my fixes to gtk2 bindings are submitted to FPC sources,
        that\'s why I decided to remove gtkglext and gdkglext units
        from my sources, since you would have to download newest
        gtk2 bindings from FPC cvs anyway.

      <li><p>First part of optimizing OpenGL display using frustum culling
        done: frustum culling without the help of octree done.

        <p>User-visible changes: added <code>--renderer-optimization</code>
        parameter for ' . news_a_href_page('view3dscene', 'view3dscene') . ', see
        ' . news_a_href_page("view3dscene", "view3dscene") . '
        page for docs of this parameter.
        <!-- Minor
        view3dscene\'s commands "Print current camera node",
        "Print raytracer command-line" work in Examine navigation mode. -->
        New ' . news_a_href_page('view3dscene', 'view3dscene') . '
        menu commands "View|Show in Examine mode camera frustum",
        "Console|Print current camera frustum".

        <p><small>Sources changes:
        VRMLFlatSceneGL unit allows new optimization method:
        roSeparateShapeStates.
        VectorMath and MatrixNavigation: done routines to calculate
        frustum\'s planes, and calculate frustum\'s geometry,
        and check whether frustum collides with sphere and TBox3d.
        TVRMLFlatSceneGL.RenderFrustum done.</small>

      <li><p>Second part of optimizing OpenGL display using frustum culling
        with the help of octree: done.

        <p><small>Done second octree based on scene ShapeStates.
        ' . news_a_href_page('view3dscene', 'view3dscene') . '
        creates it, and can display it\'s statistics.
        TOctree.ItemsInNonLeafNodes property added to allow
        octree nodes to store all cummulated items of their children.
        TOctree.EnumerateCollidingOctreeItems implemented and
        TVRMLFlatSceneGL.RenderFrustumOctree implemented.</small>

      <li><p>Added to sources file <code>units/vrml/opengl/README.optimization_notes</code>
        that describes how current optimization works, what are the possible
        drawbacks and what are the possible alternatives (and what
        drawbacks are hidden in those alternatives :).
        In case you\'re interested how it works but you don\'t want to download
        my whole sources, you can read this document
        <a href="' . CURRENT_URL . 'apidoc/html/introduction.html#OpenGLOptimization">
        online</a>.

      <li><p>gprof rulez &mdash; small bug that was harmless but was
        causing a lot of slowdown in TVRMLFlatScene.ValidateFog
        (combined with roSeparateShapeStates) fixed.
        Also problem in VRMLFlatSceneGL with GL_COMPILE_AND_EXECUTE solved.
        Also problem with updating Caption too often (this caused
        some noticeable slowdown on XWindows on my system).

      <li><p>Example program <code>units/vrml/opengl/simpleViewModel_2.dpr</code>
        added.

      <!-- /* echo a_href_page(\'malfunction\', \'malfunction\'); (1.0.2):
        small code adjustments to be compileable. */ -->

      <li><p>Mnemonics for GLWindow menus implemented.
        ' . news_a_href_page('view3dscene', 'view3dscene') . ' (1.1.1),
        ' . news_a_href_page('glViewImage', 'glviewimage') . ' (1.1.1),
        ' . news_a_href_page('glplotter', 'glplotter_and_gen_function') . ' (1.1.1),
        ' . news_a_href_page('bezier_curves', 'bezier_curves') . ' (1.1.1)
        all updated with mnemonics.
    </ul>
    </div>

  <div class="old_news_item"><p><span class="old_news_date">February 28, 2005:</span>
    <ul>
      <li>Finally, GLWindow unit may be based on GTK 2 instead of that old GTK 1.
        Besides obvious usability benefits of using GTK 2, which is just
        better than GTK 1, also fullscreen mode is now better (things like
        gnome-panel don\'t cover your window).

        <p>' . news_a_href_page('view3dscene', 'view3dscene') . ',
        ' . news_a_href_page('glViewImage', 'glviewimage') . ',
        ' . news_a_href_page('glplotter', 'glplotter_and_gen_function') . ',
        ' . news_a_href_page('bezier_curves', 'bezier_curves') . '
        are updated (minor version number++, to 1.1.0,
        Linux/FreeBSD users are encouraged to upgrade).

        <p>Inside sources, opengl/gtk/gtkglext/ directory is created with
        GdkGLExt and GtkGLExt units.

    <!-- Minor:
      Some more things that were only for FPC 1.0.10 are removed from sources.
      Some more things translated to English, as usual.
    -->

      <li>glWinEvents, menu_test_alternative, test_glwindow_gtk_mix
        added to units/opengl/examples/

      <li>All www pages marked in footer as licensed on GNU GPL,
        added page explaining
        <a href="https://michalis.xyz/why_not_gfdl.php">why
        I do not use GNU FDL</a>.

      <li>F5 is now the standard key shortcut for save screen
        (F10 was conflicting with standard "drop menu" key on gnome and win32),
        changed ' . news_a_href_page('view3dscene', 'view3dscene') . ',
        ' . news_a_href_page('malfunction', 'malfunction') . ',
        ' . news_a_href_page('lets_take_a_walk', 'lets_take_a_walk') . ',
        ' . news_a_href_page('glplotter', 'glplotter_and_gen_function') . ',
        ' . news_a_href_page('bezier_curves', 'bezier_curves') . '.
        ' . news_a_href_page('view3dscene', 'view3dscene') . ' and
        ' . news_a_href_page('glplotter', 'glplotter_and_gen_function') . '
        display FileDialog before saving screen.

        <!-- malfunction, lets_take_a_walk: version release++ to 1.0.1 -->

    <!-- jamy i nory and bad blaster moved to the bottom of index page,
         since they are rather uninteresting and not maintained anymore -->

      <li>Switching to FPC 1.9.8. Some archives on these pages still remain with
        binaries compiled with FPC 1.9.6, but I will replace them
        at their next update. In any case, you should use FPC 1.9.8
        if you\'re going to compile my code.
    </ul>
    </div>

  <div class="old_news_item"><p><span class="old_news_date">February 3, 2005:</span>
    <ul>
      <li><p>Complete rework of Images unit interface.
        Now it has object-oriented interface, much safer and cleaner.
        Unfortunately compatibility with previous versions is broken.

        <p><b>Details</b>:
        Records TImageRec, TRGBImageRec, TAlphaImageRec are now replaced
        with classes TImage, TRGBImage, TAlphaImage (and there\'s also TRGBEImage
        class).

        <p>No functionality is lost, but now using these classes is
        more straightforward,
        no longer need to maintain "dummy" conversion routines
        ImageRec(To|From)(RGB|Alpha).
        Also now you can use Images unit to define new TImage descendant.
        Also many things are now safer and checked by compiler at compile-time.

        <p>Also many docs updated and translated to English in Images unit.

        <p>Sources of most programs needed to be changed accordingly.
        Changed: ' . news_a_href_page('view3dscene', 'view3dscene') . ',
        ' . news_a_href_page('glViewImage', 'glviewimage') . ',
        ' . news_a_href_page('rayhunter', 'rayhunter') . ',
        ' . news_a_href_page('malfunction', 'malfunction') . ',
        ' . news_a_href_page('kulki', 'kambi_lines') . ',
        ' . news_a_href_page('lets_take_a_walk', 'lets_take_a_walk') . ',
        ' . news_a_href_page('bezier_curves', 'bezier_curves') . '.

      <li><p>Improvement in KambiPng/KambiZlib units:

        <p><b>Details</b>:
        Now programs using these units (e.g. indirectly by using Images unit)
        do not require libpng+zlib to be installed on user system.
        <ol>
          <li>view3dscene now runs without libpng and/or zlib installed.
            When opening kings_head.wrl, it displays a warning:
            <i>view3dscene: WARNING: Exception ELibPngNotAvailable occurred when
            trying to load texture from filename textures/crown.png :
            LibPng is not available</i>
            and loads kings_head.wrl correctly (well, without texture
            textures/crown.png).
          <li>No need to define any symbol NOT_USE_LIBPNG at compilation of
            programs that must not depend on libpng+zlib, but must depend on Images
            unit (concerns glcaps and glcaps_glut).
        </ol>

      <li><p>GTK GLWindow: key shortcuts in menus are displayed and handled
        entirely by GTK, not by some hacks in GLWindow unit.

        <p>Also WinAPI key shortcuts to menus are now displayed as they should
        (justified to the right).

      <li><p>key shortcuts changed to conform to be more standard (I\'m trying
        to follow GNOME HIG, although I know that my programs are pretty
        far from it right now), and also to not cause problems as GTK 1
        or GTK 2 menu item shortcuts:
        <ul>
          <li>glViewImage, glplotter: help: F1 (was: \'?\'),
          <li>glViewImage, lets_take_a_walk, view3dscene, glplotter:
            FullScreen on/off: Ctrl+F (was: Tab)
          <li>bezier_curves: Delete selected point: d (was: delete)
            Nothing selected: n (was: backspace).
            (I think that backspace or delete or Ctrl+Shift+A would be a better
            shortcut, but backspace or delete is impossible with GTK 1
            and Ctrl+Shift+A is impossible because of temporary lacks in GLWindow
            interface...)
        </ul>

      <li><p>All my Pascal programs get a version number.
        Existing programs on these pages are initially marked as version 1.0.0.
        Added ' . news_a_href_page('page describing my versioning scheme',
        'versioning') . '.

        <p>All programs with version number accept <code>-v</code> (or <code>--version</code>)
        command-line parameter to display version number.
        Page with ' . news_a_href_page(
        'some notes about parameters understood by my programs',
        'common_options') . ' updated.

      <li><p>From now on, all FPC programs on these pages will be compiled
        with FPC 1.9.6. Compatibility with FPC 1.0.10 is dropped,
        and I will do not even guarantee that my programs compile with FPC 1.9.4.
        So all programs are recompiled and all sources updated,
        ' . news_a_href_page('sources','sources') . '
        page is also updated.

      <li><p>Published imageToPas in <code>units/images/tools/</code>.

      <li><p>Small example of MathExprParser unit in units/base/examples/kambi_calc.dpr.

      <li><p>Polish version of page with ' . news_a_href_page('some notes about parameters understood by my programs',
        'opengl_options') . ' is removed. Only English version will
        be maintained from now on.

      <li><p>glViewImage has new Edit menu with some simple commands that change
        viewed image. This was done mainly to basically test that these functions
        work, but may be useful anyhow.

      <li><p>Fixed some problems with using <code>--fullscreen-custom</code> under Win32.

      <li><p>Removed from sources many files that were needed only for FPC 1.0.10:
        randomconf.inc, mtrand.pas, 10 files *_defpars.inc

      <li><p>UNIX (Linux, FreeBSD) versions of malfunction, kulki,
        lets_take_a_walk again have ability to change screen resolution
        (<code>--fullscreen-custom</code> parameter)
        that was not available due to bug in FPC 1.0.10.

      <li><p>Some usability problems with ' . news_a_href_page('kulki',
        'kambi_lines') . ' solved.
    </ul>
    </div>

  <div class="old_news_item"><p><span class="old_news_date">January 12, 2005:</span>
    Added to ' . news_a_href_page('sources', 'sources') . '
    nice example programs that demonstrate some higher-level
    functionality of my units:
    <ul>
      <li><code>units/opengl/examples/menuTest.dpr</code> (GLWindow with menu)
      <li><code>units/vrml/opengl/examples/simpleViewModel.dpr</code>
        (simple demo of loading and rendering VRML/3DS models and
        allowing user to walk in them; something like extremely-simplified
        view3dscene)
      <li><code>units/vrml/examples/many2vrml.dpr</code> (converting 3DS and others
        to VRML)
    </ul>
    Also ' . news_a_href_page('sources', 'sources') . ' page updated with
    comments about FPC 1.9.6 version. Also some small changes in sources,
    as usual. Added \'xxx.dylib\' library names for Darwin.
    </div>

  <div class="old_news_item"><p><span class="old_news_date">December 10, 2004:</span>
    ' . news_a_href_page('Sources of units and view3dscene', 'sources') . '
    updated: units and view3dscene compile with FPC 1.9.5 from CVS
    from 2004-12-07 (at least under Linux), <code>units/base/examples/</code>
    subdirectory with two small example programs.
    </div>

  <div class="old_news_item"><p><span class="old_news_date">December 5, 2004:</span>
    ' . news_a_href_page('Sources of units', 'sources') . '
     updated to commit many improvements to docs
    (some translations to English and some preparations to generate
    nice docs with pasdoc). No big changes.

    <p>Note: Don\'t expect any new things to happen on these pages
    this month (I\'m busy in some commercial project since some time,
    and I probably won\'t have time this month).
    However expect many work to happen here next year.
    </div>

  <div class="old_news_item"><p><span class="old_news_date">August 23, 2004:</span>
    <ul>
      <li>I ported all programs (except edytorek) to FreeBSD.
        You can download FreeBSD releases (tar.gz archives) from pages
        of appropriate programs.
      <li>All ' . news_a_href_page('Pascal sources', 'sources') . ' updated,
        and the page ' . news_a_href_page('Pascal sources', 'sources') . '
        itself is also updated (more detailed and up-to-date info about FPC
        versions).
      <li>Also, some small updates and bug-fixes for Linux and Windows.
        All programs updated.
        <!--
          Linux: in OpenGL small update to improve some error
            message under newer NVidia drivers,
          Linux: ProcTimer fixed,
          all: DOC/ subdirectory in archives contains documentation
        -->
      <!-- gen_pole_kier removed -->
      <!--
      /*

      DONE:
      release version of every Pascal program compiled and tested
        for FreeBSD and Windows and Linux
      check Linux rayhunter - - was it working before fixing ProcTimer ?
        (probably too late - - it seems it was already recompiled with
        ProcTimer fixed, nevermind)
      check for broken links, check for valid html
      uaktualnic wszystkie archives programow Pascalowych
      uaktualnic wszystkie sources
      upload *.php, everything in sources and archives
      remove from server everything for gen_pole_kier

      ProcTimer corrected under FreeBSD,
      rayhunter updated,
      units updated
      */
      -->
    </ul>
    </div>

  <div class="old_news_item"><p>(August 7: another update of units\' sources, small changes)
        <!--
          update malfunction-src (progress interface changed),
          update units-src
          ( some improvements to work with Unix/BaseUnix units,
            KambiUtils.ProcTimer fixed,
            some functionality from KambiUtils moved to new EnumerateFiles unit,
            OpenGLh corrected to write appropriate TLS warning
              even with new nVidia drivers,
            EXTENDED_EQUALS_DOUBLE )
        --></div>

  <div class="old_news_item"><p><span class="old_news_date">August 2, 2004:</span>
    <ul>
      <li>Updated ' . news_a_href_page('sources', 'sources') . ' of
        standard units, view3dscene and rayhunter. Using correct FPC UNIX
        RTL (instead of Libc always) with FPC 1.9.x basically done, everything
        seems quite ready to be ported to other UNIX-like systems,
        many comments translated to English and, as always, some random small
        improvements.
    </ul></div>

  <div class="old_news_item"><p><span class="old_news_date">31 July 2004:</span>
    <ul>
      <li>Various small internal changes/improvements in
        ' . news_a_href_page('sources', 'sources') . ', ProgressUnit improved.
      <li>Polish versions of glViewImage and view3dscene docs removed,
        they were too outdated.
    </ul>
    <!-- (on HTML pages: /s/~/$HOME/, consequently) --></div>

  <div class="old_news_item"><p><span class="old_news_date">27 June 2004:</span>
    <ul>
      <li>' . news_a_href_page('view3dscene', 'view3dscene') . ' updated:
        "Configure scene loading" submenu
        (it\'s just a GUI for <code>--scene-changes-xxx</code> command-line params)
      <li>Some small updates: to HTML pages,
        to ' . news_a_href_page('malfunction', 'malfunction') . '
        under Linux (no GTK dependency),
        to ' . news_a_href_page('lets_take_a_walk', 'lets_take_a_walk') . '
        under Windows (default device = DirectSound3D),
        to ' . news_a_href_page('rayhunter', 'rayhunter') . '
        (allowed warnings while loading scene)
      <li>Many small improvements in
        ' . news_a_href_page('sources', 'sources') . ',
        among other things units/Makefile supports separate compilation
        of units and things are now more prepared
        for pasdoc. Be ready for more sources updates in the near future &mdash;
        I want to translate many things to English (both user docs
        for some programs and comments in sources) and I want to generate
        nice sources documentation using pasdoc.
    </ul>
    </div>

  <div class="old_news_item"><p><span class="old_news_date">29 May 2004:</span>
    <ul>
      <li>' . news_a_href_page('view3dscene',  'view3dscene') . ' updated:
        <ul>
          <li>Big improvement: "Open File" menu item (key shortcut Ctrl+O),
            i.e. finally changing loaded scene at runtime is fully allowed.
            This also means that now you don\'t have to specify a filename
            to open at command-line.
          <li>Fixed treating of material transparency in OpenGL rendering,
            now you can really see that various values for transparency
            (like 0.1, 0.5, 0.9) make a difference.
            ' . news_a_href_page("Example VRMLs",
              "kambi_vrml_test_suite") . ' extended to confirm this:
            new test scene transparent_materials.wrl.
          <li>FPS timing after the very 1st frame fixed.
          <li>view3dscene now honours AsciiText.justification value,
            text.wrl file (in ' . news_a_href_page("Example VRMLs",
              "kambi_vrml_test_suite") . ') updated to demonstrate this.
          <li>You can now change color of background in view3dscene using
            comfortable dialog box. (GTK dialog box under Linux or WinAPI dialog box).
        </ul>

      <!--
        lets_take_a_walk updated (to get "treating of material transparency" fix)
        rayhunter updated (to get "SFString not enclosed in quotes" fix)
        src of malfunction, kulki, glViewImage, glcaps updated
          (because in the past I completely forgot about GPL headers
          in those sources, now it\'s fixed)
      -->
    </ul>
    </div>

  <div class="old_news_item"><p><span class="old_news_date">25 May 2004:</span>
    <ul>
      <li>bezier_curves updated: smooth interpolated curves
        (smoothly connected Bezier curves) implemented,
        changing colors (using color dialog box) implemented.
        <!--
        <li>fixed some visual unpleasance when opening incorrect files from
          bezier_curves in Linux
        -->
    </ul>
    </div>

  <div class="old_news_item"><p><span class="old_news_date">20 May 2004:</span>
    <ul>
      <li>' . news_a_href_page("view3dscene", "view3dscene") . ' updated:
        <ul>
          <li>small updates in interface ([l] key restored,
            \'...\' added to some menu\'s Caption),
          <li>view3dscene works with scenes with BoundingBox = EmptyBox3d,
          <li>default texture minification method is now LINEAR_MIPMAP_LINEAR
            (best looking),
          <li>Added "When picking with left mouse button, show ..." menu
            to control amount of information shown when picking objects
            with left mouse button, this allows showing some extra info
            aobut materials, lights and shadows, thus making picking
            objects with mouse more usable.
          <li>Small thing in VRML parsing corrected: SFString fields
            not enclosed in double quotes are now parsed correctly.
            <!-- All VRML files generated by Blender should now load correctly. -->
        </ul>
      <li>' . news_a_href_page("Example VRMLs",
        "kambi_vrml_test_suite") . '
        improved: added new tests (empty_xxx.wrl),
        filenames reorganized, model castle.wrl <i>greatly</i> improved
        (with nice textures !).
      <li>' . news_a_href_page("malfunction", "malfunction") . ' updated:
        level "sunny day" completely redesigned,
        rest of levels corrected. I\'m still not satisfied with these levels,
        but at least now they are slightly better.
    </ul>
    </div>

  <div class="old_news_item"><p><span class="old_news_date">8th of May, 2004</span>
    <ul>
      <li>Finally ! ' . news_a_href_page(
        'Sources for Pascal programs are published !', 'sources') . '
        Of course license is GNU GPL.
        On 10th of May I added sources of malfunction and kulki.
        This means that <i>all</i> programs available on these pages,
        with the exception of edytorek, are distributed with sources.
      <li>New program: ' . news_a_href_page('bezier_curves', 'bezier_curves') . '
      <li>Default main page is in English now. Alternative Polish version
        is still available.
    </ul>
    </div>

<!--
  27.04: bezier_curves "raw" wrzucone
-->

  <div class="old_news_item"><p><span class="old_news_date">26th of April, 2004</span>
    <ul>
      <li>UI improved:
        ' . news_a_href_page('glViewImage', 'glviewimage') . ' and
        ' . news_a_href_page('view3dscene', 'view3dscene') . '
        now use GTK / Windows Open/Save file dialogs.
        "Checked" menu items made possible,
        ' . news_a_href_page('view3dscene', 'view3dscene') . '
        and ' . news_a_href_page('glplotter', 'glplotter_and_gen_function') . '
        improved.
        ' . news_a_href_page('view3dscene', 'view3dscene') . '
        uses special menus while raytracing.
      <li>Some problems under Windows with some OpenGLs resolved
        (somewhat random "Invalid floating point operation" errros)
      <li>' . news_a_href_page('glViewImage', 'glviewimage') . ' :
        Ctrl+O adds images to images list,
        menu "Image list" is updated at runtime,
        english docs improved, polish docs dropped.
      <!-- glcaps with - -single and - -double updated -->
      <!-- suma:  glViewImage, view3dscene, glplotter, gen_pole_kier,
           glcaps, glcaps_glut updated -->
    </ul>
    </div>

<!--
14th of April, small update, view3dscene now uses legally free
Bistream Vera fonts. view3dscene archives updated,  rayhunter-src sources
remade.
-->

  <div class="old_news_item"><p><span class="old_news_date">13th of April, 2004</span>
    <ul>
      <li>On 10.04 my small page joined strike against software patents in Europe.
      <li>' . news_a_href_page('glViewImage', 'glviewimage') . ',
          ' . news_a_href_page('glplotter', 'glplotter_and_gen_function') . '
           updated: Linux (GTK) versions stabilized.
      <li>' . news_a_href_page('view3dscene', 'view3dscene') . '
        updated:
        <p>Full English docs finally available.
        Maintenance of Polish docs dropped. Many general reorganizations in docs.

        <p>Many changes in user interface in view3dscene too: some rarely used
        key bindings removed (m, g, n), many parts of menu extended to something
        more comfortable, "lights kind" replaced with 3 separate settings:
        "light calculate", "head light", "use scene lights".
        --lights-kind parameter dropped, new --light-calculate (should be much more
        useful) parameter added.

        <p>Linux (GTK) version stabilized.
      <li>Last but not least: first, unofficial release of sources of
        ' . news_a_href_page('rayhunter', 'rayhunter') . '
        is available here (<i>link not available anymore</i>).
        Currently everything
        is just packaged in one tar.gz file,
        rayhunter-src.tar.gz (<i>link not available anymore</i>). Of course it\'s GNU GPL-licensed.

        <p>You need <a href="http://www.freepascal.org">FreePascal Compiler</a>
        to compile this. You also need to slightly modify sources of FPC.
        Here are diffs for FPC 1.0.10 and FPC 1.9.3 (<i>link not available anymore</i>).
        Note that FPC 1.9.3 (downloadable
        from FreePascal CVS server) requires considerably lesser amount of
        "hacking" to compile rayhunter. If you use FPC 1.9.3, you will only have to
        apply changes to packages/libc unit under Linux, and under Windows
        everything should compile with unmodified FPC 1.9.3 version.
        While with FPC 1.0.10 there are many more patches
        and you will have to add some additional units.
        So I strongly suggest you to use FPC 1.9.3.
    </ul>
    </div>

  <div class="old_news_item"><p><span class="old_news_date">18th of march, 2004</span>: I updated ' . news_a_href_page("view3dscene",
    "view3dscene") . ' and ' . news_a_href_page("glViewImage", "glviewimage") . '.
    Now both programs have a useful menu bar, under Windows and Linux.
    Under Linux this requires installation of GTK 1.x and gtkglarea libraries.
    This is some attempt to make user interface of those programs a little
    more friendly. I\'m curious about your observations about this improvement &mdash;
    how do you like it, how does it work under various Linux and Windows
    versions etc.
    </div>

<p>I started to maintain this update log at 18th march, 2004.</p>
    ')
);
