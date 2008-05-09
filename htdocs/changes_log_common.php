<?php
  /* This is common content for both making RSS feed of changes_log,
     and HTML version. You must have already common_set_page_functions
     done before requiring this !
  */

/* Beware that making timestamps using PHP is broken:

     function date_timestamp($year, $month, $day)
     {
       return gmmktime(0, 0, 0, $month, $day, $year);
     }

     echo date_timestamp(2007, 07, 25) . '<br/>';
     echo date_timestamp(2007, 08,  1);

   results in
     1185321600
     1164931200

   which is obviously wrong, since the 2nd timestamp is smaller than the
   1st one... This is with PHP 5.2.3-1+b1 from Debian package.
   For safety, I'll just generate timestamps with ../scripts/date_to_timestamp.sh.
   This is correct:
     $ ./date_to_timestamp.sh '2007-07-25'
     1185321600 // ok, just like PHP gmmktime
     $ ./date_to_timestamp.sh '2007-08-01'
     1185926400 // ok, larger than previous one, different than PHP gmmktime
*/

function this_a_href_page($title, $page_name)
{
  /* For RSS feed, URLs must be absolute (some RSS readers,
     like Google RSS on main page, don't handle relative URLs as they
     should. And indeed, no standard guarantees that relative URLs
     in RSS would be handled OK). */
  return '<a href="http://vrmlengine.sourceforge.net/' . $page_name .
    '.php">' . $title . '</a>';
}

  /* This an array of changes_log entries.
     It is in format accepted by rss_generator class, but also has some
     extras for changes_log_to_html:
     - year, month, day fields: this must always match pubDate timestamp

     They must be ordered from the newest to the oldest.
     While it doesn't matter for RSS (feed will be sorted anyway by news
     reader), my HTML converting code depends on it (first feed is the "latest
     update", and feeds are presented in given order on changes_log page).
  */

  $changes_log = array(

/* --------------------------------------------------------------------------- */

    array('title' => 'Demo movies',
          'year' => 2008,
          'month' => 5,
          'day' => 9,
          'pubDate' => /* date_to_timestamp.sh '2008-05-09' */ 1210334400,
          'guid' => '2008-05-09',
          'link' => 'http://vrmlengine.sourceforge.net/',
          'short_description' => '',
          'description' =>
"I present " . a_href_page('three demo movies', 'movies') . "
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
          'pubDate' => /* date_to_timestamp.sh '2008-02-25' */ 1203940800,
          'guid' => '2008-02-25',
          'link' => 'http://vrmlengine.sourceforge.net/',
          'short_description' => '',
          'description' =>
"Released " . a_href_page('engine version 1.3.1', 'kambi_vrml_game_engine') . ":
fixed Lazarus packages compilation, for developers that want to use our
engine with Lazarus."),

    array('title' => 'Engine 1.3.0 release, view3dscene 2.4.0, castle 0.8.1, many other releases',
          'year' => 2008,
          'month' => 2,
          'day' => 19,
          'pubDate' => /* date_to_timestamp.sh '2008-02-19' */ 1203422400,
          'guid' => '2008-02-19',
          'link' => 'http://vrmlengine.sourceforge.net/',
          'short_description' =>
"<p>Many long-awaited graphic features implemented in our engine.
Released " . a_href_page('engine version 1.3.0', 'kambi_vrml_game_engine') . ",
" . this_a_href_page('view3dscene 2.4.0', 'view3dscene') . " and
" . this_a_href_page('castle 0.8.1', 'castle') . ".
Below is only a shortcut of the most important changes
(see " . a_href_page('changes_log', 'changes_log') . " for a full list of changes) :</p>

<ul>
  <li><a href=\"http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#ext_bump_mapping\">Bump
    mapping</a>. Various bump mapping methods are implemented,
    the most advanced being steep parallax mapping with self-shadowing.</li>

  <li>Shaders support, including <a href=\"http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#ext_shaders\">specifying GLSL
    shaders in VRML</a>. Programmers may easily initialize
    GLSL and ARB assembly shaders.</li>

  <li>Anti-aliasing available in both "
    . this_a_href_page('view3dscene', 'view3dscene') . "
    and " . this_a_href_page('castle', 'castle') . ".</li>

  <li>Collada model format basic support (1.3.x and 1.4.x)
    added to the engine, you can also convert Collada files to VRML 2.0.</li>

  <li><i>Examine</i> mode allows to rotate and move the scene by mouse
    dragging.</li>

  <li><tt>--screenshot</tt> command-line option for
    " . this_a_href_page('view3dscene', 'view3dscene') . ",
    to take screenshots of the scene in batch mode.</li>

  <li>" . this_a_href_page('Our Blender VRML 97 exporter script', 'blender_stuff') . "
    improved: <i>set solid / set smooth / autosmooth / autosmooth degrees</i>
    settings from Blender are correctly exported to VRML.</li>
</ul>

<p>Other releases:
" . this_a_href_page('Kambi VRML test suite 2.1.0', 'kambi_vrml_test_suite') . "
has many new tests/demos for new features (bump mapping, GLSL,
Collada format). Also released most other programs,
to bring them up-to-date with current engine state.</p>
",
          'description' =>

"<p>Released " . a_href_page('engine version 1.3.0', 'kambi_vrml_game_engine') . ",
" . this_a_href_page('view3dscene 2.4.0', 'view3dscene') . " and
" . this_a_href_page('castle 0.8.1', 'castle') . ".
Many long-awaited graphic features implemented:</p>

<ul>
  <li><p><b>Bump mapping</b>: <a href=\"http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#ext_bump_mapping\">VRML
    renderer allows bump mapping</a>. Various bump mapping methods
    are implemented (best method is auto-detected and used at runtime):
    dot by multitexturing (not normalized and normalized by cube map),
    dot by GLSL (optionally with parallax mapping, optionally with
    steep parallax mapping and self-shadowing).</p>

    <p>" . this_a_href_page('view3dscene', 'view3dscene') . " allows to easily
    turn on bump mapping, assuming model specifies normal maps.<br/>
    " . this_a_href_page('castle', 'castle') . " uses bump mapping, for now only
    on the \"fountain\" level.</p>

    <p><i>For programmers</i>: see also <tt>kambi_vrml_game_engine/3dmodels.gl/examples/bump_mapping</tt> demo in engine sources,
    it demonstrates emboss, dot and all other bump mapping
    methods built in VRML engine. Also my notes about emboss and dot
    (by multitexturing) bump mapping methods may be interesting:
    see <a href=\"http://vrmlengine.svn.sourceforge.net/viewvc/*checkout*/vrmlengine/trunk/kambi_vrml_game_engine/3dmodels.gl/examples/bump_mapping/README\">bump_mapping/README</a>.</p>
  </li>

  <li><p><b>GLSL shaders support:</b> engine supports easily using
    ARB vertex / fragment programs (assembly shaders) and&nbsp;GLSL.</p>

    <p>You can also <a href=\"http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#ext_shaders\">directly
    specify GLSL shaders inside VRML file</a>, which is a great feature
    for VRML authors. Syntax of shaders in VRML follows X3D specification.</p>

    <p>" . this_a_href_page('view3dscene', 'view3dscene') . " allows to control
    GLSL shaders and even simply assign GLSL shaders
    (check out <i>Edit -&gt; Simply assign GLSL shader to all objects</i>
    menu item), so you can test your shaders with any 3D model.</p>

    <p><i>For programmers</i>: you may find useful my notes about shading languages in
    <a href=\"http://vrmlengine.svn.sourceforge.net/viewvc/*checkout*/vrmlengine/trunk/kambi_vrml_game_engine/opengl/examples/shading_langs/README\">shading_langs_demo/README</a>.</p>

  <li><p><b>Anti-aliasing</b> available (if multisampling is supported by
    graphic card). " . this_a_href_page('view3dscene', 'view3dscene') . "
    has comfortable menu <i>File -&gt; Startup Preferences -&gt; Anti aliasing</i>
    and also a command-line option <tt>--anti-alias</tt> to control this,
    " . this_a_href_page('castle', 'castle') . " has comfortable menu item
    in <i>Video options</i>.</p>

  <li><p><b>Collada model format</b> basic support (1.3.x and 1.4.x)
    added to the engine (" . this_a_href_page('view3dscene', 'view3dscene') . "
    and everything else open Collada files, just like any other model format;
    you can also convert Collada files to VRML 2.0).</p>
  </li>

  <li><p>Wavefront OBJ format handling improved (we handle normal vectors,
    materials, textures).</p>
  </li>

  <li><p><tt>Examine</tt> mode allows to rotate and move the scene by mouse
    dragging. This is more intuitive and sometimes comfortable.
    This feature is mostly noticeable in
    " . this_a_href_page('view3dscene', 'view3dscene') . ", although
    some example programs in engine demos also use such examine mode
    and also benefit from this.</p>
  </li>
</ul>

<p>Besides improvements above, some improvements specific to
" . this_a_href_page('view3dscene', 'view3dscene') . ":</p>

<ul>
  <li><tt>--screenshot</tt> command-line option, requested a few times,
    to take screenshots of the scene in batch mode.</li>
  <li>Various <i>fill modes</i>. Previously only <i>normal</i> and
    <i>wireframe</i> were available, now we have a couple more,
    like solid wireframe and silhouette. Keys changed (\"w\" key no longer works,
    \"f\" key changes fill mode instead of fog state).</li>
</ul>

<p>Also " . this_a_href_page('our Blender VRML 97 exporter script', 'blender_stuff') . "
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
    <tt>kambi_vrml_game_engine/3dmodels.gl/examples/plane_mirror_and_shadow.pasprogram</tt>
    to test plane-projected shadows and plane mirrors. Plane-projected shadows
    is only for simple demo (we have implemented shadow volumes, thousand times better
    algorithm, after all), but plane mirrors will be implemented
    in the future in the VRML engine (using \"mirror\" <tt>Material</tt>
    field automatically).</li>
</ul>

<p>Other releases:
" . this_a_href_page('Kambi VRML test suite 2.1.0', 'kambi_vrml_test_suite') . "
has many new tests/demos for new features (bump mapping, GLSL,
Collada format). Also released:
" . this_a_href_page('rayhunter 1.2.2', 'rayhunter') . ",
" . this_a_href_page('lets_take_a_walk 1.2.1', 'lets_take_a_walk') . ",
" . this_a_href_page('malfunction 1.2.4', 'malfunction') . ",
" . this_a_href_page('kambi_lines 1.1.4', 'kambi_lines') . ",
" . this_a_href_page('glplotter 1.2.1', 'glplotter_and_gen_function') . ",
" . this_a_href_page('glViewImage 1.2.2', 'glviewimage') . ",
" . this_a_href_page('bezier_curves 1.1.6', 'bezier_curves') . ",
" . this_a_href_page('glcaps 1.1.2', 'glcaps') . ",
mainly to bring them up-to-date with current engine state.</p>
"),

/* --------------------------------------------------------------------------- */

    array('title' => 'castle 0.8.0, view3dscene 2.3.0 released',
          'year' => 2007,
          'month' => 11,
          'day' => 17,
          'pubDate' => /* date_to_timestamp.sh '2007-11-17' */ 1195300800,
          'guid' => '2007-11-17',
          'link' => 'http://vrmlengine.sourceforge.net/',
          'short_description' =>
"<p>A lot of updates today. Here's a shortcut of only the most important changes
(see " . a_href_page('changes_log', 'changes_log') . " for a full list of changes) :
<ul>
  <li>" . a_href_page('"The Castle" 0.8.0', 'castle') . " released:
    new demo level <i>the fountain</i> (VRML 2.0, dynamic shadows),
    many shadows improvements (z-fail, proper detection z-pass/z-fail, shadow
    culling etc.), conserve memory feature (all Radeon issues should be fixed
    now).</li>
  <li>" . a_href_page('view3dscene 2.3.0', 'view3dscene') . " released:
    prototypes (both <tt>PROTO</tt> and <tt>EXTERNPROTO</tt>),
    VRML 2.0 lights are correctly handled,
    handling of colors for <tt>IndexedFaceSet</tt> and <tt>IndexedLineSet</tt>,
    <a href=\"http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#ext_text3d\">Text3D extension</a>.</li>
  <li>" . a_href_page('Kambi VRML game engine 1.2.0', 'kambi_vrml_game_engine') . "
    released: most things mentioned above were actually implemented in the base
    engine units, also: x86-64 port.</li>
  <li>" . a_href_page('Kambi VRML test suite 2.0.0', 'kambi_vrml_test_suite') . "
    released: many new tests for new features.</li>
  <li>" . a_href_page('Blender VRML stuff page added, with improved VRML 2.0
    exporter and kanim exporter', 'blender_stuff') . ".</li>
  <li>Updated version of " . a_href_page('VRML engine documentation',
    'vrml_engine_doc') . " is available, with a
    <a href=\"http://vrmlengine.sourceforge.net/vrml_engine_doc/output/xsl/html/chapter.shadows.html\">chapter
    about shadows implementation.</a></li>
</ul>",
          'description' =>

"<p>" . this_a_href_page('"The Castle" 0.8.0', 'castle') . " released:

<ul>
  <li><p>New demo level: <i>the fountain</i>, done in pure VRML 2.0
    format (no more VRML 1.0). Shadows for whole level are generated dynamically.
    In the next release, this level is supposed to be augmented with some
    eye candy graphical effects, for now enjoy VRML 2.0 and shadows :)</p></li>

  <li><p>Shadows improvements (see also
    <a href=\"http://vrmlengine.sourceforge.net/vrml_engine_doc/output/xsl/html/chapter.shadows.html\">new chapter
    in documentation about shadows</a>) :</p>

    <ul>
      <li>First of all, z-fail implemented and proper detection when z-fail
        is needed implemented, so faster z-pass is used when possible.
        \"The Castle\" shows (toggle with Tab, just like for FPS) number
        of shadows qualified as z-pass, z-fail, z-fail with light cap needed etc.
      <li>Shadow volumes silhouette optimization improved: now models don't have
        to be perfect manifold to use this. See
        <tt>kambi_vrml_game_engine/3dmodels.gl/examples/shadow_volume_test/</tt>
        demo, in particular the <tt>shadow_volume_test_ball_with_tentacles.sh</tt>
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

<p>" . this_a_href_page('view3dscene 2.3.0', 'view3dscene') . " released:

<ul>
  <li>Prototypes (both <tt>PROTO</tt> and <tt>EXTERNPROTO</tt>)
    VRML 2.0 feature is fully implemented now !</li>
  <li>VRML 2.0 lights are correctly handled (<tt>DirectionalLight</tt>
    affects every sibling, positional lights affect whole scene taking
    <tt>radius</tt> into account).</li>
  <li><tt>ROUTE</tt> constructs of VRML 2.0 are parsed now.
    Although they still don't actually <b>do</b> anything.
    So at least scenes using routes are partially handled (routes are simply
    ignored), instead of just producing an error.</li>
  <li>Default blending dest factor for view3dscene is
    <tt>GL_ONE_MINUS_SRC_ALPHA</tt>, since this is expected by most VRML
    authors.</li>
  <li>VRML files compressed by gzip are handled OK even if they have
    normal <tt>.wrl</tt> extension.</li>
  <li>--write-to-vrml fixed</li>
  <li>Handling of colors (<tt>color</tt>, <tt>colorPerVertex</tt>,
    <tt>colorIndex</tt>) for <tt>IndexedFaceSet</tt> and <tt>IndexedLineSet</tt>
    done.</li>
  <li>NavigationInfo.speed is now handled correctly (it sets speed per second)</li>
  <li><a
    href=\"http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#ext_text3d\">Text3D extension</a>.</li>
</ul>

<p>" . this_a_href_page('Kambi VRML game engine 1.2.0', 'kambi_vrml_game_engine') . "
released. Most features mentioned above for view3dscene and castle
(shadows, optimizations, all VRML 2.0 features) are actually implemented
in the engine, and other programs only use them. Additionally, some
more internal features not mentioned above:</p>

<ul>
  <li><p>Engine is ported and works flawlessly on x86-64 on Linux.
    No more only 32-bit :) Also, it's partially ported to Windows x84-64
    (tested compilation with cross compiler, no actual run tests).</p>

    <p>This also results in the change of archive binary names:
    they all get <tt>i386</tt> after their name, eventually I may release
    precompiled versions for <tt>x86-64</tt> too.</p></li>

  <li><p>GLWindow allows to change cursor shape.</p></li>

  <li><p>Everything is compiled using new FPC 2.2.0.</p></li>
</ul>

<p>" . this_a_href_page('Kambi VRML test suite 2.0.0', 'kambi_vrml_test_suite') . "
released: many new tests to test new features (protos, external protos,
colors, light scope, running path to test NavigationInfo.speed, 3d text),
some important VRML 1.0 tests ported to VRML 2.0 too (castle,
relative_names, texture_test, house behind the glass).</p>

<p>" . this_a_href_page('Blender VRML stuff page added, with improved VRML 2.0
exporter and kanim exporter', 'blender_stuff') . ".</p>

<p>Updated version of " . this_a_href_page('VRML engine documentation',
'vrml_engine_doc') . " is available, with a chapter about shadows
implementation.</p>
"),

/* --------------------------------------------------------------------------- */

    array('title' => 'glplotter 1.2.0 and view3dscene 2.2.1 released',
          'year' => 2007,
          'month' => 9,
          'day' => 6,
          'pubDate' => /* date_to_timestamp.sh '2007-09-06' */ 1189080000,
          'guid' => '2007-09-06',
          'link' => 'http://vrmlengine.sourceforge.net/',
          'short_description' => '',
          'description' =>

"<ul>
  <li>" . this_a_href_page('glplotter 1.2.0 and gen_function 1.0.2',
    'glplotter_and_gen_function') . " released: glplotter GUI greatly improved:
    Open/Add menu items to open graphs from files
    and to generate graphs from function expressions.
    This means that now you don't have to specify graphs at command-line,
    and now you don't have to write pipes with gen_function.
    Also documentation and some options translated finally to English.</li>
  <li>" . this_a_href_page('view3dscene 2.2.1', 'view3dscene') . " released:
    bug fix release. Fixed crash when removing geometry node from
    VRML 2.0 hierarchy. Fixed jagged animation when world time was
    really large (usually occurs when \"on display\" time pass was
    really large for some time). Fixed messing the gravity
    \"up\" vector when reopening the scene.</li>
  <li>" . this_a_href_page('Kambi VRML game engine 1.1.1',
    'kambi_vrml_game_engine') . " released: changes needed by
    view3dscene and glplotter above.</li>
  <li><a href=\"http://vrmlengine.sourceforge.net/changes_log_feed.php\">RSS
    feed</a> listing all changes is available now.
    <small>SouceForge already made RSS feeds for our project,
    but they didn't allow me HTML code there, and HTML links
    are simply useful for my changes_log messages.</small></li>
</ul>"),

/* --------------------------------------------------------------------------- */

    array('title' => 'view3dscene 2.2.0 and related releases',
          'year' => 2007,
          'month' => 8,
          'day' => 25,
          'pubDate' => /* date_to_timestamp.sh '2007-08-25' */ 1188043200,
          'guid' => '2007-08-25',
          'link' => 'http://vrmlengine.sourceforge.net/',
          'short_description' => '',
          'description' =>

"<ul>
  <li>" . this_a_href_page('view3dscene 2.2.0', 'view3dscene') . " release:
    view3dscene can display animations now (for now in " .
     this_a_href_page(
    "Kanim (Kambi VRML engine animations) format", 'kanim_format') . " and
    MD3).</li>
  <li>" . this_a_href_page('Kambi VRML test suite 1.1.0',
    'kambi_vrml_test_suite') . " release: many kanim demos added.</li>
  <li>" . this_a_href_page('Kambi VRML game engine 1.1.0',
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
          'pubDate' => /* date_to_timestamp.sh '2007-07-25' */ 1185364800,
          'guid' => '2007-07-25',
          'link' => 'http://vrmlengine.sourceforge.net/',
          'short_description' => '',
          'description' =>

"<p>The move of <i>Kambi VRML game engine</i> project to SourceForge is finished !
In fact, if you're reading this text, then you already view our page
as hosted on SourceForge.</p>

<p>Being on SourceForge gives us many new features, most important ones:
<a href=\"http://sourceforge.net/project/showfiles.php?group_id=200653\">file
downloads</a> use all the power and speed of SF mirrors,
development is done inside
<a href=\"http://vrmlengine.svn.sourceforge.net/viewvc/vrmlengine/\">publicly
visible SVN repository</a>, we have a public " . MAILING_LIST_LINK . ",
we have trackers for
<a href=\"" .  BUGS_TRACKER_URL . "\">bugs</a>,
<a href=\"" .  FEATURE_REQUESTS_TRACKER_URL . "\">feature requests</a>,
<a href=\"" .  PATCHES_TRACKER_URL . "\">patches</a>,
there's <a href=\"http://sourceforge.net/export/rss2_projfiles.php?group_id=200653\">RSS
feed to monitor new releases</a>.</p>"),

/* --------------------------------------------------------------------------- */

    array('title' => 'Moving to SourceForge: using SF download system',
          'year' => 2007,
          'month' => 7,
          'day' => 23,
          'pubDate' => /* date_to_timestamp.sh '2007-07-23' */ 1185192000,
          'guid' => '2007-07-23',
          'link' => 'http://vrmlengine.sourceforge.net/',
          'short_description' => '',
          'description' =>

"<p>Download links for most VRML stuff on this page direct to SourceForge
file release system now. This is another step in moving to
<a href=\"http://sourceforge.net/projects/vrmlengine\">vrmlengine
on SourceForge</a>.

<p>Also, some things now get version numbers:
" . this_a_href_page('Kambi VRML game engine', 'kambi_vrml_game_engine') . " (1.0.0),
" . this_a_href_page("Kambi VRML test suite", "kambi_vrml_test_suite") . " (1.0.0).
</p>")

  );

/* --------------------------------------------------------------------------- */

$month_names = array(
  1 => 'January',
  2 => 'February',
  3 => 'March',
  4 => 'April',
  5 => 'May',
  6 => 'June',
  7 => 'July',
  8 => 'August',
  9 => 'September',
  10 => 'October',
  11 => 'November',
  12 => 'December'
);

function change_log_to_html($change_log_item, $full_description = true)
{
  global $month_names;

  $description = ($full_description || $change_log_item['short_description'] == '' ?
    $change_log_item['description'] :
    $change_log_item['short_description']);

  return '<p><b>' .
    $change_log_item['title'] . '</b> (' .
    $month_names[$change_log_item['month']] . ' ' .
    $change_log_item['day'] . ', ' .
    $change_log_item['year'] . ') :</p>' .
    $description;
}

function last_change_log_to_html($full_description = true)
{
  global $changes_log;

  return change_log_to_html($changes_log[0], $full_description);
}

?>
