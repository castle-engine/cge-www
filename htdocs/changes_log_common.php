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

    array('title' => 'castle 0.8.0, view3dscene 2.3.0 released',
          'year' => 2007,
          'month' => 11,
          'day' => 15,
          'pubDate' => /* date_to_timestamp.sh '2007-11-15' */ 1195128000,
          'guid' => '2007-11-15',
          'link' => 'http://vrmlengine.sourceforge.net/',
          'description' =>

"<p>" . this_a_href_page('"The Castle" 0.8.0', 'castle') . " released:

<ul>
  <li><p>New demo level: <i>the fountain</i>, done in pure VRML 2.0
    format (no more VRML 1.0). Shadows for whole level are generated dynamically.
    In the next release, this level is supposed to be augmented with some
    eye candy graphical effects, for now enjoy VRML 2.0 and shadows :)</p></li>

  <li><p>Shadows improvements:</p>

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
</ul>

<p>" . this_a_href_page('view3dscene 2.3.0', 'view3dscene') . " released:

<ul>
  <li>Prototypes (both <tt>PROTO</tt> and <tt>EXTERNPROTO</tt>)
    VRML 2.0 feature is fully implemented now !</li>
  <li>VRML 2.0 lights are correctly handled (<tt>DirectionalLight</tt>
    affects every sibling, positional lights affect whole scene taking
    <tt>radius</tt> into account).<li>
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
</ul>

<p>Updated version of " . this_a_href_page('VRML engine documentation',
'vrml_engine_doc') . " is available, with a chapter about shadows
implementation.</p>
"),

    array('title' => 'glplotter 1.2.0 and view3dscene 2.2.1 released',
          'year' => 2007,
          'month' => 9,
          'day' => 6,
          'pubDate' => /* date_to_timestamp.sh '2007-09-06' */ 1189080000,
          'guid' => '2007-09-06',
          'link' => 'http://vrmlengine.sourceforge.net/',
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

    array('title' => 'view3dscene 2.2.0 and related releases',
          'year' => 2007,
          'month' => 8,
          'day' => 25,
          'pubDate' => /* date_to_timestamp.sh '2007-08-25' */ 1188043200,
          'guid' => '2007-08-25',
          'link' => 'http://vrmlengine.sourceforge.net/',
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

    array('title' => 'Move to SourceForge finished',
          'year' => 2007,
          'month' => 7,
          'day' => 25,
          'pubDate' => /* date_to_timestamp.sh '2007-07-25' */ 1185364800,
          'guid' => '2007-07-25',
          'link' => 'http://vrmlengine.sourceforge.net/',
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

    array('title' => 'Moving to SourceForge: using SF download system',
          'year' => 2007,
          'month' => 7,
          'day' => 23,
          'pubDate' => /* date_to_timestamp.sh '2007-07-23' */ 1185192000,
          'guid' => '2007-07-23',
          'link' => 'http://vrmlengine.sourceforge.net/',
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

function change_log_to_html($change_log_item)
{
  global $month_names;

  return '<p><b>' .
    $change_log_item['title'] . '</b> (' .
    $month_names[$change_log_item['month']] . ' ' .
    $change_log_item['day'] . ', ' .
    $change_log_item['year'] . ') :</p>' .
    $change_log_item['description'];
}

function last_change_log_to_html()
{
  global $changes_log;

  return change_log_to_html($changes_log[0]);
}

?>
