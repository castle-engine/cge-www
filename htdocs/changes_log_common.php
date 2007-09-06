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

    array('title' => 'view3dscene 2.2.0 and related releases',
          'year' => 2007,
          'month' => 8,
          'day' => 25,
          'pubDate' => /* date_to_timestamp.sh '2007-08-25' */ 1188000000,
          'guid' => '2007-08-25',
          'link' => 'http://vrmlengine.sourceforge.net/',
          'description' =>

"<ul>
  <li>" . a_href_page('view3dscene 2.2.0', 'view3dscene') . " release:
    view3dscene can display animations now (for now in " . a_href_page(
    "Kanim (Kambi VRML engine animations) format", 'kanim_format') . " and
    MD3).</li>
  <li>" . a_href_page('Kambi VRML test suite 1.1.0',
    'kambi_vrml_test_suite') . " release: many kanim demos added.</li>
  <li>" . a_href_page('Kambi VRML game engine 1.1.0',
    'kambi_vrml_game_engine') . " release: many changes, for animations
    in view3dscene, also GLMenu and GameSoundEngine units added
    (some \"The Castle\" code improved and moved to a generally-usefull
    units area), bugfixes to MD3 texture handling.</li>
</ul>"),

    array('title' => 'Move to SourceForge finished',
          'year' => 2007,
          'month' => 7,
          'day' => 25,
          'pubDate' => /* date_to_timestamp.sh '2007-07-25' */ 1185321600,
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
          'pubDate' => /* date_to_timestamp.sh '2007-07-23' */ 1185148800,
          'guid' => '2007-07-23',
          'link' => 'http://vrmlengine.sourceforge.net/',
          'description' =>

"<p>Download links for most VRML stuff on this page direct to SourceForge
file release system now. This is another step in moving to
<a href=\"http://sourceforge.net/projects/vrmlengine\">vrmlengine
on SourceForge</a>.

<p>Also, some things now get version numbers:
" . a_href_page('Kambi VRML game engine', 'kambi_vrml_game_engine') . " (1.0.0),
" . a_href_page("Kambi VRML test suite", "kambi_vrml_test_suite") . " (1.0.0).
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
