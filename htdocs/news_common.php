<?php /* -*- mode: php -*- */
/* Common content for presenting news as an RSS feed and HTML pages. */

require_once 'castle_engine_functions.php';

/* For given date, convert it to a Unix timestamp
   (seconds since 1970-01-01 00:00:00 UTC).

   Date is converted with time set to 12:00 UTC this day.
   This way the timestamp should represent the same day,
   when represented as date/time in all timezones.
   Otherwise people around the world could see actually a different
   day in their RSS readers than the day written on WWW page like
   [http://castle-engine.sourceforge.net/news.php].

   I don't want to write everywhere that my dates are in UTC,
   as since I only want to set dates --- I can make them the same regardless
   of timezone. */
function date_timestamp($year, $month, $day)
{
  return gmmktime(12, 0, 0, $month, $day, $year);
}

/* For RSS feed, URLs must be absolute (some RSS readers,
   like Google RSS on main page, don't handle relative URLs as they
   should. And indeed, no standard guarantees that relative URLs
   in RSS would be handled OK).

   Because of this, use news_a_href_page and news_a_href_page_hashlink
   for news. */
function news_a_href_page($title, $page_name)
{
  return '<a href="' . CURRENT_URL . $page_name . '.php">' . $title . '</a>';
}

function news_a_href_page_hashlink($title, $page_name, $anchor)
{
  return '<a href="' . CURRENT_URL . $page_name . '.php#' .
    $anchor . '">' . $title . '</a>';
}

/* --------------------------------------------------------------------------- */

  /* An array of news entries.
     It is in format accepted by rss_generator class, but also has some
     extras for news_to_html:

     - year, month, day fields (shown at some places,
       and pubDate timestamp will be auto-generated based on them)

     - short_description: HTML teaser description on the main page.
       If empty, we will take teaser from the normal 'description',
       up to the magic delimiter <!-- teaser ... -->.

       "..." inside this comment will be the additional text present
       only in the teaser. You should use this to close HTML elements
       that should close now in teaser, but remain open in full version.

       If this delimiter is not present, then teaser is just equal to
       full description.

     - guid will be used also for NEWS-ID, to have news.php?item=NEWS-ID page.
       guid is optional --- we'll generate default guid based on date and title,
       if not set.

     - link: do not give it here.
       We'll set link to the URL like xxx/news.php?id=xxx.

     - description: HTML full description. (also used by rss_generator.)
     - title: title (not HTML, i.e. special chars will be escaped). (also used by rss_generator.)

     They must be ordered from the newest to the oldest.
     While it doesn't matter for RSS (feed will be sorted anyway by news
     reader), my HTML converting code depends on it (first feed is the "latest
     update", and feeds are presented in given order on news page).
  */

  $news = array();
  require_once 'news_2011.php';
  require_once 'news_2010.php';
  require_once 'news_2009.php';
  require_once 'news_2008.php';
  require_once 'news_2007.php';

/* --------------------------------------------------------------------------- */

foreach ($news as &$log_entry)
{
  $log_entry['pubDate'] = date_timestamp(
    $log_entry['year'],
    $log_entry['month'],
    $log_entry['day'],
    (isset($log_entry['hour'])   ? $log_entry['hour']   : 0),
    (isset($log_entry['minute']) ? $log_entry['minute'] : 0));

  if (!isset($log_entry['guid']))
    $log_entry['guid'] =
      $log_entry['year'] . '-' .
      $log_entry['month'] . '-' .
      $log_entry['day'] . '-' .
      /* For safety and to make guid look nicer, remove special characters.
         Not all these replacements are really necessary, only <> and &
         to avoid breaking XML, and " to avoid breaking HTML.
         guid is used in both RSS XML and in HTML. */
      strtr(strtolower($log_entry['title']), ' &;,:*/()<>+"', '_____________');

  $log_entry['id'] = $log_entry['guid'];

  $log_entry['link'] = CURRENT_URL . 'news.php?id=' . $log_entry['id'];
}
unset($log_entry);

define('TEASER_DELIMITER_BEGIN', '<!-- teaser ');
define('TEASER_DELIMITER_END', '-->');

function castle_news_date_long($news_item)
{
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
  return $month_names[$news_item['month']] . ' ' .
    $news_item['day'] . ', ' .
    $news_item['year'];
}

function news_to_html($news_item, $full_description = true, $link_to_self = false)
{
  if ($full_description)
  {
    $description = $news_item['description'];
  } else
  if ($news_item['short_description'] != '')
  {
    $description = $news_item['short_description'];
  } else
  {
    $description = $news_item['description'];
    $teaser_delimiter = strpos($description, TEASER_DELIMITER_BEGIN);
    if ($teaser_delimiter !== FALSE)
    {
      $teaser_delimiter_end = strpos($description, TEASER_DELIMITER_END);

      $teaser_closing_str = substr($description,
        $teaser_delimiter + strlen(TEASER_DELIMITER_BEGIN),
        $teaser_delimiter_end -
        ($teaser_delimiter + strlen(TEASER_DELIMITER_BEGIN)));

      $description = substr($description, 0, $teaser_delimiter) .
        '<p><a href="' . CURRENT_URL . 'news.php?item=' .
        $news_item['id'] . '">[read more]</a></p>' .
        $teaser_closing_str;
    }
  }

  $title = $news_item['title'];
  if ($link_to_self)
    $title = '<a href="' . CURRENT_URL . 'news.php?item=' .
      $news_item['id'] . '">' . $title . '</a>';
  $title = '<span class="news_title">' . $title . '</span>';

  return '<p>' . $title . '<br/><span class="news_date">(' .
    castle_news_date_long($news_item) . ')</span></p>' .
    $description;
}

function last_news_to_html($full_description = true)
{
  global $news;

  return news_to_html($news[0], $full_description, /* link to self */ true);
}

?>
