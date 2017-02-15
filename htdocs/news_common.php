<?php /* -*- mode: kambi-php -*- */

/* PHP functions for handling news, and actual $news array.
   This is a common file for everything that deals with displaying
   news as an RSS feed and HTML content. */

require_once 'castle_engine_functions.php';

/* For given date, convert it to a Unix timestamp
   (seconds since 1970-01-01 00:00:00 UTC).

   Date is converted with time set to 12:00 UTC this day.
   This way the timestamp should represent the same day,
   when represented as date/time in all timezones.
   Otherwise people around the world could see actually a different
   day in their RSS readers than the day written on WWW page like
   [https://castle-engine.sourceforge.io/news.php].

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
  /* calculate $title */
  $title = $news_item['title'];
  // use short_title if applicable
  if (!$full_description && !empty($item['short_title'])) {
    $title = $item['short_title'];
  }
  $title = '<span class="news_title">' . $title . '</span>';

  /* calculate $date */
  $date = castle_news_date_long($news_item);
  $date = '<span class="news_date">' . $date . '</span>';

  /* calculate $description, return result */
  if ($full_description) {
    $description = $news_item['description'];

    /* add images to $description */
    if (isset($news_item['images'])) {
      $description = castle_thumbs($news_item['images']) . $description;
    }

    return '<p>' . $title . '<br/></p>' . $date . $description;
  } else {
    if ($news_item['short_description'] != '') {
      $description = $news_item['short_description'];
    } else {
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
          /*
          '<p><a href="' . CURRENT_URL . 'news.php?item=' .
          $news_item['id'] . '">[read more]</a></p>' .
          */
          $teaser_closing_str;
      }
    }

    $result = '<p class="news_title_wrapper">' . $title . '</p>' .
      '<p class="news_date_wrapper">' . $date . '</p>';

    /* add images to $result */
    if (isset($news_item['images'][0])) {
      $image = $news_item['images'][0];
      $result = '<img
        class="news-teaser-image"
        src="' . CURRENT_URL . 'images/teaser_size/' . $image['filename'] . '"
        alt="' . $image['titlealt'] . '"
      />' . $result;
    }

    // wrap in a link if needed
    if ($link_to_self) {
      $result = '<a href="' . CURRENT_URL . 'news.php?item=' .
        $news_item['id'] . '">' . $result . '</a>';
    }

    return $result;
  }
}

function castle_news_date_short($news_item)
{
  return sprintf('%04d-%02d-%02d',
    $news_item['year'],
    $news_item['month'],
    $news_item['day']);
}

function castle_sitemap_add_news()
{
  global $castle_sitemap, $news;
  foreach ($news as $news_item)
  {
    $castle_sitemap['news']['sidebar'] = true;
    $castle_sitemap['news']['sub']['news.php?id=' . $news_item['id']] =
      array('title' =>  '(' . castle_news_date_short($news_item) . ') ' .
        $news_item['title']);
  }
}

/* Return news item from $id. Finds also previous and next news items
   (NULL if current item is first or last). Returns all three items
   as NULL if not found.

   So to easily detect if the item was found, it's enough to just
   check if returned $current === NULL.  */
function castle_news_item_by_id($id, &$previous, &$current, &$next)
{
  global $news;

  $previous = NULL;
  $current = NULL;
  $next = NULL;

  foreach($news as $news_item)
  {
    $previous = $current;
    $current = $next;
    $next = $news_item;

    if ($current !== NULL &&
        (   $current['id']              == $id ||
          @($current['alternative_id']) == $id ) )
      return;
  }

  /* one more step */
  $previous = $current;
  $current = $next;
  $next = NULL;
  if ($current !== NULL && $current['id'] == $id)
    return;

  $previous = NULL;
  $current = NULL;
  $next = NULL;
}

/* --------------------------------------------------------------------------- */

/* An array of news entries.
   It is in format accepted by rss_generator class, but also has some
   extras for news_to_html:

   - short_title: used instead of title, if defined, for short view.

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
     if not set. If you provide it, make sure it does not contain any special
     characters for HTML or XML attributes or content.

   - link: do not give it here.
     We'll set link to the URL like xxx/news.php?id=xxx.

   - images: list of images, like arguments to castle_thumbs.
     1st image is the main one.

   - description: HTML full description. (also used by rss_generator.)
   - title: title (not HTML, i.e. special chars will be escaped). (also used by rss_generator.)
   - alternative_id: if set, then it's an alternative id (may be used in URLs
     like https://castle-engine.sourceforge.io/news.php?id=XXX).
     Useful when original id is too long, but I don't want to change it
     (as this would affect RSS readers that would see new id).

   They must be ordered from the newest to the oldest.
   While it doesn't matter for RSS (feed will be sorted anyway by news
   reader), my HTML converting code depends on it (first feed is the "latest
   update", and feeds are presented in given order on news page).
*/

$news = array();
require_once 'news_2015.php';
require_once 'news_2014.php';
require_once 'news_2013.php';
require_once 'news_2012.php';
require_once 'news_2011.php';
require_once 'news_2010.php';
require_once 'news_2009.php';
require_once 'news_2008.php';
require_once 'news_2007.php';

/* Post-process $news -------------------------------------------------------- */

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
