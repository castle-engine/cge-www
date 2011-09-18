<?php

/* Functions ----------------------------------------------------------------- */

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
    $castle_sitemap['index']['sidebar'] = true;
    $castle_sitemap['index']['sub']['news.php?id=' . $news_item['id']] =
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

    if ($current !== NULL && $current['id'] == $id)
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

/* End of functions ---------------------------------------------------------- */

  require_once "castle_engine_functions.php";
  require_once 'news_common.php';

  /* Must be called when $castle_sitemap is defined
     (by castle_engine_functions.php) and $news is defined
     (by news_common.php), but before castle_header is called
     (which actually searches sitemap and renders sidebar). */
  castle_sitemap_add_news();

  /* calculate $previous_item, $item, $next_item */
  if (isset($_GET['id']))
  {
    castle_news_item_by_id($_GET['id'], $previous_item, $item, $next_item);
    if ($item === NULL)
      die('Invalid news item "' . $_GET['id'] . '"');
  } else
  {
    /* By default, if someone uses just "news.php" URL,
       choose the latest news item. */
    $previous_item = NULL;
    $item = $news[0];
    if (count($news) > 1)
      $next_item = $news[1]; else
      $next_item = NULL;
  }

  /* set $page_basename explicitly */
  $page_basename = 'news.php?id=' . $item['id'];

  castle_header($item['title'] . ' | News', NULL,
    array(MAIN_PAGE_BASENAME, $page_basename));

  /* Calculate $older_newer_bar.
     Remember that naming "previous / next" in the array is a little reversed,
     "previous" is actually older and "next" is newer. */
  $older_newer_bar = '';
  if ($next_item !== NULL || $previous_item !== NULL)
  {
    $older_newer_bar .= '<table class="news_older_newer"><tr>';
    if ($previous_item !== NULL)
      $older_newer_bar .= '<td class="news_newer"><a title="' . $previous_item['title'] . '" href="news.php?id=' . $previous_item['id'] . '">&laquo; Newer</a></td>';
    if ($next_item !== NULL)
      $older_newer_bar .= '<td class="news_older"><a title="' . $next_item['title'] . '" href="news.php?id=' . $next_item['id'] . '">Older &raquo;</a></td>';
    $older_newer_bar .= '</tr></table>';
  }

  echo $older_newer_bar .
    '<div class="news_item" style="clear: both">' . news_to_html($item) . '</div>' .
    $older_newer_bar;

  castle_footer();
?>
