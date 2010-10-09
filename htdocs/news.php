<?php

/* Functions ----------------------------------------------------------------- */

function vrmlengine_news_date_short($news_item)
{
  return sprintf('%04d-%02d-%02d',
    $news_item['year'],
    $news_item['month'],
    $news_item['day']);
}

function vrmlengine_sitemap_add_news()
{
  global $vrmlengine_sitemap, $news;
  foreach ($news as $news_item)
  {
    $vrmlengine_sitemap['index']['sub']['news']['sidebar'] = true;
    $vrmlengine_sitemap['index']['sub']['news']['sub']['news.php?item=' . $news_item['id']] =
      array('title' =>  '(' . vrmlengine_news_date_short($news_item) . ') ' .
        $news_item['title']);
  }
}

/* Return news item from $id. Finds also previous and next news items
   (NULL if current item is first or last). Returns all three items
   as NULL if not found.

   So to easily detect if the item was found, it's enough to just
   check if returned $current === NULL.  */
function vrmlengine_news_item_by_id($id, &$previous, &$current, &$next)
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

  require_once "vrmlengine_functions.php";
  require_once 'news_common.php';

  /* Must be called when $vrmlengine_sitemap is defined
     (by vrmlengine_functions.php) and $news is defined
     (by news_common.php), but before vrmlengine_header is called
     (which actually searches sitemap and renders sidebar). */
  vrmlengine_sitemap_add_news();

  if (isset($_GET['item']))
  {
    vrmlengine_news_item_by_id($_GET['item'], $previous_item, $item, $next_item);
    if ($item === NULL)
      die('Invalid news item "' . $_GET['item'] . '"');
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

  vrmlengine_header($item['title'] . ' | News', NULL,
    array('index', 'news', 'news.php?item=' . $item['id']));

  /* Remember that naming "previous / next" in the array
     is reversed to what user considers previous/next (earlier/later) */
  if ($next_item)
    echo '<div class="news_previous_next" style="float: left"><a title="' . $next_item['title'] . '" href="news.php?item=' . $next_item['id'] . '">&laquo; Previous</a></div>';
  if ($previous_item)
    echo '<div class="news_previous_next" style="float: right"><a title="' . $previous_item['title'] . '" href="news.php?item=' . $previous_item['id'] . '">Next &raquo;</a></div>';

  echo '<div class="news_item" style="clear: both">' . news_to_html($item) . '</div>';

  vrmlengine_footer();
?>
