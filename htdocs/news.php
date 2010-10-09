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

/* Return news item from $id, or NULL if not found. */
function vrmlengine_news_item_by_id($id)
{
  global $news;
  foreach($news as $news_item)
    if ($news_item['id'] == $id)
      return $news_item;
  return NULL;
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
    $item = vrmlengine_news_item_by_id($_GET['item']);
    if ($item === NULL)
      die('Invalid news item "' . $_GET['item'] . '"');
  } else
    /* By default, if someone uses just "news.php" URL,
       choose the latest news item. */
    $item = $news[0];

  vrmlengine_header($item['title'] . ' | News', NULL,
    array('index', 'news', 'news.php?item=' . $item['id']));

  echo '<div class="news_item">' . news_to_html($item) . '</div>';

  vrmlengine_footer();
?>
