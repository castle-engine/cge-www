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
    $vrmlengine_sitemap['index']['sub']['news']['sub']['news.php?item=' . $news_item['anchor']] =
      array('title' =>  '(' . vrmlengine_news_date_short($news_item) . ') ' .
        $news_item['title']);
  }
}

/* Return news item from $anchor, or NULL if not found. */
function vrmlengine_news_item_by_anchor($anchor)
{
  global $news;
  foreach($news as $news_item)
    if ($news_item['anchor'] == $anchor)
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

  $item = NULL;
  if (isset($_GET['item']))
  {
    $item = vrmlengine_news_item_by_anchor($_GET['item']);
    if ($item === NULL)
      die('Invalid news item "' . $_GET['item'] . '"');
  }

  if ($item === NULL)
    vrmlengine_header('News', NULL, array('index', 'news')); else
    vrmlengine_header($item['title'] . ' | News', NULL,
      array('index', 'news', 'news.php?item=' . $item['anchor']));

  if ($item === NULL)
  {
    echo pretty_heading($page_title);
    foreach ($news as $news_item)
      echo '<div class="news_item">' . news_to_html($news_item) . '</div>';
  } else
  {
    echo '<div class="news_item">' . news_to_html($item) . '</div>';
  }

  if (!IS_GEN_LOCAL) {
    $counter = php_counter("news", TRUE);
  };

  vrmlengine_footer();
?>
