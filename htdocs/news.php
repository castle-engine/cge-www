<?php
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
    $vrmlengine_sitemap['index']['sub']['news']['sub']['news#' . $news_item['anchor']] =
      array('title' =>  '(' . vrmlengine_news_date_short($news_item) . ') ' .
        $news_item['title']);
  }
}

  require_once "vrmlengine_functions.php";
  require_once 'news_common.php';

  /* Must be called when $vrmlengine_sitemap is defined
     (by vrmlengine_functions.php) and $news is defined
     (by news_common.php), but before vrmlengine_header is called
     (which actually searches sitemap and renders sidebar). */
  vrmlengine_sitemap_add_news();

  vrmlengine_header('News', NULL, array('index'));

  echo pretty_heading($page_title);

  foreach ($news as $news_item)
    echo '<div class="news_item">' . news_to_html($news_item) . '</div>';

  if (!IS_GEN_LOCAL) {
    $counter = php_counter("news", TRUE);
  };

  vrmlengine_footer();
?>
