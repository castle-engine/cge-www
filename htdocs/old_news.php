<?php

require_once "castle_engine_functions.php";
require_once 'news_common.php';

/* Must be called when $castle_sitemap is defined
   (by castle_engine_functions.php) and $news is defined
   (by news_common.php), but before castle_header is called
   (which actually searches sitemap and renders sidebar). */
castle_sitemap_add_news();

/* Calculate $previous_item, $item, $next_item.
   For compatibility with old links, we handle also ?item=xxx like ?id=xxx. */
if (isset($_GET['id']) || isset($_GET['item']))
{
  $news_id = isset($_GET['id']) ? $_GET['id'] : $_GET['item'];
  castle_news_item_by_id($news_id, $previous_item, $item, $next_item);
  if ($item === NULL)
    die('Invalid news item "' . $news_id . '"');
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
$page_basename = 'old_news.php?id=' . $item['id'];

$castle_header_parameters = array(
  'path' =>  array('news', $page_basename),
  // canonical URL will point to the current news post
  'canonical_url' => CASTLE_PROD_URL . $page_basename
);
/* use the first news image as social_share_image, if possible */
if (isset($item['images'][0]['filename'])) {
  $castle_header_parameters['social_share_image'] = $item['images'][0]['filename'];
}
castle_header($item['title'] . ' | News', $castle_header_parameters);

/* Calculate $older_newer_bar.
   Remember that naming "previous / next" in the array is a little reversed,
   "previous" is actually older and "next" is newer. */
$older_newer_bar = '';
if ($next_item !== NULL || $previous_item !== NULL)
{
  $older_newer_bar .= '<div class="btn-group btn-group-justified" role="group">';
  if ($previous_item !== NULL)
    $older_newer_bar .= '<a title="' . htmlspecialchars($previous_item['title']) . '" href="old_news.php?id=' . $previous_item['id'] . '" class="btn btn-secondary">&laquo; Newer</a>';
  if ($next_item !== NULL)
    $older_newer_bar .= '<a title="' . htmlspecialchars($next_item['title']) . '" href="old_news.php?id=' . $next_item['id'] . '" class="btn btn-secondary">Older &raquo;</a>';
  $older_newer_bar .= '</div>';
}

echo $older_newer_bar;

if ($item === $news[0])
  echo '<h2>Latest news:</h2>'; else
  echo '<br>';
echo '<div class="news_item">' . news_to_html($item) . '</div>';

echo $older_newer_bar;

castle_footer();
