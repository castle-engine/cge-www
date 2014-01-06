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
$page_basename = 'news.php?id=' . $item['id'];

castle_header($item['title'] . ' | News', NULL,
  array('news', $page_basename));

/* Calculate $older_newer_bar.
   Remember that naming "previous / next" in the array is a little reversed,
   "previous" is actually older and "next" is newer. */
$older_newer_bar = '';
if ($next_item !== NULL || $previous_item !== NULL)
{
  $older_newer_bar .= '<table class="news_older_newer"><tr>';
  if ($previous_item !== NULL)
    $older_newer_bar .= '<td class="news_newer"><a title="' . htmlspecialchars($previous_item['title']) . '" href="news.php?id=' . $previous_item['id'] . '">&laquo; Newer</a></td>';
  if ($next_item !== NULL)
    $older_newer_bar .= '<td class="news_older"><a title="' . htmlspecialchars($next_item['title']) . '" href="news.php?id=' . $next_item['id'] . '">Older &raquo;</a></td>';
  $older_newer_bar .= '</tr></table>';
}

function castle_news_bar($contents)
{
  return '<table class="news_older_newer"> <tr><td class="news_newer">' .
    $contents . '</td></tr> </table>';
}

echo $older_newer_bar .
  '<div class="castle_rss_link"><a href="news_feed.php">RSS</a></div>';

if ($item === $news[0])
  echo '<h2>Latest news:</h2>';

echo '<div class="news_item">' . news_to_html($item) . '</div>';
?>

<?php
  echo castle_news_bar('
    <b>Comments?</b>
    Go to our ' . FORUM_LINK . ' or ' . MAILING_LIST_LINK . '.<br/><br/>

    <b>Google+?</b>
    You can <a href="https://plus.google.com/101185352355602218697" rel="publisher">follow our engine / view3dscene news on Google+</a>.<br/><br/>

    <b>Watch engine development:</b>
    To <i>really</i> watch the engine development closely, you can
    <a href="https://sourceforge.net/p/castle-engine/code/feed">watch the commits through RSS feed</a>.
    <!-- not polled unfortunately:
    <a href="http://cia.vc/stats/project/castle-engine">Castle Game Engine on
    cia.vc</a>.
    -->
    There is also <a href="https://www.ohloh.net/p/castle-engine">our
    project page on Ohloh</a> (please rate and click on
    <i>"I use this"</i> button there!).');
?>

<br/>

<?php
echo $older_newer_bar;

castle_footer();
?>