<?php


require_once 'castle_engine_functions.php';
header('Location: ' . CURRENT_URL . 'wp/feed/');
?>

<?php
/*
This is (sometimes?) failing with internal error since around 2017-02-20.
Also this is not useful anymore, as we now have our own feed made by Wordpress.
header('Location: http://gplusrss.com/rss/feed/c3dd3cd5ec6725d92b8b5b0633b2daa852f29730ecc6b');
*/
?>

<?php
/*
  require_once 'castle_engine_functions.php';
  require_once 'castle-engine-website-base/class_rss_generator.inc.php';
  require_once 'news_common.php';

  define('FEED_MAX_COUNT', 10);
  if (count($news) > FEED_MAX_COUNT)
    $news = array_slice($news, 0, FEED_MAX_COUNT);

  header('Content-type: application/rss+xml; charset=utf-8');

  $rss = new rss_generator('News about the Castle Game Engine');
  $rss->link = 'https://castle-engine.io/';
  $rss->description = 'All the news about the Castle Game Engine - changes, releases and more.';
  echo $rss->get($news);
*/
?>