<?php
  require_once 'castle_engine_functions.php';

  $page_lang = LANG_EN;
  common_set_page_functions();

  require_once 'kambi-php-lib/class_rss_generator.inc.php';
  require_once 'news_common.php';

  define('FEED_MAX_COUNT', 10);
  if (count($news) > FEED_MAX_COUNT)
    $news = array_slice($news, 0, FEED_MAX_COUNT);

  header('Content-type: application/rss+xml; charset=utf-8');

  $rss = new rss_generator('News about the Castle Game Engine');
  $rss->link = 'http://castle-engine.sourceforge.net/';
  $rss->description = 'All the news about the Castle Game Engine - changes, releases and more.';
  echo $rss->get($news);
?>