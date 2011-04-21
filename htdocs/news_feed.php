<?php
  require_once 'vrmlengine_functions.php';

  $page_lang = LANG_EN;
  common_set_page_functions();

  require_once 'kambi-php-lib/class_rss_generator.inc.php';
  require_once 'news_common.php';

  define('FEED_MAX_COUNT', 10);
  if (count($news) > FEED_MAX_COUNT)
    $news = array_slice($news, 0, FEED_MAX_COUNT);

  $rss = new rss_generator('News about the Kambi VRML game engine');
  $rss->link = 'http://vrmlengine.sourceforge.net/';
  $rss->description = 'All the news about the Kambi VRML game engine - changes, releases and more.';
  echo $rss->get($news);
?>