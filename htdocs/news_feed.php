<?php

/* This link is used by http://planetdev.freegamedev.net/ , so keep it working.

   We could notify them to change link to Wordpress, see
   http://forum.freegamedev.net/viewtopic.php?f=24&t=7209
   http://forum.freegamedev.net/viewtopic.php?f=24&t=24
   Actually, we could submit a pull request:
   https://github.com/FreeGameDev/planet-config
   https://github.com/FreeGameDev/planet-config/blob/master/fgdplanet-dev-feeds.ini

   But it's more future-proof for us to just change this page to redirect
   wherever necessary. */

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
  require_once 'kambi-php-lib/class_rss_generator.inc.php';
  require_once 'news_common.php';

  define('FEED_MAX_COUNT', 10);
  if (count($news) > FEED_MAX_COUNT)
    $news = array_slice($news, 0, FEED_MAX_COUNT);

  header('Content-type: application/rss+xml; charset=utf-8');

  $rss = new rss_generator('News about the Castle Game Engine');
  $rss->link = 'https://castle-engine.sourceforge.io/';
  $rss->description = 'All the news about the Castle Game Engine - changes, releases and more.';
  echo $rss->get($news);
*/
?>