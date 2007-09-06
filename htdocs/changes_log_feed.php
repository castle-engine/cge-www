<?php
  require 'vrmlengine_functions.php';

  $page_lang = LANG_EN;
  common_set_page_functions();

  require 'kambi-php-lib/class_rss_generator.inc.php';
  require 'changes_log_common.php';

  $rss = new rss_generator('Kambi VRML game engine - changes log');
  $rss->link = 'http://vrmlengine.sourceforge.net/';
  $rss->description = 'Kambi VRML game engine - all changes, releases, etc.';
  echo $rss->get($changes_log);
?>