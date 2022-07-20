<?php

/* For the navigation purposes (which main menu tab to show as "chosen"),
   we're the page 'news'. */
global $page_basename;
$page_basename = 'news';

castle_header(wp_title('&raquo;', false), array(
  'path' => array('news'),
  // Wordpress will write canonical URL HTML tags, don't let castle_header auto-guess the canonical URL
  'canonical_url' => NULL
));
?>
