<?php

/* For the navigation purposes (which main menu tab to show as "chosen"),
   we're the page 'news'. */
global $page_basename;
$page_basename = 'news';

castle_header(wp_title('&raquo;', false), array(
  'path' => array('news')
));
?>
