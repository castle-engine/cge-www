<?php

/* Disable disqus on Wordpress pages,
   looks weird -- both Wordpress and Disqus ways to comment, it's too much */
global $disqus_form_already_done;
$disqus_form_already_done = true;

/* For the navigation purposes (which main menu tab to show as "chosen"),
   we're the page 'news'. */
global $page_basename;
$page_basename = 'news';

castle_header(wp_title('&raquo;', false), array(
  'path' => array('news')
));
?>
