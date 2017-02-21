<?php


/* It would be more intuitive to set

     $castle_php_relative_path = '../../../../';

   but the header.php is included in a special way it seems
   (probably using eval and not require), so the current directory right now
   is exactly in the Wordpress root (it is not inside wp-content/themes/cge/).
   So we only need to go 1 level higher, from Wordpress root to CGE website root.
*/
global $castle_php_relative_path;
$castle_php_relative_path = '../';
require_once $castle_php_relative_path . 'castle_engine_functions.php';

global $castle_wordpress;
$castle_wordpress = true;

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
