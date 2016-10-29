<?php
require_once 'castle_engine_functions.php';
tutorial_header('High-level classes to create 3D games');
?>

<p>Contents:

<?php
$current_sitemap = $castle_sitemap
    ['documentation']['sub']
    ['tutorial_intro']['sub']
    ['tutorial_high_level_3d_classes']['sub'];
echo '<ul>';
foreach ($current_sitemap as $menu_item_page => $menu_item) {
    echo '<li><a href="' . en_page_url($menu_item_page) . '">' .
        $menu_item['title'] . '</a></li>';
}
echo '</ul>';
?>

<?php
tutorial_footer();
?>
