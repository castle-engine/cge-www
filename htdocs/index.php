<?php
// define('CASTLE_GITHUB_NAME', 'castle-engine');

require_once 'castle_engine_functions.php';
require_once 'news_common.php';

global $main_page;
$main_page = true;

/* disable disqus on main page, looks dirty */
global $disqus_form_already_done;
$disqus_form_already_done = true;

castle_header('Download', array(
  'meta_description' => 'Free open-source 3D and 2D game engine. Supports a lot of 2D and 3D data formats, including X3D, glTF, VRML, Collada, Spine. Cross-platform, for desktops (Windows, Linux, macOS...), mobile (Android, iOS), console (Nintendo Switch). Many beatiful 3D features (shadows, mirrors) available. Using modern Object Pascal.'
));
?>

<!-- Free open-source game engine for <a href="http://www.freepascal.org/">FreePascal and Lazarus</a>. Excellent support for many 3D and 2D data formats, portable (desktops, Android, iOS...), many advanced graphic effects, comfortable API.</p -->

<div class="row">
    <div class="col-sm-8">
        <div class="centered-wrapper">
            <div class="centered">
                <h1 class="main-title">Castle Game Engine</h1>
                <p class="main-subtitle">Free open-source <!-- (<a href="#section_license">LGPL / GPL</a>) -->
                3D and 2D game engine using modern Object Pascal</p>
            </div>
        </div>

        <div class="centered-download-wrapper">
            <div class="download jumbotron">
                <?php echo castle_download_button('<span class="glyphicon glyphicon-fire" aria-hidden="true"></span><br>Download<br><p class="download-button-hint">6.5 - Next Release Beta<br>(Recommended)</p>',
                  CGE_LATEST_UNSTABLE_DOWNLOAD); ?>
                <?php echo castle_download_button('<span class="glyphicon glyphicon-leaf" aria-hidden="true"></span><br>Download<br><p class="download-button-hint">6.4 - Stable Release<br>&nbsp;</p>',
                  CGE_LATEST_STABLE_DOWNLOAD); ?>

                <div class="download-hints">
                    <p>After downloading, read the <?php echo a_href_page('"Getting Started"', 'documentation'); ?>
                    and <?php echo a_href_page('manual', 'manual_intro'); ?>.
                    <?php /* <a href="< ?php echo reference_link(); ? >">reference</a>, */ ?>
                    <?php /* <a href="https://castle-engine.io/modern_pascal_introduction.html">modern Pascal introduction</a> */ ?>
                    <?php /* and the <a href="documentation_more.php">rest of our documentation</a>. */ ?><br>

                    <?php /* You can also download <?php echo a_href_page('view3dscene', 'view3dscene'); ?>, our model viewer.</p>
                    */ ?>
                </div>

                <?php echo download_donate_footer(); ?>
            </div>
        </div>

        <div class="centered-wrapper">
            <div class="centered">
                <ul>
                    <li>Use <b>any 3D or 2D software</b> to create your models in any format: X3D, glTF, VRML<!--?php echo a_href_page('VRML / X3D', 'vrml_x3d'); ?-->, Collada, OBJ, MD3,
                      <!--a href="https://github.com/castle-engine/castle-engine/wiki/Spine"-->Spine...
                    <li>Develop <b>cross-platform</b> applications, for desktop (<b>Windows, Linux, macOS, FreeBSD...</b>), mobile (<b>Android, iOS</b>), consoles (<b>Nintendo Switch</b>) and other devices (<b>Raspberry Pi</b>).
                    <li><b>Visual editor</b> to design games UI and to build applications, powerful command-line <b>build tool</b> under the hood.
                    <li>Optimized rendering with a lot of graphic effects (<b>shadows, mirrors, bump mapping, shader effects</b>...).
                    <li><b>Build and edit</b> the scene graph (X3D) <b>at runtime</b>.
                      <!--Load and save images and X3D graph as needed.-->
                      Create 3D processing, visualization tools and CAD applications.
                    <li>Extensible system for game objects, with <b>physics, creatures with AI and navmesh</b>, and more.
                    <li>Access numerous <b>services, like in-app purchases and game services</b> on mobile devices.
                    <li>Create <b>cross-platform user-interface with anchors and automatic scaling</b>.
                </ul>
            </div>
        </div>
    </div>
    <div class="col-sm-4">
        <!-- iframe class="hidden-xs main-page-thumbnail" width="560" height="315" src="https://www.youtube.com/embed/o5q7guVkYVo" frameborder="0" allowfullscreen></iframe -->

        <img src="images/original_size/dragon_large.png"
            alt="Dragon 2D animation designed in Spine"
            title="Dragon 2D animation designed in Spine"
            class="main-page-thumbnail hidden-xs" />

        <!-- <img src="images/original_size/rhan_shrine_5_everything.png" -->
        <!--     alt="Bump mapping and shadow maps from multiple light sources" -->
        <!--     title="Bump mapping and shadow maps from multiple light sources" -->
        <!--     class="main-page-thumbnail" -->
        <!--     style="margin-bottom: 0.5em" /> -->
        <!-- <img src="images/original_size/barna29_nice_shadows.png" -->
        <!--     alt="Real-time water with caustics, reflections, shadows" -->
        <!--     title="Real-time water with caustics, reflections, shadows" -->
        <!--     class="main-page-thumbnail" /> -->
        <!-- <img src="images/original_size/castle_spine_screen_9.png" -->
        <!--     alt="Dragon 2D animation designed in Spine" -->
        <!--     title="Dragon 2D animation designed in Spine" -->
        <!--     class="main-page-thumbnail" /> -->
    </div>
</div>

<?php
if (!HTML_VALIDATION &&
  (CASTLE_ENVIRONMENT == 'production' ||
   CASTLE_ENVIRONMENT == 'development'))
{
    /* Load Wordpress PHP now
       (in global namespace, just like wp-blog-header.php does) */
    if (!isset($wp_did_header)) {
        chdir('wp');
        //require_once 'index.php';
        $wp_did_header = true;
        // Load the WordPress library.
        require_once('wp-load.php');
        chdir('..');
    }

    function castle_wp_post_tile($post)
    {
        if (has_post_thumbnail($post)) {
            $image = get_the_post_thumbnail($post, 'news-teaser');
        } else {
            $image = '';
        }
        $title = '<p class="news_title_wrapper">' .
            '<span class="news_title">' .
            esc_html($post->post_title) .
            '</span>' .
            '</p>';
        $date = '<p class="news_date_wrapper">' .
            '<span class="news_date">' .
            get_the_date('F j, Y', $post->ID) .
            '</span>' .
            '</p>';
        return '<a href="' . get_permalink($post) . '">' .
            $image . $title . $date . '</a>';
    }

    function echo_news()
    {
        $posts = get_posts(array(
            'numberposts' => 4,
            'post_type' => 'post',
            'post_status' => 'publish',
        ));

        echo '<div class="news-row-wrapper">';
            echo '<div class="row-title">LATEST NEWS:</div>';
            echo '<div class="row">';

            //global $post;
            foreach ($posts as $current_post) {
                /* Set our current post as the post used with functions
                   that work inside "The Loop" of Wordpress.
                   See https://digwp.com/2011/05/loops/
                   https://codex.wordpress.org/Function_Reference/setup_postdata
                   Alternative https://developer.wordpress.org/reference/functions/query_posts/
                   is not adviced.
                   And I need to use some functions that only work inside "The Loop".
                   Later: after all, I did not use any functions
                   that only work inside "The Loop",
                   so this is commented out -- better to not touch global vars
                   if you can.
                */
                // $post = $current_post;
                // setup_postdata($post);

                echo '<div class="col-sm-3">';
                    echo castle_wp_post_tile($current_post);
                echo '</div>';
            }

            echo '</div>';
        echo '</div>';
    }
    echo_news();
}
?>

<?php
castle_footer();
?>
