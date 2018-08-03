<?php /* -*- mode: kambi-php -*- */

/* PHP functions common for castle-engine WWW pages. */

/* Calculate CASTLE_ENVIRONMENT constant. */
function castle_detect_environment()
{
  /* CASTLE_ENVIRONMENT equal "offline"
     means that we will set CURRENT_URL to '' and do not connect
     to the outside world for resources (scripts, images, including analytics).
     All the links lead to remote (to CASTLE_FINAL_URL/xxx.php),
     this is realized by kambi_common.php .

     Useful to generate documentation working offline
     (for PasDoc docs in CGE release).

     Note that some things cannot work correctly in this mode:
     - Stuff that can be included by WordPress will be included
       incorrectly, as WordPress base path is in any subdirectory,
       like "2017/02/12/hello-world/".
     - RSS feed generated will have relative URLs,
       which means it would not work when embedded on other servers.
     - og:image contents should be an absolute URL (Facebook warns
       about it otherwise).
  */
  // For now, the environment "offline" is never auto-detected here.

  /* CASTLE_ENVIRONMENT equal "development"
     means that we are testing the page on development server,
     not on production server. It removes *some* stuff that embeds online
     content on our webpages, see castle_engine_externals.php.
     The idea is to test our webpages through http://localhost/ easily.
  */
  if (isset($_SERVER['SERVER_NAME']) &&
      ($_SERVER['SERVER_NAME'] == '127.0.0.1' ||
       $_SERVER['SERVER_NAME'] == 'localhost')) {
    define('CASTLE_ENVIRONMENT', 'development');
    return;
  }

  if (isset($_SERVER['SERVER_NAME']) &&
      ($_SERVER['SERVER_NAME'] == 'michalis.ii.uni.wroc.pl')) {
    define('CASTLE_ENVIRONMENT', 'preview');
    return;
  }

  define('CASTLE_ENVIRONMENT', 'production');
}

if (!defined('CASTLE_ENVIRONMENT')) {
  castle_detect_environment();
}

if (CASTLE_ENVIRONMENT != 'production') {
  /* Helpful for debugging, to see PHP warnings, notices. */
  error_reporting(E_ALL);
  ini_set('display_errors', 'stderr');
}

/* Calculate CURRENT_URL constant. */
switch (CASTLE_ENVIRONMENT) {
  case 'offline':
    define('CURRENT_URL', '');
    break;
  case 'development':
    define('CURRENT_URL', 'http://localhost/~michalis/castle-engine/');
    break;
  case 'preview':
    define('CURRENT_URL', 'https://michalis.ii.uni.wroc.pl/cge-www-preview/');
    break;
  case 'production':
    define('CURRENT_URL', 'https://castle-engine.io/');
    break;
  default:
    throw new ErrorException('Unrecognized CASTLE_ENVIRONMENT value: ' . CASTLE_ENVIRONMENT);
}

/* The final (not testing, offline, or preview) website URL. */
define('CASTLE_FINAL_URL', 'https://castle-engine.io/');
define('KAMBI_NO_HOME_LINK', true);

// other constants
define('SF_UNIX_NAME', 'castle-engine'); // used only by download links.
define('WIKI_URL',            'https://github.com/castle-engine/castle-engine/wiki');
define('MAILING_LIST_URL',    'https://lists.sourceforge.net/lists/listinfo/castle-engine-main');
define('FORUM_URL',           'https://sourceforge.net/p/castle-engine/discussion/general/');
define('PATREON_URL',         'https://patreon.com/castleengine');

define('CGE_LATEST_DOWNLOAD', 'https://github.com/castle-engine/castle-engine/releases/download/v6.4/castle_game_engine-6.4-src.zip');

function reference_link()
{
  global $castle_apidoc_url;
  return $castle_apidoc_url . 'index.html';
}

/* PHP file relative path from current file to "our root"
   (where castle-engine-website-base/ is a subdirectory).
   The original PHP file that handles the request can define it at the beginning,
   and require this file (...functions.php) using:

     require_once $castle_php_relative_path . 'castle_engine_functions.php';

   This may be used to set include path correctly.
*/
global $castle_php_relative_path;
if (empty($castle_php_relative_path)) {
  $castle_php_relative_path = '';
}

/* Set path to include PHP files.
   - Because some time ago on SourceForge, requiring castle-engine-website-base/kambi_common.php
     was not reliable without this (sometimes fails, sometimes not).
     Using set_include_path to include the castle-engine-website-base/ fixed the issue.
   - Because we actually depend on it for geshi. */
global $castle_php_relative_path;
set_include_path(get_include_path() .
  PATH_SEPARATOR . $castle_php_relative_path . 'castle-engine-website-base/' .
  PATH_SEPARATOR . $castle_php_relative_path . 'geshi/');

require_once 'castle-engine-website-base/kambi_common.php';
require_once 'generated_versions.php';
require_once 'castle_engine_externals.php';
require_once 'castle_engine_books.php';
require_once 'geshi.php';

define('CASTLE_GENERATE_OFFLINE',
  isset($_GET['CASTLE_GENERATE_OFFLINE']) &&
  $_GET['CASTLE_GENERATE_OFFLINE'] == 'true');

define('S_INSTALLATION_INSTRUCTIONS_SHORT',
  'No installation is required. Just download and unpack these archives wherever
  you want, and run the program inside. The documentation
  (this web page) is also included inside
  (look in the <code>documentation/</code> subdirectory) for offline viewing.');

define('FPC_CFG_DOCS', 'see <a href="http://www.freepascal.org/docs-html/user/usersu10.html">FPC documentation <i>"Configuration file"</i> to know where you can find your <code>fpc.cfg</code> file</a>');

global $castle_apidoc_url;
if (CASTLE_ENVIRONMENT == 'development') {
  $castle_apidoc_url = 'https://castle-engine.io/apidoc-unstable/html/';
} else {
  $castle_apidoc_url = page_url('apidoc/html/');
}

global $site_title;
$site_title = 'Castle Game Engine';

/* Complete sitemap of our website.
   This determines a lot of navigational stuff: header menu, sidebar,
   breadcrumbs.

   An array, mapping page basename => into the information about the page
   and it's subitems.
   Page basename may also contain (after hash) an anchor inside this page.
   (Although this is not allowed for now for top-level items, but it may
   be fixed when needed.)

   Page information is another array, that contains

   - title: html safe (with character entities already escaped etc.)
     title of the page. Required.
     Used for everything (header menu (if no title-for-header-menu),
     sidebar, breadcrumbs.)

   - title-for-header-menu: optional alternative to title, used (if set) by
     header menu.

   - hint: optional tooltip ("title" over <a> in HTML, don't confuse
     with our title). Used now only over the header menu.

   - sidebar: optional. If set, and true, then this is the "root"
     of the sidebar. The algorithm used to display the sidebar looks
     for the 1st item in the path (from sitemap root) that has
     sidebar=true. If no such item, then no sidebar.
     Sidebar follows the items defined in 'sub'.

   - sub: optional array of pages under this page.

   - url: target URL (ready to be used as HTML attribute, i.e. already escaped).
     Optional, if not present we will generate proper link to internal page
     using the key as page basename.
*/
global $castle_sitemap;
$castle_sitemap = array(
  MAIN_PAGE_BASENAME       => array('title' => 'Download',
    /* 'sub' and 'sidebar' of this will be calculated by news.php page,
       since it may be time-consuming (requires reading large $news table). */
  ),

  'news' => array('title' => 'News',
    'url' => page_url('wp/'),
    /* The 'news' menu is a parent menu for
       - old news items, generated by old_news.php,
         that adds also 'sub' and 'sidebar' of this.
         The 'sub' is only calculated by old_news.php,
         since it may be time-consuming (requires reading large $news table),
         and is not needed on other pages.
       - new news, implemented by going to Wordpress.
         Clicking on "News" in main menu should go there.
    */
  ),

  'features' => array('title' => 'Features'),

  'documentation' => array('title' => 'Documentation',
    'dropdown' => array(
      'documentation' => array('title' => 'Getting Started'),
      'manual_intro' => array('title' => 'Manual'),
      'creating_data_intro' => array('title' => 'Creating Game Data'),
      'reference' => array('title' => 'API Reference', 'url' => reference_link()),
      'modern_pascal_introduction' => array('title' => 'Modern Object Pascal Introduction', 'url' => 'https://castle-engine.io/modern_pascal_introduction.html'),
      'documentation_more' => array('title' => 'More...'),
    ),
    'sidebar' => true,
    'sub' => array(
      /* 'pascal_intro' => array('title' => 'Quick Modern Object Pascal for Programmers', */
      /*   'url' => 'https://castle-engine.io/modern_pascal_introduction.html', */
      /*   'sub' => array('pascal_intro_pdf' =>  array( */
      /*       'title' => 'PDF version', */
      /*       'url' => 'https://castle-engine.io/modern_pascal_introduction.pdf', */
      /*     ), */
      /*   ) */
      /* ), */
      'documentation' => array('title' => 'Getting Started'),
      'manual_intro' => array('title' => 'Manual',
        'sub' => array(
          'manual_window' => array('title' => 'Start: Display a window'),
          'manual_quick_2d_game' => array('title' => 'Quick 2D game (basic window events)'),
          'manual_load_3d' => array('title' => 'Loading, displaying a scene'),
          'manual_scene' => array('title' => 'Transform, animate, duplicate, build a scene'),
          'manual_cross_platform' => array('title' => 'Cross-platform (desktop and mobile) games',
            'sub' => array(
              'manual_build_tool' => array('title' => 'Build Tool (wiki)', 'url' => 'https://github.com/castle-engine/castle-engine/wiki/Build-Tool'),
              'manual_castleenginemanifest_examples' => array('title' => 'CastleEngineManifest.xml (wiki)', 'url' => 'https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples'),
              'manual_android' => array('title' => 'Android (wiki)', 'url' => 'https://github.com/castle-engine/castle-engine/wiki/Android'),
              //'manual_android_faq' => array('title' => 'Android FAQ (wiki)', 'url' => 'https://github.com/castle-engine/castle-engine/wiki/Android-FAQ'),
              'manual_android_services' => array('title' => 'Android Services (like Google Play Games and In-App Purchases) (wiki)', 'url' => 'https://github.com/castle-engine/castle-engine/wiki/Android-Project-Services-Integrated-with-Castle-Game-Engine'),
              'manual_ios' => array('title' => 'iOS (wiki)', 'url' => 'https://github.com/castle-engine/castle-engine/wiki/iOS'),
              'manual_ios_services' => array('title' => 'iOS Services (like Apple Game Center and In-App Purchases) (wiki)', 'url' => 'https://github.com/castle-engine/castle-engine/wiki/iOS-Services'),
              //'manual_web_plugin' => array('title' => 'Web Plugin (wiki)', 'url' => 'https://github.com/castle-engine/castle-engine/wiki/Web-Plugin'),
            ),
          ),
          'manual_user_interface_and_2d_drawing' => array('title' => 'User interface and 2D drawing',
            'sub' => array(
              'manual_2d_user_interface' => array('title' => 'User interface, standard controls, viewports'),
              'manual_2d_ui_custom_drawn' => array('title' => 'Custom drawn 2D controls: player HUD'),
              'manual_text' => array('title' => 'Text and fonts'),
              'manual_on_screen_menu' => array('title' => 'On-screen menu'),
              'manual_notifications' => array('title' => 'Notifications'),
            )
          ),
          'manual_2d_games' => array('title' => 'Two approaches for rendering 2D Games (wiki)', 'url' => 'https://github.com/castle-engine/castle-engine/wiki/2D-Games'),
          'manual_high_level_3d_classes' => array('title' => 'High-level classes for games',
            'sub' => array(
              'manual_game_level' => array('title' => 'Loading game level'),
              'manual_player' => array('title' => 'Player'),
              'manual_resources' => array('title' => 'Defining creatures and items'),
              'manual_resources_using_existing' => array('title' => 'Using creatures and items'),
              'manual_resources_extending' => array('title' => 'Extending creatures and items classes'),
            ),
          ),
          'manual_physics' => array('title' => 'Physics'),
          'manual_sound' => array('title' => 'Sound'),
          'manual_screenshots' => array('title' => 'Screenshots'),
          'manual_network' => array('title' => 'Network and downloading'),
          'manual_recording_movies' => array('title' => 'Recording movies'),
          'manual_log' => array('title' => 'Logging'),
          'manual_user_prefs' => array('title' => 'Persistent data (user preferences, savegames)'),
          'manual_key_mouse' => array('title' => 'Key/mouse shortcuts'),
          'manual_optimization' => array('title' => 'Optimization and profiling',
            'sub' => array(
              'profiling' => array('title' => 'Profiling Using Valgrind (wiki)', 'url' => 'https://github.com/castle-engine/castle-engine/wiki/Profiling-Using-Valgrind'),
              'prettier' => array('title' => 'How To Make Rendering Prettier (wiki)', 'url' => 'https://github.com/castle-engine/castle-engine/wiki/How-to-make-rendering-prettier%3F'),
            )
          ),
          'manual_classes_overview' => array('title' => 'Classes overview (cheatsheet)'),
          'manual_miscellaneous' => array('title' => 'Miscellaneous knowledge',
            'sub' => array(
              'manual_up' => array('title' => 'Which way is up?'),
              'manual_transformation_hierarchy' => array('title' => 'Transformation hierarchy'),
              'manual_lazarus_focus' => array('title' => 'How to handle the focus of TCastleControl'),
            )
          ),
        ),
      ),
      'creating_data_intro' => array('title' => 'Creating Game Data',
        'sub' => array(
          'creating_data_export' => array('title' => 'Exporting 3D and 2D models',
            'sub' => array(
              'creating_data_blender' => array('title' => 'Blender'),
              'creating_data_3dsmax' => array('title' => '3ds Max'),
              'creating_data_maya' => array('title' => 'Maya'),
              'creating_data_spine' => array('title' => 'Spine (wiki)', 'url' => 'https://github.com/castle-engine/castle-engine/wiki/Spine'),
              'creating_data_dragon_bones' => array('title' => 'Dragon Bones'),
            )
          ),
          'creating_data_xml' => array('title' => 'XML files describing game data (level.xml, resource.xml and others)'),
          'creating_data_levels' => array('title' => 'Levels'),
          'creating_data_resources' => array('title' => 'Resources (creatures and items)'),
          'creating_data_player' => array('title' => 'Player configuration'),
          'creating_data_material_properties' => array('title' => 'Material properties configuration'),
          'creating_data_auto_generated_textures' => array('title' => 'Auto-generated compressed and scaled textures'),
          'creating_data_sound' => array('title' => 'Sound'),
        )
      ),
      'reference' => array('title' => 'API Reference', 'url' => reference_link()),
      'modern_pascal_introduction' => array('title' => 'Modern Object Pascal Introduction', 'url' => 'https://castle-engine.io/modern_pascal_introduction.html'),
      'documentation_more' => array('title' => 'More...',
        'sub' => array(
          // 'android' => array('title' => 'Android (wiki)', 'url' => 'https://github.com/castle-engine/castle-engine/wiki/Android'),
          // 'ios' => array('title' => 'iOS (wiki)', 'url' => 'https://github.com/castle-engine/castle-engine/wiki/iOS'),
          'supported_compilers' => array('title' => 'Supported compilers and IDEs'),
          'planned_features' => array('title' => 'Planned features'),
          'helping' => array('title' => 'Helping in engine development',
            'sub' => array(
              'donate_other' => array('title' => 'Donate')
            ),
          ),
          'engine_doc' => array('title' => 'Internals documentation'),
          'movies' => array('title' => 'Movies on YouTube', 'url' => 'https://www.youtube.com/channel/UCq9jJ5ivIXC5VEWiUAfxBxw'),
          'cloud_builds' => array('title' => 'Automatic cloud builds for Castle Game Engine projects', 'url' => 'https://michalis.ii.uni.wroc.pl/jenkins/'),
          'license' => array('title' => 'License'),
        )
      ),
    ),
  ),

  'vrml_x3d' => array('title' => 'Scene Graph (X3D)',
    'sidebar' => true,
    'sub' => array(
      'demo_models' => array('title' => 'Demo models'),
      'x3d_implementation_status' => array('title' => 'Standard X3D Nodes',
        'sub' => array(
          'x3d_implementation_core'                 => array('title' => 'Core'                            ),
          'x3d_implementation_time'                 => array('title' => 'Time',
            'sub' => array(
              'x3d_implementation_time_extensions' => array('title' => 'Extensions'),
            ),
          ),
          'x3d_implementation_networking'           => array('title' => 'Networking'                      ),
          'x3d_implementation_grouping'             => array('title' => 'Grouping'                        ),
          'x3d_implementation_rendering'            => array('title' => 'Rendering'                       ),
          'x3d_implementation_shape'                => array('title' => 'Shape',
            'sub' => array(
              'x3d_implementation_shape_extensions' => array('title' => 'Extensions'),
            ),
          ),
          'x3d_implementation_geometry3d'           => array('title' => 'Geometry3D',
            'sub' => array(
              'x3d_implementation_geometry3d_extensions' => array('title' => 'Extensions'),
            ),
          ),
          'x3d_implementation_geometry2d'           => array('title' => 'Geometry2D'                      ),
          'x3d_implementation_text'                 => array('title' => 'Text',
            'sub' => array(
              'x3d_implementation_text_extensions' => array('title' => 'Extensions'),
            ),
          ),
          'x3d_implementation_sound'                => array('title' => 'Sound'                           ),
          'x3d_implementation_lighting'             => array('title' => 'Lighting'                        ),
          'x3d_implementation_texturing'            => array('title' => 'Texturing',
            'sub' => array(
              'x3d_implementation_texturing_extensions' => array('title' => 'Extensions'),
              'x3d_multi_texturing' => array('title' => 'X3D MultiTexturing problems and proposed solutions'),
            ),
          ),
          'x3d_implementation_interpolation'        => array('title' => 'Interpolation ("how to animate things")',
            'sub' => array(
              'x3d_implementation_interpolation_extensions' => array('title' => 'Extensions'),
            ),
          ),
          'x3d_implementation_pointingdevicesensor' => array('title' => 'Pointing device sensor'          ),
          'x3d_implementation_keydevicesensor'      => array('title' => 'Key device sensor'               ),
          'x3d_implementation_environmentalsensor'  => array('title' => 'Environmental sensor'            ),
          'x3d_implementation_navigation'           => array('title' => 'Navigation',
            'sub' => array(
              'x3d_implementation_navigation_extensions' => array('title' => 'Extensions'),
            ),
          ),
          'x3d_implementation_environmentaleffects' => array('title' => 'Environmental effects'           ),
          'x3d_implementation_hanim'                => array('title' => 'H-Anim'                          ),
          'x3d_implementation_nurbs'                => array('title' => 'NURBS',
            'sub' => array(
              'x3d_implementation_nurbs#section_homogeneous_coordinates' => array('title' => 'Control points are in homogeneous coordinates'),
            ),
          ),
          'x3d_implementation_scripting'            => array('title' => 'Scripting'                       ),
          'x3d_implementation_eventutilities'       => array('title' => 'Event utilities',
            'sub' => array(
              'x3d_implementation_eventutilities_extensions' => array('title' => 'Extensions'),
            ),
          ),
          'x3d_implementation_shaders'              => array('title' => 'Programmable shaders'            ),
          'x3d_implementation_cadgeometry'          => array('title' => 'CAD geometry'                    ),
          'x3d_implementation_texturing3d'          => array('title' => 'Texturing3D'                     ),
          'x3d_implementation_cubemaptexturing'     => array('title' => 'Cube map environmental texturing'),
        )
      ),
      'x3d_larger_extensions' => array('title' => 'Larger X3D Extensions',
        'sub' => array(
          'compositing_shaders' => array('title' => 'Compositing Shaders'),
          'x3d_extensions_screen_effects' => array('title' => 'Screen Effects'),
          'x3d_extensions_shadow_maps' => array('title' => 'Shadow Maps'),
          'x3d_extensions_shadow_volumes' => array('title' => 'Shadow Volumes'),
          'x3d_extensions_vrml1' => array('title' => '(Old) VRML 1.0'),
        ),
      ),
      'x3d_extensions' => array('title' => 'Complete list of X3D Extensions'),
      'castle_script' => array('title' => 'CastleScript language reference'),
      'castle_animation_frames' => array('title' => 'Castle Animation Frames (castle-anim-frames) file format'),
      'x3d_time_origin_considered_uncomfortable' => array('title' => 'VRML / X3D time origin considered uncomfortable'),
      'nist_vrml_test_suite' => array('title' => 'NIST conformace test suite'),
    ),
  ),

  'view3dscene'            => array('hint' => 'VRML / X3D browser, and a viewer for other 3D model formats', 'title' => 'view3dscene'),

  'talk' => array('hint' => 'Ask for help, report bugs, discuss features', 'title' => 'Talk with us'),

  // Do not show, for now we focus on Patreon funding.
  // 'donate' => array('title' => 'Donate'),

  'all_programs' => array('hint' => 'All the games and tools using our 3D engine', 'title' => 'Games and Tools',
    'sub' => array(
      'mountains_of_fire' => array('title' => 'Mountains Of Fire'),
      'darkest_before_dawn' => array('title' => 'Darkest Before the Dawn'),
      'castle'                 => array('hint' => 'First-person perspective game, in a dark fantasy setting'   , 'title' => 'The Castle',
        'sidebar' => true,
        'sub' => array(
          'castle-advanced'    => array('title' => 'Additional notes (troubleshooting)'),
          'castle-credits'     => array('title' => 'Credits'),
        ),
      ),
      'lets_take_a_walk' => array('title' => 'lets_take_a_walk'),
      'malfunction' => array('title' => 'malfunction'),
      'kambi_lines' => array('title' => 'kambi_lines'),
      'glviewimage' => array('title' => 'glViewImage'),
      'glplotter_and_gen_function' => array('title' => 'glplotter and gen_function'),
      'rayhunter' => array('title' => 'rayhunter',
        'sub' => array(
          'raytr_gallery' => array('title' => 'Small gallery of images rendered using rayhunter'),
        ),
      ),
      'glinformation' => array('title' => 'glinformation'),
      'kambi_mgf2inv' => array('title' => 'kambi_mgf2inv'),

      'common_options' => array('title' => 'Standard command-line options'),
      'opengl_options' => array('title' => 'Standard command-line options for OpenGL programs'),
      'openal' => array('title' => 'OpenAL (3D sound)'),
      'macosx_requirements' => array('title' => 'Dependencies on Mac OS X'),
      'versioning' => array('title' => 'Versioning scheme of programs'),
      'all_programs_sources' => array('title' => 'All Programs Sources'),
    ),
  ),
);

function _castle_bootstrap()
{
  kambi_bootstrap();
  global $castle_sitemap;
  castle_sitemap_book_correct('manual',
    $castle_sitemap['documentation']['sub']['manual_intro']['sub']);
  castle_sitemap_book_correct('creating_data',
    $castle_sitemap['documentation']['sub']['creating_data_intro']['sub']);
}

/* Call this immediately, to modify $castle_sitemap even before calling
   castle_header. */
_castle_bootstrap();

/* Internal for _castle_sidebar* usage.

   Return a formatted link to given page.
   $page is the page basename (like for a_href_page), or a basename(hash)anchor.
   $pageinfo must contain 'title', may contain 'url'.

   Looks at global $page_basename, to avoid turning current page name
   into a link. */
function _castle_sidebar_link($page, $pageinfo)
{
  $pagelink = explode('#', $page);
  if (count($pagelink) == 1)
  {
    global $page_basename;
    /* If it's the current page, don't make a link to it */
    if ($pagelink[0] == $page_basename)
      return $pageinfo['title']; else
    if (isset($pageinfo['url']))
      return '<a href="' . $pageinfo['url'] .'">' . $pageinfo['title'] . '</a>'; else
      return a_href_page($pageinfo['title'], $pagelink[0]);
  } else
  if (count($pagelink) == 2)
    return a_href_page_hashlink($pageinfo['title'], $pagelink[0], $pagelink[1]); else
    return '<b>Invalid sidebar link ' . htmlspecialchars($pagelink) . '</b>';
}

/* Internal for _castle_sidebar* usage.
   Return a <ul> listing items on $sub. */
function _castle_sidebar_menu($sub)
{
  $result = '<ul>';
  foreach($sub as $page => $pageinfo)
  {
    $result .= '<li>' . _castle_sidebar_link($page, $pageinfo);
    if (isset($pageinfo['sub']))
      $result .= _castle_sidebar_menu($pageinfo['sub']);
    $result .= '</li>';
  }
  $result .= '</ul>';
  return $result;
}

/* Return a rendered HTML sidebar. */
function _castle_sidebar($page, $pageinfo)
{
  $result = '
  <div class="sidebar">
    <div class="sidebar_title">' . _castle_sidebar_link($page, $pageinfo) . '</div>';

  if (isset($pageinfo['sub']))
    $result .= _castle_sidebar_menu($pageinfo['sub']);

/*
  $result .= '
    <div style="text-align: center; width: 200px; margin-right: auto; margin-left: auto; padding: 0.1em; border: medium outset #FFF;">
    <p><a href="https://castle-engine.io/donate.php">Donate</a></p>
    <p>' . flattr_button(false) . '</p>
    <p>' . paypal_button() . '</p>
    </div>';
*/

  $result .= '</div>';

  return $result;
}

function _castle_header_menu($current_page)
{
  global $castle_sitemap;

  $result = '
    <!-- Uncomment this for toggable navbar -->
    <!--div class="collapse navbar-collapse" id="main-navbar-collapse-1" -->
    <ul class="nav nav-tabs">';

  foreach($castle_sitemap as $menu_item_page => $menu_item)
  {
    // output <li ...>
    $result .= '<li class="';
    if ($menu_item_page == $current_page) {
      $result .= ' active';
    }
    if (isset($menu_item['dropdown'])) {
      $result .= ' dropdown';
    }
    $result .= '">';

    // output <a href="..."
    $result .= '<a href="';
    if (isset($menu_item['dropdown'])) {
      $result .= '#';
    } else
    if (isset($menu_item['url'])) {
      $result .= $menu_item['url'];
    } else {
      $result .= page_url($menu_item_page);
    }
    $result .= '"';

    // output optional <a> dropdown attributes
    if (isset($menu_item['dropdown'])) {
      $result .= ' class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false"';
    }

    // output title="..."
    if (isset($menu_item['hint'])) {
      $result .= ' title="' . $menu_item['hint'] . '"';
    }

    // output <a> content
    $result .= '>';
    if (isset($menu_item['title-for-header-menu'])) {
      $result .= $menu_item['title-for-header-menu'];
    } else {
      $result .= $menu_item['title'];
    }
    if (isset($menu_item['dropdown'])) {
      $result .= ' <span class="caret"></span>';
    }
    $result .=  '</a>';

    // output dropdown contents
    if (isset($menu_item['dropdown'])) {
      $result .= '<ul class="dropdown-menu">';
      foreach ($menu_item['dropdown'] as $dropdown_item_page => $dropdown_item) {
        $result .= '<li>';

        // output <a href="..."
        $result .= '<a href="';
        if (isset($dropdown_item['url'])) {
          $result .= $dropdown_item['url'];
        } else {
          $result .= page_url($dropdown_item_page);
        }
        $result .= '"';

        // output <a> content
        $result .= '>';
        if (isset($dropdown_item['title-for-header-menu'])) {
          $result .= $dropdown_item['title-for-header-menu'];
        } else {
          $result .= $dropdown_item['title'];
        }
        $result .=  '</a>';

        $result .= '</li>';
      }
      $result .= '</ul>';
    }

    // finish </li>
    $result .= '</li>';
  }
  unset($menu_item);
  unset($menu_item_page);

  $result .= '
    </ul>
    <!-- Uncomment this for toggable navbar -->
    <!--/div-->';

  return $result;
}

function _castle_breadcrumbs($path)
{
  $result = '';

  /* There is no point in showing breadcrumbs containing only "Home"
     (because home link is visible anyway on the header).
     Also, no point in showing breadcrumbs if we're at the 1st page
     in header menu, as then header menu tab indicates the page.
     Also, no point when we're inside a page directly under header menu
     (because then header menu tab + page title shows where we are). */
  if (count($path) > 2)
  {
    global $castle_sitemap;

    $result = '<div class="header_breadcrumbs">' .
      a_href_page('Home', MAIN_PAGE_BASENAME);

    $path_item_num = 0;
    $path_item = '';
    $path_itemsub = $castle_sitemap;

    while ($path_item_num < count($path) - 1)
    {
      $path_item = $path[$path_item_num];

      if (!isset($path_itemsub[$path_item]))
        throw new ErrorException('No page named ' . $path_item . ' at current level of castle_sitemap');

      $path_itemtitle = $path_itemsub[$path_item]['title'];
      if (isset($path_itemsub[$path_item]['sub']))
        $path_itemsub = $path_itemsub[$path_item]['sub']; else
        $path_itemsub = NULL;

      $result .= ' &#187; ' . a_href_page($path_itemtitle, $path_item);

      $path_item_num ++;
    }

    $result .= '</div>';
  }

  return $result;
}

function _castle_find_in_sitemap($path)
{
  global $castle_sitemap;

  $path_itemsub = $castle_sitemap;
  $result = NULL;

  foreach ($path as $path_item) {
    if (!isset($path_itemsub[$path_item])) {
      throw new ErrorException('No page named ' . $path_item . ' at current level of castle_sitemap');
    }
    $result = $path_itemsub[$path_item];

    if (isset($result['sub'])) {
      $path_itemsub = $result['sub'];
    } else {
      $path_itemsub = NULL;
    }
  }

  return $result;
}

function castle_toc_from_sitemap()
{
  global $castle_page_path;
  $page_map = _castle_find_in_sitemap($castle_page_path);
  if (!isset($page_map['sub'])) {
    throw new ErrorException('Requested castle_toc_from_sitemap for page that does not have "subitems" according to castle_sitemap');
  }

  $result = '<ul>';
  foreach ($page_map['sub'] as $menu_item_page => $menu_item) {

    if (isset($menu_item['url'])) {
      $menu_item_url = $menu_item['url'];
    } else {
      $menu_item_url = page_url($menu_item_page);
    }

    $result .=  '<li><a href="' . $menu_item_url . '">' .
      $menu_item['title'] . '</a></li>';
  }
  $result .= '</ul>';
  return $result;
}

function echo_header_bonus ()
{
  global $geshi;
  $geshi = new GeSHi();
  $geshi->enable_classes();
  //$geshi->set_overall_class('sourcecode'); // not needed anymore

  ?>

<link rel="alternate" type="application/rss+xml"
  title="Castle Game Engine - News Feed"
  href="<?php echo page_url('news_feed'); ?>">

<link type="text/css" rel="stylesheet" media="all" href="<?php echo page_requisite('castle-engine-website-base/castle-engine.css'); ?>">

<?php if (defined('CASTLE_ENGINE_CUSTOM_CSS')) { ?>
  <link type="text/css" rel="stylesheet" media="all" href="<?php echo page_requisite(CASTLE_ENGINE_CUSTOM_CSS); ?>">
<?php } ?>

<style type="text/css">
<?php
/* looks like we need to set_language before get_stylesheet,
   otherwise not everything necessary is output. */
$geshi->set_language('delphi');
echo $geshi->get_stylesheet(false);
$geshi->set_language('VRML / X3D');
echo $geshi->get_stylesheet(false);
$geshi->set_language('C');
echo $geshi->get_stylesheet(false);
$geshi->set_language('XML');
echo $geshi->get_stylesheet(false);
?>
</style>

<script type="text/javascript">
var _bftn_options = {
//always_show_widget: true // @type {Boolean}
};
</script>
<!-- script src="https://widget.battleforthenet.com/widget.js" async></script -->

<?php
/* echo flattr_header(); - Flattr not used now */
echo googleplus_header();

/* Don't use facebook_header on other pages, as it makes all links
   from FB (and G+?) lead to main page --- I actually don't want this. */
global $main_page;
if ($main_page) echo facebook_header();

global $castle_wordpress;
if (empty($castle_wordpress) && CASTLE_ENVIRONMENT != 'offline') {
/* Add icons, using same HTML code as Wordpress.
   We do not do this in CASTLE_ENVIRONMENT == offline
   just because I did not adjust
   castle-engine/doc/pasdoc/html-parts/Makefile
   to copy these icon files.
   And I would prefer to store them in a directory outside wp/ then...
*/
?>
<link rel="icon" href="<?php echo page_requisite('wp/wp-content/uploads/2018/03/cropped-DefaultIconSquare-32x32.png'); ?>" sizes="32x32" />
<link rel="icon" href="<?php echo page_requisite('wp/wp-content/uploads/2018/03/cropped-DefaultIconSquare-192x192.png'); ?>" sizes="192x192" />
<link rel="apple-touch-icon-precomposed" href="<?php echo page_requisite('wp/wp-content/uploads/2018/03/cropped-DefaultIconSquare-180x180.png'); ?>" />
<meta name="msapplication-TileImage" content="<?php echo page_requisite('wp/wp-content/uploads/2018/03/cropped-DefaultIconSquare-270x270.png'); ?>" />
<?php
}
}

/* Echo a header.
   Set various global variables about this page.

   $parameters fields:
   - 'meta_description' (like for common_header)
   - 'path' (array of strings;
     a list of page names, a path in the tree of $castle_sitemap,
     to the current page.
     The $page_basename is added at the end, if not already there.
     It will be set as global $castle_page_path.)
   - 'social_share_image' (string,
     name of image to be used as Facebook share image (og:image);
     relative to in images/original_size/ , unless it's an absolute URL,
     with a protocol like http[s] etc.)
*/
function castle_header($a_page_title, array $parameters = array())
{
  /* call common_header with proper params */
  $common_header_parameters = array();
  if (isset($parameters['meta_description'])) {
    $common_header_parameters['meta_description'] = $parameters['meta_description'];
  }
  if (!empty($parameters['social_share_image'])) {
    if (kambi_url_absolute($parameters['social_share_image'])) {
      $social_share_image_url = $parameters['social_share_image'];
    } else {
      $social_share_image_url = page_url('images/original_size/' . $parameters['social_share_image']);
    }
    $common_header_parameters['bonus_head_html'] =
      '<meta property="og:image" content="' . $social_share_image_url . '"/>';
  }
  common_header($a_page_title, $common_header_parameters);

  $path = array();
  if (isset($parameters['path'])) {
    $path = $parameters['path'];
  }
  echo_castle_header_suffix($path);
}

function echo_castle_header_suffix($path, $enable_sidebar = true)
{
  global $castle_sidebar;
  global $castle_sitemap;
  global $page_basename;
  global $castle_page_path;

  /* calculate $castle_page_path and $path */
  /* make sure $path ends with $page_basename.
     This way, we also make sure $path is never empty. */
  if (count($path) == 0 || $path[count($path) - 1] != $page_basename) {
    $path[] = $page_basename;
  }
  $castle_page_path = $path;

  /* traverse $castle_sitemap, along the $path.
     Find which items should be used for a sidebar, if any. */
  $sidebarroot_num = -1;
  $sidebarroot_page = NULL;
  $sidebarroot_info = NULL;
  $sidebarroot_sidebar = false;
  $sidebarroot_sub = $castle_sitemap;
  if ($enable_sidebar) {
    while (!$sidebarroot_sidebar)
    {
      $sidebarroot_num ++;
      if ($sidebarroot_num == count($path))
      {
        /* end of path, nothing wants sidebar */
        $sidebarroot_page = NULL;
        $sidebarroot_info = NULL;
        break;
      } else
      {
        $sidebarroot_page = $path[$sidebarroot_num];

        if (!isset($sidebarroot_sub[$sidebarroot_page]))
          throw new ErrorException('No page named ' . $sidebarroot_page . ' at current level of castle_sitemap');

        $sidebarroot_info = $sidebarroot_sub[$sidebarroot_page];
        $sidebarroot_sidebar =
          isset($sidebarroot_info['sidebar']) &&
                $sidebarroot_info['sidebar'];
        if (isset($sidebarroot_info['sub']))
          $sidebarroot_sub = $sidebarroot_info['sub']; else
          $sidebarroot_sub = NULL;
      }
    }
  }

  /* make sidebar */
  if ($sidebarroot_page !== NULL && $sidebarroot_info !== NULL)
  {
    $castle_sidebar = _castle_sidebar($sidebarroot_page, $sidebarroot_info);
  } else {
    $castle_sidebar = '';
  }

  if (defined('CASTLE_GITHUB_NAME')) {
    $github_ribbon = '<a href="https://github.com/castle-engine/' . CASTLE_GITHUB_NAME . '" class="github-ribbon"><img src="' . page_requisite('images/forkme_right_orange_ff7600.png') . '" alt="Fork me on GitHub"></a>';
  } else {
    $github_ribbon = '';
  }

  $rendered = '
  <nav class="navbar navbar-default">
    <div class="container-fluid">
      <!-- Uncomment this for toggable navbar -->
      <!--
      <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#main-navbar-collapse-1" aria-expanded="false">
        <span class="sr-only">Toggle navigation</span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
      </button>
      -->

      <ul class="nav nav-tabs navbar-right">
        <li><a href="' . PATREON_URL . '" class="navbar-link">Support the engine on<br><img class="patreon-logo" src="' . page_requisite('images/patreonlogoorange_45px.png') . '" alt="Patreon" /></a></li>
      </ul>

      <!--button type="button" class="btn btn-default navbar-btn navbar-right" style="margin-right: 0px;"><a href="' . PATREON_URL . '" class="navbar-link">Support us on<br><img style="height: 40px" src="' . page_requisite('images/patreonlogoorange.png') . '" alt="Patreon" /></a></button-->

      <!--p class="navbar-text navbar-right"><a href="' . PATREON_URL . '" class="navbar-link">Support us on<br><img style="height:50px" src="' . page_requisite('images/patreonlogoorange.png') . '" alt="Patreon" /></a></p-->

      <div class="navbar-header">
        <a class="navbar-brand" href="'.en_page_url(MAIN_PAGE_BASENAME).'">
          <img alt="" src="' . page_requisite('images/header_icon.png') . '">
        </a>
        <a class="navbar-brand" href="'.en_page_url(MAIN_PAGE_BASENAME).'">
          Castle Game Engine
        </a>
      </div>
      ' .
      // TODO: No idea now where to place this, so it doesn't interfere with "patreon"
      // google_custom_search_box() .
      '
      ' . _castle_header_menu($path[0]) . '
    </div>

    ' . $github_ribbon . '
  </nav>';

  if (CASTLE_ENVIRONMENT == 'preview') {
    $rendered .= '<div class="alert alert-warning preview-warning" role="alert"><strong>This is a preview!</strong> This is not the official <i>Castle Game Engine</i> website (<a href="' . CASTLE_FINAL_URL . '">official website is here</a>). This is only a preview for developers, to see the next website before the release, and to see the documentation for the unstable engine version (from <a href="https://github.com/castle-engine/castle-engine">GitHub master</a>).</div>';
  }

  // make sure to start container-fluid for bootstrap container
  if (empty($castle_sidebar)) {
    $rendered .=  _castle_breadcrumbs($path) .
    '<div class="container-fluid">';
  } else {
    $rendered .=
    '<div class="container-fluid">
      <div class="row">
        <div class="col-sm-9 col-sm-push-3 content-near-sidebar">
          ' . _castle_breadcrumbs($path);
  }

//  $rendered .= google_custom_search_results();

  echo $rendered;
}

function castle_footer()
{
  echo disqus_form();

  /* This should be done after jQuery JS loaded.
     But, actually it's not needed, it seems.
  ? >
  <script type="text/javascript">
  jQuery('.dropdown-toggle').dropdown();
  </script>
  < ?php
  */

  global $castle_sidebar;
  if (empty($castle_sidebar)) {
    echo '</div>';
  } else {
    echo '</div><div class="col-sm-3 col-sm-pull-9 well sidebar">' .$castle_sidebar. '</div></div></div>';
  }

  common_footer(page_requisite('castle-engine-website-base/castle-engine.js'));
}

function echo_footer ()
{
  ?>

  <div class="panel-footer">
    <p><span class="page_copyright">Copyright <a href="http://michalis.ii.uni.wroc.pl/~michalis/">Michalis Kamburelis</a> and Paweł Wojciechowicz from <a href="https://cat-astrophe-games.com/">Cat-astrophe Games</a>.
    This documentation itself is also open-source,
    you can redistribute it on terms of the
    <a href="http://www.gnu.org/licenses/gpl.html">GNU General Public License</a>.</span>

    <p>We use <a href="https://en.wikipedia.org/wiki/HTTP_cookie">cookies</a>
    for analytics.
    Like every other frickin' website on the Internet.
    See our <a href="<?php echo CURRENT_URL; ?>donate.php">privacy policy</a>.
  </div>

  <?php

  /* Insert tracking code */
  echo_piwik_tracking();
  echo_google_analytics_tracking();
}

/* Return SVN URL to appropriate path with repository trunk.
   If $prefix_command is true then also will add 'svn checkout ' text
   at the beginning. */
function sf_checkout_link($prefix_command, $path, $force_https = false)
{
  return
    ($prefix_command ? 'svn checkout ' : '') .
    ($force_https ? 'https' : 'http' ).
    '://svn.code.sf.net/p/castle-engine/code/trunk/' .
    $path;
}

function castle_download_button($title, $url)
{
  return '<a class="btn btn-primary btn-lg" href="' .
    htmlspecialchars($url) .  '">' . $title. '</a>';
}

function sf_download_url($file_name)
{
  return 'http://downloads.sourceforge.net/' . SF_UNIX_NAME . '/' . $file_name;
}

/* Makes a link to a download from SourceForge file release system. */
function sf_download($title, $file_name)
{
  return castle_download_button($title, sf_download_url($file_name));
}

function download_donate_footer()
{
  return /* '
    <hr/>
      Please <a href="' . PATREON_URL . '">support the engine development on Patreon!</a>'; */

      /* <div style="float: left">' . paypal_button(false) . '</div> */

      /* <small>If you like this software, <a href="' . CURRENT_URL . 'donate.php">please consider donating</a>.</small>'; */

    '<a class="btn btn-success btn-lg btn-patreon" href="' . PATREON_URL .
    '"><span class="glyphicon glyphicon-heart" aria-hidden="true"></span> Suppport on Patreon</a>';
}

/* This echoes a list to download various versions of the same program.
     < ?php echo castle_download_button("Foo for Linux", ....); ? >
   where $prog_nice_name = Foo.
   The $prog_archive_basename is used to generate download URLs for SourceForge
   (using sf_download_url), ignored if you supply $os_arch_urls for now.

   If $prog_version is '' then the whole -version part will be omitted
   (i.e. $prog_version = '' causes also the dash '-' before version
   to disappear, since this is what you usually want).

   $os_arch_urls is the list of supported OS/architectures (keys)
   and appropriate URLs (values, may be NULL to use SourceForge link).
   Leave NULL (default) to use the default set of OS/architectures hardcoded
   inside, and SourceForge download links. */
function echo_standard_program_download(
  $prog_nice_name, $prog_archive_basename, $prog_version, $macosx_dmg = false,
  $os_arch_urls = NULL)
{
  global $this_page_name, $os_arch_caption, $os_arch_extension;

  $arch_name_start = $prog_archive_basename;
  if ($prog_version != '')
    $arch_name_start .= '-' . $prog_version;
  $arch_name_start .= '-';

  $nice_name_start = 'Download ' . $prog_nice_name;
  /* no, looks bad:
  if ($prog_version != '')
    $nice_name_start .= ' (' . $prog_version . ')';
  */

  if ($os_arch_urls === NULL) {
    $os_arch_urls = array(
      'win-i386' => NULL,
      'linux-i386' => NULL,
      'linux-x86_64' => NULL,
      'macosx-i386' => NULL,
    );
  }

  $os_arch_caption = array(
    'win-i386'     => ' Windows<br/>(all versions, 32 or 64-bit)',
    'linux-i386'   => ' Linux<br/>(32 bit)',
    'linux-x86_64' => ' Linux<br/>(64 bit, x86_64)',
    'macosx-i386'  => ' Mac OS X<br/>(all versions, 32 or 64-bit)',
  );

  $os_arch_extension = array(
    'win-i386'     => '.zip',
    'linux-i386'   => '.tar.gz',
    'linux-x86_64' => '.tar.gz',
    'macosx-i386'  => $macosx_dmg ? '.dmg' : '.tar.gz',
  );

  $os_arch_icon = array(
    'win-i386'     => 'win',
    'linux-i386'   => 'linux32',
    'linux-x86_64' => 'linux64',
    'macosx-i386'  => 'macosx'
  );

  echo '<div class="download jumbotron">';

  if (CASTLE_GENERATE_OFFLINE) {
    /* Since the download links contain so many things
       (program version, available OSes, availability on servers
       (github ? sf ?)), it's safer to not put this information
       in the locally generated page (since it changes to often).
       Instead, we can place a link to the WWW page. */
    echo '<p><a href="' . page_url($this_page_name) .
      '" class="btn btn-primary btn-lg">Download ' . $prog_nice_name . ' from our WWW page</a></p>';
  } else
  {
    echo '<div class="download_title">' . $nice_name_start . ':</div>
      <div class="download_platforms_list">';
    foreach ($os_arch_urls as $os_arch => $download_url)
    {
      // calculate non-empty $download_url
      if (empty($download_url)) {
        // for Mac OS X dmg, there is no "-i386" in the filename
        $os_arch_filename = $os_arch;
        if ($os_arch_filename == 'macosx-i386' && $macosx_dmg) {
          $os_arch_filename = 'macosx';
        }
        $download_url = sf_download_url(
          $arch_name_start . $os_arch_filename . $os_arch_extension[$os_arch]);
      }

      echo '<div class="download_platform">';
      echo castle_download_button(
        '<img src="' .
        page_requisite('images/os_icons/' .
          /* This size should be synched with images/Makefile rule */
          $os_arch_icon[$os_arch] . '.png') .
        '" width="64" height="64" alt="' .
        $os_arch_caption[$os_arch] . '"><br/>' .
        $os_arch_caption[$os_arch],
        $download_url);
      echo '</div>' . "\n";
    }
    echo "</div>\n";
  }

  echo download_donate_footer() . '
  </div>';
  /* <!-- This helps Michalis to spend more time on developing our engine, our tools and games :) Details about how you can donate are here</a>.<--> */
}

/* Return html (<table> or <div>) with image links.

   Each $images array item is another associative array:
   - filename: name of the image file (with extension, without path).
     Must exist within original_size/ of our images, and be generated
     to our other sizes (like thumb_size and thumb_const_height_size).
     You can omit this,
     if you instead provide ready URLs in filename_full and filename_thumbnail.
   - url_full: URL to the full-size image file.
     If missing, we will derive it from 'filename'.
   - url_thumb: URL to the thumb-size image file.
     If missing, we will derive it from 'filename'.
   - titlealt - text used for both title and alt.
   - html - if set, the rest (except colspan) is ignored,
     and we simply put this html into cell content.
   - colspan (int) - colspan of table cell. It's your responsibility to make it
     look sensible then.

   $align is 'left', 'center' or 'right'.

   $columns may be an int, it then specifies number of columns of the table.
   We will automatically divide $images into rows with $columns images
   (the last row may be left shorter).

   $columns may also be a string 'auto', in which case we will put images
   in a div, and we will fit as many images as the parent width allows.

   The generated links are absolute (starting with CURRENT_URL).
   The important work is always done directly (without need for CSS classes),
   so this content is suitablefor inclusion also in HTML RSS feeds.
*/
function castle_thumbs($images, $columns=1, $align='right', $thumb_size = NULL)
{
  if (count($images) == 0) {
    return '';
  }

  if ($columns === 'auto') {
    $result = '<div class="thumbnails thumbnails-align-' . $align . '">';
  } else {
    $result = '<table class="thumbnails thumbnails-align-' . $align . '"><tr>';
  }

  if ($thumb_size === NULL) {
    $thumb_size = ($columns !== 'auto' ? 'thumb_size' : 'thumb_const_height_size');
  }

  $column_now = 0;
  $image_index = 0;

  foreach ($images as $image)
  {
    if ($columns !== 'auto') {
      if ($column_now == 0 && $image_index != 0) {
        $result .= '</tr><tr>';
      }

      // special trick for the last image when columns > 1:
      // add empty cells, so that the last image is at right.
      if ($column_now == 0 &&
          $image_index == count($images) - 1 &&
          !isset($image['colspan'])) {
        $result .= str_repeat('<td></td>', $columns - 1);
      }

      if (isset($image['colspan']))
        $colspan = ' colspan="' . (int)$image['colspan'] . '"'; else
        $colspan = '';

      $result .= '<td' . $colspan . '>';
    }

    if (isset($image['html']))
    {
      $result .= $image['html'];
    } else
    {
      $url_full = isset($image['url_full']) ? $image['url_full'] :
        page_url('images/original_size/' . $image['filename']);
      $url_thumb = isset($image['url_thumb']) ? $image['url_thumb'] :
        page_requisite('images/' . $thumb_size . '/' . $image['filename']);
      $result .= '
          <a href="' . $url_full . '"
             class="screenshot"
             title="' . $image['titlealt'] . '"><img
            style="float: right"
            src="' . $url_thumb . '"
            alt="' . $image['titlealt'] . '"
          /></a>';
    }

    if ($columns !== 'auto') {
      $result .= '</td>';
    }

    // increase $column_now
    if (isset($image['colspan']))
      $column_now += (int)$image['colspan']; else
      $column_now++;
    if ($column_now >= $columns) {
      $column_now = 0;
    }

    $image_index++;
  }

  if ($columns !== 'auto') {
    $result .= '</tr></table>';
  } else {
    $result .= '</div>';
  }

  return $result;
}

/* Thumbnail for this program. Filename and alt/title auto-generated
   from $prog_name. */
function default_program_thumbnail($prog_name)
{
  return castle_thumbs(array(
    array('filename' => $prog_name . '_screen_demo.png', 'titlealt' => 'Image from &quot;' . $prog_name . '&quot;'),
  ));
}

/* Constants and things that need a_href_page* */

define('SOURCES_OF_THIS_PROG_ARE_AVAIL',
  'This is free/open-source software. Developers can ' .
  a_href_page('download sources of this program', 'all_programs_sources') . '.');

/* DEPENDS_ consts and funcs */

define('DEPENDS', 'Requirements');
define('DEPENDS_OPENGL',
  '<a href="http://www.opengl.org/documentation/implementations/">OpenGL</a>
  <!--
  (on all modern OSes, OpenGL is probably already installed and working on
  your system) -->');
define('DEPENDS_LIBPNG_AND_ZLIB',
  '<a href="http://www.libpng.org/">Libpng</a>,
   <a href="http://www.gzip.org/zlib/">Zlib</a>
   (under Windows appropriate DLL files are already included
   in program\'s archive, so you don\'t have to do anything)');
define('SUGGESTS_OPENAL',
  a_href_page_hashlink('OpenAL', 'openal_notes', 'section_install') .
  ' is strongly suggested if you want to hear sound
  (under Windows appropriate DLL files are already included
  in program\'s archive, so you don\'t have to do anything)');
define('SUGGESTS_OPENAL_VORBISFILE',
  a_href_page_hashlink('OpenAL', 'openal_notes', 'section_install') .
  ' and <a href="http://xiph.org/vorbis/">OggVorbis (VorbisFile and dependencies)</a>
  libraries are strongly suggested if you want to hear sound
  (under Windows appropriate DLL files are already included
  in program\'s archive, so you don\'t have to do anything)');
define('DEPENDS_UNIX_CASTLE_WINDOW_GTK_1',
  'Under Unices (Linux, FreeBSD, Mac OS X):
  <a href="http://www.gtk.org/">GTK+</a> 1.x and gtkglarea');
define('DEPENDS_UNIX_CASTLE_WINDOW_GTK_2',
  'Under Unix (Linux, FreeBSD, Mac OS X):
  <a href="http://www.gtk.org/">GTK+</a> >= 2.6 and
  <a href="http://gtkglext.sourceforge.net/">GtkGLExt</a> >= 1.0.6');
  /* I also use some GTK >= 2.8 features, but since Mac OS X fink stable
     includes only GTK 2.6, we work Ok with GTK 2.6 too. */
define('DEPENDS_MACOSX',
  'Mac OS X users should look at the ' .
  a_href_page('list of dependencies on Mac OS X', 'macosx_requirements') );

function depends_par($depends_array)
{
  $result = '';
  foreach($depends_array as $item)
  {
    if ($result != '') $result .= ', ';
    $result .= $item;
  }

  $result = "<p><b>" . DEPENDS . ":</b> $result.";
  return $result;
}

function depends_ul($depends_array)
{
  return array_to_ul($depends_array);
}

function api_link($title, $href, $output = true)
{
  global $castle_apidoc_url;
  $result = '<a href="' . $castle_apidoc_url . htmlspecialchars($href) . '">'
    . htmlspecialchars($title) . '</a>';
  if ($output) echo $result;
  return $result;
}

/* Highlight XML code, detecting [[xxx|yyy]] as links to API doc.

   We do not use geshi for this now, since as far as I know we cannot do this
   with geshi (geshi's "keyword URLs" are too weak for what we want,
   we want various XML attributes (sometimes named the same, but under different
   elements) to lead to different URL). */
function xml_highlight($source)
{
  $source = str_replace('&', '&amp;', $source);
  $source = str_replace('<', '&lt;', $source);
  $source = str_replace('>', '&gt;', $source);
  $source = str_replace('&lt;!--', '<span class="xml_highlight_comment">&lt;!--', $source);
  $source = str_replace('--&gt;', '--&gt;</span>', $source);
  $source = '<pre class="xml">' . $source . '</pre>';

  // replace [[http:xxx|yyy] with <a href="http:xxx">yyy</a>
  $source = preg_replace('/\\[\\[(https?:.+)\\|(.+)\\]\\]/',
    '<a href="\\1">\\2</a>', $source);

  // replace [[http:xxx] with <a href="http:xxx">xxx</a>
  $source = preg_replace('/\\[\\[(https?:.+)\\]\\]/',
    '<a href="\\1">\\1</a>', $source);

  // replace [[xxx|yyy] with <a href="$castle_apidoc_url/xxx">yyy</a>
  global $castle_apidoc_url;
  $source = preg_replace('/\\[\\[(.+)\\|(.+)\\]\\]/',
    '<a href="' . $castle_apidoc_url . '\\1">\\2</a>', $source);

  return $source;
}

function xml_full_highlight($source)
{
  global $geshi;
  $geshi->set_source($source);
  $geshi->set_language('xml');
  $geshi->set_footer_content('');
  return $geshi->parse_code();
}

function pascal_highlight($source)
{
  global $geshi;
  $geshi->set_source($source);
  $geshi->set_language('delphi'); // tested: order of set_source, set_language doesn't matter
  if (!HTML_VALIDATION) {
    $geshi->set_footer_content(''); // remove set_footer_content by pascal_highlight_file
  }
  return $geshi->parse_code();
}

function pascal_highlight_file($file_name)
{
  // trim, to avoid final newline
  $source = trim(file_get_contents($file_name));

  global $geshi;
  $geshi->set_source($source);
  $geshi->set_language('delphi');
  /* Unfortunately, tidy says that this isn't OK, putting <div>
     (with set_footer_content) inside a <pre> */
  if (!HTML_VALIDATION) {
    $geshi->set_footer_content('<br><a class="download_code" href="' . htmlspecialchars($file_name) . '">// This is a complete source code that you can compile. Download it!</a>');
  }
  return $geshi->parse_code();
}

function x3d_spec_latest_url($component_name, $anchor = '')
{
  if ($anchor != '') {
    $anchor = '#' . $anchor;
  }
  return 'http://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/' .
    $component_name . '.html' . $anchor;
}

function vrmlx3d_highlight($source)
{
  global $geshi;
  $geshi->set_source($source);
  $geshi->set_language('VRML / X3D');
  $geshi->set_footer_content('');
  return $geshi->parse_code();
}

function glsl_highlight($source)
{
  global $geshi;
  $geshi->set_source($source);
  $geshi->set_language('C'); // don't use glSlang, for some reason it's broken for me
  $geshi->set_footer_content('');
  return $geshi->parse_code();
}
