<?php
/* PHP functions common for castle-engine WWW pages. */

/* You can temporarily change this (but don't commit!) to true.
   This removes *some* stuff that embeds online content on our webpages,
   see castle_engine_externals.php.
   It also sets CURRENT_URL to '', which means that things that normally
   refer using absolute URL will instead refer to local copy
   (this concerns things like links to thumbnails in news items;
   they use full absolute URLs, because they are
   also used inside RSS feeds, which may be displayed on other servers).

   Overall, this allows to browse our webpages locally
   (through http://localhost/...) faster. */
define('CASTLE_OFFLINE', false);

/* Constants that should be defined before including kambi_common.php */
define('ENV_VARIABLE_NAME_LOCAL_PATH', 'CASTLE_ENGINE_PATH');
if (CASTLE_OFFLINE)
  define('CURRENT_URL', ''); else
  define('CURRENT_URL', 'http://castle-engine.sourceforge.net/');
define('CURRENT_URL_SHORT', 'castle-engine.sf.net');
define('KAMBI_NO_HOME_LINK', true);

define('CASTLE_REFERENCE_URL', CURRENT_URL . 'apidoc/html/');
//define('CASTLE_REFERENCE_URL', 'http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/docs/reference/html/');

function reference_link()
{
  return CASTLE_REFERENCE_URL . 'index.html';
}

/* This set_include_path is needed on SourceForge, otherwise
   includes from within kambi-php-lib sometimes fail. */
set_include_path('.:kambi-php-lib/:geshi/');
require_once 'kambi-php-lib/kambi_common.php';
require_once 'generated_versions.php';
require_once 'castle_engine_externals.php';
require_once 'castle_engine_books.php';
require_once 'geshi.php';

define('S_INSTALLATION_INSTRUCTIONS_SHORT',
  'No installation is required. Just download and unpack these archives wherever
  you want, and run the program inside. The documentation
  (this web page) is also included inside
  (look in the <tt>documentation/</tt> subdirectory) for offline viewing.');

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
  MAIN_PAGE_BASENAME       => array('title' => 'Intro',
    /* 'sub' and 'sidebar' of this will be calculated by news.php page,
       since it may be time-consuming (requires reading large $news table). */
  ),

  'news'                   => array('title' => 'News',
    /* 'sub' and 'sidebar' of this will be calculated by news.php page,
       since it may be time-consuming (requires reading large $news table). */
  ),

  'view3dscene'            => array('hint' => 'VRML / X3D browser, and a viewer for other 3D model formats', 'title' => 'view3dscene'),

  'castle'                 => array('hint' => 'First-person perspective game, in a dark fantasy setting'   , 'title' => 'The Castle',
    'sidebar' => true,
    'sub' => array(
      'castle-advanced'    => array('title' => 'Additional notes (troubleshooting)'),
      'castle-credits'     => array('title' => 'Credits'),
    ),
  ),

  'all_programs' => array('hint' => 'All the games and tools using our 3D engine', 'title' => 'All Programs',
    'sub' => array(
      'mountains_of_fire' => array('title' => 'Mountains Of Fire'),
      'darkest_before_dawn' => array('title' => 'Darkest Before the Dawn'),
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
      'bezier_curves' => array('title' => 'bezier_curves'),
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

  'forum' => array('hint' => 'Ask for help, report bugs, discuss features', 'title' => 'Forum'),

  'donate' => array('title' => 'Donate'),

  'engine' => array('hint' => 'Sources and documentation for developers', 'title' => 'Engine overview for developers', 'title-for-header-menu' => 'Engine',
    'sidebar' => true,
    'sub' => array(
      'tutorial_intro' => array('title' => 'Tutorial',
        'sub' => array(
          'tutorial_intro' => array('title' => 'Introduction'),
          'tutorial_install' => array('title' => 'Download, install, try demos'),
          'tutorial_opengl_context' => array('title' => 'OpenGL context'),
          'tutorial_scene_manager' => array('title' => 'Scene Manager'),
          'tutorial_load_3d' => array('title' => 'Simple loading of 3D models'),
          'tutorial_game_level' => array('title' => 'Loading game level'),
          'tutorial_player' => array('title' => 'Player'),
          'tutorial_mobile' => array('title' => 'Developing mobile (and cross-platform) games'),
          'tutorial_resources' => array('title' => 'Creatures and items',
            'sub' => array(
              'tutorial_resources_using_existing' => array('title' => 'Using existing creatures/items classes'),
              'tutorial_resources_extending' => array('title' => 'Extending existing creatures/items classes'),
            ),
          ),
          'tutorial_up' => array('title' => 'Which way is up?'),
          'tutorial_3d_custom' => array('title' => 'Define other 3D objects'),
          'tutorial_player_2d_controls' => array('title' => 'Display 2D controls: player HUD'),
          'tutorial_on_screen_menu' => array('title' => 'On-screen menu'),
          'tutorial_notifications' => array('title' => 'Notifications'),
          'tutorial_sound' => array('title' => 'Sound'),
          'tutorial_screenshots' => array('title' => 'Screenshots'),
          'tutorial_network' => array('title' => 'Network and downloading'),
          'tutorial_recording_movies' => array('title' => 'Recording movies'),
          'tutorial_log' => array('title' => 'Logging'),
          'tutorial_user_prefs' => array('title' => 'User preferences'),
          'tutorial_key_mouse' => array('title' => 'Key/mouse shortcuts'),
          'tutorial_optimization' => array('title' => 'Optimization and profiling'),
          'tutorial_transformation_hierarchy' => array('title' => 'Transformation hierarchy'),
          'tutorial_classes_overview' => array('title' => 'Classes overview (cheatsheet)'),
        ),
      ),
      'creating_data_intro' => array('title' => 'Creating game data',
        'sub' => array(
          'creating_data_intro' => array('title' => 'Introduction'),
          'creating_data_3d' => array('title' => '3D models'),
          'creating_data_xml' => array('title' => 'XML files describing game data (level.xml, resource.xml and others)'),
          'creating_data_levels' => array('title' => 'Levels'),
          'creating_data_resources' => array('title' => 'Resources (creatures and items)'),
          'creating_data_player' => array('title' => 'Player configuration'),
          'creating_data_material_properties' => array('title' => 'Material properties configuration'),
          'creating_data_sound' => array('title' => 'Sound'),
        )
      ),
      'reference' => array('title' => 'Reference', 'url' => reference_link()),
      'android' => array('title' => 'Android', 'url' => 'http://sourceforge.net/p/castle-engine/wiki/Android%20development/'),
      'ios' => array('title' => 'iOS (iPhone, iPad)', 'url' => 'http://sourceforge.net/p/castle-engine/wiki/iOS%20Development/'),
      'engine_doc' => array('title' => 'Internals documentation'),
      'movies' => array('title' => 'Movies on YouTube', 'url' => 'https://www.youtube.com/channel/UCq9jJ5ivIXC5VEWiUAfxBxw'),
    ),
  ),

  'vrml_x3d' => array('hint' => 'Our extensions and status of VRML/X3D implementation', 'title' => 'VRML / X3D support', 'title-for-header-menu' => 'VRML/X3D' /* shorter title */,
    'sidebar' => true,
    'sub' => array(
      'demo_models' => array('title' => 'Demo models'),
      'x3d_extensions' => array('title' => 'Extensions',
        'sub' => array(
          'compositing_shaders' => array('title' => 'Compositing Shaders'),
          'x3d_extensions_screen_effects' => array('title' => 'Screen Effects'),
          'x3d_extensions_shadow_maps' => array('title' => 'Shadow Maps'),
          'x3d_extensions_vrml1' => array('title' => '(Old) VRML 1.0'),
        ),
      ),
      'x3d_implementation_status' => array('title' => 'X3D Components',
        'sub' => array(
          'x3d_implementation_core'                 => array('title' => 'Core'                            ),
          'x3d_implementation_time'                 => array('title' => 'Time'                            ),
          'x3d_implementation_networking'           => array('title' => 'Networking'                      ),
          'x3d_implementation_grouping'             => array('title' => 'Grouping'                        ),
          'x3d_implementation_rendering'            => array('title' => 'Rendering'                       ),
          'x3d_implementation_shape'                => array('title' => 'Shape'                           ),
          'x3d_implementation_geometry3d'           => array('title' => 'Geometry3D'                      ),
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
              'x3d_multi_texturing' => array('title' => 'X3D MultiTexturing problems and proposed solutions'),
              'x3d_implementation_texturing_extensions' => array('title' => 'Extensions'),
            ),
          ),
          'x3d_implementation_interpolation'        => array('title' => 'Interpolation',
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
      'nist_vrml_test_suite' => array('title' => 'NIST conformace test suite'),
      'castle_script' => array('title' => 'CastleScript language reference'),
      'kanim_format' => array('title' => 'Kanim (precalculated animations) file format'),
      'x3d_time_origin_considered_uncomfortable' => array('title' => 'VRML / X3D time origin considered uncomfortable'),
    ),
  ),

  'blender' => array('title' => 'Blender X3D exporter', 'hint' => 'Customized Blender X3D exporter', 'title-for-header-menu' => 'Blender'),
);

function _castle_bootstrap()
{
  kambi_bootstrap();
  global $castle_sitemap;
  castle_sitemap_book_correct('tutorial',
    $castle_sitemap['engine']['sub']['tutorial_intro']['sub']);
  castle_sitemap_book_correct('creating_data',
    $castle_sitemap['engine']['sub']['creating_data_intro']['sub']);
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
    <p><a href="http://castle-engine.sourceforge.net/donate.php">Donate</a></p>
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

  $menu_for_users = 7 * 2 + 1;
  $menu_for_developers = 2 * count($castle_sitemap) + 1 - $menu_for_users;

  /* It's a hack even to use a table cell for this.
     It's even bigger hack to insert empty <div> here, but it's required
     (on Google Chrome and Konqueror and IE, not on FireFox)
     to place the lower border of separator at exactly the same level
     as lower border of tabs.
     Using border-bottom on td.lower_separator doesn't work reliably for this. */
  $td_separator = '<td class="lower_separator"><div>&nbsp;</div></td>';

  $result = '
    <table class="header_menu">
      <tr>
        <td colspan="' . $menu_for_users . '" class="higher higher_left">&larr; Users</td>
        <td colspan="' . $menu_for_developers . '" class="higher higher_right">Developers &rarr;</td>
      </tr>
      <tr>' . $td_separator;

  foreach($castle_sitemap as $menu_item_page => $menu_item)
  {
    $result .= '<td class="lower"><a href="';
    if (isset($menu_item['url']))
      $result .= $menu_item['url']; else
      $result .= en_page_url($menu_item_page);
    $result .= '"';
    if (isset($menu_item['hint']))
      $result .= ' title="' . $menu_item['hint'] . '"';
    if ($menu_item_page == $current_page)
      $result .= ' id="current"';
    if (isset($menu_item['title-for-header-menu']))
      $title = $menu_item['title-for-header-menu']; else
      $title = $menu_item['title'];
    $result .= '>' . $title . '</a></td>' . $td_separator;
  }
  unset($menu_item);
  unset($menu_item_page);

  $result .= '
      </tr>
    </table>';

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

function echo_header_bonus ()
{
  global $geshi;
  $geshi = new GeSHi();
  $geshi->enable_classes();
  $geshi->set_overall_class('sourcecode');
  /* looks like we need to set_language before get_stylesheet,
     otherwise not everything necessary is output. */
  $geshi->set_language('delphi');

  ?>

<link rel="alternate" type="application/rss+xml"
  title="Castle Game Engine - News Feed"
  href="<?php echo CURRENT_URL; ?>news_feed.php">

<link type="text/css" rel="stylesheet" media="all" href="castle-engine.css">
<link type="text/css" rel="stylesheet" href="colorbox/example3/colorbox.css">

<?php if (defined('CASTLE_ENGINE_CUSTOM_CSS')) { ?>
  <link type="text/css" rel="stylesheet" media="all" href="<?php echo CASTLE_ENGINE_CUSTOM_CSS; ?>">
<?php } ?>

<script type="text/javascript" src="castle-engine.js"></script>
<script type="text/javascript" src="js/jquery.min.js"></script>
<script type="text/javascript" src="colorbox/jquery.colorbox-min.js"></script>

<style type="text/css"><!--
<?php echo $geshi->get_stylesheet(false); ?>
 -->
</style>

<?php
echo flattr_header();
echo googleplus_header();

/* Don't use facebook_header on other pages, as it makes all links
   from FB (and G+?) lead to main page --- I actually don't want this. */
global $main_page;
if ($main_page) echo facebook_header();
?>

  <?php
}

/* $path is a list of page names, a path in the tree of $castle_sitemap,
   to the current page. The $page_basename is added at the end,
   if not already there. */
function castle_header($a_page_title, $meta_description = NULL, $path = array())
{
  common_header($a_page_title, LANG_EN, $meta_description);

  global $castle_sidebar;
  global $castle_sitemap;
  global $page_basename;

  /* make sure $path ends with $page_basename.
     This way, we also make sure $path is never empty. */
  if (count($path) == 0 || $path[count($path) - 1] != $page_basename)
    $path[] = $page_basename;

  /* traverse $castle_sitemap, along the $path.
     Find which items should be used for a sidebar, if any. */
  $sidebarroot_num = -1;
  $sidebarroot_page = NULL;
  $sidebarroot_info = NULL;
  $sidebarroot_sidebar = false;
  $sidebarroot_sub = $castle_sitemap;
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

  /* make sidebar */
  if ($sidebarroot_page !== NULL && $sidebarroot_info !== NULL)
    $castle_sidebar = _castle_sidebar($sidebarroot_page, $sidebarroot_info); else
    $castle_sidebar = '';

  $rendered = '
  <div class="header">
    <!--img class="header_icon" src="images/header_icon.png" alt="Castle Game Engine icon" /-->
    ' . google_custom_search_box() . '
    <div class="header_title"><a href="'.en_page_url(MAIN_PAGE_BASENAME).'">Castle Game Engine</a></div>
    ' . _castle_header_menu($path[0]) . '
  </div>';

  if (empty($castle_sidebar))
    $rendered .=  _castle_breadcrumbs($path) . '<div class="content">'; else
    $rendered .= '<table class="layout" cellspacing="0">
      <col class="content_column">
      <col class="sidebar_column">
      <tr><td class="layout content">' . _castle_breadcrumbs($path);

//  $rendered .= google_custom_search_results();

  echo $rendered;
}

function castle_footer()
{
  global $castle_sidebar;

  if (empty($castle_sidebar))
    echo '</div>'; else
    echo '</td><td class="layout">' .$castle_sidebar. '</td></tr></table>';

  ?>
<script type="text/javascript">
  jQuery('a.screenshot').colorbox({opacity: 0.9, rel:'screenshot', maxWidth:'90%', maxHeight:'90%'});
</script>
  <?php

  common_footer();
}

function echo_footer ()
{
  if (IS_GEN_LOCAL) { ?>
    <address>
    By Michalis Kamburelis, as part of
    <?php echo "<a href=\"" . CURRENT_URL . "\">Castle Game Engine</a>"; ?>.
    </address>
    <?php
  }

  ?>

  <table class="footer-table"><tr>

    <td><a href="http://sourceforge.net/projects/castle-engine">Hosted by SourceForge.net</a></td>

    <td><?php echo PAGE_COPYRIGHT; ?><br/>
      <small>We use <a href="http://en.wikipedia.org/wiki/HTTP_cookie">cookies</a>.
      Like every other frickin' website on the Internet.
      Blink twice if you understand.</small>
    </td>

    <?php if (!IS_GEN_LOCAL) { ?>
      <td> <a href="http://validator.w3.org/check/referer"> <img border="0" src="images/valid-html401.png" alt="Valid HTML 4.01!" height="31" width="88"></a> <?php // style="padding-bottom: 3%" ?> </td>
    <?php } ?>

  </tr></table>



  <?php

  /* Insert piwik code */
  if ( (!IS_GEN_LOCAL) &&
       isset($_SERVER["HTTP_HOST"]) &&
       ($_SERVER["HTTP_HOST"] == 'castle-engine.sourceforge.net') )
  {
/* Note: only one piwik.js should be included,
   so don't report to multiple Piwik installations.
   This Piwik code must be synched with
   ../../papers/compositing_shaders_doc/xsl/html_piwik.xsl
   ../../vrml_engine_doc/xsl/html_piwik.xsl
   ../../castle_game_engine/doc/pasdoc/footer.html
*/
?>

<!-- Piwik -->
<script type="text/javascript">
  var _paq = _paq || [];
  _paq.push(["trackPageView"]);
  _paq.push(["enableLinkTracking"]);

  (function() {
    var u=(("https:" == document.location.protocol) ? "https" : "http") + "://michalis.ii.uni.wroc.pl/piwik-castle-engine/";
    _paq.push(["setTrackerUrl", u+"piwik.php"]);
    _paq.push(["setSiteId", "1"]);
    var d=document, g=d.createElement("script"), s=d.getElementsByTagName("script")[0]; g.type="text/javascript";
    g.defer=true; g.async=true; g.src=u+"piwik.js"; s.parentNode.insertBefore(g,s);
  })();
</script>
<!-- End Piwik Code -->

<noscript>
<!-- Piwik Image Tracker -->
<img src="http://michalis.ii.uni.wroc.pl/piwik-castle-engine/piwik.php?idsite=1&amp;rec=1" style="border:0" alt="" />
<!-- End Piwik -->
</noscript>

<?php
  }
}

define('SF_UNIX_NAME', 'castle-engine'); // used only by download links.

define('WIKI_URL',            'https://sourceforge.net/p/castle-engine/wiki/');
define('MAILING_LIST_URL',    'https://lists.sourceforge.net/lists/listinfo/castle-engine-main');
define('FORUM_URL',           'https://sourceforge.net/p/castle-engine/discussion/');
define('TICKETS_TRACKER_URL', 'https://sourceforge.net/p/castle-engine/tickets/');

define('MAILING_LIST_LINK', '<a href="' . MAILING_LIST_URL . '">castle-engine-main mailing list</a>');
define('FORUM_LINK', '<a href="' . FORUM_URL . '">forum</a>');

/* Return SVN URL to appropriate path with repository trunk.
   If $prefix_command is true then also will add 'svn checkout ' text
   at the beginning. */
function sf_checkout_link($prefix_command, $path)
{
  return
    ($prefix_command ? 'svn checkout ' : '') .
    'http://svn.code.sf.net/p/castle-engine/code/trunk/' . $path;
}

/* Makes a link to a download from SourceForge file release system. */
function sf_download($title, $file_name)
{
  return '<a href="http://downloads.sourceforge.net/' . SF_UNIX_NAME .
    '/' . $file_name . '">' . $title. '</a>';
}

function download_donate_footer()
{
  return '
    <hr/>
      <div style="float: left">' . paypal_button(false) . '</div>
      <small>If you like this software, <a href="' . CURRENT_URL . 'donate.php">please consider donating</a>.</small>';
}

/* This echoes a list to download for all platforms where I compile
   my programs. Each item looks like
     < ?php echo sf_download("Foo for Linux", "foo-version-os-arch.tar.gz"); ? >
   where $prog_nice_name = Foo, $prog_archive_basename = foo.

   If $prog_version is '' then the whole -version part will be omitted
   (i.e. $prog_version = '' causes also the dash '-' before version
   to disappear, since this is what you usually want). */
function echo_standard_program_download(
  $prog_nice_name, $prog_archive_basename, $prog_version, $macosx_dmg = false)
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

  $macosx_arch = $macosx_dmg ? 'macosx' : 'macosx-i386';

  /* Hardcode $os_arch_list for now.
     It used to be configurable, but it was more trouble than it was worth. */
  $os_arch_list = array(
    'linux-i386',
    'linux-x86_64',
    'win-i386',
    $macosx_arch);

  $os_arch_caption = array(
    'linux-i386'   => ' Linux<br/>(32 bit)',
    'linux-x86_64' => ' Linux<br/>(64 bit, x86_64)',
    'win-i386'     => ' Windows<br/>(32 bit, works on 64-bit too)',
    $macosx_arch   => ' Mac OS X<br/>(32 bit)',
  );

  $os_arch_extension = array(
    'linux-i386'   => '.tar.gz',
    'linux-x86_64' => '.tar.gz',
    'win-i386'     => '.zip',
    $macosx_arch   => $macosx_dmg ? '.dmg' : '.tar.gz',
  );

  $os_arch_icon = array(
    'linux-i386'   => 'linux32',
    'linux-x86_64' => 'linux64',
    'win-i386'     => 'win',
    $macosx_arch   => 'macosx'
  );

  echo '<div class="download">';

  if (IS_GEN_LOCAL)
  {
    /* Since the download links contain so many things
       (program version, available OSes, availability on servers
       (camelot ? stoma ? sf ?)), it's safer to not put this information
       in the locally generated page (since it changes to often).
       Instead, we can place a link to the WWW page. */
    echo '<p><a href="' . CURRENT_URL . $this_page_name .
      '">Download ' . $prog_nice_name . ' from it\'s WWW page</a>.</p>';
  } else
  {
    echo '<div class="download_title">' . $nice_name_start . ':</div>
      <table><tr>';
    foreach ($os_arch_list as $os_arch)
    {
      echo '<td>';
      echo sf_download(
        '<img src="images/os_icons/' .
        /* This size should be synched with images/Makefile rule */
        $os_arch_icon[$os_arch] . '.png" width="64" height="64" alt="' .
        $os_arch_caption[$os_arch] . '"><br/>' .
        $os_arch_caption[$os_arch],
        $arch_name_start . $os_arch . $os_arch_extension[$os_arch]);
      echo '</td>' . "\n";
    }
    echo "</tr></table>\n";
  }

  echo download_donate_footer() . '
  </div>';
  /* <!-- This helps Michalis to spend more time on developing our engine, our tools and games :) Details about how you can donate are here</a>.<--> */
}

/* Return <table> with image links.

   Each $images array item is another associative array:
   - filename (name of the file, with extension, without path).
     Assumed to exist within original_size and medium_size subdirs.
   - titlealt - text used for both title and alt.
   - html - if set, the rest (except colspan) is ignored,
     and we simply put this html into cell content.
   - colspan (int) - colspan of table cell. It's your responsibility to make it
     look sensible then.
   - linktarget (string) - optional URL (full URL!) where the link leads.
     If not specified, the link leads to full image size.

   If $align non-empty, the table is floating left / right.

   $columns specifies number of columns of the table.
   We will automatically divide $images into rows with $columns images
   (the last row may be left shorter).

   The table uses absolute links (starting with CURRENT_URL).
   The important work is always done directly (without need for CSS classes),
   so $castle_force_absolute_url makes content suitable
   for inclusion also in HTML RSS feeds.
   (Maybe it will be conditional on global variable
   $castle_force_absolute_url = true in the future.
   If global variable $castle_force_absolute_url = false
   and resource exists locally (is_file_available_locally) then
   only local link will be done.)
*/
function castle_thumbs($images, $columns=1, $align='right')
{
  /* style="clear: right" is added to work nicely with Flattr images,
     that are on some pages (like castle.php) directly above this table
     and also aligned to the right. */
  $result = '<table' . ($align != '' ? ' align="'.$align.'"' : '') .
    ' style="clear: right">';

  $column_now = 0;

  foreach ($images as $image)
  {
    if ($column_now == 0) $result .= '<tr>';

    if (isset($image['colspan']))
      $colspan = ' colspan="' . (int)$image['colspan'] . '"'; else
      $colspan = '';

    $result .= '<td' . $colspan . '>';
    if (isset($image['html']))
    {
      $result .= $image['html'];
    } else
    {
      if (isset($image['linktarget']))
        $linktarget = $image['linktarget']; else
        $linktarget = CURRENT_URL . 'images/original_size/' . $image['filename'];
      $result .= '
          <a href="' . $linktarget . '"
             class="screenshot"
             title="' . $image['titlealt'] . '"><img
            align="right"
            src="' . CURRENT_URL . 'images/thumb_size/' . $image['filename'] . '"
            alt="' . $image['titlealt'] . '"
          /></a>';
    }
    $result .= '</td>';

    if (isset($image['colspan']))
      $column_now += (int)$image['colspan']; else
      $column_now++;

    if ($column_now >= $columns) { $result .= '</tr>'; $column_now = 0; }
  }

  $result .= '</table>';

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
  $result = '<a href="' . CASTLE_REFERENCE_URL . htmlspecialchars($href) . '">'
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
  // $geshi = new GeSHi($source, 'xml');
  // return $geshi->parse_code();

  $source = str_replace('&', '&amp;', $source);
  $source = str_replace('<', '&lt;', $source);
  $source = str_replace('>', '&gt;', $source);
  $source = '<pre class="xml sourcecode">' . $source . '</pre>';
  $source = preg_replace('/\\[\\[(.+)\\|(.+)\\]\\]/',
    '<a href="' . CASTLE_REFERENCE_URL . '\\1">\\2</a>', $source);
  return $source;
}

function pascal_highlight($source)
{
  global $geshi;
  $geshi->set_source($source);
  $geshi->set_language('delphi');
  return $geshi->parse_code();
}

?>
