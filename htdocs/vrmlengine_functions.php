<?php
/* PHP functions common for vrmlengine WWW pages. */

/* Constants that should be defined before including kambi_common.php */
define('COUNTER_DATA_PATH', '/home/project-web/vrmlengine/persistent/');
define('ENV_VARIABLE_NAME_LOCAL_PATH', 'CASTLE_ENGINE_HTDOCS_LOCAL_PATH');
define('CURRENT_URL', 'http://vrmlengine.sourceforge.net/');
define('CURRENT_URL_SHORT', 'vrmlengine.sf.net');
define('KAMBI_NO_HOME_LINK', true);

// before making 1st release under "castle game engine" name:
define('ENGINE_NAME', 'Kambi VRML Game Engine'); // 'Castle Game Engine');
define('ENGINE_DIR_NAME', 'kambi_vrml_game_engine'); // 'castle_game_engine');

/* This set_include_path is needed on SourceForge, otherwise
   includes from within kambi-php-lib sometimes fail. */
set_include_path('.:kambi-php-lib/');
require_once 'kambi-php-lib/kambi_common.php';
require_once 'generated_versions.php';

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
*/
global $vrmlengine_sitemap;
$vrmlengine_sitemap = array(
  MAIN_PAGE_BASENAME       => array('title' => 'News' /* for sidebar on news.php */, 'title-for-header-menu' => 'Intro and News'
    /* 'sub' and 'sidebar' of this will be calculated by news.php page,
       since it may be time-consuming (requires reading large $news table). */
  ),

  'view3dscene'            => array('hint' => 'VRML / X3D browser, and a viewer for other 3D model formats', 'title' => 'view3dscene'),

  'castle'                 => array('hint' => 'First-person perspective game, in a dark fantasy setting'   , 'title' => 'The Castle',
    'sidebar' => true,
    'sub' => array(
      'castle-advanced'    => array('title' => 'Additional notes (troubleshooting)'),
      'castle-development' => array('title' => 'Development'),
      'castle-credits'     => array('title' => 'Credits'),
    ),
  ),

  'all_programs' => array('hint' => 'All the games and tools using our 3D engine', 'title' => 'All Programs',
    'sub' => array(
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
    ),
  ),

  'forum' => array('hint' => 'Ask for help, report bugs, discuss features', 'title' => 'Forum'),

  'donate' => array('title' => 'Donate'),

  'engine' => array('hint' => 'Sources and documentation for developers', 'title' => 'Engine overview for developers', 'title-for-header-menu' => 'Engine',
    'sidebar' => true,
    'sub' => array(
      'reference' => array('title' => 'Reference'),
      'vrml_engine_doc' => array('title' => 'Documentation'),
      'movies' => array('title' => 'Demo movies'),
    ),
  ),

  'vrml_x3d' => array('hint' => 'Our extensions and status of VRML/X3D implementation', 'title' => 'VRML / X3D support', 'title-for-header-menu' => 'VRML/X3D' /* shorter title */,
    'sidebar' => true,
    'sub' => array(
      'demo_models' => array('title' => 'Demo models'),
      'kambi_vrml_extensions' => array('title' => 'Extensions',
        'sub' => array(
          'compositing_shaders' => array('title' => 'Compositing Shaders'),
          'kambi_vrml_extensions_screen_effects' => array('title' => 'Screen Effects'),
          'kambi_vrml_extensions_shadow_maps' => array('title' => 'Shadow Maps'),
          'kambi_vrml_extensions_vrml1' => array('title' => '(Old) VRML 1.0'),
        ),
      ),
      'vrml_implementation_status' => array('title' => 'Implementation status',
        'sub' => array(
          'vrml_implementation_core'                 => array('title' => 'Core'                            ),
          'vrml_implementation_time'                 => array('title' => 'Time'                            ),
          'vrml_implementation_networking'           => array('title' => 'Networking'                      ),
          'vrml_implementation_grouping'             => array('title' => 'Grouping'                        ),
          'vrml_implementation_rendering'            => array('title' => 'Rendering'                       ),
          'vrml_implementation_shape'                => array('title' => 'Shape'                           ),
          'vrml_implementation_geometry3d'           => array('title' => 'Geometry3D'                      ),
          'vrml_implementation_geometry2d'           => array('title' => 'Geometry2D'                      ),
          'vrml_implementation_text'                 => array('title' => 'Text'                            ),
          'vrml_implementation_sound'                => array('title' => 'Sound'                           ),
          'vrml_implementation_lighting'             => array('title' => 'Lighting'                        ),
          'vrml_implementation_texturing'            => array('title' => 'Texturing',
            'sub' => array(
              'vrml_implementation_texturing#section_multi_texturing' => array('title' => 'Clarifications to X3D multi-texturing'),
              'vrml_implementation_texturing#section_dds'             => array('title' => 'DDS (DirectDraw Surface)'),
            ),
          ),
          'vrml_implementation_interpolation'        => array('title' => 'Interpolation'                   ),
          'vrml_implementation_pointingdevicesensor' => array('title' => 'Pointing device sensor'          ),
          'vrml_implementation_keydevicesensor'      => array('title' => 'Key device sensor'               ),
          'vrml_implementation_environmentalsensor'  => array('title' => 'Environmental sensor'            ),
          'vrml_implementation_navigation'           => array('title' => 'Navigation'                      ),
          'vrml_implementation_environmentaleffects' => array('title' => 'Environmental effects'           ),
          'vrml_implementation_hanim'                => array('title' => 'H-Anim'                          ),
          'vrml_implementation_nurbs'                => array('title' => 'NURBS',
            'sub' => array(
              'vrml_implementation_nurbs#section_homogeneous_coordinates' => array('title' => 'Control points are in homogeneous coordinates'),
            ),
          ),
          'vrml_implementation_scripting'            => array('title' => 'Scripting'                       ),
          'vrml_implementation_eventutilities'       => array('title' => 'Event utilities'                 ),
          'vrml_implementation_shaders'              => array('title' => 'Programmable shaders'            ),
          'vrml_implementation_cadgeometry'          => array('title' => 'CAD geometry'                    ),
          'vrml_implementation_texturing3d'          => array('title' => 'Texturing3D'                     ),
          'vrml_implementation_cubemaptexturing'     => array('title' => 'Cube map environmental texturing'),
        )
      ),
      'nist_vrml_test_suite' => array('title' => 'NIST conformace test suite'),
      'kambi_script' => array('title' => 'KambiScript language reference'),
      'kanim_format' => array('title' => 'Kanim (precalculated animations) file format'),
      'vrml_time_origin_considered_uncomfortable' => array('title' => 'VRML / X3D time origin considered uncomfortable'),
    ),
  ),

  'blender' => array('title' => 'Blender X3D exporter', 'hint' => 'Customized Blender X3D exporter', 'title-for-header-menu' => 'Blender'),
);

/* Internal for _vrmlengine_sidebar* usage.

   Return a formatted link to given page.
   $page is the page basename (like for a_href_page), or a basename(hash)anchor.
   $pageinfo must contain 'title'.

   Looks at global $page_basename, to avoid turning current page name
   into a link. */
function _vrmlengine_sidebar_link($page, $pageinfo)
{
  $pagelink = explode('#', $page);
  if (count($pagelink) == 1)
  {
    global $page_basename;
    /* If it's the current page, don't make a link to it */
    if ($pagelink[0] == $page_basename)
      return $pageinfo['title']; else
      return a_href_page($pageinfo['title'], $pagelink[0]);
  } else
  if (count($pagelink) == 2)
    return a_href_page_hashlink($pageinfo['title'], $pagelink[0], $pagelink[1]); else
    return '<b>Invalid sidebar link ' . htmlspecialchars($pagelink) . '</b>';
}

/* Internal for _vrmlengine_sidebar* usage.
   Return a <ul> listing items on $sub. */
function _vrmlengine_sidebar_menu($sub)
{
  $result = '<ul>';
  foreach($sub as $page => $pageinfo)
  {
    $result .= '<li>' . _vrmlengine_sidebar_link($page, $pageinfo);
    if (isset($pageinfo['sub']))
      $result .= _vrmlengine_sidebar_menu($pageinfo['sub']);
    $result .= '</li>';
  }
  $result .= '</ul>';
  return $result;
}

/* Return a rendered HTML sidebar. */
function _vrmlengine_sidebar($page, $pageinfo)
{
  $result = '
  <div class="sidebar">
    <div class="sidebar_title">' . _vrmlengine_sidebar_link($page, $pageinfo) . '</div>';

  if (isset($pageinfo['sub']))
    $result .= _vrmlengine_sidebar_menu($pageinfo['sub']);

  $result .= '</div>';

  return $result;
}

function _vrmlengine_header_menu($current_page)
{
  global $vrmlengine_sitemap;

  $menu_for_users = 6 * 2 + 1;
  $menu_for_developers = 2 * count($vrmlengine_sitemap) + 1 - $menu_for_users;

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

  foreach($vrmlengine_sitemap as $menu_item_page => $menu_item)
  {
    $result .= '<td class="lower"><a href="'.en_page_url($menu_item_page).'"';
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

function _vrmlengine_breadcrumbs($path)
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
    global $vrmlengine_sitemap;

    $result = '<div class="header_breadcrumbs">' .
      a_href_page('Home', MAIN_PAGE_BASENAME);

    $path_item_num = 0;
    $path_item = '';
    $path_itemsub = $vrmlengine_sitemap;

    while ($path_item_num < count($path) - 1)
    {
      $path_item = $path[$path_item_num];
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
  ?>

<link rel="alternate" type="application/rss+xml"
  title="Castle Game Engine - News Feed"
  href="<?php echo CURRENT_URL; ?>news_feed.php">

<link type="text/css" rel="stylesheet" media="all"  href="castle-engine.css">

<script type="text/javascript" src="vrmlengine.js"></script>

<script type="text/javascript">
/* <![CDATA[ */
    (function() {
        var s = document.createElement('script'), t = document.getElementsByTagName('script')[0];

        s.type = 'text/javascript';
        s.async = true;
        s.src = 'http://api.flattr.com/js/0.6/load.js?mode=auto';

        t.parentNode.insertBefore(s, t);
    })();
/* ]]> */
</script>

<!--script type="text/javascript" src="https://apis.google.com/js/plusone.js"></script-->

  <?php
}

/* $path is a list of page names, a path in the tree of $vrmlengine_sitemap,
   to the current page. The $page_basename is added at the end,
   if not already there. */
function vrmlengine_header($a_page_title, $meta_description = NULL, $path = array())
{
  common_header($a_page_title, LANG_EN, $meta_description);

  global $vrmlengine_sidebar;
  global $vrmlengine_sitemap;
  global $page_basename;

  /* make sure $path ends with $page_basename.
     This way, we also make sure $path is never empty. */
  if (count($path) == 0 || $path[count($path) - 1] != $page_basename)
    $path[] = $page_basename;

  /* traverse $vrmlengine_sitemap, along the $path.
     Find which items should be used for a sidebar, if any. */
  $sidebarroot_num = -1;
  $sidebarroot_page = NULL;
  $sidebarroot_info = NULL;
  $sidebarroot_sidebar = false;
  $sidebarroot_sub = $vrmlengine_sitemap;
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
    $vrmlengine_sidebar = _vrmlengine_sidebar($sidebarroot_page, $sidebarroot_info); else
    $vrmlengine_sidebar = '';

  $rendered = '
  <div class="header">
    <!--img class="header_icon" src="images/header_icon.png" alt="Castle Game Engine icon" /-->
    <div class="header_title"><a href="'.en_page_url(MAIN_PAGE_BASENAME).'">Castle Game Engine</a></div>
    ' . _vrmlengine_header_menu($path[0]) . '
  </div>';

  if (empty($vrmlengine_sidebar))
    $rendered .=  _vrmlengine_breadcrumbs($path) . '<div class="content">'; else
    $rendered .= '<table class="layout" cellspacing="0">
      <col class="content_column">
      <col class="sidebar_column">
      <tr><td class="layout content">' . _vrmlengine_breadcrumbs($path);

  echo $rendered;
}

function vrmlengine_footer()
{
  global $vrmlengine_sidebar;

  if (empty($vrmlengine_sidebar))
    echo '</div>'; else
    echo '</td><td class="layout">' .$vrmlengine_sidebar. '</td></tr></table>';

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

  <table><tr>

    <td><a href="http://sourceforge.net/projects/castle-engine">Hosted by SourceForge.net</a></td>

    <td><?php echo PAGE_COPYRIGHT; ?></td>

    <?php if (!IS_GEN_LOCAL) { ?>
      <td> <a href="http://validator.w3.org/check/referer"> <img border="0" src="images/valid-html401.png" alt="Valid HTML 4.01!" height="31" width="88"></a> <?php // style="padding-bottom: 3%" ?> </td>
    <?php } ?>

  </tr></table>

  <?php

  /* Insert piwik code */
  if ( (!IS_GEN_LOCAL) &&
       isset($_SERVER["HTTP_HOST"]) &&
       ($_SERVER["HTTP_HOST"] == 'vrmlengine.sourceforge.net') )
  {
/* Tracking code for old piwik as SF hosted app.
   This piwik is broken, see
   https://sourceforge.net/apps/trac/sourceforge/ticket/17978
   (and many dups, like https://sourceforge.net/apps/trac/sourceforge/ticket/18121 ).
   So I use my local piwik installation.
   Only one piwik.js should be included, so this one is just commented out.

<!-- Piwik -->
<script type="text/javascript">
var pkBaseURL = (("https:" == document.location.protocol) ? "https://apps.sourceforge.net/piwik/vrmlengine/" : "http://apps.sourceforge.net/piwik/vrmlengine/");
document.write(unescape("%3Cscript src='" + pkBaseURL + "piwik.js' type='text/javascript'%3E%3C/script%3E"));
</script><script type="text/javascript">
piwik_action_name = '';
piwik_idsite = 1;
piwik_url = pkBaseURL + "piwik.php";
piwik_log(piwik_action_name, piwik_idsite, piwik_url);
</script>
<object><noscript><p><img src="http://apps.sourceforge.net/piwik/vrmlengine/piwik.php?idsite=1" alt="piwik"/></p></noscript></object>
<!-- End Piwik Tag -->

*/
?>

<!-- Piwik -->
<script type="text/javascript">
var pkBaseURL = (("https:" == document.location.protocol) ? "https://vrmlengine.sourceforge.net/piwik/" : "http://vrmlengine.sourceforge.net/piwik/");
document.write(unescape("%3Cscript src='" + pkBaseURL + "piwik.js' type='text/javascript'%3E%3C/script%3E"));
</script><script type="text/javascript">
try {
var piwikTracker = Piwik.getTracker(pkBaseURL + "piwik.php", 1);
piwikTracker.trackPageView();
piwikTracker.enableLinkTracking();
} catch( err ) {}
</script><noscript><p><img src="http://vrmlengine.sourceforge.net/piwik/piwik.php?idsite=1" style="border:0" alt="" /></p></noscript>
<!-- End Piwik Tracking Code -->

<?php
  }
}

define('SF_UNIX_NAME', 'vrmlengine');

define('MAILING_LIST_URL', 'https://lists.sourceforge.net/lists/listinfo/vrmlengine-main');
define('FORUM_URL', 'http://apps.sourceforge.net/phpbb/vrmlengine/viewforum.php?f=3');
define('BUGS_TRACKER_URL',             'http://sourceforge.net/tracker/?group_id=200653&amp;atid=974391');
define('FEATURE_REQUESTS_TRACKER_URL', 'http://sourceforge.net/tracker/?group_id=200653&amp;atid=974394');
define('PATCHES_TRACKER_URL',          'http://sourceforge.net/tracker/?group_id=200653&amp;atid=974393');

define('MAILING_LIST_LINK', '<a href="' . MAILING_LIST_URL . '">vrmlengine-main mailing list</a>');
define('FORUM_LINK', '<a href="' . FORUM_URL . '">forum</a>');

/* Return SVN URL to appropriate vrmlengine subproject repository place.
   If $prefix_command is true then also will add 'svn&nbsp;checkout&nbsp;' line
   at the beginning. */
function sf_checkout_link($prefix_command, $vrmlengine_subproject)
{
  return
    ($prefix_command ? 'svn checkout ' : '') .
    'https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/' .
    $vrmlengine_subproject;
}

/* Makes a link to a download from SourceForge file release system. */
function sf_download($title, $file_name)
{
  return '<a href="http://downloads.sourceforge.net/' . SF_UNIX_NAME .
    '/' . $file_name . '">' . $title. '</a>';
}

$os_arch_caption = array(
  'linux-i386'   => ' for Linux (i386)',
  'linux-x86_64' => ' for Linux (x86_64)',
  'win-i386'     => ' for Windows (i386)',
  'freebsd-i386' => ' for FreeBSD (i386)',
  'macosx-i386'  => ' for Mac OS X (i386)',
  /* TODO: these are only temporary os_arch names, without i386 suffix.
     That's because when my engine was i386 only, I didn't add i386 suffix.
     Until every program is re-released with i386 suffix, old programs
     will use this. */
  'linux'        => ' for Linux (i386)',
  'win'          => ' for Windows (i386)',
  'freebsd'      => ' for FreeBSD (i386)',
  'macosx'       => ' for Mac OS X (i386)',
);

$os_arch_extension = array(
  'linux-i386'   => '.tar.gz',
  'linux-x86_64' => '.tar.gz',
  'win-i386'     => '.zip',
  'freebsd-i386' => '.tar.gz',
  'macosx-i386'  => '.tar.gz',
  'linux'        => '.tar.gz',
  'win'          => '.zip',
  'freebsd'      => '.tar.gz',
  'macosx'       => '.tar.gz',
);

$std_releases_pre_1_2_0 = array('linux', 'win', 'freebsd', 'macosx');
$std_releases_post_1_2_0 = array('linux-i386', 'win-i386', 'macosx-i386');
$std_releases_post_1_8_0 = array('linux-i386', 'linux-x86_64', 'win-i386', 'macosx-i386');

/* This echoes an <ul> list with items for all platforms where I compile
   my programs. Each item looks like
     < ?php echo sf_download("Foo for Linux", "foo-version-os-arch.tar.gz"); ? >
   where $prog_nice_name = Foo, $prog_archive_basename = foo.

   If $prog_version is '' then the whole -version part will be omitted
   (i.e. $prog_version = '' causes also the dash '-' before version
   to disappear, since this is what you usually want).

   $os_arch_list is a list of os_arch for which this program was released.
   They have to be a suffix of the released filenames. They also have to
   be an entries to $os_arch_caption and similar arrays. */
function echo_standard_program_download(
  $prog_nice_name, $prog_archive_basename, $prog_version,
  $os_arch_list)
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
  /*
    Still unsure if this looks good:

    echo '<table align="right"><tr><td>
        <a href="http://sourceforge.net/donate/index.php?group_id=200653"><img src="http://images.sourceforge.net/images/project-support.jpg" width="88" height="32" border="0" alt="Support This Project" /> </a>
      </td></tr></table>';
  */
    echo '<div class="download_title">' . $nice_name_start . ':</div>';
    echo '<ul>' . "\n";
    foreach ($os_arch_list as $os_arch)
    {
      echo '<li>';
      if ($os_arch == 'stub-macosx-later')
        echo '<i>Mac OS X release will follow later.</i>'; else
        /*  BTW, <a href="http://vrmlengine.sourceforge.net/macosx_requirements.php#section_help_wanted">programmers who want to help make better Mac OS X releases are wanted</a>. */
        echo sf_download($os_arch_caption[$os_arch],
          $arch_name_start . $os_arch . $os_arch_extension[$os_arch]);
      echo '</li>' . "\n";
    }
    echo "</ul>\n";
  }

  echo '</div>';
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
   so $vrmlengine_force_absolute_url makes content suitable
   for inclusion also in HTML RSS feeds.
   (Maybe it will be conditional on global variable
   $vrmlengine_force_absolute_url = true in the future.
   If global variable $vrmlengine_force_absolute_url = false
   and resource exists locally (is_file_available_locally) then
   only local link will be done.)
*/
function vrmlengine_thumbs($images, $columns=1, $align='right')
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
          <a href="' . $linktarget . '" class="screenshot">
            <img align="right" src="' . CURRENT_URL . 'images/thumb_size/' . $image['filename'] . '"
            alt="' . $image['titlealt'] . '"
            title="' . $image['titlealt'] . '"
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
  return vrmlengine_thumbs(array(
    array('filename' => $prog_name . '_screen_demo.png', 'titlealt' => 'Image from &quot;' . $prog_name . '&quot;'),
  ));
}
?>
