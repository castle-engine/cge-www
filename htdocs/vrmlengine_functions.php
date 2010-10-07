<?php
/* PHP functions common for vrmlengine WWW pages. */

/* This set_include_path is needed on SourceForge, otherwise
   includes from within kambi-php-lib sometimes fail.
   See ../old_tests/sf_inclusion_test.php for details. */
set_include_path('.:kambi-php-lib/');
require_once 'kambi-php-lib/kambi_common.php';
require_once 'generated_versions.php';

define('COUNTER_DATA_PATH', '/home/groups/v/vr/vrmlengine/persistent/');
define('ENV_VARIABLE_NAME_LOCAL_PATH', 'VRMLENGINE_HTDOCS_LOCAL_PATH');
define('CURRENT_URL', 'http://vrmlengine.sourceforge.net/');
define('CURRENT_URL_SHORT', 'vrmlengine.sf.net');
define('KAMBI_NO_HOME_LINK', true);

define('S_INSTALLATION_INSTRUCTIONS_SHORT',
  'No installation is required. Just download and unpack these archives wherever
  you want, and run the program inside. The documentation
  (this web page) is also included inside
  (look in the <tt>documentation/</tt> subdirectory) for offline viewing.');

global $vrmlengine_sitemap;
$vrmlengine_sitemap = array(
  MAIN_PAGE_BASENAME       => array('title' => 'Intro and News'),
  'view3dscene'            => array('hint' => 'VRML / X3D browser, and a viewer for other 3D model formats', 'title' => 'view3dscene'),
  'castle'                 => array('hint' => 'First-person perspective game, in a dark fantasy setting'   , 'title' => 'The Castle'),
  'all_programs'           => array('hint' => 'All the games and tools using our 3D engine'                , 'title' => 'All Programs'),
  'support'                => array('hint' => 'Ask for help, report bugs, discuss features'                , 'title' => 'Support'),

  'kambi_vrml_game_engine' => array('hint' => 'Sources and documentation for developers'                   , 'title' => 'Engine overview for developers', 'title-for-header-menu' => 'Engine',
    'sub' => array(
      'reference' => array('title' => 'Reference'),
      'vrml_engine_doc' => array('title' => 'General documentation'),
    ),
  ),

  'vrml_x3d'               => array('hint' => 'Our extensions and status of VRML/X3D implementation'       , 'title' => 'VRML / X3D support', 'title-for-header-menu' => 'VRML/X3D' /* shorter title */,
    'sub' => array(
      'kambi_vrml_extensions' => array('title' => 'Extensions'),
      'kambi_vrml_test_suite' => array('title' => 'Test suite'),
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
          'vrml_implementation_nurbs'                => array('title' => 'NURBS'                           ),
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

  'other'                  => array('hint' => 'Blender VRML, Wiki, Other documentation pages'              , 'title' => 'Other'),
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

function echo_header_bonus ()
{
  ?>

<link rel="alternate" type="application/rss+xml"
  title="Kambi VRML game engine - changes log RSS feed"
  href="<?php echo CURRENT_URL; ?>changes_log_feed.php">

<link type="text/css" rel="stylesheet" media="all"  href="vrmlengine.css">

<script type="text/javascript" src="vrmlengine.js"></script>

  <?php
}

function vrmlengine_header($a_page_title, $meta_description = NULL, $path = NULL)
{
  common_header($a_page_title, LANG_EN, $meta_description);

  global $vrmlengine_sidebar;
  global $vrmlengine_sitemap;

  if ($path !== NULL)
  {
    /* TODO: for now, $path can only have 1 item,
       and always indicates sidebar */
    $vrmlengine_sidebar = _vrmlengine_sidebar($path[0], $vrmlengine_sitemap[$path[0]]);
  }

  $menu_for_users = 5 * 2 + 1;
  $menu_for_developers = 2 * count($vrmlengine_sitemap) + 1 - $menu_for_users;

  $rendered = '
  <div class="header">
    <img class="header_icon" src="images/header_icon.png" alt="Kambi VRML game engine icon" />
    <div class="header_title"><a href="'.en_page_url(MAIN_PAGE_BASENAME).'">Kambi VRML game engine</a></div>
    <table class="header_menu">
      <tr>
        <td colspan="' . $menu_for_users . '" class="higher higher_left">&larr; Users</td>
        <td colspan="' . $menu_for_developers . '" class="higher higher_right">Developers &rarr;</td>
      </tr>
      <tr><td class="lower_separator"></td>';

  global $page_basename;
  foreach($vrmlengine_sitemap as $menu_item_page => $menu_item)
  {
    $rendered .= '<td class="lower"><a href="'.en_page_url($menu_item_page).'"';
    if (isset($menu_item['hint']))
      $rendered .= ' title="' . $menu_item['hint'] . '"';
    if ($page_basename == $menu_item_page)
      $rendered .= ' id="current"';
    if (isset($menu_item['title-for-header-menu']))
      $title = $menu_item['title-for-header-menu']; else
      $title = $menu_item['title'];
    $rendered .= '>' . $title . '</a></td><td class="lower_separator"></td>';
  }
  unset($menu_item);
  unset($menu_item_page);

  $rendered .= '
      </tr>
    </table>
  </div>

  <div class="header_breadcrumbs">
    <a href="index.php">Home</a>
    &#187;
    <a href="vrml_implementation_status.php">VRML / X3D implementation status</a>
  </div>';

  if (empty($vrmlengine_sidebar))
    $rendered .= '<div class="content">'; else
    $rendered .= '<table class="layout" cellspacing="0">
      <col class="content_column">
      <col class="sidebar_column">
      <tr><td class="layout content">';

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
    <?php echo "<a href=\"" . CURRENT_URL . "\">Kambi VRML game engine</a>"; ?>.
    </address>
    <?php
  }

  ?>

  <table><tr>

    <td><a href="http://sourceforge.net/projects/vrmlengine"><img src="http://sflogo.sourceforge.net/sflogo.php?group_id=200653&amp;type=11" width="120" height="30" border="0" alt="Get Kambi VRML game engine at SourceForge.net. Fast, secure and Free Open Source software downloads" /></a></td>

    <td><a href="http://sourceforge.net/donate/index.php?group_id=200653"><img src="http://images.sourceforge.net/images/project-support.jpg" width="88" height="32" border="0" alt="Support This Project" /> </a></td>

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
?>

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
    ($prefix_command ? 'svn&nbsp;checkout&nbsp;' : '') .
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
      echo '<li>' . sf_download($os_arch_caption[$os_arch],
        $arch_name_start . $os_arch . $os_arch_extension[$os_arch]) . "</li>\n";
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

   If $align non-empty, the table is floating left / right.

   $columns specifies number of columns of the table.
   We will automatically divide $images into rows with $columns images
   (the last row may be left shorter).

   Table uses absolute links (starting with CURRENT_URL),
   and important work is done directly (without need for CSS classes),
   so it's suitable for inclusion also in HTML RSS feeds.
*/
function table_demo_images($images, $columns=1, $align='right')
{
  $result = '<table' . ($align != '' ? ' align="'.$align.'"' : '') . '>';

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
      $result .= '
          <a href="' . CURRENT_URL . 'images/progs_demo/original_size/' . $image['filename'] . '" class="screenshot">
            <img align="right" src="' . CURRENT_URL . 'images/progs_demo/medium_size/' . $image['filename'] . '"
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

/* $image_name = nazwa obrazka istniej±ca w progs_demo/original_size/ i
   progs_demo/medium_size/.
   Zwróci odpowiednie tagi które zawieraj± <img...> do obrazka w medium_size
   i s± zawarte w <a href...> do obrazka w original_size.
   Jezeli $aligned to obrazek bêdzie dosuniêty do prawej, tzn. mieæ align="right".
   $prog_name zostanie u¿yte aby wygenerowaæ odpowiedni tekst dla atrybutu
   alt= obrazka, to bêdzie co¶ w rodzaju "Obrazek z &quot;$prog_name&quot;",
   odpowiednio przet³umaczone na $page_lang.

   Mo¿e zwróciæ '' je¶li obrazki nie s± dostêpne lokalnie. Mówi±c bardziej
   ogólnie, mo¿esz u¿ywaæ tej funkcji bez wzglêdu na warto¶æ IS_GEN_LOCAL -
   ona sobie poradzi tak czy tak. */
function medium_image_progs_demo($image_name, $prog_name, $aligned = true)
{
  global $page_lang;
  switch ($page_lang)
  {
    case LANG_PL: $alt = "Obrazek z &quot;$prog_name&quot;"; break;
    case LANG_EN: $alt = "Image from &quot;$prog_name&quot;"; break;
  }

  return medium_image_progs_demo_core($image_name, $alt, '', ($aligned ? 'right' : ''));
}

function medium_image_progs_demo_core($image_name, $alt, $title = '$alt',
  $align = '', $online_if_not_available = false)
{
  $image_name_original_size = "images/progs_demo/original_size/$image_name";
  $image_name_medium_size   = "images/progs_demo/medium_size/$image_name";

  if (!is_file_available_locally($image_name_original_size) ||
      !is_file_available_locally($image_name_medium_size))
  {
    if ($online_if_not_available) {
      /* Then image links will be done as normal, except will use full URL
         always pointing to online version. */
      $image_name_original_size = CURRENT_URL . $image_name_original_size;
      $image_name_medium_size   = CURRENT_URL . $image_name_medium_size  ;
    } else
    {
      return '';
    }
  }

  if ($title === '$alt')
    $title = $alt;

  return
   "<a href=\"$image_name_original_size\" class=\"screenshot\">
      <img "
       . ($align != '' ? "align=\"$align\"" : '') .
       " src=\"$image_name_medium_size\" alt=\"$alt\" "
       . ($title != '' ? "title=\"$title\"" : '') .
       "/>
    </a>";
}

/* To samo co medium_image_progs_demo tyle ¿e tutaj $image_name jest generowany
   automatycznie jako "$prog_name_screen_demo.png" */
function default_medium_image_progs_demo($prog_name)
{
  return medium_image_progs_demo("${prog_name}_screen_demo.png", $prog_name);
}
?>
