<?php
/* PHP functions common for vrmlengine WWW pages. */

define('COUNTER_DATA_PATH', '/home/groups/v/vr/vrmlengine/persistent/');
define('ENV_VARIABLE_NAME_LOCAL_PATH', 'VRMLENGINE_HTDOCS_LOCAL_PATH');
define('CURRENT_URL', 'http://vrmlengine.sourceforge.net/');
define('CURRENT_URL_SHORT', 'vrmlengine.sf.net');

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
  if ((!IS_GEN_LOCAL) && ($_SERVER["HTTP_HOST"] == 'vrmlengine.sourceforge.net'))
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

/* This set_include_path is needed on SourceForge, otherwise
   includes from within kambi-php-lib sometimes fail.
   See ../old_tests/sf_inclusion_test.php for details. */
set_include_path('.:kambi-php-lib/');

require_once 'kambi-php-lib/kambi_common.php';

require_once 'generated_versions.php';

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
    echo "<ul>\n";
    foreach ($os_arch_list as $os_arch)
    {
      echo '<li>' . sf_download(
        $nice_name_start . $os_arch_caption[$os_arch],
        $arch_name_start . $os_arch . $os_arch_extension[$os_arch]) . "</li>\n";
    }
    echo "</ul>\n";
  }
}

/* Return <table> with image links.

   Each $images array item is another associative array:
   - filename (name of the file, with extension, without path).
     Assumed to exist within original_size and medium_size subdirs.
   - titlealt - text used for both title and alt.

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

    $result .= '
      <td>
        <a href="' . CURRENT_URL . 'images/progs_demo/original_size/' . $image['filename'] . '" class="screenshot">
          <img align="right" src="' . CURRENT_URL . 'images/progs_demo/medium_size/' . $image['filename'] . '"
          alt="' . $image['titlealt'] . '"
          title="' . $image['titlealt'] . '"
        /></a>
      </td>';

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
