<?php
/* PHP functions common for vrmlengine WWW pages. */

define('COUNTER_DATA_PATH', '/tmp/persistent/vrmlengine/counters/');
define('ENV_VARIABLE_NAME_LOCAL_PATH', 'VRMLENGINE_LOCAL_PATH');
define('CURRENT_URL', 'http://vrmlengine.sourceforge.net/');
define('CURRENT_URL_SHORT', 'vrmlengine.sf.net');

function echo_header_bonus ()
{
  if (!IS_GEN_LOCAL)
  {
    ?>
      <link rel="alternate" type="application/rss+xml"
        title="Kambi VRML game engine - changes log RSS feed"
        href="changes_log_feed.php">
    <?php
  }
}

function echo_footer_local_address ()
{
?>
  By Michalis Kamburelis, as part of
    <?php echo "<a href=\"" . CURRENT_URL . "\">Kambi VRML game engine</a>"; ?>.
<?php
}

function echo_footer_non_local_bonus ()
{
?>
  <p>Services for the vrmlengine project provided by<br />
  <a href="http://sourceforge.net"><img src="http://sflogo.sourceforge.net/sflogo.php?group_id=200653&amp;type=3" width="125" height="37" border="0" alt="SourceForge.net Logo" /></a><br />
  See also <a href="http://sourceforge.net/projects/vrmlengine">vrmlengine
  project page on SourceForge</a>.</p>
<?php
}

/* This set_include_path is needed on SourceForge, otherwise
   includes from within kambi-php-lib sometimes fail.
   See ../old_tests/sf_inclusion_test.php for details. */
set_include_path('.:kambi-php-lib/');

require_once 'kambi-php-lib/kambi_common.php';

require_once 'generated_versions.php';

define('SF_UNIX_NAME', 'vrmlengine');

define('MAILING_LIST_URL',
  'https://lists.sourceforge.net/lists/listinfo/vrmlengine-main');

define('MAILING_LIST_LINK',
  '<a href="' . MAILING_LIST_URL . '">vrmlengine-main mailing list</a>');

define('BUGS_TRACKER_URL',             'http://sourceforge.net/tracker/?group_id=200653&amp;atid=974391');
define('FEATURE_REQUESTS_TRACKER_URL', 'http://sourceforge.net/tracker/?group_id=200653&amp;atid=974394');
define('PATCHES_TRACKER_URL',          'http://sourceforge.net/tracker/?group_id=200653&amp;atid=974393');

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

/* This echoes an <ul> list with items for all platforms where I compile
   my programs. Each item looks like
     <? php echo sf_download("Foo for Linux", "foo-version-os-arch.tar.gz"); ? >
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
?>
