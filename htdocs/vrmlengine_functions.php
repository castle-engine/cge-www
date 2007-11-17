<?php
/* PHP functions common for vrmlengine WWW pages. */

define('COUNTER_DATA_PATH', '/tmp/persistent/vrmlengine/counters/');
define('ENV_VARIABLE_NAME_LOCAL_PATH', 'VRMLENGINE_LOCAL_PATH');
define('CURRENT_URL', 'http://vrmlengine.sourceforge.net/');
define('CURRENT_URL_SHORT', 'vrmlengine.sf.net');

function echo_header_bonus ()
{
?>
  <link rel="alternate" type="application/rss+xml"
    title="Kambi VRML game engine - changes log RSS feed"
    href="changes_log_feed.php">
<?php
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

/* This echoes an <ul> list with items for all platforms where I compile
   my programs. Each item looks like
     <? php echo sf_download("Foo for Windows", "foo-version-win.zip"); ? >
   where $prog_nice_name = Foo, $prog_archive_basename = foo.

   If $prog_version is '' then the whole -version part will be omitted
   (i.e. $prog_version = '' causes also the dash '-' before version
   to disappear, since this is what you usually want).

   $arch_suffix = false, $freebsd_version = true by default because this
   was the standard. In the future, $arch_suffix should become always true,
   and instead of $freebsd_version maybe some list of os-arch will be made. */
function echo_standard_program_download(
  $prog_nice_name, $prog_archive_basename, $prog_version,
  $arch_suffix = false, $freebsd_version = true)
{
  global $page_lang, $this_page_name;

  switch ($page_lang)
  {
    case LANG_PL:
      $for_linux_str   = ' dla Linuxa (i386)';
      $for_win_str     = ' dla Windowsa (i386)';
      $for_freebsd_str = ' dla FreeBSD (i386)';
      $for_macosx_str  = ' dla Mac OS X (i386)';
      break;
    case LANG_EN:
      $for_linux_str   = ' for Linux (i386)';
      $for_win_str     = ' for Windows (i386)';
      $for_freebsd_str = ' for FreeBSD (i386)';
      $for_macosx_str  = ' for Mac OS X (i386)';
      break;
  }

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
    if ($arch_suffix)
      $s_arch_suffix = '-i386'; else
      $s_arch_suffix = '';

    echo '<ul>
      <li>' . sf_download($nice_name_start . $for_linux_str  , $arch_name_start . "linux${s_arch_suffix}.tar.gz"  ) .
      ($freebsd_version ?
     '<li>' . sf_download($nice_name_start . $for_freebsd_str, $arch_name_start . "freebsd${s_arch_suffix}.tar.gz") : '') . '
      <li>' . sf_download($nice_name_start . $for_macosx_str , $arch_name_start . "macosx${s_arch_suffix}.tar.gz" ) . '
      <li>' . sf_download($nice_name_start . $for_win_str    , $arch_name_start . "win${s_arch_suffix}.zip"       ) . '
    </ul>';
  }
}
?>
