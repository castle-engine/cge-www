<?php
/* PHP functions common for vrmlengine WWW pages. */

define('SF_UNIX_NAME', 'vrmlengine');

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
   to disappear, since this is what you usually want). */
function echo_standard_program_download(
  $prog_nice_name, $prog_archive_basename, $prog_version)
{
  global $page_lang, $this_page_name;

  switch ($page_lang)
  {
    case LANG_PL:
      $for_linux_str   = ' dla Linuxa';
      $for_win_str     = ' dla Windowsa';
      $for_freebsd_str = ' dla FreeBSD';
      $for_macosx_str  = ' dla Mac OS X';
      break;
    case LANG_EN:
      $for_linux_str   = ' for Linux';
      $for_win_str     = ' for Windows';
      $for_freebsd_str = ' for FreeBSD';
      $for_macosx_str  = ' for Mac OS X';
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
    echo '<ul>
      <li>' . sf_download($nice_name_start . $for_linux_str  , $arch_name_start . "linux.tar.gz"  ) . '
      <li>' . sf_download($nice_name_start . $for_freebsd_str, $arch_name_start . "freebsd.tar.gz") . '
      <li>' . sf_download($nice_name_start . $for_macosx_str , $arch_name_start . "macosx.tar.gz" ) . '
      <li>' . sf_download($nice_name_start . $for_win_str    , $arch_name_start . "win.zip"       ) . '
    </ul>';
  }
}
?>
