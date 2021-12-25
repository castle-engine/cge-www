<?php
require_once 'castle_engine_functions.php';
castle_header('All Programs Sources');
echo pretty_heading($page_title);

$toc = new TableOfContents(
  array(
    new TocItem('Introduction', 'intro'),
    new TocItem('Download', 'download_src'),
    // new TocItem("Getting latest version from GitHub", 'git'),
    // new TocItem("Getting latest version from SVN", 'svn'),
  )
);
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>Sources for all final programs related to our engine may be
downloaded below.</p>

<ol>
  <li><p><b>Download</b> the sources that interest you,
    and additionally download
    <?php echo a_href_page('Castle Game Engine', 'index'); ?> sources.
    Unpack them anywhere you like.</li>

  <li><b>Compile</b> the program:

    <ul>
      <li>
        <p>You can <b>compile using Lazarus</b>.</p>
        <ol>
          <li>Install <a href="http://lazarus.freepascal.org/">Lazarus</a>
            (along with <a href="http://freepascal.org/">Free Pascal Compiler</a>).
          <li>Compile in Lazarus the engine packages in
            <code>castle_game_engine/packages/</code>.
          <li>Open and compile in Lazarus the program <code>xxx.lpi</code> project file.
        </ol>
      </li>

      <li><p>Or you can <b>use our
        <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">build tool</a>
        to compile.</b></p>
        <ol>
          <li>Install the <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">build tool</a>
            (along with <a href="http://freepascal.org/">Free Pascal Compiler</a>).
          <li>Then compile the game by running the <code>castle-engine compile</code> command
            inside it's directory. <code>castle-engine</code> will find
            the <code>CastleEngineManifest.xml</code> file and use it.
          <li>Most games include also a <code>Makefile</code> that calls our build tool,
            so you can also just run <code>make</code> to compile.
        </ol>
      </li>
    </ul>

<?php /*
    <li><p>For other games:

    <b>Unpack</b> them such that
    <code>castle_game_engine/</code> and program-specific directory
    (like <code>castle/</code> or <code>view3dscene/</code>)
    are siblings.

    <b>Compile</b> the program by using <code>compile.sh</code> script inside
    it's directory.
    Or you can compile using Lazarus project files (be sure to first
    compile Lazarus packages in <code>castle_game_engine/packages/</code>.

    <p><a href="http://www.freepascal.org/">FPC (Free Pascal Compiler)</a>
    is required for compilation, <?php echo a_href_page_hashlink('see
    information about minimal FPC version', 'documentation', 'section_fpc_ver'); ?>
    (in short: use latest stable FPC version).
*/ ?>

  <li><p><b>Install the required libraries</b>. See
    <?php echo a_href_page('engine documentation', 'documentation'); ?> for the list of libraries we use,
    and see download page of each particular program.

  <li><p><b>Optionally install required program data</b>. In most cases you don't
    have to do anything &mdash; as programs below either don't have any data,
    or they use smart URL <a href="manual_data_directory.php">castle-data:/xxx</a>
    to load data files. It will automatically use current directory, if the data is there.

    <p>On Unix you can also move/symlink data to <code>$HOME/.local/share/xxx</code>
    or <code>/usr/local/share/xxx</code> or <code>/usr/share/xxx</code>.
    You can do this easily by executing <code>make install</code> inside program
    directory.
</ol>

<p>The sources below contain everything you need.
Source code (units, program files), scripts, also game data sources
(GIMP *.xcf files, Blender *.blend files and so on).
Everything is open source,
<a href="http://www.gnu.org/licenses/gpl.html">GNU GPL</a> &gt;= 2.

<?php
function github_source_url($github_name, $version)
{
  return 'https://github.com/castle-engine/' .
    $github_name .
    '/archive/v' .
    $version .
    '.zip';
  /*
  return
    'https://github.com/castle-engine/'
    . $github_name .
    '/releases/download/v' .
    $version .
    '/' .
    $package_name .
    '-' .
    $version .
    '-src.zip';
  */
}

function download_engine_version($engine_version)
{
  return '<a href="' . github_source_url('castle-engine', $engine_version) .
    '">Castle Game Engine version ' . $engine_version . '</a>';
}
?>

<!--
<p>Note about compatibility: Sometimes I happen to break backwards
compatibility in the engine API. The sources in SVN are always
updated, and should be totally compatible.
But if you download tar.gz sources below, you may find that they
are not compatible with current engine version... that's why
they are notes like <i>compatible with engine XXX version</i>
notes near some programs below.
-->

<?php echo $toc->html_section(); ?>

<dl>
<?php
  /* $internal_name is both the basename of the archive and
     the subdirectory name within SVN repository.
     $github_name is the project name on GitHub. */
  function echo_src_archive_2($title, $internal_name, $github_name, $engine_ver)
  {
    $version_const_name = 'VERSION_' . str_replace('-', '_', strtoupper($internal_name));
    $version = constant($version_const_name);

    echo '<dt>' . $title . '</dt><dd>';

    if ($engine_ver != 'ancient') // so old engine version that we don't want to show it, not hosted on GitHub etc.
    {
      echo '<p><b>Stable version</b>: <a href="' . github_source_url($github_name, $version) .
        '">Download sources of ' . $title . '</a>';

      if ($engine_ver == VERSION_CASTLE_GAME_ENGINE) {
        echo '. <br>The stable version is compatible with the latest ' .
          download_engine_version($engine_ver);
        echo '.';
      } else
      if ($engine_ver != '') {
        echo '. <br>The stable version is compatible with the ' .
          download_engine_version($engine_ver);
      } else
      {
        throw new Exception('Invalid engine_ver');
      }
    }

    ?>

    <p><b>Unstable (latest) version</b>: You can download it from the GitHub:
    <a href="https://github.com/castle-engine/<?php echo $github_name; ?>">https://github.com/castle-engine/<?php echo $github_name; ?></a><br>
    It is compatible with the <a href="https://github.com/castle-engine/castle-engine/">latest Castle Game Engine version from GitHub</a>.
  </dd>

  <?php
  }

  function echo_src_archive($title_and_internal_name, $github_name, $engine_ver)
  {
    echo_src_archive_2($title_and_internal_name, $title_and_internal_name,
      $github_name, $engine_ver);
  }

  echo_src_archive('view3dscene', 'view3dscene', '7.0-alpha.1');
  echo_src_archive('castle-view-image', 'castle-view-image', '7.0-alpha.1');
  echo_src_archive('castle', 'castle-game', 'ancient'); // '4.1.1');
  echo_src_archive('rayhunter', 'rayhunter', 'ancient'); // '4.0.1');
  echo_src_archive('malfunction', 'malfunction', 'ancient'); // '4.0.1');
  echo_src_archive('kambi_lines', 'kambi-lines', 'ancient'); // '4.0.1');
  echo_src_archive('glplotter', 'glplotter', '7.0-alpha.snapshot'); // '4.0.1');
?>
</dl>

<!--
<p>Note: archives above do not contain user documentation for
these programs. For now you can just go to the page of appropriate
program and read documentation there (if you downloaded binary
version of program you will also have documentation there).
-->

<?php /*

<?php echo $toc->html_section(); ?>

<p>All these programs have also their project page on GitHub,
as part of <a href="https://github.com/castle-engine/">GitHub
Castle Game Engine organization</a>.

<p>You can download them using a GIT client, SVN client, or as a ZIP.

< ?php echo $toc->html_section(); ? >

<p>You can get all the sources from our Subversion repository.
If you don't know about Subversion, see
<a href="http://subversion.tigris.org/">Subversion main site</a> and
<a href="http://svnbook.red-bean.com/">the <i>excellent</i>
book about the Subversion</a>.</p>

<p>To download full sources for all projects, do</p>

<pre>< ?php echo sf_checkout_link(true, ''); ? ></pre>

<p>Please note that the full <code>trunk</code> is quite large.
It contains everything: the core engine sources (<code>castle_game_engine</code> subdirectory),
webpages stuff (in <code>www</code> subdirectory),
<code>view3dscene</code> sources, <code>castle</code> sources etc.
Often you want to download only specific subdirectories of it.</p>

<p>You can also <a href="https://sourceforge.net/p/castle-engine/code/">browse
the SVN repository</a>.</p>

<p>Code from SVN is always the bleeding-edge current
version of the work. That said, usually it's quite stable (I have a personal
policy to try to commit only code that is compileable and somewhat tested).
So feel free to peek, and please report eventual bugs you spot.
You can also download the code from one of
<code>http://svn.code.sf.net/p/castle-engine/code/tags/</code>
subdirectories, these contain frozen code from specific versions of my programs,
so should be 100% stable.</p>

*/ ?>

<?php
castle_footer();
?>
