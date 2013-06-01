<?php
require_once 'castle_engine_functions.php';
castle_header('All Programs Sources | Castle Game Engine', NULL, array('all_programs'));
echo pretty_heading('All Programs Sources');

$toc = new TableOfContents(
  array(
    new TocItem('Introduction', 'intro'),
    new TocItem('Download', 'download_src'),
    new TocItem("Getting bleeding-edge (development) version from Subversion (SVN)", 'svn'),
  )
);
$toc->echo_numbers = true;
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>Sources for all final programs related to our engine may be
downloaded below.</p>

<ol>
  <li><p><b>Download</b> the sources that interest you,
    and additionally download <?php echo a_href_page('Castle Game Engine', 'engine'); ?> sources.</li>

  <li><p><b>Unpack</b> them such that
    <tt>castle_game_engine/</tt> and program-specific directory
    (like <tt>castle/</tt> or <tt>view3dscene/</tt>)
    are siblings.

  <li><p><b>Compile</b> every program by using <tt>compile.sh</tt> script inside
    it's directory. The one exception is <tt>castle</tt> code,
    which should be compiled by executing <tt>make</tt>.

    <p>Alternatively, you can compile using Lazarus project files (be sure to first
    compile Lazarus packages in <tt>castle_game_engine/packages/</tt>.

    <p><a href="http://www.freepascal.org/">FPC (Free Pascal Compiler)</a>
    is required for compilation, <?php echo a_href_page_hashlink('see
    information about minimal FPC version', 'engine', 'section_fpc_ver'); ?>
    (in short: use latest stable FPC version).

  <li><p><b>Install required libraries</b>. See
    <?php echo a_href_page_hashlink('engine download page', 'engine',
    'section_download_src'); ?> for the list of libraries we use,
    and see download page of each particular program.

  <li><p><b>Optionally install required program data</b>. In most cases you don't
    have to do anything &mdash; as programs below either don't have any data,
    or they use smart function (<a href="http://castle-engine.sourceforge.net/apidoc/html/CastleFilesUtils.html#ApplicationData">ApplicationData</a>)
    to detect data directory. It will automatically
    use current directory, if the data is there.

    <p>But you can also move/symlink data to <tt>$HOME/.local/share/xxx</tt>
    or <tt>/usr/local/share/xxx</tt> or <tt>/usr/share/xxx</tt>.
    You can do this easily by executing <tt>make install</tt> inside program
    directory.
</ol>

<p>The sources below contain everything you need.
Source code (units, program files), scripts, also game data sources
(GIMP *.xcf files, Blender *.blend files and so on).
Everything is open source,
<a href="http://www.gnu.org/licenses/gpl.html">GNU GPL</a> &gt;= 2.

<?php
function older_engine_version($older_version)
{
  echo sf_download($older_version . ' version',
    'castle_game_engine-' . $older_version . '-src.tar.gz');
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

<ul>
<?php
  function echo_src_svnonly($name)
  {
    echo '<li><p>' . $name . ': only from Subversion by:<br><tt class="terminal small">' .
        sf_checkout_link(true, $name) . '</tt></li>
      ';
  }

  /* Internal name is both the basename of the archive and
     the subdirectory name within SVN repository. */
  function echo_src_archive_2($title, $internal_name, $engine_ver)
  {
    $version_const_name = 'VERSION_' . strtoupper($internal_name);
    $version = constant($version_const_name);

    echo '<li><p>' .
      sf_download('sources of '.$title,
        $internal_name . '-' . $version . '-src.tar.gz');

    if ($engine_ver == VERSION_CASTLE_GAME_ENGINE)
    {
      echo '<br/>These tar.gz sources are compatible with latest engine ';
      older_engine_version($engine_ver);
      echo '.';
    } else
    {
      echo '<br/>These tar.gz sources were tested with engine ';
      older_engine_version($engine_ver);
      echo ', use SVN to get sources compatible with latest engine version.';
    }

    echo
      '<p>Download from Subversion by:</p><pre class="terminal small">' .
        sf_checkout_link(true, $internal_name) . '</pre></li>
      ';
  }

  function echo_src_archive($title_and_internal_name, $engine_ver)
  {
    echo_src_archive_2($title_and_internal_name, $title_and_internal_name,
      $engine_ver);
  }

  echo_src_archive('view3dscene', '4.0.1');
  echo_src_archive('castle', '4.0.1');
  echo_src_archive('rayhunter', '4.0.1');

  //echo_src_archive('lets_take_a_walk', '3.0.0');
  echo_src_archive('malfunction', '4.0.1');
  echo_src_archive('kambi_lines', '4.0.1');

  echo_src_archive('glplotter', '4.0.1');
  echo_src_archive('gen_function', '4.0.1');

  echo_src_archive_2('glViewImage', 'glviewimage', '4.0.1');
  echo_src_archive('bezier_curves', '4.0.1');
  echo_src_archive_2('glinformation and glinformation_glut', 'glinformation', '4.0.1');
?>
</ul>

<!--
<p>Note: archives above do not contain user documentation for
these programs. For now you can just go to the page of appropriate
program and read documentation there (if you downloaded binary
version of program you will also have documentation there).
-->

<?php echo $toc->html_section(); ?>

<p>You can get all the sources from our Subversion repository.
If you don't know about Subversion, see
<a href="http://subversion.tigris.org/">Subversion main site</a> and
<a href="http://svnbook.red-bean.com/">the <i>excellent</i>
book about the Subversion</a>.</p>

<p>To download full sources for all projects, do</p>

<pre class="terminal small"><?php echo sf_checkout_link(true, ''); ?></pre>

<p>Please note that the full <tt>trunk</tt> is quite large.
It contains everything: the core engine sources (<tt>castle_game_engine</tt> subdirectory),
webpages stuff (in <tt>www</tt> subdirectory),
<tt>view3dscene</tt> sources, <tt>castle</tt> sources etc.
Often you want to download only specific subdirectories of it.</p>

<p>You can also <a href="https://sourceforge.net/p/castle-engine/code/">browse
the SVN repository</a>.</p>

<p>Code from SVN is always the bleeding-edge current
version of the work. That said, usually it's quite stable (I have a personal
policy to try to commit only code that is compileable and somewhat tested).
So feel free to peek, and please report eventual bugs you spot.
You can also download the code from one of
<tt class="terminal">http://svn.code.sf.net/p/castle-engine/code/tags/</tt>
subdirectories, these contain frozen code from specific versions of my programs,
so should be 100% stable.</p>

<?php
castle_footer();
?>
