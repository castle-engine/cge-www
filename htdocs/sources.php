<?php
  require_once 'vrmlengine_functions.php';

  common_header("Sources of Kambi VRML game engine and related programs",
    LANG_EN);

  $toc = new TableOfContents(
    array(
      new TocItem('License', 'license'),
      new TocItem('Base engine sources', 'engine_src'),
      new TocItem("Related programs' sources", 'program_sources'),
      new TocItem("Subversion (SVN) notes", 'program_sources'),
      new TocItem('FPC version required', 'fpc_ver')
    )
  );
?>

<h1>Sources of Kambi VRML game engine and related programs</h1>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>The engine and all related sources are licensed on terms of
<a href="http://www.gnu.org/licenses/gpl.html">GNU General Public License</a>.
See <a href="http://www.gnu.org/">www.gnu.org</a> for more information
about this license (including translations of this license to various
languages) and philosophy of free software.
In the future I may change the license to more liberal than GNU GPL &mdash;
most probably to modified LGPL (the one used by FreePascal RTL). Contact me
if you want the change to LGPL to happen <i>now</i>.

<?php echo $toc->html_section(); ?>

<p>Sources of <?php echo a_href_page('Kambi VRML game engine',
'kambi_vrml_game_engine'); ?>:<br>
 <?php echo sf_download('Download engine sources',
  'kambi_vrml_game_engine-' . VERSION_KAMBI_VRML_GAME_ENGINE .
  '-src.tar.gz') ?><br>
 Or download from Subversion by:<br><tt>
 <?php echo sf_checkout_link(true, 'kambi_vrml_game_engine'); ?></tt>

<p>This archive contains the whole engine sources.
<!-- Too unimportant:
  There are some helper files used when making documentation of
  my units with the help of
<a href="http://pasdoc.sourceforge.net/">pasdoc</a>.
There are helper scripts and files to compile my programs.  -->
There are many simple example programs included in this archive
(see <tt>examples/</tt> subdirectories),
and some small tools-like programs that are not released in compiled form
on these pages (see <tt>tools/</tt> subdirectories).
<!-- I plan to add some more usable tools:
<tt>font2pascal</tt> (to generate those units in fonts/ directory),
<tt>kambi_bfnt_show</tt> and <tt>kambi_ttf_show</tt>
(to view TTF fonts as bitmap or outline OpenGL fonts)
and more examples of using my units. All those programs are ready,
I just have to upload them here as soon as I will have some time. -->

<!--
Too unimportant:
<p>In the future I may
split this archive to a couple of smaller ones (like "units for graphics",
"units for OpenAL" etc.).
-->

<p>Compile everything with simple <tt>make</tt> inside the <tt>kambi_vrml_game_engine/</tt>
subdirectory.
Compile examples and tools <!-- (the ones that do not require Lazarus) --> by
simple <tt>make examples</tt>.

<!--
Don't mention Lazarus here &mdash; the explanations that actually the engine
main OpenGL initialization method is <b>not</b> the Lazarus TOpenGLControl
takes too much space.

<p>There are also some Lazarus packages and examples (e.g. to extend Lazarus
<tt>TOpenGLControl</tt> component), they have to be compiled from
within Lazarus. Although note that the engine doesn't require LCL
for anything.
these are not an
essential part of the engine for now.
The main way for
intiializing OpenGL for games is by GLWindow unit that doesn't depend on
any Lazarus units. -->

<p>Note: under Windows you will need some DLLs
to successfully run some of the programs.
I collected all DLLs needed by my programs
<?php echo current_www_a_href_size('in this archive',
  'miscella/win32_dlls.zip'); ?>, so just download
this and put in program's exe directory.
These DLLs are already included in binary archives of my programs,
but are not included in source archives (since they are only usable
for Windows users). In general, for all OSes, see section
"Requirements" in the documentation of programs and make sure that
you have appropriate libraries installed on your system.</p>

<?php echo $toc->html_section(); ?>

<p>Below are sources for specific programs.
Each of them contains program-specific modules, main program file
and script <tt>compile.sh</tt> to simply compile whole program using FPC.
Download those that you are interested in and unpack them into
the same directory where you unpacked
<?php echo a_href_page('Kambi VRML game engine', 'kambi_vrml_game_engine'); ?>.
Then execute <tt>compile.sh</tt> scripts to compile the programs you want.

<?php
function older_engine_version($older_version)
{
  echo sf_download($older_version . ' version',
    'kambi_vrml_game_engine-' . $older_version . '-src.tar.gz');
}
?>

<p>Note about compatibility: I broke slightly compatibility between 1.0.0
and 1.1.0 engine release, and again for 1.2.0. If you download tar.gz sources,
some programs below may fail to compile with latest engine,
in that case use older <?php older_engine_version('1.1.0'); ?>
 or <?php older_engine_version('1.0.0'); ?> version. Or simply download code
from SVN, everything in SVN should be always up-to-date.

<ul>
<?php
  function echo_src_svnonly($name)
  {
    echo '<li>' . $name . ': only from Subversion by:<br><tt>' .
        sf_checkout_link(true, $name) . '</tt></li>
      ';
  }

  /* Internal name is both the basename of the archive and
     the subdirectory name within SVN repository. */
  function echo_src_archive_2($title, $internal_name)
  {
    $version_const_name = 'VERSION_' . strtoupper($internal_name);
    $version = constant($version_const_name);

    echo '<li>' .
      sf_download('sources of '.$title,
        $internal_name . '-' . $version . '-src.tar.gz') . '<br>
      Or download from Subversion by:<br><tt>' .
        sf_checkout_link(true, $internal_name) . '</tt></li>
      ';
  }

  function echo_src_archive($title_and_internal_name)
  {
    echo_src_archive_2($title_and_internal_name, $title_and_internal_name);
  }

  echo_src_archive('castle');
  echo_src_archive('lets_take_a_walk');
  echo_src_archive('malfunction');
  echo_src_archive('kambi_lines');

  echo_src_archive('view3dscene');
  echo_src_archive('rayhunter');

  echo_src_archive_2('glViewImage', 'glviewimage');
  echo_src_archive('glplotter');
  echo_src_archive('bezier_curves');
  echo_src_archive_2('glcaps and glcaps_glut', 'glcaps');

  echo_src_archive('gen_function');
  echo_src_svnonly('sandbox');
  echo_src_svnonly('rift');
?>
</ul>

<p>Note: archives above do not contain user documentation for
these programs. For now you can just go to the page of appropriate
program and read documentation there (if you downloaded binary
version of program you will also have documentation there).

<?php echo $toc->html_section(); ?>

<p>As you can clearly see above, all sources may be downloaded from
Subversion repository. If you don't know about Subversion... well,
then you should get to know it. See
<a href="http://subversion.tigris.org/">Subversion main site</a> and
<a href="http://svnbook.red-bean.com/">the <i>excellent</i>
book about the Subversion</a>.</p>

<p>Code from SVN is always the bleeding-edge current
version of the work. That said, usually it's quite stable (I have a personal
policy to try to commit only code that is compileable and somewhat tested).
So feel free to peek, and please report eventual bugs you spot.
You can also download the code from one of
<tt>
https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/tags/</tt>
subdirectories, these contain frozen code from specific versions of my programs,
so should be 100% stable.</p>

<p>You can also <a href="http://vrmlengine.svn.sourceforge.net/viewvc/vrmlengine/">browse
the SVN repository using ViewVC</a>.</p>

<p>The sources layout in SVN repository:
at the top level, there's <tt>trunk/</tt>.
Beneath <tt>trunk/</tt>, there are subdirectories for each project &mdash;
like <tt>kambi_vrml_game_engine</tt>, <tt>castle</tt>, <tt>www</tt> and so on.
At the top level, there's also subdirectory <tt>tags</tt>, and again, it has
subdirectories for each project that I'm keeping tags on (e.g. <tt>castle/</tt>,
and inside <tt>castle/</tt> there are subdirectories
<tt>0.6.6</tt>, <tt>0.7.0</tt> etc.).</p>

<div class="note">
<p class="note_title">A note to developers familiar with SVN</p>

<p>I know that my layout is a little
non-standard: standard way is to make subdirectory for each project
at top level (like <tt>kambi_vrml_game_engine</tt>, <tt>castle</tt> etc.)
and beneath each of them make subdirectories <tt>trunk</tt>, <tt>tags</tt>.
But current layout is more comfortable for users: usually you want to
download both <tt>kambi_vrml_game_engine</tt> and one of the other directories
(like <tt>castle</tt>) and have them as siblings, since this allows you
to compile most things by using ready provided compilation scripts.</p>
</div>

<?php echo $toc->html_section(); ?>

<p>The engine is meant to be compiled with
<a href="http://www.freepascal.org/">Free Pascal Compiler (FPC)</a>.
About support for various FPC versions:

<ul>
  <li><p><em>FPC 2.2.0</em> : This is the simplest and best option
    &mdash; FPC 2.2.0 works perfectly, and you can download already
    compiled packages of it from
    <a href="http://www.freepascal.org/">www.freepascal.org</a>.

  <li><p><em>FPC 2.0.4</em> : will work OK for some time from now
    (2007-09-20).

  <li><p><em>Latest FPC development version,
    downloadable from FPC SVN repository</em>, usually works.
    However, you must remember that SVN version changes rapidly
    and no-one can give any guarantee about current stability of FPC from SVN
    (or stability of my programs compiled with this FPC).

  <li><p><em>Older FPC versions</em>: while they may work,
    they are not really supported.

    <p>There was a time when various versions of FPC 1.0.x
    were allowed (including my own patched versions of FPC 1.0.6
    and later 1.0.10), there was a time when various FPC 1.9.x
    were allowed and so on. But it's all gone now. I even deleted
    various notes about support for older FPC versions from this
    page. Just upgrade to latest stable FPC release.
</ul>

<?php
  if (!IS_GEN_LOCAL) {
    $counter = php_counter("sources", TRUE);
  };

  common_footer();
?>
