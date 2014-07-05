<?php
require_once 'castle_engine_functions.php';
tutorial_header('Download and install the engine, try the demos');
?>

<p>If you haven't done it yet, <?php echo a_href_page('download the
engine source code with examples', 'engine'); ?>.</p>

<p>The example program that shows most of the features presented in
this tutorial is inside <tt>examples/fps_game/fps_game.lpr</tt> .
I suggest looking at it's source
code for a complete implementation that uses most the code snippets
shown in this tutorial.</p>

<ul>
  <li><p><b>If you want to develop using <a href="http://www.lazarus.freepascal.org/">Lazarus</a>:</b>
    Run Lazarus,
    and open packages in the <tt>castle_game_engine/packages/</tt>
    subdirectory. Open and compile all three packages (<tt>castle_base.lpk</tt>,
    <tt>castle_components.lpk</tt>, <tt>castle_window.lpk</tt>), to test that things compile
    OK. Then install the package <tt>castle_components.lpk</tt> (it will
    also install <tt>castle_base.lpk</tt> by dependency).
    (Do not install
    <tt>castle_window.lpk</tt> &mdash; you don't want to have this installed in
    Lazarus.)</p>

    <p>Once packages are successfully installed, Lazarus restarts, and you
    should see the <i>"Castle"</i> tab with our components.
    <!--
     at the top (TODO: screenshot). Sorry,
    we don't have icons for our components yet, so it looks a little
    boring. Mouse over the icons to see component names.--></p>
  </li>

  <li><p><b>Alternatively, our engine can be used without Lazarus and LCL
    (Lazarus Component Library). Only <a href="http://www.freepascal.org/">FPC
    (Free Pascal Compiler)</a> is required</b>. Many of the example programs
    use our own <tt>CastleWindow</tt> unit to communicate with window manager,
    and they do not depend on Lazarus LCL.
    You can use command-line <tt>xxx_compile.sh</tt> scripts (or just call
    <tt>make examples</tt>) to compile them using FPC.

    <p>You will not be able to compile components and examples using LCL
    of course (things inside <tt>src/components/</tt> and <tt>examples/lazarus/</tt>).
  </li>
</ul>

<p>Let's quickly open and run some demos, to make sure that everything
works. I suggest running at least
<tt>examples/lazarus/model_3d_viewer/</tt> (demo using Lazarus LCL) and
<tt>examples/fps_game/</tt> (demo using our CastleWindow unit).</p>

<p>Make sure you have installed the necessary libraries first, or some of
the demos will not work. The required libraries are mentioned near the <?php
echo a_href_page_hashlink('engine download', 'engine', 'section_download_src'); ?>.
<!--
<a
href="http://castle-engine.sourceforge.net/apidoc/html/introduction.html#SectionLibraries">Requirements
-&gt; Libraries</a> section of our reference introduction.
-->
Under Windows, you will usually want to grab the necessary DLLs from: <a href="http://castle-engine.sourceforge.net/miscella/win32_dlls.zip">here (32-bit libraries zipped)</a>  or <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/external_libraries/i386-win32/">here (directory with 32-bit libraries)</a> or <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/external_libraries/x86_64-win64/">here (directory with 64-bit libraries)</a>. Then place dll files
somewhere on your $PATH, or just place them in every directory
with .exe files that you compile with our engine.</p>

<p>If you are interested in developing for Android or iOS, you will want
to later read also
<ul>
  <li><a href="http://sourceforge.net/p/castle-engine/wiki/Android%20development/">Developing for Android</a>
  <li><a href="http://sourceforge.net/p/castle-engine/wiki/iOS%20Development/">Developing for iOS (iPhone, iPad)</a>
</ul>

<p>Now we'll start creating our own game from scratch.</p>

<?php
tutorial_footer();
?>
