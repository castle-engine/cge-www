<?php
require_once 'castle_engine_functions.php';
// define('CASTLE_GITHUB_NAME', 'castle-engine'); // too ugly, we have links to GitHub from main page
manual_header('Installation and building your first application');

/*
echo '<img src="images/castle_game_engine_icon.png"
  alt="Castle Game Engine icon"
  class="engine-icon" />';
*/

$toc = new TableOfContents(
  array(
    new TocItem('Installation', 'install'),
    new TocItem('Build and run your first application', 'fist_app'),
    new TocItem('Optional: Install Lazarus packages', 'lazarus'),
  )
);

//echo pretty_heading('Castle Game Engine Documentation', VERSION_CASTLE_GAME_ENGINE, 'Getting Started');
//echo pretty_heading('Getting Started');
?>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<ol>
  <li><p>First, install <a href="https://www.lazarus-ide.org/">Lazarus and FPC</a> (Pascal IDE and compiler). Go to the <a href="https://www.lazarus-ide.org/">Lazarus website</a> and download the latest release.

    <?php
    echo castle_thumbs(array(
      array('filename' => 'lazarus_website.png', 'titlealt' => 'Lazarus website'),
    ), 'auto', 'left', 'thumb_size');
    ?>

    <p><b>Advanced:</b> You can install FPC and Lazarus in other ways.
    <a href="https://github.com/castle-engine/castle-engine/wiki/fpcupdeluxe">You can install using FpcUpDeluxe</a>.
    You can also install <a href="https://www.freepascal.org/">only FPC</a> and use
    any Pascal code editor you like, like <a href="https://code.visualstudio.com/">VS Code</a>
    or <a href="https://atom.io/">Atom</a>.

  <li><p>Download the latest <a href="/">Castle Game Engine</a>. Unpack the engine ZIP wherever you like.

  <li><p>Inside the unpacked <code>castle_game_engine</code>, you will find a subdirectory <code>bin</code>. Run the <code>castle-editor</code> executable inside.

  <li><p>Configure FPC and Lazarus locations in editor <i>Preferences</i>, if they have not been auto-detected correctly. <!-- The editor will display a clear warning on the main form if they need configuration (which means it could not find them automatically). -->

    <?php
    echo castle_thumbs(array(
      array('filename' => 'cge_editor_prefs.png', 'titlealt' => 'Castle Game Engine Editor Preferences'),
    ), 'auto', 'left');
    ?>
</ol>

<?php echo $toc->html_section(); ?>

<ol>
  <li><p>Create a new project in the <i>Castle Game Engine Editor</i>.

    <p>Use any of the suggested <i>New Project</i> templates. These templates have been designed to show you most important features of our engine, along with the most advised way to use them.

    <?php
    echo castle_thumbs(array(
      array('filename' => 'cge_editor_new_project.png', 'titlealt' => 'Castle Game Engine Editor New Project'),
    ), 'auto', 'left');
    ?>

  <li><p><i>Compile</i> and <i>Run</i> the project using the editor <i>Run</i> menu. Or just press F9.

    <p><i>Note:</i> First compilation of a new project needs to build the engine as well. Subsequent compilations will be lighting fast.

  <li><p>That's it! The world is your oyster now :)

    <p>Open and try numerous example projects from the engine <code>examples</code> subdirectory. Open them in the editor by <i>Open Project</i> and point at the example's <a href="https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples">CastleEngineManifest.xml</a> file. Projects can also be compiled using <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">Castle Game Engine command-line build tool</a>.
</ol>

<!--p>The editor executes under the hood our <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">build tool</a> which in turn executes under the hood FPC (and some other tools, depending on the platform).-->

<!--
<p>The editor and build tool are most natural to build
applications that do not depend on LCL (<i>Lazarus Component Library</i>).
This means that you should initialize your game window using
the <?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?> class.
Our documentation and most examples follow this approach too.
-->

<?php echo $toc->html_section(); ?>

<p>We advise using a full-featured Pascal IDE
like <a href="http://lazarus.freepascal.org/">Lazarus</a> for development.
To make Lazarus "aware" of <i>Castle Game Engine</i> you need to compile in Lazarus
some engine <i>packages</i>.

<ol>

  <li><p>Open and <b>compile the package <code>castle_base.lpk</code></b>
    You will find it in the <code>castle_game_engine/packages/</code> subdirectory.
    Use the Lazarus menu item <i>"Package -&gt; Open Package File (.lpk)"</i>
    to open the package file, press <i>"Compile"</i> in a dialog that appears.

    <?php
    echo castle_thumbs(array(
      array('filename' => 'lazarus-install-01.png', 'titlealt' => 'castle_base: Open Package File'),
      array('filename' => 'lazarus-install-02.png', 'titlealt' => 'castle_base: Choose the file'),
      array('filename' => 'lazarus-install-03.png', 'titlealt' => 'castle_base: Compile'),
    ), 'auto', 'left', 'small_thumb_const_height_size');
    ?>

  <li><p>Similarly, open and <b>compile the package <code>castle_window.lpk</code></b>.

    <p>Note: <i>do not</i> install the <code>castle_window</code> package.

    <?php
    echo castle_thumbs(array(
      array('filename' => 'lazarus-install-21.png', 'titlealt' => 'castle_window: Choose the file'),
      array('filename' => 'lazarus-install-22.png', 'titlealt' => 'castle_window: Compile'),
    ), 'auto', 'left', 'small_thumb_const_height_size');
    ?>

  <li><p>Finally, open and <b>install the package <code>castle_components.lpk</code></b>.
    In the package dialog, the option to <i>"Install"</i> package is under the <i>"Use"</i> button.

    <!--p>Note: Installing the <code>castle_components</code> package
    will also automatically install the package <code>castle_base</code>,
    as a dependency. That's cool, let it happen.-->

    <?php
    echo castle_thumbs(array(
      array('filename' => 'lazarus-install-31.png', 'titlealt' => 'castle_components: Choose the file'),
      array('filename' => 'lazarus-install-32.png', 'titlealt' => 'castle_components: Install'),
      array('filename' => 'lazarus-install-33.png', 'titlealt' => 'castle_components: Confirm Lazarus rebuild'),
    ), 'auto', 'left', 'small_thumb_const_height_size');
    ?>
</ol>

<p>Once <code>castle_components.lpk</code> is successfully installed,
Lazarus restarts, and you should see the <i>"Castle"</i> tab
with our components.
<!--
 at the top (TODO: screenshot). Sorry,
we don't have icons for our components yet, so it looks a little
boring. Mouse over the icons to see component names.--></p>

<p><b>You're done:)</b>
Now compile and run from Lazarus any engine example.
Open the project file (<code>xxx.lpi</code>) using Lazarus, and compile and run from Lazarus (F9).
A good examples to try at the beginning are
<code>examples/fps_game/fps_game.lpi</code> and
<code>examples/lazarus/model_3d_viewer/</code>.</p>

<?php
echo castle_thumbs(array(
  array('filename' => 'lazarus-install-41.png', 'titlealt' => 'fps_game: Open Project'),
  array('filename' => 'lazarus-install-42.png', 'titlealt' => 'fps_game: Choose the file'),
  array('filename' => 'lazarus-install-43.png', 'titlealt' => 'fps_game: Run'),
  array('filename' => 'lazarus-install-44.png', 'titlealt' => 'fps_game: Running!'),
), 'auto', 'left', 'small_thumb_const_height_size');
?>

<?php
manual_footer();
?>
