<?php
require_once 'castle_engine_functions.php';
// define('CASTLE_GITHUB_NAME', 'castle-engine'); // too ugly, we have links to GitHub from main page
castle_header('Installation and building your first application');

/*
echo '<img src="images/castle_game_engine_icon.png"
  alt="Castle Game Engine icon"
  class="engine-icon" />';
*/

$toc = new TableOfContents(
  array(
    new TocItem('Installation', 'install'),
    new TocItem('Build and run your first application', 'fist_app'),
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

  <li><p>While you are there (<i>Preferences</i> dialog, default tab <i>FPC and Lazarus</i>) also click on the button <i>"Register Lazarus Packages"</i>. This will make compilation of CGE applications using Lazarus go smoothly.

    <?php
    echo castle_thumbs(array(
      array('filename' => 'cge_lazarus_registered_ok.png', 'titlealt' => 'Lazarus packages registration confirmation'),
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

<?php
castle_footer();
?>
