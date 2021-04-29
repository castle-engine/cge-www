<?php
require_once 'castle_engine_functions.php';
manual_header('Editor');

$toc = new TableOfContents(
  array(
    new TocItem('Video Introduction', 'video'),
    new TocItem('Create, build, run projects', 'projects'),
    new TocItem('Design user interfaces and 3D/2D transformation hierarchies', 'design'),
    new TocItem('Edit source code', 'source_code'),
    new TocItem('File browser', 'file_browser'),
    new TocItem('Custom (project-specific) components in the visual designer', 'custom_components'),
  )
);
?>

<?php echo $toc->html_toc(); ?>

<p><i>Castle Game Engine Editor</i> is the most important visual tool of our engine.
It allows to create, edit and build applications with CGE.
Get it by <a href="index.php">downlading latest Castle Game Engine release</a>
and run <code>castle-editor</code> executable in the <code>bin</code> subdirectory.

<?php echo $toc->html_section(); ?>

<iframe width="560" height="315" src="https://www.youtube.com/embed/zdwN4mdQG_8" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<?php echo $toc->html_section(); ?>

<p>You can create a project (using a number of templates) or open an existing one.
You can compile, run and package the project using the <i>Run</i> menu.

<p>A <i>Castle Game Engine</i> project is a directory
containing the <a href="https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples">CastleEngineManifest.xml</a>
file. Most often it also contains a subdirectory <code>data/</code>
that is accessed from code using the
<a href="manual_data_directory.php">castle-data:/xxx</a> URLs.
Everything else is up to you, organize your source code and data however you like.

<p>Note that compiling and packaging a project <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">can also be done using the command-line CGE build tool</a>.
Actually, the editor just calls the build tool under the hood.
The build tool in turn calls a compiler (FPC) and some other tools
(e.g. Android-specific packaging tools).

<?php echo $toc->html_section(); ?>

<p>You can visually design:

<ol>
  <li><p>A hierarchy of user-interface controls. Anything descending from <?php api_link('TCastleUserInterface', 'CastleUIControls.TCastleUserInterface.html'); ?>, like a button, label, or a powerful viewport (that contains a hierarchy of 3D / 2D scenes and transformations inside).

    <p>Saved as <code>xxx.castle-user-interface</code> files (somewhere in the <code>data/</code> subdirectory).

    <p>They are typically loaded in your application by setting the <?php api_link('TUIState.DesignUrl', 'CastleUIState.TUIState.html#DesignUrl'); ?> (see almost any engine example or "New Project" template). Other approaches are possible too, e.g. you can load using <?php api_link('UserInterfaceLoad', 'CastleUIControls.html#UserInterfaceLoad'); ?>, <code>TSerializedComponent.UserInterfaceLoad</code> and more. See examples like <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/advanced_editor">advanced_editor</a>.

  <li><p>A hierachy of 3D / 2D scenes and transformations. Anything descending from <?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?>.

    <p>Saved as <code>xxx.castle-transform</code> files (somewhere in the <code>data/</code> subdirectory).

    <p>You can load it in your game using <?php api_link('TransformLoad', 'CastleComponentSerialize.html#TransformLoad'); ?> and insert into existing hierarchy of <code>TCastleViewport.Items</code>. You can also use <a href="https://castle-engine.io/apidoc-unstable/html/CastleTransformExtra.TCastleTransformDesign.html">TCastleTransformDesign</a> to use it in other designs (thus having a reusable composition of 3D/2D objects). See examples like <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/advanced_editor">advanced_editor</a>.
</ol>

<p>The <code>xxx.castle-user-interface</code> and <code>xxx.castle-transform</code> are simple JSON text files. You should commit them to the version control, just like your source code. You can have as many such files inside your project as you need to.

<p>Let us emphasize that <i>when using the editor, you still code using the same CGE API as described throughout this manual</i>. At any point you can load an instance of <code>TCastleUserInterface</code> or <code>TCastleTransform</code> from a designed file and insert it into the running application.

<p>Open various example projects to see the editor, with various components, in action. We recommend trying out
various templates (create "New Project" in editor),
<a href="https://github.com/castle-engine/castle-engine/tree/master/examples/component_gallery">examples/component_gallery</a>,
<a href="https://github.com/castle-engine/castle-engine/tree/master/examples/tiled/strategy_game_demo">examples/tiled/strategy_game_demo</a>.

<?php echo $toc->html_section(); ?>

<p>The editor integrates with <a href="https://www.lazarus-ide.org/">Lazarus</a> to edit Pascal code.

<p>To edit the Pascal unit you have various options:

<ul>
  <li><p>In CGE editor: enter the <code>code/</code> subdirectory, and double-click on a Pascal unit.

  <li><p>In CGE editor: use menu item <i>"Code -> Edit Unit ..."</i>.

  <li><p>In CGE editor: press F12 when some design is open. This will open the associated unit.

  <li><p>In Lazarus: open the project in Lazarus, and open units from Lazarus then. All Pascal files found on the search path are automatically part of the LPI project, they are visible in <i>Project Inspector</i> in Lazarus.
</ul>

<p>We use <a href="https://www.lazarus-ide.org/">Lazarus</a> by default as it features code-completion that understands Pascal syntax perfectly, integrated debugger and more. We also automatically set up Lazarus project files, so that you can press <i>Run</i> from Lazarus, and it will build (and run, debug) your project (for the current system).

<p>Note: If you're bothered by the default multiple-windows UI of Lazarus, install in Lazarus package <code>components/anchordocking/design/anchordockingdsgn.lpk</code> (you will find this in Lazarus source code). It will recompile Lazarus and give you an IDE with docked windows.

<p>Note: Instead of Lazarus, you can use any text editor to edit Pascal files. <a href="https://code.visualstudio.com/">Visual Studio Code</a>, <a href="https://atom.io/">Atom</a>, <a href="https://www.gnu.org/software/emacs/download.html">Emacs</a>... A preference option to make CGE editor invoke a custom editor will be added soon.

<!--
TODO: make editor configurable

TODO: use Delphi automatically, if installed
-->

<?php echo $toc->html_section(); ?>

<p>You can browse the application files. Our <i>"Files"</i> browser at the bottom just displays the files inside your project directory. It merely hides some known unimportant things, like temporary <code>castle-engine-output</code> directory.

<ul>
  <li><p>Note that the <code>data/</code> subdirectory is somewhat special. It is automatically detected, it is automatically packaged (e.g. in Android apk), and it can always be accessed by <a href="https://castle-engine.io/manual_data_directory.php">castle-data:/xxx URL</a>. You will place there 3D models, 2D images, designs (<code>xxx.castle-user-interface</code>, <code>xxx.castle-transform</code> files) and everything else you load in the game.

    <p>It is somewhat equivalent to Unity <code>Assets/</code> subdirectory. See <a href="https://github.com/castle-engine/castle-engine/wiki/Castle-Game-Engine-for-Unity-developers">Castle Game Engine for Unity developers</a> for more pointers, if you come with knowledge about Unity.

  <li><p>Note that your Pascal source code should be outside the <code>data/</code> subdirectory. Your source code can be anywhere within the project, we don't have any strict requirement here, although we recommend <code>code/</code> subdirectory and the compiler is set to search it by default. Remember to list any new code subdirectory in <code>&lt;search_paths&gt;</code> in <a href="https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples">CastleEngineManifest.xml</a> file (for now, just edit this file in any text editor; in the future CGE editor can allow to edit it through a GUI dialog).

    <!--
    If you really want, you can of course place source code in the `data/` subdirectory, but it usually doesn't make sense. Unless you really want to distribute to end-users your source code this way (but there are better ways to distribute source code, e.g. use _"Package Source"_).

    This is in contrast to Unity (that requires putting source code also inside `Assets/` directory).
    -->

  <li><p>Double-clicking on various files runs a tool suitable to preview/edit them:

    <ul>
      <li><p>On 3D and 2D models we run <a href="view3dscene.php">view3dscene</a>.
      <li><p>On images we run <a href="castle-view-image.php">castle-view-image</a>.
      <li><p>On Pascal files we run Lazarus (see above).
      <li><p>Design files are opened in the current editor.
      <li><p>On other files, we run the default system application for them.
    </ul>

  <li><p>Scenes, images and audio files have a <i>preview</i> window once you select them in the <i>Files</i> panel. You can even quickly listen to audio files this way.

  <li><p>Drag files from the <i>"Files"</i> area onto the <code>TCastleViewport</code> instance in a visual designer. This automatically creates a <code>TCastleScene</code> with the given scene loaded.
</ul>

<?php echo $toc->html_section(); ?>

<p>Projects may define custom components (descendants of the <?php api_link('TCastleUserInterface', 'CastleUIControls.TCastleUserInterface.html'); ?> or <?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?>). It is possible to include your custom components within the <i>Castle Game Engine Editor</i>, so that they can be added and configured at design-time, just like standard CGE components. To do this:

<ol>
  <li><p>In the <code>initialization</code> section of some unit (it is usually the same unit where you define your custom component), register it by calling something like this:

<?php echo pascal_highlight(
'RegisterSerializableComponent(TMyButton, \'My Button\');'); ?>

    <p>The <code>RegisterSerializableComponen</code> is in the <code>CastleComponentSerialize</code> unit, so make sure it is in the uses clause.

  <li><p>Inside your <a href="https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples">CastleEngineManifest.xml</a>, set the attribute <code>editor_units</code> to list all the units that call the <code>RegisterSerializableComponent</code>. It is a comma-separated list, like <code>editor_units="MyButtonUnit, MyMenuUnit"</code>.

  <li><p>Make sure:

    <ol>
      <li><p><i>Lazarus location</i> is correctly set in the editor <i>"Preferences"</i> window. Alternatively you could adjust <code>$PATH</code> (make sure we can find <code>lazbuild</code> from Lazarus).

      <li><p>Make sure the CGE location is correctly set. It should be detected automatically if you use the engine package (but you can always customize it using the environment variable <code>$CASTLE_ENGINE_PATH</code>).
    </ol>

  <li><p>Use menu item <i>"Project -> Restart Editor (With Custom Components)"</i> in the editor (once you open a project).

    <p>Alternatively, use the command-line <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">build tool</a> command: <code>castle-engine editor</code>.

    <p>Both ways will rebuild and run a customized version of the editor that includes your custom components.

    <p>You can confirm you are running an editor with custom components by looking at the window title, it should include <i>"(With Custom Components)"</i>.
</ol>

<p>See <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/advanced_editor">advanced_editor</a> as an example that defines and registers <code>TImageGrid</code> component in the <a href="https://github.com/castle-engine/castle-engine/blob/master/examples/advanced_editor/gamecontrols.pas">GameControls</a> unit.

<?php
manual_footer();
?>
