<?php
require_once 'castle_engine_functions.php';
castle_header('Editor');

$toc = new TableOfContents(
  array(
    new TocItem('Video Introduction', 'video'),
    new TocItem('Create, build, run projects', 'projects'),
    new TocItem('Design user interfaces and 3D/2D transformation hierarchies', 'design'),
    new TocItem('Edit source code', 'source_code'),
    new TocItem('File browser', 'file_browser'),
    new TocItem('Custom components in the editor', 'custom_components'),
  )
);
?>

<?php echo $toc->html_toc(); ?>

<p><i>Castle Game Engine Editor</i> is the most important visual tool of our engine.
It allows to create, edit and build applications with CGE.
Get it by <a href="/">downlading latest Castle Game Engine release</a>
and run <code>castle-editor</code> executable in the <code>bin</code> subdirectory.

<?php echo $toc->html_section(); ?>

<iframe width="560" height="315" src="https://www.youtube.com/embed/zdwN4mdQG_8" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<?php echo $toc->html_section(); ?>

<p>You can create a project (using a number of templates) or open an existing one.
You can compile, run and package the project using the <i>Run</i> menu.

<p>A <i>Castle Game Engine</i> project is a directory
containing the <a href="https://castle-engine.io/project_manifest">CastleEngineManifest.xml</a>
file. Most often it also contains a subdirectory <code>data/</code>
that is accessed from code using the
<a href="manual_data_directory.php">castle-data:/xxx</a> URLs.
Everything else is up to you, organize your source code and data however you like.

<p>Note that compiling and packaging a project <a href="https://castle-engine.io/build_tool">can also be done using the command-line CGE build tool</a>.
Actually, the editor just calls the build tool under the hood.
The build tool in turn calls a compiler (FPC) and some other tools
(e.g. Android-specific packaging tools).

<?php echo $toc->html_section(); ?>

<p>You can visually design:

<ol>
  <li><p>A hierarchy of user-interface controls. Anything descending from <?php api_link('TCastleUserInterface', 'CastleUIControls.TCastleUserInterface.html'); ?>, like a button, label, or a powerful viewport (that contains a hierarchy of 3D / 2D scenes and transformations inside).

    <p>Saved as <code>xxx.castle-user-interface</code> files (somewhere in the <code>data/</code> subdirectory).

    <p>They are typically loaded in your application by setting the <?php api_link('TUIState.DesignUrl', 'CastleUIState.TUIState.html#DesignUrl'); ?> (see almost any engine example or "New Project" template). Other approaches are possible too, e.g. you can load using <?php api_link('UserInterfaceLoad', 'CastleUIControls.html#UserInterfaceLoad'); ?>, <code>TSerializedComponent.UserInterfaceLoad</code> and more. See examples like <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/advanced_editor/advanced_loading_designs">advanced_editor/advanced_loading_designs</a>.

  <li><p>A hierachy of 3D / 2D scenes and transformations. Anything descending from <?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?>.

    <p>Saved as <code>xxx.castle-transform</code> files (somewhere in the <code>data/</code> subdirectory).

    <p>You can load it in your game using <?php api_link('TransformLoad', 'CastleTransform.html#TransformLoad'); ?> and insert into existing hierarchy of <code>TCastleViewport.Items</code>. You can also use <a href="https://castle-engine.io/apidoc-unstable/html/CastleTransformExtra.TCastleTransformDesign.html">TCastleTransformDesign</a> to use it in other designs (thus having a reusable composition of 3D/2D objects). See examples like <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/advanced_editor/advanced_loading_designs">advanced_editor/advanced_loading_designs</a>.

  <li><p>A hierachy of non-visual classes (really anything descending from <a href="https://www.freepascal.org/docs-html/rtl/classes/tcomponent.html">TComponent</a> although we advise to descend from our extended <?php api_link('TCastleComponent', 'CastleClassUtils.TCastleComponent.html'); ?>).

    <p>Saved as <code>xxx.castle-component</code> files (somewhere in the <code>data/</code> subdirectory).

    <p>You can load it in your game using <?php api_link('ComponentLoad', 'CastleComponentSerialize.html#ComponentLoad'); ?>. Do whatever you want with the resulting components. You can find the named components in your design using the <a href="https://castle-engine.io/apidoc-unstable/html/CastleComponentSerialize.TComponentHelper.html#FindRequiredComponent">FindRequiredComponent</a> method, like this:

<?php echo pascal_highlight(
'var
  ComponentRoot, ComponentOwner: TComponent;
  MySound: TCastleSound;
  MyFont: TCastleFont;
begin
  ComponentOwner := TComponent.Create(Application);
  ComponentRoot := ComponentLoad(\'castle-data:/my_design.castle-component\', ComponentOwner);
  MySound := ComponentOwner.FindRequiredComponent(\'MySound\') as TCastleSound;
  MyFont := ComponentOwner.FindRequiredComponent(\'MyFont\') as TCastleFont;
end;'); ?>

</ol>

<p>The <code>xxx.castle-user-interface</code>, <code>xxx.castle-transform</code>, <code>xxx.castle-component</code> are simple JSON text files. You should commit them to the version control, just like your source code. You can have as many such files inside your project as you need to.

<p>Let us emphasize that <i>when using the editor, you still code using the same CGE API as described throughout this manual</i>. At any point you can load an instance of a component from a designed file and use it as you wish.

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

<?php
echo castle_thumbs(array(
  array('filename' => 'custom-code-editor.png', 'titlealt' => 'Code Editor Preferences'),
));
?>

<p>Note: Instead of Lazarus, you can use any text editor to edit Pascal files. <a href="https://code.visualstudio.com/">Visual Studio Code</a>, <a href="https://atom.io/">Atom</a>, <a href="https://www.gnu.org/software/emacs/download.html">Emacs</a>... In CGE editor, go to <i>"Preferences -> Code Editor"</i> to configure your custom editor, so it is run when you e.g. double-click on Pascal files from the editor or use various other <i>"Code"</i> menu features. See <a href="https://castle-engine.io/wp/2021/05/16/use-custom-code-editor-emacs-atom-vs-code-when-opening-pascal-source-from-cge-editor/">here for details</a>.

<!--
TODO: make editor configurable

TODO: use Delphi automatically, if installed
-->

<?php echo $toc->html_section(); ?>

<p>You can browse the application files. Our <i>"Files"</i> browser at the bottom just displays the files inside your project directory. It merely hides some known unimportant things, like temporary <code>castle-engine-output</code> directory.

<ul>
  <li><p>Note that the <code>data/</code> subdirectory is somewhat special. It is automatically detected, it is automatically packaged (e.g. in Android apk), and it can always be accessed by <a href="https://castle-engine.io/manual_data_directory.php">castle-data:/xxx URL</a>. You will place there 3D models, 2D images, designs (<code>xxx.castle-user-interface</code>, <code>xxx.castle-transform</code>, <code>xxx.castle-component</code> files) and everything else you load in the game.

    <p>It is somewhat equivalent to Unity <code>Assets/</code> subdirectory. See <a href="https://castle-engine.io/castle_game_engine_for_unity_developers">Castle Game Engine for Unity developers</a> for more pointers, if you come with knowledge about Unity.

  <li><p>Note that your Pascal source code should be outside the <code>data/</code> subdirectory. Your source code can be anywhere within the project, we don't have any strict requirement here, although we recommend <code>code/</code> subdirectory and the compiler is set to search it by default. Remember to list any new code subdirectory in <code>&lt;search_paths&gt;</code> in <a href="https://castle-engine.io/project_manifest">CastleEngineManifest.xml</a> file (for now, just edit this file in any text editor; in the future CGE editor can allow to edit it through a GUI dialog).

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

<p>You can register your own components to use them within CGE editor. This is a powerful mechanism to define e.g. your own user interface elements. See the <a href="doc/custom_components">documentation about custom components</a>.

<?php
castle_footer();
?>
