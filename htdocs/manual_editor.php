<?php
require_once 'castle_engine_functions.php';
manual_header('Editor');
?>

<p>Since version 6.5, <i>Castle Game Engine</i> includes an <i>Editor</i>.
Run it using the <code>castle-editor</code> executable in the <code>bin</code>
subdirectory of the <a href="/">latest Castle Game Engine release</a>.

<p>The editor
<!-- is an integrated environment to design and build applications using Castle Game Engine. In practical terms, this means that: -->
allows to:

<ol>
  <li><p><b>Create and build CGE projects</b>.

    <p>A <i>Castle Game Engine</i> project is just a directory
    containing the <a href="https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples">CastleEngineManifest.xml</a>
    file. Most often it also contains a subdirectory <code>data/</code>
    that is accessed from code using the
    <a href="manual_data_directory.php">castle-data:/xxx</a> URLs.
    Everything else is up to you, organize your source code and data
    however you like.

    <p>From the editor you can create a project using a number of templates.
    You can compile, run and package the project using the <i>Run</i> menu.

    <p>Note that compiling and packaging a project <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">can also be done using the command-line CGE build tool</a>.
    Actually, the editor just calls the build tool under the hood.
    The build tool in turn calls a compiler (FPC) and some other tools
    (e.g. Android-specific packaging tools).

  <li><p><b>Design user interfaces and 3D/2D transformation hierarchies</b>.

    <p>A <i>design</i> is a hierarchy of components descending from
    <a href="https://castle-engine.io/apidoc-unstable/html/CastleUIControls.TCastleUserInterface.html">TCastleUserInterface</a>
    (for user interfaces)
    or
    <a href="https://castle-engine.io/apidoc-unstable/html/CastleTransform.TCastleTransform.html">TCastleTransform</a>
    (for 3D and 2D worlds).
    Each design is saved into a file with extension <code>.castle-user-interface</code>
    or <code>.castle-transform</code>
    and can be loaded at any point in your game using
    the
    <a href="https://castle-engine.io/apidoc-unstable/html/CastleComponentSerialize.html#UserInterfaceLoad">UserInterfaceLoad</a>
    or
    <a href="https://castle-engine.io/apidoc-unstable/html/CastleComponentSerialize.html#TransformLoad">TransformLoad</a>
    functions.
    E.g. <code>UserInterfaceLoad('castle-data:/my-ui.castle-user-interface')</code>.

    <p>You can also use
    <a href="https://castle-engine.io/apidoc-unstable/html/CastleComponentSerialize.TSerializedComponent.html">TSerializedComponent</a>
    class to efficiently load a design once, and instantiate it many times.
    There is also
    <a href="https://castle-engine.io/apidoc-unstable/html/CastleUIState.TUIState.html#InsertUserInterface">TUIState.InsertUserInterface</a>,
    comfortable to insert UI into
    <a href="https://castle-engine.io/apidoc-unstable/html/CastleUIState.TUIState.html">TUIState</a>
    (which is typically used like a form in Lazarus/Delphi: each TUIState
    represents a functional user interface).

    <p>Everything related to designing is in the editor <i>Design</i> menu (once you open any project). You can open any example project from the engine designed using the editor (e.g. <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/tiled/strategy_game_demo">examples/tiled/strategy_game_demo</a>) and play around with it.

  <li><p><b>Browse the files in your project</b>. Double-click on files in the <i>file browser</i> at the bottom of the editor to:

    <ul>
      <li><p>Open <a href="creating_data_model_formats.php">model</a> in <a href="view3dscene.php">view3dscene</a>,
      <li><p>Open image in <a href="glviewimage.php">glViewImage</a>,
      <li><p>Open design in the current editor,
      <li><p>Open other files in the default associated application.
    </ul>

    <p>The tools, like <a href="view3dscene.php">view3dscene</a> and <a href="glviewimage.php">glViewImage</a> are already distributed with CGE, so you don't have to install anything additional. Just double-click from the editor and it will run a suitable viewer.
</ol>

<p><a href="https://github.com/castle-engine/castle-engine/blob/master/tools/castle-editor/README.md">More detailed documentation about the editor is here.</a>

<p>See also the movie about the editor:

<p><iframe width="560" height="315" src="https://www.youtube.com/embed/podM_zBXGEg" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<p>The source code of the editor is inside
<a href="https://github.com/castle-engine/castle-engine/tree/master/tools/castle-editor">tools/castle-editor</a>
subdirectory of sources. You can compile it yourself from Lazarus.

<?php
manual_footer();
?>
