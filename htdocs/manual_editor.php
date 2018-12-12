<?php
require_once 'castle_engine_functions.php';
manual_header('Editor');
?>

<p>Since version 6.5, <i>Castle Game Engine</i> includes an <i>Editor</i>.
You can use the editor to design a user interface,
which is just a tree of <code>TCastleUserInterface</code> instances.
Such user interface can be saved to a file with extension <code>.castle-user-interface</code>
and later loaded in your game using a call like
<code>UserInterfaceLoad('castle-data:/my-ui.castle-user-interface')</code>.

<p>The editor can be used with any <i>Castle Game Engine</i> project
containing a
<a href="https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples">CastleEngineManifest.xml</a>
file. The editor has the same "concept" of a project as our command-line
<a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">build tool</a>,
in fact the editor can also be used to compile and run the project.

<p><a href="https://github.com/castle-engine/castle-engine/blob/master/tools/castle-editor/README.md">More detailed documentation about the editor is here.</a>

<p>See also the movie explaining th editor:

<p><iframe width="560" height="315" src="https://www.youtube.com/embed/podM_zBXGEg" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<?php
manual_footer();
?>
