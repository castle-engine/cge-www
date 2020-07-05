<?php
require_once 'castle_engine_functions.php';
creating_data_header('Exporting from Maya', array(
  'social_share_image' => 'export-maya-hextraction1.png',
));
?>

<h2>New approach: glTF</h2>

<p>Use the Babylon glTF exporter, see <a href="https://doc.babylonjs.com/resources/maya_to_gltf">Maya to glTF</a> and <a href="https://doc.babylonjs.com/resources/maya">Babylon Maya resources</a>.

<p>This should allow you to export to <a href="creating_data_model_formats.php#section_gltf">glTF 2.0</a>, a format for which we have powerful support in CGE.

<h2>Old approach: OBJ</h2>

<p><i>Maya</i> doesn't support exporting to X3D now. The <a href="http://rawkee.sourceforge.net/">RawKee</a> project developed Maya plugins to add X3D export, but their plugins are only for the older Maya versions (&lt;= 2008).

<p>The simplest option to export static meshes to our engine from the latest Maya version is to export as an <i>OBJ</i> format.

<ul>
  <li>You may need to enable it first, by going to the <i>Windows -&gt; Settings/Preferences -&gt; Plug-in Manager</i>. Find the <code>objExport.mll</code> on the list, and check <i>Loaded</i> (check <i>Auto load</i> too, to have it active always).
  <li>Then you can export using the <i>File -&gt; Export All...</i> menu item, and choosing <i>OBJExport</i> as the file type.
  <li>See the screenshots below for help.
</ul>

<?php
echo castle_thumbs(array(
  array('filename' => 'export-maya1.png', 'titlealt' => 'Maya Plug-in Manager menu'),
  array('filename' => 'export-maya2.png', 'titlealt' => 'Maya Plug-in Manager window'),
  array('filename' => 'export-maya3.png', 'titlealt' => 'Maya Export All menu'),
  array('filename' => 'export-maya4.png', 'titlealt' => 'Maya Export All dialog'),
  array('filename' => 'export-maya5-view3dscene.png', 'titlealt' => 'View in view3dscene city model exported from Maya. Model By Keenan Crane | From http://opengameart.org/content/abstract-city'),
), 'auto', 'left');
?>

<p>If the exported things seem completely black, fix the material to have non-black diffuse color. (I don't know why Maya sets diffuse color to black on my test models...) You can edit the material in <a href="view3dscene.php">view3dscene</a>: open the OBJ file, select the object with <i>Ctrl + right mouse click</i>, and use <i>Edit -&gt; Edit Material -&gt; Diffuse Color</i> menu item. You can save the resulting file as X3D using view3dscene <i>File -&gt; Save As VRML/X3D</i> menu item.

<p>Bonus points: OBJ format supports normalmaps, so we will read the normalmaps you set in Maya correctly.

<p>To export animations, you can export each keyframe as a still OBJ file, and then create a <a href="castle_animation_frames.php">simple XML file with the .castle-anim-frames extension to define an animation</a>.

<?php
echo castle_thumbs(array(
  array('filename' => 'export-maya-hextraction1.png', 'titlealt' => 'Maya Sample Model By Hextraction | Base Player Pod by ComboMash Entertainment Inc -- http://www.hextraction.com/ | From http://opengameart.org/content/hextraction-base-player-pod'),
  array('filename' => 'export-maya-hextraction2-view3dscene.png', 'titlealt' => 'View in view3dscene Hextraction model exported from Maya'),
), 'auto', 'left');
?>

<!--
that added excellent capability:

You can download <code>rawkee-1.1.0-windows-maya2008.exe</code> installer from the <a href="https://sourceforge.net/projects/rawkee/files/1%29%20Windows%20Maya%204.5-2008/RawKee%201.1.0%20Beta%20for%20Maya%202008%20on%20Windows/">RawKee 1.1.0 Beta for Maya 2008</a> (this is the latest plugin version).
-->

<?php
creating_data_footer();
?>
