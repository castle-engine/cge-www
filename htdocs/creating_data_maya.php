<?php
require_once 'castle_engine_functions.php';
creating_data_header('Exporting from Maya', array(
  'social_share_image' => 'export-maya-hextraction1.png',
));
?>

<p>Exporting Maya models to Castle Game Engine can be done in many ways:</p>
<ul>
  <li><p>from FBX format to <a href="creating_data_model_formats.php#section_gltf">glTF</a> with the <a href="https://github.com/facebookincubator/FBX2glTF">FBX2glTF</a> tool</p>
  </li>
  <li><p>directly from Maya to <a href="creating_data_model_formats.php#section_gltf">glTF</a> with <a href="https://github.com/BabylonJS/Exporters/">Babylon glTF</a> plugin</p>
  </li>
  <li><p>directly from Maya to <a href="creating_data_model_formats.php#section_gltf">glTF</a> with <a href="https://github.com/iimachines/Maya2glTF">Maya2glTF</a> script</p>
  </li>
  <li><p>directly from Maya to VRML format (old approach)</p>
  </li>
</ul>

<h2>Export from FBX format to glTF (FBX2glTF)</h2>

<p>If you received assets from a graphic designer, and you aren't a Maya expert, the easiest option is to use files in the FBX format and the FBX2glTF tool (most often, in addition to the .ma file, you will also get a .fbx file). In this case, it's a high probability that you will get a good conversion result without any additional work (the graphic designer adjusted the model when he created FBX file).</p>

<p>The export itself is very simple. Just call FBX2glTF tool in the console with one parameter - FBX model file name:</p>

<code>
FBX2glTF model.fbx
</code><br><br>

<p>After the export, the modelname_out subfolder will appear with the exported model.</p>

<p>The conversion result can be easily compared using <a href="https://castle-engine.io/view3dscene.php">view3dcene</a> and <a href="https://www.autodesk.com/products/fbx/fbx-review">FBX Review</a>.</p>

<h2>Export to glTF format with Babylon glTF</h2>

<p>If the FBX2glTF conversion effect is unsatisfactory or you are a graphic designer, a good solution is to use the <a href="https://github.com/BabylonJS/Exporters/">Babylon glTF</a> plugin. In this case, the model will need to be adjusted before performing the conversion.</p>

<h3>Plugin installation</h3>
<p>Babylon glTF plugin comes with a handy installer, which you can download from <a href="https://github.com/BabylonJS/Exporters/releases">Babylon Exporter Github Releases</a> page. The installation itself consists in clicking the install button.</p>

<?php
echo castle_thumbs(array(
  array('filename' => 'export-maya_babylon_installer.png', 'titlealt' => 'Babylon glTF Installer'),
  array('filename' => 'export-maya_babylon_installer_plugin_installed.png', 'titlealt' => 'Babylon glTF plugin installed'),
  array('filename' => 'export-maya_babylon_01_open_plugin_manager.png', 'titlealt' => 'Open Plug-in Manager'),
  array('filename' => 'export-maya_babylon_02_plugin_manager_check_load_on_maya2babylon.nll.dll.png', 'titlealt' => 'Check Load on Maya2Babylon.nkk.dll'),
), 'auto', 'left');
?>

<h3>Import sample</h3>
<p>The steps to be followed depend largely on the imported model. Screenshots below can be used only as an overview not full tutorial.</p>

<?php
echo castle_thumbs(array(
  array('filename' => 'export-maya_babylon_03_open_material_editor.png', 'titlealt' => 'Open material editor'),
  array('filename' => 'export-maya_babylon_04_show_input_output_connections.png', 'titlealt' => 'Select material and show input output connections'),
  array('filename' => 'export-maya_babylon_05_change_material_to_aiStandardSurface_and_connect_textures.png', 'titlealt' => 'Add aiStandardSurface material (Arnold Renderer) and connect textures. Other materials have glitches in my model case.'),
  array('filename' => 'export-maya_babylon_06_run_exporter.png', 'titlealt' => 'Run exporter'),
  array('filename' => 'export-maya_babylon_07_configure_and_export_file.png', 'titlealt' => 'Change format to gltf/glb, select output file and click Export.'),
  array('filename' => 'export-maya_file_adjusted_and_exported_by_babylon.png', 'titlealt' => 'Result in view3dscene'),
), 'auto', 'left');
?>

<h3>Plugin updating</h3>

<p>The Babylon glTF exporter is being actively developed. It is worth checking the possibility of updating the plugin from time to time by running the installer. When new plugin version is available  the Update button will be showed.</p>

<h3>More info</h3>

<p>See <a href="https://doc.babylonjs.com/resources/maya_to_gltf">Maya to glTF</a> and <a href="https://doc.babylonjs.com/resources/maya">Babylon Maya resources</a> pages for more info.</p>

<h2>Export to glTF format with Maya2glTF script</h2>

<p>If you have problems with Babylon glTF, you can try Maya2glTF script. To install run <code>maya2gltfDeploy.bat</code>. After loading the model, just type <code>maya2glTF_UI</code> in the script window to display the export UI. <br><br> For more info see <a href="https://github.com/iimachines/Maya2glTF/">Maya2glTF Github page</a>.</p>

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
