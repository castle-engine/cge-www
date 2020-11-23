<?php
require_once 'castle_engine_functions.php';
creating_data_header('Exporting from 3ds Max', array(
  'social_share_image' => 'export-3dsmax2.png',
));
?>

<p>Exporting 3ds Max models to Castle Game Engine can be done in many ways:</p>
<ul>
  <li><p>from FBX format to <a href="creating_data_model_formats.php#section_gltf">glTF</a> with the <a href="https://github.com/facebookincubator/FBX2glTF">FBX2glTF</a> tool</p>
  </li>
  <li><p>directly from MAX format to <a href="creating_data_model_formats.php#section_gltf">glTF</a> with <a href="https://github.com/BabylonJS/Exporters/">Babylon glTF</a> plugin</p>
  </li>
  <li><p>directly from MAX format to VRML format</p>
  </li>
</ul>

<h2>Export from FBX format to glTF (FBX2glTF)</h2>

<p>If you received assets from a graphic designer, and you aren't an expert in 3ds MAX, the easiest option is to use files in the FBX format and the FBX2glTF tool (most often, in addition to the .max file, you will also get a .fbx file). In this case, there is a high probability that you will get a good conversion result without any additional work (the graphic designer adjusted the model when he created FBX file).</p>

<p>The export itself is very simple. Just call FBX2glTF tool in the console with one parameter - FBX model file name:</p>

<code>
FBX2glTF model.fbx
</code>

<p>After the export, the modelname_out subfolder will appear with the exported model.</p>

<p>The conversion result can be easily compared using <a href="https://castle-engine.io/view3dscene.php">view3dcene</a> and <a href="https://www.autodesk.com/products/fbx/fbx-review">FBX Review</a>.</p>

<h2>Export to glTF format with Babylon glTF</h2>

<p>If the FBX2glTF conversion effect is unsatisfactory or you are a graphic designer, a good solution is to use the <a href="https://github.com/BabylonJS/Exporters/">Babylon glTF</a> plugin. In this case, the model will need to be adjusted before performing the conversion.</p>

<p>The most important changes are:</p>
<ul>
  <li>reduction of Bone Affect Limit to 4
  </li>
  <li>preparation of textures in Bitmap format
  </li>
</ul>

<h3>Plugin installation</h3>
<p>Babylon glTF plugin comes with a handy installer, which you can download from <a href="https://github.com/BabylonJS/Exporters/releases">Babylon Exporter Github Releases</a> page. The installation itself consists in clicking the install button.</p>

<?php
echo castle_thumbs(array(
  array('filename' => 'export-3ds_babylon_installer.png', 'titlealt' => 'Babylon glTF Installer'),
  array('filename' => 'export-3ds_babylon_installer_plugin_installed.png', 'titlealt' => 'Babylon glTF plugin installed'),
), 'auto', 'left');
?>

<h3>Import sample</h3>
<p>The steps to be followed depend largely on the imported model. Screenshots below can be used only as an overview not full tutorial.</p>

<?php
echo castle_thumbs(array(
  array('filename' => 'export-3ds_babylon_01_select_mesh.png', 'titlealt' => 'Select Mesh'),
  array('filename' => 'export-3ds_babylon_02_click_modify_tab.png', 'titlealt' => 'Open Modifier list'),
  array('filename' => 'export-3ds_babylon_03_open_modifier_list_and_click_skin.png', 'titlealt' => 'Click skin'),
  array('filename' => 'export-3ds_babylon_04_open_advanced_parameters.png', 'titlealt' => 'Open advanced parameters'),
  array('filename' => 'export-3ds_babylon_05_change_bone_affect_limit_to_4.png', 'titlealt' => 'Change bone affect limit to 4'),
  array('filename' => 'export-3ds_babylon_06_check_material_settings.png', 'titlealt' => 'Open material editor'),
  array('filename' => 'export-3ds_babylon_07_textures_must_be_bitmap_type.png', 'titlealt' => 'Textures need be Bitmap type'),
  array('filename' => 'export-3ds_babylon_08_textures_must_be_bitmap_type_changed.png', 'titlealt' => 'Textures need be Bitmap type'),
  array('filename' => 'export-3ds_babylon_081_textures_must_be_bitmap_type_changed.png', 'titlealt' => 'The same in second material editor, Glossines adjusted to 3'),
  array('filename' => 'export-3ds_babylon_09_run_exporter.png', 'titlealt' => 'Run exporter'),
  array('filename' => 'export-3ds_babylon_10_choose_format_gltf_or_glb.png', 'titlealt' => 'Choose gltf or glb'),
  array('filename' => 'export-3ds_file_loaded_in_3dsmax_and_adjusted_to_export_by_babylon_gltf.png', 'titlealt' => 'Result in view3dscene'),
), 'auto', 'left');
?>

<h3>Export differences</h3>
<p>Using different exporters gives slightly different results, it's always worth experimenting. You can also try to load the FBX file into 3ds MAX and save it with the Babylon glTF plugin.</p>

<?php
echo castle_thumbs(array(
  array('filename' => 'export-fbx_exported_by_fbx2gltf.png', 'titlealt' => 'fbx file exported to glTF by FBX2glTF'),
  array('filename' => 'export-3ds_file_loaded_in_3dsmax_and_adjusted_to_export_by_babylon_gltf.png', 'titlealt' => 'MAX file adjusted in 3ds Max and exported by Babylon glTF'),
  array('filename' => 'export-3ds_fbx_loaded_in_3dsmax_and_saved_by_babylon_gltf.png', 'titlealt' => 'FBX file loaded in 3ds Max and exported by Babylon glTF'),
  array('filename' => 'export-3ds_max_file_adjusted_and_saved_as_fbx_then_exported_by_fbx2gltf.png', 'titlealt' => 'MAX file adjusted in 3ds Max and saved as FBX, then exported to glTF by FBX2glTF'),
), 'auto', 'left');
?>

<h3>Plugin updating</h3>

<p>The Babylon glTF exporter is being actively developed. It is worth checking the possibility of updating the plugin from time to time by running the installer. When new plugin version is available  the Update button will be showed.</p>

<h3>More info</h3>

<p>See <a href="https://doc.babylonjs.com/resources/3dsmax_to_gltf">3DSMax to glTF</a> and <a href="https://doc.babylonjs.com/resources/3dsmax">Babylon 3DSMax resources</a> pages for more info.

<h2>Old approach: VRML</h2>

<p>To export from older <i>3ds Max</i>, it's easiest to export to the <i>VRML 97</i> format (extension <code>.wrl</code>). <i>VRML</i> is an older version of <i>X3D</i>, it's basically a subset of X3D, and we support it 100% in the engine.</p>

<p>Choose the <i>Export</i> option from the main menu, then change the file type to <i>VRML97</i>, as seen on the screenshots on this page. A window with <i>VRML97 Exporter</i> settings will appear, at the beginning you can just accept the defaults &mdash; they are sensible.</p>

<?php
echo castle_thumbs(array(
  array('filename' => 'export-3dsmax2.png', 'titlealt' => '3ds Max Export menu'),
  array('filename' => 'export-3dsmax3.png', 'titlealt' => '3ds Max Export file dialog - choose VRML97'),
  array('filename' => 'export-3dsmax4.png', 'titlealt' => '3ds Max VRML97 Exporter options'),
  array('filename' => 'export-3dsmax5-view3dscene.png', 'titlealt' => 'View in view3dscene model exported from 3ds Max. Model By raptorus | From http://opengameart.org/content/golemmarine'),
), 'auto', 'left');
?>

<p>Notes:

<ul>
  <!-- NOPE. li>Instead of <i>Export</i>, you can also choose <i>Export Selected</i>. It works using the same exporter, so it works equally well.
  -->

  <li><p>To have the textures correctly picked up, you have to set the "<i>Bitmap URL Prefix -&gt; Use Prefix</i>". It should be the directory, relative to the <i>.wrl</i> file location, where the textures lie.</p>
  </li>

  <li><p>The exporter by default does not export normals, and it also does not set the <code>creaseAngle</code> field in VRML, leaving it at zero. This means that everything will have <i>flat shading</i>. The fix this, it's easiest to check "<i>Normals</i>" at exporting.</p>

    <p>You could also edit the VRML file afterwards, or edit the VRML/X3D node graph when loading, to set <code>creaseAngle</code> field at <code>IndexedFaceSet</code> to something non-zero, to have auto-generated smooth normals. <code>creaseAngle</code> is in radians, e.g. value 4 (greater than Pi) will make a completely smooth model. But that's usually more involved.</p>
  </li>
</ul>

<h2>Other formats</h2>
<p>Note that we also support some other file formats that you could export from 3ds Max, like <i>OBJ</i> and <i>3DS</i>. They work fine to export simple mesh with textures, but these format don't support animations, and many other advanced features. So it's better to use glTF.

<!-- TODO: check animations? -->

<!-- TODO: add level placeholders type="3dsmax"? -->

<?php
creating_data_footer();
?>
