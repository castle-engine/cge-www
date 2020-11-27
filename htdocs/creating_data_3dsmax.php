<?php
require_once 'castle_engine_functions.php';
creating_data_header('Exporting from 3ds Max', array(
  'social_share_image' => 'export-3dsmax2.png',
));

$toc = new TableOfContents(
  array(
    new TocItem('Export to FBX (if necessary) and convert FBX to glTF using FBX2glTF'),
    new TocItem('Export to glTF with Babylon'),
      new TocItem('Plugin installation', NULL, 1),
      new TocItem('Import sample', NULL, 1),
      new TocItem('Export differences', NULL, 1),
      new TocItem('Plugin updating', NULL, 1),
      new TocItem('More info', NULL, 1),
    new TocItem('Old approach: VRML'),
    new TocItem('Other formats'),
  )
);
?>

<p>Exporting <i>3ds Max</i> models to <i>Castle Game Engine</i> can be done in various ways.</p>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>If you received assets from a graphic designer in both the <code>.max</code> and <code>.fbx</code> formats, it is easiest to convert files from the FBX format using the <a href="https://github.com/facebookincubator/FBX2glTF">FBX2glTF</a> tool to <a href="creating_data_model_formats.php#section_gltf">glTF</a>. In this case, there is a high probability that you will get a good conversion result without any additional work, as the graphic designer adjusted the model when (s)he created FBX file.</p>

<p>You can also use built-in export from <i>3ds Max</i> to FBX, and then convert with <a href="https://github.com/facebookincubator/FBX2glTF">FBX2glTF</a>.

<p>The export itself is very simple. Just call FBX2glTF tool in the console with one parameter - FBX model file name:</p>

<pre>
FBX2glTF model.fbx
</pre>

<p>After the export, the <code>modelname_out</code> subfolder will appear with the exported model.</p>

<p>The conversion result can be easily compared using <a href="https://castle-engine.io/view3dscene.php">view3dcene</a> and <a href="https://www.autodesk.com/products/fbx/fbx-review">FBX Review</a>.</p>

<?php echo $toc->html_section(); ?>

<p>If the FBX2glTF conversion effect is unsatisfactory or you are a graphic designer and want a direct solution (without intermediate FBX file) at the cost of some additional setup, a good solution is to use the <a href="https://github.com/BabylonJS/Exporters/">Babylon glTF</a> plugin. In this case, the model will often need to be adjusted before performing the conversion.</p>

<p>The most important changes are:</p>
<ul>
  <li>reduction of <i>Bone Affect Limit</i> to 4
  </li>
  <li>preparation of textures in Bitmap format
  </li>
</ul>

<?php echo $toc->html_section(); ?>

<p>Babylon glTF plugin comes with a handy installer, which you can download from <a href="https://github.com/BabylonJS/Exporters/releases">Babylon Exporter Github Releases</a> page. The installation itself consists in clicking the install button.</p>

<?php
echo castle_thumbs(array(
  array('filename' => 'export-3ds_babylon_installer.png', 'titlealt' => 'Babylon glTF Installer'),
  array('filename' => 'export-3ds_babylon_installer_plugin_installed.png', 'titlealt' => 'Babylon glTF plugin installed'),
), 'auto', 'left');
?>

<?php echo $toc->html_section(); ?>

<p>The steps to be followed depend largely on the imported model. Screenshots below can be used as an overview.</p>

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

<?php echo $toc->html_section(); ?>

<p>Using different exporters gives slightly different results, it's always worth experimenting. You can also try to load the FBX file into 3ds MAX and save it with the Babylon glTF plugin.</p>

<?php
echo castle_thumbs(array(
  array('filename' => 'export-fbx_exported_by_fbx2gltf.png', 'titlealt' => 'fbx file exported to glTF by FBX2glTF'),
  array('filename' => 'export-3ds_file_loaded_in_3dsmax_and_adjusted_to_export_by_babylon_gltf.png', 'titlealt' => 'MAX file adjusted in 3ds Max and exported by Babylon glTF'),
  array('filename' => 'export-3ds_fbx_loaded_in_3dsmax_and_saved_by_babylon_gltf.png', 'titlealt' => 'FBX file loaded in 3ds Max and exported by Babylon glTF'),
  array('filename' => 'export-3ds_max_file_adjusted_and_saved_as_fbx_then_exported_by_fbx2gltf.png', 'titlealt' => 'MAX file adjusted in 3ds Max and saved as FBX, then exported to glTF by FBX2glTF'),
), 'auto', 'left');
?>

<?php echo $toc->html_section(); ?>

<p>The Babylon glTF exporter is being actively developed. It is worth checking the possibility of updating the plugin from time to time by running the installer. When new plugin version is available the <i>Update</i> button will be showed.</p>

<?php echo $toc->html_section(); ?>

<p>See <a href="https://doc.babylonjs.com/resources/3dsmax_to_gltf">3DSMax to glTF</a> and <a href="https://doc.babylonjs.com/resources/3dsmax">Babylon 3DSMax resources</a> pages for more info.

<?php echo $toc->html_section(); ?>

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

<?php echo $toc->html_section(); ?>

<p>Note that we also support some other file formats that you could export from 3ds Max, like <i>OBJ</i> and <i>3DS</i>. They work fine to export simple mesh with textures, but these format don't support animations, and many other advanced features. So it's better to use glTF.

<!-- TODO: add level placeholders type="3dsmax"? -->

<?php
creating_data_footer();
?>
