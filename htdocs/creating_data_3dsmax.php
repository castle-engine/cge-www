<?php
require_once 'castle_engine_functions.php';
creating_data_header('Exporting from 3ds Max', array(
  'social_share_image' => 'export-3dsmax2.png',
));
?>

<p>To export from <i>3ds Max</i>, it's easiest to export to the <i>VRML 97</i> format (extension <code>.wrl</code>). <i>VRML</i> is an older version of <i>X3D</i>, it's basically a subset of X3D, and we support it 100% in the engine.</p>

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

<p>Note that we also support some other file formats that you could export from 3ds Max, like <i>OBJ</i> and <i>3DS</i>. They work fine to export simple mesh with textures, but these format don't support animations, and many other advanced features. So it's better to use VRML.

<!-- TODO: check animations? -->

<!-- TODO: add level placeholders type="3dsmax"? -->

<?php
creating_data_footer();
?>
