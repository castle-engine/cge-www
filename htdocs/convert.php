<?php
require_once 'castle_engine_functions.php';
castle_header('Online Converter for 3D and 2D Models', array(
  'social_share_image' => 'castle-model-viewer_gltf_helmet.png',
  'meta_description' => 'Online and free converter for many 3D model formats. As input supports a lot of 3D and 2D formats (everything supported by Castle Game Engine), like glTF, X3D, VRML, OBJ, STL, Collada, 3DS, Spine JSON. As output, supports X3D and STL.',
  'meta_keywords' => 'convert, glTF, OBJ, 3DS, X3D, VRML, STL',
));

/* TODO:

Button "Make a screenshot"

    Makes a screenshot from the 1st viewpoint found in your scene.

    (Allows to also choose a predefined "best fit" viewpoint, like castle-model-viewer)

    How to set a viewpoint?

    * Open the model in <a>castle-model-viewer</a>, choose preferred navigation method (Examine, Walk), position camera as you like, and use "Clipboard->Print Camera Xxx".
    * Paste the copied Viewport at the top of your your VRML or X3D file.  (If it's not an VRML or X3D, you can convert it to VRML or X3D, using the form on this page... on using castle-model-viewer "File -> Save As").

    Checkbox: Try to download all linked resources, necessary to correctly display the model (textures, inline 3D models...). They must be specified inside the file as http:/ or https:/ URLs.

    Generates 4 screens: png/jpg large/small

    *The generated screenshots do not have any watermark, logo or other attempts at promotion from us.* If you use our tools, we would appreciate it if you mention us, but it's your choice.
*/
?>

<div class="single-column-page">

<?php echo pretty_heading('Online Converter for 3D and 2D Models'); ?>

<ul>
  <li>
    <p><b>Convert from</b> <a href="creating_data_model_formats.php">any model format supported by Castle Game Engine: glTF, X3D, VRML, Wavefront OBJ, STL, Collada, 3DS, MD3, Spine JSON and more</a>.

  <li>
    <p><b>Convert to</b> X3D and STL. <i>We plan to add glTF as an additional output format soon.</i>
</ul>

<p>Can also be used to <b>convert between X3D encodings (classic, XML)</b> and pretty-print X3D and VRML models.

  <!--li><p>You can also use this for a <b>basic validation</b>. Although we don't focus on validation, but in practice (since we need to understand the input) we perform a lot of checks whether the input makes sense.-->

<form action="convert-output.php" method="post" enctype="multipart/form-data">
  <div class="convert-form jumbotron">
    <div class="input-group">
      <input type="file" class="form-control" multiple name="input-file[]">
      <span class="input-group-btn">
        <input type="submit" value="Convert" class="btn btn-primary">
      </span>
    </div><!-- /input-group -->
    <div class="output-format-group">
      <p class="output-format-title">Output:</p>
      <div class="output-format-radio">
        <p><input type="radio" id="output-format-x3d-xml" name="output-format" value="x3d-xml" checked><label for="output-format-xml">X3D, XML encoding (.x3d extension)</label></p>
        <p><input type="radio" id="output-format-x3d-classic" name="output-format" value="x3d-classic"><label for="output-format-classic">X3D, classic encoding (.x3dv extension)</label></p>
        <p><input type="radio" id="output-format-stl" name="output-format" value="stl"><label for="output-format-stl">STL, binary (.stl extension)</label></p>
      </div>
    </div><!-- /input-group -->
    <div class="convert-patreon">
      <a class="btn btn-success btn-lg btn-patreon" href="<?php echo PATREON_URL; ?>"><span class="glyphicon glyphicon-heart" aria-hidden="true"></span> Support on Patreon</a>
    </div>
  </div>
</form>

<p>This tool is completely free (for any usage, commercial or not). If you find it useful <a href="https://www.patreon.com/castleengine">please support me on Patreon</a>. <!--Every month, I commit time to developing features solely requested by Patreons.-->

<p>Advanced usage notes:

<ul>
  <li><p>You <b>can upload multiple files</b> (select them with <i>Ctrl</i> key in the open dialog) if it makes sense for the given model format. E.g. upload glTF JSON + optional binary data, or Wavefront OBJ + optional MTL file.

  <li><p>This tool is <b>free and open-source software</b>. It is a set of scripts (<a href="https://github.com/castle-engine/cge-www/tree/master/online-model-converter">PHP, Docker and shell scripts</a>) using <a href="/">Castle Game Engine</a> and <a href="castle-model-converter">castle-model-converter</a> under the hood.

  <li><p>This online converter has some limits on the uploaded file size and conversion time. If needed <b>you can just download <a href="castle-model-viewer">castle-model-viewer</a> or <a href="castle-model-converter">castle-model-converter</a> and perform the same conversion on your own computer</b>, without any limits. You can also generate screenshots or perform mass conversion from your scripts this way.

  <li><p>The models you upload here, and the resulting output, are not stored on the server longer than absolutely necessary (to perform the conversion, and then the output is kept for 15 minutes to allow you download it). <b>Your models remain yours, we don't keep them.</b>

  <li><p>If you experience any problems, please <a href="talk.php">contact us</a>. You can submit bugs to the <a href="https://github.com/castle-engine/castle-model-viewer/issues">castle-model-viewer bugtracker</a>. There is no warranty.
</ul>

</div>

<?php
castle_footer();
?>
