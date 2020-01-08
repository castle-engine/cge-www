<?php

/* Show error.

   Note that $error_message is assumed to be already-safe HTML,
   it is not sanitized here anymore.

   $conversion_log may be NULL if empty. Otherwise it will output (after sanitization).
*/
function output_error($error_message, $conversion_log)
{
  echo '<p><b>Error:</b> <i>' . $error_message . '</i></b>';

  if (!empty($conversion_log)) {
    ?>
    <br>
    <a id="toggle-details" href="#">Click to toggle the details.</a>
    <!-- details are by default visible in this case -->
    <pre id="details"><?php echo htmlspecialchars($conversion_log); ?></pre>
    <?php
  }
}

/* Show success.

   $output_file_id (string) is an id for convert-download.php?id=xxx.

   $output_file_suggested_name (string) is a suggested name, also for convert-download.php
   parameter.

   $output_file_size (integer) is the size in bytes.

   $encoding is 'classic' or 'xml'.

   $conversion_log may be NULL if empty. Otherwise it will output (after sanitization).
*/
function output_success($output_file_id, $output_file_suggested_name, $output_file_size,
  $encoding, $conversion_log)
{
  ?>
  <p><b>Success!</b><br>
  The resulting X3D file size: <?php echo readable_byte_size($output_file_size); ?>.

  <?php
  if (!empty($conversion_log)) {
    ?>
    <br>
    <a id="toggle-details" href="#">Click to see the conversion details.</a>
    <pre style="display:none" id="details"><?php echo $conversion_log; ?></pre>
    <?php
  }
  ?>

  <p><a href="convert-download.php?id=<?php echo htmlspecialchars($output_file_id); ?>&amp;encoding=<?php echo htmlspecialchars($encoding); ?>&amp;suggested-name=<?php echo htmlspecialchars($output_file_suggested_name); ?>" class="btn btn-primary btn-lg">Download the resulting X3D file.</a></p>

  <div class="convert-patreon">
    <a class="btn btn-success btn-lg btn-patreon" href="<?php echo PATREON_URL; ?>">Do you like this tool?<br><span class="glyphicon glyphicon-heart" aria-hidden="true"></span> Support us on Patreon.</a>
  </div>
  <?php
}


/* Random alphanumeric string.
   See https://code.tutsplus.com/tutorials/generate-random-alphanumeric-strings-in-php--cms-32132
   https://www.php.net/manual/en/function.random-int.php
*/
function random_alphanum($length)
{
  $permitted_chars = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $permitted_chars_len = strlen($permitted_chars);
  $result = '';
  for($i = 0; $i < $length; $i++) {
    $random_character = $permitted_chars[random_int(0, $permitted_chars_len - 1)];
    $result .= $random_character;
  }
  return $result;
}

/* Delete non-empty directory.
   Based on https://stackoverflow.com/questions/3349753/delete-directory-with-files-in-it
*/
function remove_directory($dir)
{
  $it = new RecursiveDirectoryIterator($dir, RecursiveDirectoryIterator::SKIP_DOTS);
  $files = new RecursiveIteratorIterator($it,
               RecursiveIteratorIterator::CHILD_FIRST);
  foreach($files as $file) {
    $real_path = $file->getRealPath();
    if ($file->isDir()){
      if (!rmdir($real_path)) {
        throw new Exception('Cannot remove empty directory ' . $real_path);
      }
    } else {
      if (!unlink($real_path)) {
        throw new Exception('Cannot remove file ' . $real_path);
      }
    }
  }
  if (!rmdir($dir)) {
    throw new Exception('Cannot remove final empty directory ' . $dir);
  }
}

/* Returns the container id (to be passed to convert-to-x3d.sh).
   Sets $container_path (string), always ends with slash.
*/
function conversion_container_get(&$container_path)
{
  $container_id = 1; // TODO
  $container_path = '/var/convert-to-x3d/containers/' . $container_id . '/contents/';
  remove_directory($container_path);
  /* Allow everyone to write in dir, this way docker user (from convert-to-x3d) can write there.
     TODO: It would be safer to do umask(0002) and only allow group to write.
     convert-to-x3d may be easily part of www-data.
     But docker user is still not part of www-data (and should not be, for security).
  */
  $old_umask = umask(0000);
  if (!mkdir($container_path)) {
    throw new Exception('Cannot create file ' . $real_path);
  }
  umask($old_umask);
  return $container_id;
}

/* Perform conversion.

   $encoding (string) is 'classic' or 'xml', just like --encoding parameter of tovrmlx3d.

   $files is the PHP uploaded files structure for the appropriate form field
   (see https://www.php.net/manual/en/features.file-upload.multiple.php ).

   $conversion_log (multiline string) is set.

   Returns boolean, whether converting was successfull.
*/
function convert_to_x3d($encoding, $files, &$conversion_log,
  &$output_file_id, &$output_file_suggested_name, &$output_file_size)
{
  $output_file_id = random_alphanum(24);

  $container_id = conversion_container_get($container_path);

  for ($i = 0; $i < count($files['tmp_name']); $i++) {
    $temp_name = $files['tmp_name'][$i];
    $dest_name = $container_path . basename($files['name'][$i]);
    if (!move_uploaded_file($temp_name, $dest_name)) {
      $conversion_log = 'Cannot move uploaded file';
      return false;
    }
  }

  $model_extensions = array(
    'x3d',
    'x3dz',
    'x3d.gz',
    'x3dv',
    'x3dvz',
    'x3dv.gz',
    'castle-anim-frames',
    'kanim',
    'glb',
    'gltf',
    'dae',
    'iv',
    '3ds',
    'md3',
    'obj',
    'geo',
    'json',
    'stl'
  );

  // calculate $main_file, $main_file_ext
  $main_file = null;
  $main_file_without_ext = null;
  $main_file_ext = null;
  foreach ($files['name'] as $possible_main_file) {
    $possible_main_file_ext = pathinfo($possible_main_file, PATHINFO_EXTENSION);
    if (in_array($possible_main_file_ext, $model_extensions)) {
      if ($main_file !== null) {
        $conversion_log = 'More than one model uploaded: ' . $main_file . ', ' . $possible_main_file;
        return false;
      }
      $main_file = $possible_main_file;
      $main_file_without_ext = pathinfo($possible_main_file, PATHINFO_FILENAME);
      $main_file_ext = $possible_main_file_ext;
    }
  }
  if ($main_file === null) {
    $conversion_log = "No valid model extension found within the uploaded files.\nThe valid model extensions are:\n" . print_r($model_extensions, true);
    return false;
  }

  exec(
    'sudo -u convert-to-x3d /home/michalis/sources/castle-engine/cge-www/convert-to-x3d/convert-to-x3d.sh ' .
    escapeshellcmd($container_id) . ' ' .
    escapeshellcmd($main_file) . ' ' .
    escapeshellcmd($encoding) . ' ' .
    escapeshellcmd($output_file_id),
    $exec_output,
    $exec_return_val
  );

  if ($exec_return_val !== 0) {
    $conversion_log = "Conversion script failed (non-zero exit status).\nOutput:\n" . implode("\n", $exec_output);
    return false;
  }

  $conversion_log = file_get_contents($container_path . 'error.log');

  $output_file_size = filesize($container_path . $output_file_id);
  $output_extension = $encoding == 'xml' ? '.x3d' : '.x3dv';
  $output_file_suggested_name = $main_file_without_ext . $output_extension;

  if (!rename($container_path . $output_file_id,
    '/var/convert-to-x3d/output/' . $output_file_id))
  {
    $conversion_log = 'Failed to move output file to the output directory';
    return false;
  }

  return !empty($output_file_size);
}

/* Process form input, call either output_error or output_success */
function process_form_post()
{
  if (!isset($_POST['encoding']) ||
      !isset($_FILES['input-file'])) {
    output_error('No uploaded file, or the uploaded file was too large.', NULL);
    return;
  }

  $encoding = $_POST['encoding'];
  $files = $_FILES['input-file'];

  if ($encoding != 'xml' &&
      $encoding != 'classic') {
    output_error('Invalid encoding specified.', NULL);
  } else
  if (!isset($files['name'][0])) {
    output_error('No input files to convert.', NULL);
  } else
  {
    $conversion_success = convert_to_x3d($encoding, $files, $conversion_log,
      $output_file_id, $output_file_suggested_name, $output_file_size);
    if ($conversion_success) {
      output_success($output_file_id, $output_file_suggested_name, $output_file_size,
        $encoding, $conversion_log);
    } else {
      output_error('Conversion failed.', $conversion_log);
    }
  }
}

/* Output page contents, run main function process_form_post ---------------------- */

require_once 'castle_engine_functions.php';
castle_header('Conversion output');
?>

<div class="single-column-page">
  <div class="convert-form convert-output jumbotron">
    <?php process_form_post(); ?>
    <p><a href="convert.php">Convert another file.</a></p>
  </div>
</div>

<?php
castle_footer();
?>
