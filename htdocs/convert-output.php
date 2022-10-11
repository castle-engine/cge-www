<?php

/*
  Copyright 2020-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine Website".

  "Castle Game Engine Website" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Castle Game Engine Website" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Castle Game Engine Website"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

  ---------------------------------------------------------------------------

  Main PHP file doing the conversion from anything -> X3D.
  Calls shell script convert-to-x3d/convert-to-x3d.sh to do the actual job.

  ---------------------------------------------------------------------------
*/

require_once 'castle_engine_config.php';
require_once 'convert-database.php';

define('CONVERSION_VOLUMES_COUNT', 10);
define('CONVERSION_DEBUG', false); // enable only temporarily for some extra logging

/* Show error.

   Note that $error_message is assumed to be already-safe HTML,
   it is not sanitized here anymore.

   $conversion_log may be NULL if empty. Otherwise it will output (after sanitization).
*/
function output_error($error_message, $conversion_log)
{
  syslog(LOG_WARNING, 'Error.' .
    ' Error: ' . $error_message .
    ' Log: ' .$conversion_log);

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
  syslog(LOG_INFO, 'Success.' .
    ' Output id: ' . $output_file_id .
    ' Output suggested name: ' . $output_file_suggested_name .
    ' Output size: ' . $output_file_size .
    ' Encoding: ' . $encoding .
    ' Log: ' .$conversion_log);

  ?>
  <p><b>Success!</b><br>
  The resulting X3D file size: <?php echo readable_byte_size($output_file_size); ?>.

  <?php
  if (!empty($conversion_log)) {
    $warnings_count = substr_count($conversion_log, 'tovrmlx3d: Warning: ');
    if ($warnings_count != 0) {
      $warnings_str = ' <b>(' . $warnings_count . ' warnings)</b>';
    } else {
      $warnings_str = '';
    }
    ?>
    <br>
    <a id="toggle-details" href="#">Click to see the conversion details<?php echo $warnings_str; ?>.</a>
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
  if (!file_exists($dir)) {
    return; // exit without errors if $dir doesn't exist
  }
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

/* Acquire a lock file on given Docker volume.
   Docker volume is simply a subdirectory like /var/convert-to-x3d/volumes/xxx/contents .
   Returns false if volume is already locked.
*/
function simple_lock($volume_id)
{
  // See https://stackoverflow.com/questions/325806/best-way-to-obtain-a-lock-in-php
  $f = @fopen('/var/convert-to-x3d/volumes/' . $volume_id . '/lock.txt', 'x');
  $result = ($f !== FALSE);
  if ($result) {
    $me = getmypid();
    $now = date('Y-m-d H:i:s');
    fwrite($f, "Locked by $me at $now\n");
    fclose($f);
  }
  return $result;
}

function simple_unlock($volume_id)
{
  if (!unlink('/var/convert-to-x3d/volumes/' . $volume_id . '/lock.txt')) {
    throw new Exception('Cannot release lock for volume ' . $volume_id);
  }
}

/* Returns the volume id (to be passed to convert-to-x3d.sh).
   Sets $volume_path (string), always ends with slash.

   Also creates a lock for this volume, to avoid using it from
   two scripts simultaneously, be sure to release it with simple_unlock.
*/
function conversion_volume_get(&$volume_path)
{
  $volume_id_found = false;
  for ($volume_id = 1; $volume_id <= CONVERSION_VOLUMES_COUNT; $volume_id++) {
    if (simple_lock($volume_id)) {
      $volume_id_found = true;
      break;
    }
  }
  if (!$volume_id_found) {
    throw new Exception('All the conversion resources are busy, please try again in a few minutes');
  }

  $volume_path = '/var/convert-to-x3d/volumes/' . $volume_id . '/contents/';
  remove_directory($volume_path);
  /* Allow everyone to write in dir, this way docker user (from convert-to-x3d) can write there.
     TODO: It would be safer to do umask(0002) and only allow group to write.
     convert-to-x3d may be easily part of www-data.
     But docker user is still not part of www-data (and should not be, for security).
  */
  $old_umask = umask(0000);
  if (!mkdir($volume_path)) {
    throw new Exception('Cannot create file ' . $real_path);
  }
  umask($old_umask);
  return $volume_id;
}

/* Perform conversion.

   $encoding (string) is 'classic' or 'xml', just like --encoding parameter of tovrmlx3d.

   $files is the PHP uploaded files structure for the appropriate form field
   (see https://www.php.net/manual/en/features.file-upload.multiple.php ).

   $conversion_log (multiline string) is set.

   Returns boolean, whether converting was successful.
*/
function convert_to_x3d($encoding, $files, &$conversion_log,
  &$output_file_id, &$output_file_suggested_name, &$output_file_size)
{
  $conversion_log = '';

  try { // exceptions inside will be nicely displayed
    $time_start = microtime(true);

    $output_file_id = random_alphanum(24);

    $volume_id = conversion_volume_get($volume_path);

    try { // be sure to call simple_unlock at end
      for ($i = 0; $i < count($files['tmp_name']); $i++) {
        $temp_name = $files['tmp_name'][$i];
        $dest_name = $volume_path . basename($files['name'][$i]);
        if (!move_uploaded_file($temp_name, $dest_name)) {
          $conversion_log .= "Cannot move uploaded file\n";
          return false;
        }
      }

      $model_extensions = array(
        'wrl',
        'wrl.gz', // TODO: will not be actually recognized, as we only take last extension
        'wrz',
        'x3d',
        'x3dz',
        'x3d.gz', // TODO: will not be actually recognized, as we only take last extension
        'x3dv',
        'x3dvz',
        'x3dv.gz', // TODO: will not be actually recognized, as we only take last extension
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
            $conversion_log .= 'More than one model uploaded: ' . $main_file . ', ' . $possible_main_file . "\n";
            return false;
          }
          $main_file = $possible_main_file;
          $main_file_without_ext = pathinfo($possible_main_file, PATHINFO_FILENAME);
          $main_file_ext = $possible_main_file_ext;
        }
      }
      if ($main_file === null) {
        $conversion_log = "No valid model extension found within the uploaded files.\nThe valid model extensions are:\n" . print_r($model_extensions, true) . "\n";
        return false;
      }

      global $cge_config;

      $exec_command = 'sudo -u convert-to-x3d ' . $cge_config['cge-www-path'] . 'convert-to-x3d/convert-to-x3d.sh ' .
        /* Note: using escapeshellarg, not escapeshellcmd,
           as escapeshellarg is good for single aguments -- it handles also spaces in name.
           Testcase: trying to convert a file with space in name, like "foo bar.stl". */
        escapeshellarg($volume_id) . ' ' .
        escapeshellarg($main_file) . ' ' .
        escapeshellarg($encoding) . ' ' .
        escapeshellarg($output_file_id);
      if (CONVERSION_DEBUG) {
        $conversion_log .= "Executing: " . $exec_command . "\n";
      }
      exec($exec_command,
        $exec_output,
        $exec_return_val
      );

      $error_log = file_get_contents($volume_path . 'error.log');
      $conversion_log .= $error_log;

      if ($exec_return_val !== 0) {
        $conversion_log .= "Conversion script failed (non-zero exit status).\n";
        if (!empty($exec_output) != 0) {
          $conversion_log .=
            "Process output:\n" .
            implode("\n", $exec_output) . "\n";
        }
        return false;
      }

      $output_file_size = filesize($volume_path . $output_file_id);
      $output_extension = $encoding == 'xml' ? '.x3d' : '.x3dv';
      $output_file_suggested_name = $main_file_without_ext . $output_extension;

      if (!rename($volume_path . $output_file_id,
        '/var/convert-to-x3d/output/' . $output_file_id))
      {
        $conversion_log .= 'Failed to move output file to the output directory';
        return false;
      }

      // input files are no longer needed, delete (we promise we don't keep them on server)
      remove_directory($volume_path);

      _cge_record_creation_time($output_file_id);
    } finally {
      simple_unlock($volume_id);
    }

    $time_end = microtime(true);
    $time = $time_end - $time_start;
    $conversion_log .=
      "\nConversion time (in seconds): " . sprintf('%.2f', $time) .
      "\nConversion slot (Docker volume): $volume_id";
  } catch (Exception $e) {
    // convert uncaught exceptions at this point to nice error messages
    $conversion_log .= $e->getMessage();
    return false;
  }

  return !empty($output_file_size);
}

/* Process form input, call either output_error or output_success */
function process_form_post()
{
  openlog('convert-to-x3d', LOG_PID, LOG_LOCAL0);

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
    <p class="another"><a href="convert.php">Convert another file.</a></p>
  </div>
</div>

<?php
castle_footer();
?>
