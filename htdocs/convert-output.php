<?php
require_once 'castle_engine_functions.php';
castle_header('Conversion output');
?>

<div class="single-column-page">

<div class="convert-form convert-output jumbotron">

<?php

/* Show error.
   Note that $error_message is assumed to be already-safe HTML,
   it is not sanitized here anymore.
   $conversion_log may be NULL if empty.
*/
function output_error($error_message, $conversion_log)
{
  echo '<p><b>Failure: ' . $error_message . '</b>';

  if (!empty($conversion_log)) {
    ?>
    <br>
    <a id="toggle-details" href="#">Click to see the details.</a>
    <pre style="display:none" id="details"><?php echo $conversion_log; ?></pre>
    <?php
  }
}

function output_success($conversion_log)
{
  ?>
  <p><b>Success!</b><br>
  The resulting X3D file size: TODO.<br>
  <a id="toggle-details" href="#">Click to see the conversion details.</a>

<pre style="display:none" id="details">
<?php echo $conversion_log; ?>
</pre>

  <p><a href="TODO" class="btn btn-primary btn-lg">Download the resulting X3D file.</a></p>

  <div class="convert-patreon">
    <a class="btn btn-success btn-lg btn-patreon" href="<?php echo PATREON_URL; ?>">Do you like this tool?<br><span class="glyphicon glyphicon-heart" aria-hidden="true"></span> Support us on Patreon.</a>';
  </div>
  <?php
}

function convert_to_x3d($encoding, $files, &$conversion_log, &$conversion_success)
{
  // TODO
  $conversion_success = true;
  $conversion_log = 'Sample sample
encoding: ' . $encoding . '
files: ' . $files;
}

/* Process form input, call either output_error or output_success */
function process_form_post()
{
  $encoding = $_POST['encoding'];
  $files = print_r($_FILES, true); // TODO just to test

  if ($encoding != 'xml' &&
      $encoding != 'classic') {
    output_error('Invalid encoding specified.', NULL);
  } else
  // if (!isset($files['input-file']['name'][0])) {
  //   output_error('No input files to convert.', NULL);
  // } else
  {
    convert_to_x3d($encoding, $files, $conversion_log, $conversion_success);
    if ($conversion_success) {
      output_success($conversion_log);
    } else {
      output_error('Conversion failed.', $conversion_log);
    }
  }
}

process_form_post();
?>

  <p><a href="convert.php">Convert another file.</a></p>
</div>

</div>

<?php
castle_footer();
?>
