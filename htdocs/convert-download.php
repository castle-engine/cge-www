<?php

// calculate and validate $encoding
if (!isset($_GET['encoding'])) {
  die('No encoding parameter');
}
$encoding = $_GET['encoding'];
if ($encoding != 'xml' &&
    $encoding != 'classic') {
  die('Invalid encoding');
}

// calculate and validate $file_id
if (!isset($_GET['id'])) {
  die('No id parameter');
}
$file_id = $_GET['id'];
if (preg_match('/[^a-z0-9]/i', $file_id)) {
  die('Invalid id parameter');
}

$extension = $encoding == 'xml' ? '.x3d' : '.x3dv';
$mime = $encoding == 'xml' ? 'model/x3d+xml' : 'model/x3d+vrml';

// on server:
// rm -Rf /var/cge-convert/ && mkdir -p /var/cge-convert/ && chown -R www-data:www-data /var/cge-convert/
$file_name = '/var/cge-convert/' . $file_id;

$file_size = filesize($file_name);
if ($file_size === FALSE) {
  die('File does not exist. Probably the link expired (you need to download the generated output within 15 minutes). Please run the conversion again.');
}

// For headers, see e.g.
// https://stackoverflow.com/questions/8485886/force-file-download-with-php-using-header
// https://usefulangle.com/post/71/http-caching-tutorial

header('Content-Description: File Transfer');
header('Content-Type: ' . $mime);
header('Content-Disposition: attachment; filename=' . $file_id . $extension);
header('Content-Transfer-Encoding: binary');
header('Content-Length: ' . $file_size);
header('Cache-Control: no-store'); // disallows caching - get a response each and every time
readfile($file_name);
