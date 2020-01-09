<?php

/*
  Copyright 2020-2020 Michalis Kamburelis.

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

  PHP that simply returns (allows user to download) the conversion result.

  ---------------------------------------------------------------------------
*/

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

// calculate and validate $suggested_name
if (!isset($_GET['suggested-name'])) {
  die('No suggested-name parameter');
}
$suggested_name = $_GET['suggested-name'];

$extension = $encoding == 'xml' ? '.x3d' : '.x3dv';
$mime = $encoding == 'xml' ? 'model/x3d+xml' : 'model/x3d+vrml';

$file_name = '/var/convert-to-x3d/output/' . $file_id;

$file_size = filesize($file_name);
if ($file_size === FALSE) {
  die('File does not exist. Probably the link expired (you need to download the generated output within 15 minutes). Please run the conversion again.');
}

// For headers, see e.g.
// https://stackoverflow.com/questions/8485886/force-file-download-with-php-using-header
// https://usefulangle.com/post/71/http-caching-tutorial

header('Content-Description: File Transfer');
header('Content-Type: ' . $mime);
header('Content-Disposition: attachment; filename=' . $suggested_name);
header('Content-Transfer-Encoding: binary');
header('Content-Length: ' . $file_size);
header('Cache-Control: no-store'); // disallows caching - get a response each and every time
readfile($file_name);
