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

  Functions to load/save database when each output file was generated,
  and to remove output files that exist longer than CONVERSION_OUTPUT_LIFETIME.

  ---------------------------------------------------------------------------
*/

define('CONVERSION_OUTPUT_TIME_DATABASE', '/var/convert-to-x3d/output-times.json');

/* Like file_get_contents but uses locking.
 * Returns NULL if file doesn't exist.
 */
function _cge_file_read($filename)
{
  $file_handle = @fopen($filename, 'rb');
  if (!$file_handle) {
    // throw new Exception('Cannot open file ' . $filename);
    return NULL;
  }
  if (!flock($file_handle, LOCK_SH)) { // shared lock, allows reading
    throw new Exception('Cannot lock file ' . $filename);
  }
  $contents = fread($file_handle, filesize($filename));
  flock($file_handle, LOCK_UN);
  fclose($file_handle);
  return $contents;
}

/* Like file_get_contents but uses locking to avoid simultaneously changing
 * the file from multiple processes.
 */
function _cge_file_write($filename, $contents)
{
  // following fopen docs, we open in "c" mode and do ftruncate after flock,
  // see https://www.php.net/manual/en/function.fopen.php
  $file_handle = fopen($filename, 'cb');
  if (!$file_handle) {
    throw new Exception('Cannot open file ' . $filename . ' for writing');
  }
  if (!flock($file_handle, LOCK_EX)) { // exclusive lock, allows writing
    throw new Exception('Cannot lock file ' . $filename . ' for writing');
  }
  ftruncate($file_handle, 0);
  fwrite($file_handle, $contents);
  flock($file_handle, LOCK_UN);
  fclose($file_handle);
}

function _cge_json_read($filename)
{
  return json_decode(_cge_file_read($filename), true);
}

function _cge_json_write($filename, $data)
{
  _cge_file_write($filename, json_encode($data));
}

function _cge_record_creation_time($file_id)
{
  $database = _cge_json_read(CONVERSION_OUTPUT_TIME_DATABASE);
  if ($database === NULL) {
    $database = array();
  }
  $database[] = array(
    'time' => time(),
    'id' => $file_id,
  );
  _cge_json_write(CONVERSION_OUTPUT_TIME_DATABASE, $database);
}
