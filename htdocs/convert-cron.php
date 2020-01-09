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

require_once 'convert-database.php';

define('CONVERSION_OUTPUT_LIFETIME', 15 * 60); // in seconds

function cge_convert_cron()
{
  $database = _cge_json_read(CONVERSION_OUTPUT_TIME_DATABASE);
  $now = time();
  $remove_earlier = $now - CONVERSION_OUTPUT_LIFETIME;
  if ($database !== NULL) {
    $something_removed = false;
    // This relies on the fact that $database is ordered from oldest to newest
    while (count($database) != 0 &&
      $database[0]['time'] < $remove_earlier)
    {
      $to_remove = array_shift($database);
      $to_remove_filename = '/var/convert-to-x3d/output/' . $to_remove['id'];
      if (!unlink($to_remove_filename)) {
        throw new Exception('Cannot remove file ' . $to_remove_filename);
      }
      $something_removed = true;
    }
    if ($something_removed) {
      _cge_json_write(CONVERSION_OUTPUT_TIME_DATABASE, $database);
    }
  }
}

cge_convert_cron();
