<?php

/*
  Copyright 2020-2024 Michalis Kamburelis.

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

  Common PHP functions and utilities for model conversion.

  ---------------------------------------------------------------------------
*/

/*
  Map internal format name -> some information.

  The keys must use only HTML-safe characters, as they are used also for HTML/CSS ids.

  Order determines display order on the form. */
$convert_output_formats = array(
  'x3d-xml' => array(
    'extension' => '.x3d',
    'mime' => 'model/x3d+xml',
    'description' => 'X3D, XML encoding (.x3d extension)',
    'default' => true, // can be true on only one item; can be omitted if false
  ),
  'x3d-classic' => array(
    'extension' => '.x3dv',
    'mime' => 'model/x3d+vrml',
    'description' => 'X3D, classic encoding (.x3dv extension)',
  ),
  'stl' => array(
    'extension' => '.stl',
    'mime' => 'application/x-stl',
    'description' => 'STL, binary (.stl extension)',
  ),
);
global $convert_output_formats;
