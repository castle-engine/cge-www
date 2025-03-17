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
    'description' => 'X3D, XML encoding',
    'default' => true, // can be true on only one item; can be omitted if false
  ),
  'x3d-classic' => array(
    'extension' => '.x3dv',
    'mime' => 'model/x3d+vrml',
    'description' => 'X3D, classic encoding',
  ),
  'stl' => array(
    'extension' => '.stl',
    'mime' => 'application/x-stl',
    'description' => 'STL, binary',
  ),
  'ifc' => array(
    'extension' => '.ifcjson',
    'mime' => 'application/x-ifc-json',
    'description' => 'IFC JSON',
  ),
);
global $convert_output_formats;

$convert_input_extensions = array(
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
  'stl',
  'castle-sprite-sheet',
  'starling-xml',
  'plist',
  'cocos2d-plist',
  'tmx',
  'ifcjson',
  /*
  All image formats are OK too, we convert them to X3D quad with proper size.
  But we don't list them here, as then auto-detection "what is main format"
  would be confused by textures selected alongside e.g. glTF or X3D files.
  So if anything, we'd need $convert_input_extensions_secondary for images.

  'png',
  'jpg',
  'ppm',
  */
);
global $convert_input_extensions;