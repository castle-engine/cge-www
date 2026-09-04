<?php
/*
  Copyright 2001-2026 Michalis Kamburelis.

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
*/

/*
  Run automated tests for Castle Game Engine PHP codebase.

  Like with the rest of our PHP, simplicity is important:)
  We maintain our own PHP codebase for our website to keep things *simple*.

  You can execute it by
  - visiting http://localhost:8777/castle_engine_auto_test.php
    (disabled on production host)
  - running PHP from the command line: `php castle_engine_auto_test.php`
*/

define('CASTLE_ENVIRONMENT', 'development'); // prevent CASTLE_ENVIRONMENT detection
error_reporting(E_ALL); // report all errors for testing
require 'castle_engine_functions.php';

/*
  Own assertion function, as PHP "assert" can only be activated using
  ini settings (https://www.php.net/manual/en/function.assert-options.php
  was deprecated and is no longer functional), which makes using them
  inconvenient, we want to be able to easily visit
  http://localhost:8777/castle_engine_auto_test.php on any dev machine
  and see errors and experiment with tests during development.
*/
function my_assert($condition, $message = 'Assertion failed')
{
  if (!$condition) {
    throw new Exception($message);
  }
}

// -----------------------------------------------------------------------------
// Test _castle_breadcrumbs

$breadcrumbs = _castle_breadcrumbs(
  array(
    'doc/documentation',
    'creating_data_intro',
    'doc/export_models',
    'doc/model_formats',
    'doc/ifc'
  )
);
//echo $breadcrumbs;
my_assert($breadcrumbs !== '');
my_assert(strpos($breadcrumbs, '<a href="/">Home</a>') !== false);
my_assert(strpos($breadcrumbs, '/model_formats">Supported model formats</a>') !== false);
// do not include the last item in the breadcrumbs
my_assert(strpos($breadcrumbs, '/ifc">IFC</a>') === false);

// -----------------------------------------------------------------------------
// Test _castle_clone_sitemap_and_trim

$trimmed = _castle_clone_sitemap_and_trim($castle_sitemap, array());
//print_r($trimmed);
my_assert(isset($trimmed['doc/download']));
my_assert(!isset($trimmed['doc/download']['sub'])); // no subs in download
my_assert(isset($trimmed['doc/documentation']));
my_assert(isset($trimmed['doc/documentation']['sub']));
my_assert(isset($trimmed['doc/documentation']['sub']['doc/manual']));
my_assert(!isset($trimmed['doc/documentation']['sub']['doc/manual']['sub'])); // sub inside manual is trimmed
my_assert(isset($castle_sitemap['doc/documentation']['sub']['doc/manual']['sub'])); // original sitemap still has the sub inside manual

// test on subtree of $castle_sitemap
$sitemap_docs = $castle_sitemap['doc/documentation']['sub'];
$trimmed = _castle_clone_sitemap_and_trim($sitemap_docs, array());
//print_r($trimmed);
my_assert(isset($trimmed['doc/manual']));
my_assert(isset($trimmed['doc/manual']['sub']));
my_assert(isset($trimmed['doc/manual']['sub']['doc/miscellaneous']));
my_assert(!isset($trimmed['doc/manual']['sub']['doc/miscellaneous']['sub'])); // sub inside miscellaneous is trimmed
my_assert(isset($sitemap_docs['doc/manual']['sub']['doc/miscellaneous']['sub'])); // original sitemap_docs still has the sub inside miscellaneous

$sitemap_docs = $castle_sitemap['doc/documentation']['sub']['doc/modern_pascal']['sub'];
$trimmed = _castle_clone_sitemap_and_trim($sitemap_docs, array(
  'doc/documentation',
  'creating_data_intro',
  'doc/export_models',
  'doc/model_formats',
  'doc/ifc'
));
// creating_data_intro doesn't occur in $sitemap_docs, so the result is just cut to level 2
my_assert(isset($trimmed['doc/modern_pascal_translations']));
my_assert(isset($trimmed['doc/modern_pascal_translations']['sub']));

echo 'All OK.' . "\n";
