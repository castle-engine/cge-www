<?php
/*
  ---------------------------------------------------------------------------
  Generates sitemap that can be consumed by search engines to help them index the site.

  See
  https://developers.google.com/search/docs/crawling-indexing/sitemaps/build-sitemap
  https://www.sitemaps.org/protocol.html

  Exposed by

  - https://castle-engine.io/sitemap.xml (although it's a standard name,
    it's unsure whether something crawls xxx/sitemap.xml automatically),

  - mentioned in robots.txt,

  - manually submitted to Google anyway.

  ----------------------------------------------------------------------------
*/

$mime = 'application/xml';
header('Content-Type: ' . $mime . '; charset=UTF-8');

?>
<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xmlns:image="http://www.google.com/schemas/sitemap-image/1.1"
        xsi:schemaLocation="http://www.sitemaps.org/schemas/sitemap/0.9
                            http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd
                            http://www.google.com/schemas/sitemap-image/1.1
                            http://www.google.com/schemas/sitemap-image/1.1/sitemap-image.xsd">
<?php

require_once 'castle_engine_functions.php';


// Output homepage with highest priority
echo '<url>
  <loc>' . htmlspecialchars(CASTLE_PROD_URL) . '</loc>
  <changefreq>weekly</changefreq>
  <priority>1.0</priority>
  <image:image>
    <image:loc>' . CASTLE_PROD_URL . 'images/combined_cge_logo_game.png</image:loc>
    <image:title>Castle Game Engine Logo</image:title>
  </image:image>
</url>' . "\n";

global $sitemap_count;
$sitemap_count = 1;

// Define page priorities and change frequencies based on content type
function get_page_priority($entry_name) {
  // Main sections get higher priority
  if (in_array($entry_name, ['doc/download', 'doc/features', 'doc/manual', 'doc/why_pascal', 'doc/modern_pascal'])) {
    return 0.9;
  }

  // other pages get standard priority
  return 0.8;
}

function output_sitemap($sitemap)
{
  global $sitemap_count;
  global $current_date;

  foreach ($sitemap as $entry_name => $entry_info) {
    if (!isset($entry_info['url'])) {
      echo '<url>
  <loc>' . htmlspecialchars(page_url($entry_name)) . '</loc>
  <priority>' . get_page_priority($entry_name) . '</priority>
</url>' . "\n";
      $sitemap_count++;
    }

    if (isset($entry_info['sub'])) {
      output_sitemap($entry_info['sub']);
    }
  }
}

global $castle_sitemap;
output_sitemap($castle_sitemap);

echo '<!-- URLs in sitemap: ' . $sitemap_count . ' -->' . "\n";
?>
</urlset>
