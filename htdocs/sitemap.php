<?php
/*
  ---------------------------------------------------------------------------
  Generates sitemap that can be consumed by search engines to help them index the site.

  Exposed on https://castle-engine.io/sitemap.xml ,
  though it should be manually submitted to at least Google anyway.

  See
  https://developers.google.com/search/docs/crawling-indexing/sitemaps/build-sitemap
  https://www.sitemaps.org/protocol.html
  ----------------------------------------------------------------------------
*/

$mime = 'application/xml';
header('Content-Type: ' . $mime . '; charset=UTF-8');

?>
<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xsi:schemaLocation="http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd">
<?php

require_once 'castle_engine_functions.php';

echo '<url><loc>' . htmlspecialchars(CASTLE_PROD_URL)  . '</loc>' . "\n";
echo '</url>' . "\n";

global $sitemap_count;
$sitemap_count = 1;

function output_sitemap($sitemap)
{
  global $sitemap_count;

  foreach ($sitemap as $entry_name => $entry_info) {
    if (!isset($entry_info['url'])) {
      echo '<url><loc>' . htmlspecialchars(page_url($entry_name)) . '</loc>' . "\n";
      echo '</url>' . "\n";
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
