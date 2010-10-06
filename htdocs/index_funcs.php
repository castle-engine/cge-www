<?php

require_once 'vrmlengine_functions.php';
$main_page = true;

$main_list_item_num = 0;
function main_list_item($item_title, $anchor_name = '')
{
  global $main_list_item_num;
  $main_list_item_num++;

  /* The leading <p> is needed for IE to keep appropriate vertical
     distance. */
  return "<p><div class=\"main_list_item\">" .
    ($anchor_name != '' ? "<a name=\"$anchor_name\">": '') .
    "$item_title" .
    ($anchor_name != '' ? "</a>": '') .
    '</div>';
}

function main_list_begin() {
  return "";
}

function main_list_end() {
  return "";
}

?>
