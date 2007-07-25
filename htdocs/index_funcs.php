<?php

require "camelot_funcs.php";
$main_page = true;

/* See private/old/index_main_list.php
   for some old ideas for main_list_ things. */

function index_header($a_page_title, $a_page_lang, $meta_description)
{
  camelot_header($a_page_title, $a_page_lang,
    $meta_description, NULL,
    '<style type="text/css"><!--
       DIV.main_list_item {
         font-family: serif;
         font-weight: bolder;
         font-size: large;

         background: #ddddd0;

         padding: 0.1em;
       }
     --></style>');
}

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
