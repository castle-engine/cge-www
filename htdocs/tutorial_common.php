<?php

require_once 'castle_engine_functions.php';

function tutorial_header($a_page_title, $subheading_text = '')
{
  global $castle_tutorial;
  global $page_basename;
  $number = $castle_tutorial[$page_basename]['number'];
  $a_page_title = $number . $a_page_title;

  castle_header($a_page_title . ' | Castle Game Engine Tutorial', NULL, array('engine', 'tutorial_intro'));
  echo pretty_heading($a_page_title, NULL, $subheading_text);
}

function tutorial_footer()
{
  castle_footer();
}
