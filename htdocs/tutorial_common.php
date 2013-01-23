<?php

require_once 'castle_engine_functions.php';

function _tutorial_bar()
{
  global $castle_tutorial;
  global $page_basename;

  $this_info = $castle_tutorial[$page_basename];

  $result = '<table class="news_older_newer"><tr>';
  if ($this_info['previous'] !== NULL)
  {
    $previous_info = $castle_tutorial[$this_info['previous']];
    $result .= '<td class="news_newer">' . a_href_page('Previous: ' .
      $previous_info['number'] . $previous_info['title'], $this_info['previous']) . '</td>';
  }
  if ($this_info['next'] !== NULL)
  {
    $next_info = $castle_tutorial[$this_info['next']];
    $result .= '<td class="news_older">' . a_href_page('Next: ' .
      $next_info['number'] . $next_info['title'], $this_info['next']) . '</td>';
  }
  $result .= '</tr></table>';

  return $result;
}

function tutorial_header($a_page_title, $subheading_text = '')
{
  global $castle_tutorial;
  global $page_basename;
  $number = $castle_tutorial[$page_basename]['number'];
  $a_page_title = $number . $a_page_title;

  castle_header($a_page_title . ' | Castle Game Engine Tutorial', NULL, array('engine', 'tutorial_intro'));
  echo pretty_heading($a_page_title, NULL, $subheading_text);
  echo _tutorial_bar();
}

function tutorial_footer()
{
  echo _tutorial_bar();
  castle_footer();
}
