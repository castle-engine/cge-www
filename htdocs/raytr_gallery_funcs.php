<?php

function image_tag($image_name)
{
  return " <a href=\"images/raytr_gallery/" . $image_name . ".png\"> " .
    "<img src=\"images/raytr_gallery/" . $image_name . "-mini.png\" " .
    "     alt=\"" . $image_name . "-mini.png\" " .
    "     width=100 height=75></a> "; //  border=\"0\"
}

function image_tags_table($image_names)
{
  $result = "<table><tr>\n";
  foreach ($image_names as $image_name)
    $result .= "  <td>" . image_tag($image_name) . "</td>\n";
  $result .= "</tr></table>\n";
  return $result;
}

function path_tracer_params_descr($min_depth, $r_roul, $non_prim_samples,
  $prim_samples, $direct_samples)
{
  global $page_lang;
  switch ($page_lang)
  {
    case LANG_PL:
      $result = "Parametry rayhuntera: minimalna g³êboko¶æ $min_depth,
        non primary samples count : $non_prim_samples,"; break;
    case LANG_EN:
      $result = "Rayhunter parameters: minimal depth $min_depth,
        non primary samples count : $non_prim_samples,"; break;
  }
  
  $result .= "<tt>--r-roul-continue $r_roul</tt>,
             <tt>--primary-samples-count $prim_samples</tt>";
  if ($direct_samples != 1) $result .=
    "<tt>--direct-illum-samples-count $direct_samples</tt>";
  $result .= '.';
  return $result;
}

?>
