<?php

  $node_format_fd_type_pad = 0;
  $node_format_fd_name_pad = 0;
  $node_format_fd_def_pad = 0;
  $node_format_fd_inout_pad = 0;

function node_begin($node_name)
/* inicjuje $node_format_* na wartosci domyslne zebys mogl po wywolaniu
   node_begin swobodnie zmieniac te wartosci (i to w takiej kolejnosci
   w jakiej chcesz, np. mozesz zmienic tylko $node_format_fd_name_pad
   a pozostale zostawic na domyslnych wartosciach) */
{
  global $node_format_fd_type_pad, $node_format_fd_name_pad,
    $node_format_fd_def_pad, $node_format_fd_inout_pad;

  $node_format_fd_type_pad = 10;
  $node_format_fd_name_pad = 10;
  $node_format_fd_def_pad = 10;
  $node_format_fd_inout_pad = 12;

  return '<pre class="vrml_extension_spec"><b>' . $node_name . ' {</b>' . "\n";
}
function node_end()
{
  return "<b>}</b>\n</pre>";
}

function node_dots($comment = '')
{
  if ($comment == '')
    $result = "..."; else
    $result = "... $comment ...";

  //  $result = '<b>' . $result . '</b>';

  $result = '  ' . $result . "\n";

  return $result;
}

function node_comment($comment)
{
  $result = "  # $comment\n";
  return $result;
}

function node_field($field_type, $field_inout, $field_name, $field_default, $field_comment = "")
{
  global $node_format_fd_type_pad, $node_format_fd_name_pad,
    $node_format_fd_def_pad, $node_format_fd_inout_pad;

  $r = sprintf("  %-" .int_to_str($node_format_fd_type_pad). "s %-"
                      .int_to_str($node_format_fd_inout_pad). "s  <b>%-"
		      .int_to_str($node_format_fd_name_pad). "s  %-"
		      .int_to_str($node_format_fd_def_pad). "s</b>",
		      $field_type, $field_inout, $field_name, $field_default);
  if ($field_comment != "") $r .= "  #&nbsp;$field_comment";
  $r .= "\n";
  return $r;
}

?>