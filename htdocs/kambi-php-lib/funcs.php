<?php

/* Copyright 2001-2013 Michalis Kamburelis.

   This file is part of "Kambi PHP library".

   "Kambi PHP library" is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   "Kambi PHP library" is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with "Kambi PHP library"; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

define("KILO", 1024);

function int_to_str($i)
{
  settype($i, "string");
  return $i;
}

/* Zwraca string 'false' lub 'true' */
function bool_to_str($bool)
{
  return ($bool ? 'true' : 'false');
}

/* funkcja zwraca zamieniony na przyjemny i czytelny dla czlowieka string
   z zapisanym $bytes (byc moze jako bajty, moze jako KB albo MB) */
function readable_byte_size($bytes)
{
  if ($bytes < KILO)
    return int_to_str($bytes) . ' bytes'; else
  {
    $kbytes = (int) ($bytes / KILO);
    if ($kbytes < KILO)
      return int_to_str($kbytes) . ' KB'; else
    {
      $mbytes = $kbytes / KILO; /* $mbytes niech bedzie floatem */
      return number_format($mbytes, 1, '.', ' ') . ' MB';
    }
  }
}

/* funkcja zwraca zamieniony na przyjemny i czytelny dla czlowieka string
   z zapisanym filesize($f_name). */
function readable_file_size($f_name)
{
  return readable_byte_size(filesize($f_name));
}

/* zwraca napis <a href="$fname">$link_title (readable_file_size($f_name)) </a> */
function a_href_size($link_title, $f_name)
{
  $rable_size = readable_file_size($f_name);
  return "<a href=\"$f_name\">$link_title ($rable_size)</a>";
}

/* zwraca boola - czy wsrod argv[0]...argv[argc-1] jest string param ?
   (tak, tak - w phpie istotne argv to [0..argc-1] zamiast [1..argc]
   w C czy Pascalu) */
function param_in_argvs($param)
{
  for ($i=0; $i<$_SERVER["argc"]; $i++)
    if ($_SERVER["argv"][$i] == $param) return true;
  return false;
}

/* $const_name:string, nazwa stalej, $default_value:'a,
   jezeli stala $const_name nie jest zdefiniowana to zdefiniuje ja
   (z nazwa case-sensitive) na $default_value */
function define_if_needed($const_name, $default_value)
{
  if (! defined($const_name)) define($const_name, $default_value);
}

/* Wszystkie 3 parametry beda traktowane tylko jako stringi.
   Jezeli $s == '' to zwraca $next_part, wpp. $s . $part_separator . $next_part
*/
function str_append_part($s, $part_separator, $next_part)
{
  return ($s == '' ?  '' : $s . $part_separator) . $next_part;
}

function str_append_part_to1st(&$s, $part_separator, $next_part)
{
  $s = str_append_part($s, $part_separator, $next_part);
}

/* Constructs something like
  "<ul>\n" . "<li>$item\n" (for each item in $arr) . "</ul>\n".
  Items are listed in the order returned by foreach. */
function array_to_ul($arr)
{
  $result = "<ul>\n";
  foreach ($arr as $item) { $result .= "  <li>$item\n"; }
  $result .= "</ul>\n";
  return $result;
}

/* Michalis' email address. Using the constant makes it easier to change
   the address everywhere at once, and once saves me from accidentaly
   making typos in my email. */
define('MICHALIS_EMAIL', 'michalis.kambi AT gmail.com');
define('MICHALIS_SF_EMAIL', 'kambi AT users.sourceforge.net');

function href_mailto($email_address)
{
  return '<a href="mailto:' . rawurlencode($email_address) . '">' .
    $email_address . '</a>';
}

/* Returns a string with
   <a href= mail to MICHALIS_EMAIL>$anchor_title</a> */
function michalis_mailto($anchor_title)
{
  return '<a href="mailto:' . rawurlencode(MICHALIS_EMAIL) . '">' .
    $anchor_title . '</a>';
}

function rand_color()
{
  $result[0] = rand(0, 255);
  $result[1] = rand(0, 255);
  $result[2] = rand(0, 255);
  return $result;
}

function color_intensity($color)
{
  return ($color[0] * 0.212671) +
         ($color[1] * 0.715160) +
         ($color[2] * 0.072169);
}

function color_to_html($color)
{
  return sprintf('#%02X%02X%02X', $color[0], $color[1], $color[2]);
}

/* This is just like color_to_html(rand_color()), but somewhat faster. */
function rand_html_color()
{
  return sprintf('#%02X%02X%02X', rand(0, 255), rand(0, 255), rand(0, 255));
}

function rand_font_family()
{
  switch (rand(0, 4)) {
    case 0: return 'serif';
    case 1: return 'sans-serif';
    case 2: return 'cursive';
    case 3: return 'fantasy';
    case 4: return 'monospace';
  }
}

/* Random float between 0 and $max */
function rand_float($max = 1.0)
{
  return ((float) rand()) / ((float) getrandmax()) * ((float) $max);
}

/* Just like the name says... yes, this is a joke. */
function echo_rand_css()
{
  /* Generated random background and foreground and links color,
    still making sure that these colors match somewhat. */
  do {
    $c = rand_color();
    $bg_c = rand_color();
  } while (
    ( abs(color_intensity($c) - color_intensity($bg_c)) < 0.8 )
    );

  do {
    $link_c = rand_color();
    $link_visited_c = rand_color();
    $link_hover_c = rand_color();
  } while (
    ( abs(color_intensity($c) - color_intensity($link_c)) > 0.2 ) &&
    ( abs(color_intensity($c) - color_intensity($link_visited_c)) > 0.2 ) &&
    ( abs(color_intensity($c) - color_intensity($link_hover_c)) > 0.2 )
    );

  $color = color_to_html($c);
  $bg_color = color_to_html($bg_c);
  $link_color = color_to_html($link_c);
  $link_visited_color = color_to_html($link_visited_c);
  $link_hover_color = color_to_html($link_hover_c);

?>
  body {
    background: <?php echo $bg_color; ?>;
    color: <?php echo $color; ?>;
    margin: <?php echo rand_float(2.0); ?>em
            <?php echo rand_float(2.0); ?>em
            <?php echo rand_float(2.0); ?>em
            <?php echo rand_float(2.0); ?>em;
    font-family: <?php echo rand_font_family(); ?>;
  }

  a:normal { color: <?php echo $link_color; ?>; }
  a:visited { color: <?php echo $link_visited_color; ?>; }
  a:hover { color: <?php echo $link_hover_color; ?>; }
<?php
}
?>
