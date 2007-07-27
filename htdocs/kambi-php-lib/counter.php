<?php

/* Copyright 2001-2007 Michalis Kamburelis.

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

   ============================================================

   Simple hits counter in PHP, for every page, with info on which
   day, from what IP, and (most usefull) from what http_referer visitors come
   here.

   You must define COUNTER_DATA_PATH before including this.
   COUNTER_DATA_PATH will be prefix for every counter file,
   it can be a relative or absolute path.
   It can be simply '' if you want counters present in current
   directory.
*/

/* reads integer value from file $counter_name . ".counter", increments
   it, writes back to the file and returns it (as a string).
   It also writes some bonus info to the $counter_name . ".counter.bonus"
   file if $counter_write_bonus = TRUE. */
function php_counter($counter_name, $counter_write_bonus = FALSE)
{
  $f_name = COUNTER_DATA_PATH . $counter_name .".counter";

  /* Opening with @fopen, we'll check $f_in for error later. */
  $f_in = @fopen($f_name, "r");

  if ($f_in === FALSE)
    $counter = 0; else
  {

    $counter = trim( fgets($f_in, 1024) );
    fclose($f_in) or exit('cannot close in file');

    /* if settype fails we DON''T set counter to 0 - this could delete our
       .counter file in case of some error ! */
    settype( $counter, "integer" ) or exit("wrong file " . $f_name . " contents");
  }

  $counter++;
  settype($counter, "string");

  $f_out = fopen($f_name, "wb"); /* fwrite/fputs requires "b"inary mode */
  fwrite($f_out, $counter);
  fclose($f_out) or exit('cannot close out file');

  /* write bonus info ***********************************/
  if ($counter_write_bonus)
  {
    $f_bonus = fopen(COUNTER_DATA_PATH . $counter_name . ".counter.bonus", "ab");

    /* getdate returns by default date/time in local server time,
       which is not nice if you move *.counter.bonus file from server
       to server (with different timezones). That's not a terrible problem
       (it's an error by at most 1 day, nothing that can "skew" e.g. your
       monthly statistics much), but still we would prefer UTC to be precise
       and elegant.

       <rant>
       PHP datetime functions are really stupid when it comes to the timezone
       stuff, especially if you're stuck with PHP 4 (on SourceForge...).
       First of all, common sense is more important than misleading wording in
       PHP docs and function design, so actually all three calls
         gmmktime()
         mktime()
         time()
       return the same thing if called without the arguments,
       and this is the timestamp that is *totally independent
       from any timezone settings*, as it should be.

       To extract this, I'd like to use getdate function, that elegantly
       deconstructs timestamp into a structure... But I can't, because getdate
       always makes local time (remember, I'm stuck with PHP 4 so
       date_default_timezone_set is not available and calling setlocale
       is just dirty).

       So I have to deconstruct date with a function like gmdate.
       I have to call this a couple of times, since it's formatting is stupid
       and I can't just generate in one call something like "day %d.%d.%d hour %d".
       </rant>
    */

    $hit_time = time();
    $str_hit_time = 'day ' . gmdate('j', $hit_time) . '.' .
                             gmdate('n', $hit_time) . '.' .
                             gmdate('Y', $hit_time) . ' hour ' .
                             gmdate('G', $hit_time);

    $bonus_str = sprintf(
      "1%s ip %s ipstr %s http-referer %s http-user-agent %s\n",
      $str_hit_time,
      $_SERVER["REMOTE_ADDR"],
      @gethostbyaddr($_SERVER["REMOTE_ADDR"]),
      /* I'm using rawurlencode (both for HTTP_REFERER and even
         HTTP_USER_AGENT, even though HTTP_USER_AGENT is not an URL),
         to avoid problems when later parsing the counter.bonus lines. */
      @rawurlencode($_SERVER["HTTP_REFERER"]),
      @rawurlencode($_SERVER["HTTP_USER_AGENT"])
    );
    fwrite($f_bonus, $bonus_str);
    fclose($f_bonus);
  }

  /* return *********************************************/
  return $counter;
}

/* wypisuje na stdout ciag odpowiednich dyrektyw <img src=*> gdzie
   * = sprintf($str_img_fnames, $item) a $item to kolejne znaki
   stringa str.
   Wiec umiesc w $counter_imf_fnames gdzies %d. */
function echo_images($str, $str_img_fnames)
{
  for ($i=0; $i<strlen($str); $i++)
    printf("<img src=$str_img_fnames>", $str[$i]);
}

/* wywoluje potem echo_images(php_counter($counter_name), $counter_img_fnames).
   Zwraca tez wynik php_counter (chociaz najczesciej do niczego ci sie
   on nie przyda) */
function php_counter_echo_images($counter_img_fnames, $counter_name, $counter_write_bonus = FALSE)
{
  $counter = php_counter($counter_name, $counter_write_bonus);
  echo_images($counter, $counter_img_fnames);
  return $counter;
}

?>
