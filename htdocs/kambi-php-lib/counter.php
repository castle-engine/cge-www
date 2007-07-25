<?php

/* You must define COUNTER_DATA_PATH before including this.
   COUNTER_DATA_PATH will be prefix for every counter file,
   it can be a relative or absolute path.
   It can be simply '' if you want counters present in current
   directory. */

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
    $hit_date = getdate();
    $bonus_str = sprintf(
      "1day %d.%d.%d hour %d ip %s ipstr %s http-referer %s http-user-agent %s\n",
      $hit_date["mday"], $hit_date["mon"], $hit_date["year"], $hit_date["hours"],
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
