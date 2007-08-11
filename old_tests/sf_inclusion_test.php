<?php
  echo '<i>debug: funcs.php: debug ' . getcwd() . ' ' . (int) file_exists('funcs.php') . ' ' . (int) file_exists('kambi-php-lib/funcs.php') . ' ' . phpversion() . ' "' . get_include_path() . '"</i>';
  require_once 'vrmlengine_functions.php';

  common_header("SF php inclusion test", LANG_EN);
?>

<h1>SF php inclusion test</h1>

<p>I sometimes get error below when on vrmlengine.sf.net pages:</p>

<pre>
Warning: main(funcs.php): failed to open stream: No such file or directory in /home/groups/v/vr/vrmlengine/htdocs/kambi-php-lib/kambi_common.php on line 100

Fatal error: main(): Failed opening required 'funcs.php' (include_path='') in /home/groups/v/vr/vrmlengine/htdocs/kambi-php-lib/kambi_common.php on line 100
</pre>

<p>It's enough to refresh any page (like this one) 5-10 times to finally get
this error. I don't know the cause. I tried to debug by placing
debug line at the beginning of script
but without success (whether I got the error or not, the state displayed
above was the same, so it's not the fault of current dir or php version).

<p>Then I changed the include to do <tt>kambi-php-lib/funcs.php</tt>
(instead of <tt>funcs.php</tt> like previously) and suddenly error
changed to analogous about <tt>kambi_toc.php</tt> ! This means that
including with <tt>kambi-php-lib/</tt> prefix is a workaround
(as script proceeds, and if I would add the same workaround for all
usage of require, maybe error would be gone).

<p>More tests show that actually another workaround is to do

<pre>
set_include_path('.:kambi-php-lib/');
</pre>

right before

<pre>
require_once 'kambi-php-lib/kambi_common.php';
</pre>

So I know that php on SF doesn't
(always) search included files in the dir of current script,
but it always searches the include_path.
I still don't know why, but at least I know a simple workaround.

<?php
//  common_footer();
?>
