<?php
  require_once "vrmlengine_functions.php";
  vrmlengine_header("Some notes about command-line options understood by my programs");
?>

<h2>Some notes about command-line options understood by my programs</h2>

<p>This page describes just some usual conventions used when interpreting
command-line options by my programs (actually, these conventions are widely
known and used by most good programs in the world) :
<ul>
  <li><p>Long options start with <tt>--</tt> (two dashes), like
    <tt>--long-option</tt>. Short options start with <tt>-</tt> (one dash)
    and consist of one letter, like <tt>-p</tt>. The advantage of the long form
    is that you can easily remember it, the advantage of the short form is
    that you have less typing. Many options have both forms : long and
    short and you can use whichever you want.

  <li><p>All my programs should accept option <tt>--help</tt>
    (short form <tt>-h</tt>). Using this option instructs the program
    to print some short usage instructions and a short description of
    available options.

  <li><p>All my programs should accept option <tt>--version</tt>
    (short form <tt>-v</tt>). Using this option instructs the program
    to print version number. Here's a description of
    <?php echo a_href_page(
    'versioning scheme used in all my programs', 'versioning'); ?>.
    Version number is always printed on standard output, never in some
    message box or something that; this allows to use calls like
    <tt>program_name --version</tt> in batch scripts, makefiles etc.

    <p><i>Note for Windows users of my programs that don't create a console</i>
    (e.g. <?php echo a_href_page('malfunction', 'malfunction'); ?>,
    <?php echo a_href_page('kambi_lines', 'kambi_lines'); ?> or
    <?php echo a_href_page('glViewImage', 'glviewimage'); ?>):
    when Windows program does not explicitly create a console,
    it usually has no standard output available.
    You must explicitly redirect it's stdout when using option <tt>--version</tt>.

  <li><p>If option requires one argument, you can give it as<br>
    <tt>--long-option=argument</tt> (with '=') or<br>
    <tt>--long-option argument</tt> (passing argument as separate parameter) or<br>
    <tt>-p=argument</tt> (same as the first one, but using short form) or<br>
    <tt>-p argument</tt> (same as the second one, but using short form).

    <p>E.g. following methods of running
    <?php echo a_href_page("view3dscene", "view3dscene"); ?>
    are equivalent:
<pre>
  view3dscene --navigation=Examine scene.wrl
  view3dscene --navigation Examine scene.wrl
</pre>

    <p>If option <i>allows</i> but <i>not requires</i> an argument,
    you have to use <tt>--long-option=argument</tt> or
    <tt>-p=argument</tt> if you want to give an argument.

  <li><p>Short options can be <i>combined</i>. This means that you
    can put more than one short option in a one parameter, e.g.
<pre>
  program -abc
</pre>
    means the same as
<pre>
  program -a -b -c
</pre>

    <p>Note that only the last option in such "combined parameter" may
    take an argument, e.g.
<pre>
  program -de=50
</pre>
    means the same as
<pre>
  program -d -e=50
</pre>
   but if you want to give an argument for <tt>-d</tt> and <tt>-e</tt>,
   you can't combine them : you must use something like
<pre>
  program -d=40 -e=50
</pre>

  <li><p>Special option <tt>--</tt> means "do not interpret following parameters".
    You can use it if you have files with names beginning with a <tt>'-'</tt>.

    <p>E.g. suppose you have a file named <tt>--file.png</tt> and you want
    to view it using <?php echo a_href_page("glViewImage", "glviewimage"); ?>.
    If you call
    <pre>  glViewImage --file.png</pre>
    then glViewImage will exit with an error <i>'invalid
    long option "--file.png"'</i>. Even worse, if you have a file named
    <tt>--geometry</tt> (<tt>--geometry</tt> not only begins with a dash
    but it even IS a valid option for glViewImage) and you call
    <pre>  glViewImage --geometry</pre>
    then glViewImage will try to interpret the <tt>--geometry</tt> option
    and will give an error <i>'missing argument for "--geometry"'</i>.
    So you can force glViewImage to treat <tt>--file.png</tt> or
    <tt>--geometry</tt> as file names using :
<pre>
  glViewImage -- --file.png
  glViewImage -- --geometry
</pre>

</ul>

<?php
  if (!IS_GEN_LOCAL) {
    $counter = php_counter("common_options", TRUE);
  };

  vrmlengine_footer();
?>
