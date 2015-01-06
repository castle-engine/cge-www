<?php
  require_once "castle_engine_functions.php";
  castle_header("Standard command-line options", NULL,
    array('all_programs'));
?>

<h2>Standard command-line options</h2>

<p>This page describes just some usual conventions used when interpreting
command-line options by my programs (actually, these conventions are widely
known and used by most good programs in the world) :
<ul>
  <li><p>Long options start with <code>--</code> (two dashes), like
    <code>--long-option</code>. Short options start with <code>-</code> (one dash)
    and consist of one letter, like <code>-p</code>. The advantage of the long form
    is that you can easily remember it, the advantage of the short form is
    that you have less typing. Many options have both forms : long and
    short and you can use whichever you want.

  <li><p>All my programs should accept option <code>--help</code>
    (short form <code>-h</code>). Using this option instructs the program
    to print some short usage instructions and a short description of
    available options.

  <li><p>All my programs should accept option <code>--version</code>
    (short form <code>-v</code>). Using this option instructs the program
    to print version number. Here's a description of
    <?php echo a_href_page(
    'versioning scheme used in all my programs', 'versioning'); ?>.
    Version number is always printed on standard output, never in some
    message box or something that; this allows to use calls like
    <code>program_name --version</code> in batch scripts, makefiles etc.

    <p><i>Note for Windows users of my programs that don't create a console</i>
    (e.g. <?php echo a_href_page('malfunction', 'malfunction'); ?>,
    <?php echo a_href_page('kambi_lines', 'kambi_lines'); ?> or
    <?php echo a_href_page('glViewImage', 'glviewimage'); ?>):
    when Windows program does not explicitly create a console,
    it usually has no standard output available.
    You must explicitly redirect it's stdout when using option <code>--version</code>.

  <li><p>If option requires one argument, you can give it as<br>
    <code>--long-option=argument</code> (with '=') or<br>
    <code>--long-option argument</code> (passing argument as separate parameter) or<br>
    <code>-p=argument</code> (same as the first one, but using short form) or<br>
    <code>-p argument</code> (same as the second one, but using short form).

    <p>E.g. following methods of running
    <?php echo a_href_page("view3dscene", "view3dscene"); ?>
    are equivalent:
<pre>
  view3dscene --navigation=Examine scene.wrl
  view3dscene --navigation Examine scene.wrl
</pre>

    <p>If option <i>allows</i> but <i>not requires</i> an argument,
    you have to use <code>--long-option=argument</code> or
    <code>-p=argument</code> if you want to give an argument.

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
   but if you want to give an argument for <code>-d</code> and <code>-e</code>,
   you can't combine them : you must use something like
<pre>
  program -d=40 -e=50
</pre>

  <li><p>Special option <code>--</code> means "do not interpret following parameters".
    You can use it if you have files with names beginning with a <code>'-'</code>.

    <p>E.g. suppose you have a file named <code>--file.png</code> and you want
    to view it using <?php echo a_href_page("glViewImage", "glviewimage"); ?>.
    If you call
    <pre>  glViewImage --file.png</pre>
    then glViewImage will exit with an error <i>'invalid
    long option "--file.png"'</i>. Even worse, if you have a file named
    <code>--geometry</code> (<code>--geometry</code> not only begins with a dash
    but it even IS a valid option for glViewImage) and you call
    <pre>  glViewImage --geometry</pre>
    then glViewImage will try to interpret the <code>--geometry</code> option
    and will give an error <i>'missing argument for "--geometry"'</i>.
    So you can force glViewImage to treat <code>--file.png</code> or
    <code>--geometry</code> as file names using :
<pre>
  glViewImage -- --file.png
  glViewImage -- --geometry
</pre>

</ul>

<?php
  castle_footer();
?>
