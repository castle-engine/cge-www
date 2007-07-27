<?php
  require "vrmlengine_functions.php";
  camelot_header("Standard command-line options understood by my OpenGL programs",
    LANG_EN);
?>

<h2>Standard command-line options understood by my OpenGL programs</h2>

<p>All <!-- or almost all --> OpenGL programs that you can find on my pages
accept some set of standard command-line options:

<dl class="command_line_options_list">
  <dt>--fullscreen</dt>

  <dd><p>Start program in fullscreen mode.

    <p>Note that you can change fullscreen mode at runtime with most
    of my programs &mdash; standard key shortcut is <tt>Ctrl+F</tt>.
  </dd>

  <dt>--fullscreen-custom WIDTHxHEIGHT</dt>

  <dd><p>Start program in fullscreen mode with screen resolution changed
    to WIDTH x HEIGHT.

    <p>If given WIDTH x HEIGHT resolution is not available then error message
    will be shown and program will start in fullscreen mode using
    current screen resolution.

  <dt>--geometry WIDTHxHEIGHT&lt;sign&gt;XOFF&lt;sign&gt;YOFF</dt>

  <dd><p>Start program in window with given WIDTH and HEIGHT
    positioned at (XOFF, YOFF).

    <p>&lt;sign&gt; is either "<tt>+</tt>" or "<tt>-</tt>".
    <tt>+XOFF</tt> means that left window border should be
    <tt>XOFF</tt> pixels from left screen border. Note that
    <tt>XOFF</tt> itself may be negative (e.g. in
    <tt>--geometry 100x100+-100+100</tt>) and then window will be positioned
    slightly offscreen. Similarly, <tt>-XOFF</tt> means that right
    window border should be <tt>XOFF</tt> pixels from right screen border.
    Similar for <tt>+YOFF</tt> or <tt>-YOFF</tt> &mdash; they specify
    distance from upper or lower screen border.

    <!-- p><i>
    (Mo¿na te¿ zamiast <tt>- -geometry</tt> napisaæ <tt>-geometry</tt> (z jednym
    my¶lnikiem). Standardowo programy pod XWindows akceptuj± parametr
    <tt>-geometry</tt> z jednym my¶lnikiem, ale ja osobi¶cie preferujê pisaæ
    dwa my¶lniki przed d³ugimi opcjami (zgodnie z konwencj± GNU) wiêc obie
    postacie s± dozwolone.)</i>

    Kambi: -geometry zabronione, w zamian za to uzywam ParsePars -->

    <p>You can omit the <tt>&lt;sign&gt;XOFF&lt;sign&gt;YOFF</tt> part
    and provide only <tt>WIDTHxHEIGHT</tt> part. You can also omit
    <tt>WIDTHxHEIGHT</tt> part and provide only
    <tt>&lt;sign&gt;XOFF&lt;sign&gt;YOFF</tt> part.

    <!--  Literka "x" w <tt>WIDTHxHEIGHT</tt> mo¿e tak naprawdê
    byæ du¿ym "X" lub ma³ym "x". -->

    <p>This option works the same as standard <tt>-geometry</tt> option for
    most XWindows programs.

    <p>Note that window manager <!-- under XWindows
    or Windows itself under Windows --> may disallow or modify requested
    window position and/or size. Note that you can also of course change
    window size and position of the window while the program is running
    (by dragging window border and such).</dd>

  <dt>--display DISPLAY-NAME</dt>

  <dd><p>Set the XWindows display to use.

    <p>Be warned that I experienced unstable behavior
    of X server (i.e. hangs, up to hanging my <i>both</i> displays)
    and SIGFPE errors at (random?) points when running some of my programs.
    Some of my (and others) programs seem to run perfectly fine on another
    display, some fail. This is related to OpenGL &mdash;
    most probably NVidia drivers problems.
    In other words: if you want to use this option, go on,
    but don't be surprised if something will not work smoothly.
  </dd>
</dl>

<p>See also <?php echo a_href_page(
"general notes about options understood by my programs", "common_options"); ?>.

<?php
  if (!IS_GEN_LOCAL) {
    $counter = php_counter("opengl_options", TRUE);
  };

  camelot_footer();
?>
