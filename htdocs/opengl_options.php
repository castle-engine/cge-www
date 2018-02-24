<?php
require_once "castle_engine_functions.php";
castle_header("Standard command-line options for OpenGL programs", array(
  'path' => array('all_programs')
));
?>

<h2>Standard command-line options (for programs with a graphical interface)</h2>

<p>Most of the programs using <?php echo a_href_page('Castle Game Engine',
'index'); ?> accept the command-line options listed below. <small>(Developers: <a href="https://castle-engine.io/apidoc/html/CastleWindow.TCastleWindowCustom.html#ParseParameters">use this in your own programs</a>).</small>

<dl class="command_line_options_list">
  <dt>--fullscreen</dt>

  <dd><p>Start in fullscreen mode.

    <p>For some programs, you can also switch between fullscreen and windowed
    mode at runtime. Standard key shortcut for this is <code>F11</code>.
  </dd>

  <dt>--fullscreen-custom WIDTHxHEIGHT</dt>

  <dd><p>Start in fullscreen mode with screen resolution changed
    to WIDTH x HEIGHT.

    <p>If given WIDTH x HEIGHT resolution is not available then error message
    will be shown and program will start in fullscreen mode using
    current screen resolution.

  <dt>--geometry WIDTHxHEIGHT&lt;sign&gt;XOFF&lt;sign&gt;YOFF</dt>

  <dd><p>Start in window mode with given WIDTH and HEIGHT
    positioned at (XOFF, YOFF).

    <p>&lt;sign&gt; is either "<code>+</code>" or "<code>-</code>".
    <code>+XOFF</code> means that left window border should be
    <code>XOFF</code> pixels from left screen border. Note that
    <code>XOFF</code> itself may be negative (e.g. in
    <code>--geometry 100x100+-100+100</code>) and then window will be positioned
    slightly offscreen. Similarly, <code>-XOFF</code> means that right
    window border should be <code>XOFF</code> pixels from right screen border.
    Similar for <code>+YOFF</code> or <code>-YOFF</code> &mdash; they specify
    distance from upper or lower screen border.

    <p>You can omit the <code>&lt;sign&gt;XOFF&lt;sign&gt;YOFF</code> part
    and provide only <code>WIDTHxHEIGHT</code> part. You can also omit
    <code>WIDTHxHEIGHT</code> part and provide only
    <code>&lt;sign&gt;XOFF&lt;sign&gt;YOFF</code> part.

    <!--  Literka "x" w <code>WIDTHxHEIGHT</code> może tak naprawdę
    być dużym "X" lub małym "x". -->

    <p>This option works the same as standard <code>-geometry</code> option for
    most XWindows programs.

    <p>Note that window manager <!-- under XWindows
    or Windows itself under Windows --> may disallow or modify requested
    window position and/or size. Note that you can also of course change
    window size and position of the window while the program is running
    (by dragging window border and such).</dd>

  <dt>--display DISPLAY-NAME</dt>
  <dd><p><i>(Only on platforms using X Windows, in practice: Unix)</i>
    <br>Set the XWindows display to use.</dd>
</dl>

<p>See also <?php echo a_href_page(
"general notes about options understood by our programs", "common_options"); ?>.

<?php
  castle_footer();
?>
