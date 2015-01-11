<?php
require_once 'castle_engine_functions.php';
tutorial_header('Sidenote about the focus of TCastleControl');
?>

<p>These notes are interesting to you only if you use
<?php api_link('TCastleControl', 'CastleControl.TCastleControl.html'); ?>
 and some other key-handling control on the same Lazarus form.

<p>Like every
proper Lazarus control, our
<?php api_link('TCastleControl', 'CastleControl.TCastleControl.html'); ?>
 receives the keys only when
it has <i>focus</i>. The control <i>does not</i> capture all the keys
pressed over the form (this would be bad, as other controls, maybe
even other <code>TCastleControl</code> on the same form, may want to handle
them). To make sure that controlling the camera by keys (arrow keys,
and/or AWSD and other keys, more about keys later) works, make sure
that your control has a focus.

<p>In the simplest case, just call

<?php echo pascal_highlight(
'Control.SetFocus;'); ?>

<p>whenever you want.

<p>There's no visual indicator when <code>TCastleControl</code> has focus
(as there's
no standard way to show it, that would be pleasing for various game
applications).

<p>How you deal with focus really depends on your application:

<ol>
  <li><p>In the simplest cases, the problem simply doesn't exist, as
    <code>TCastleControl</code> is the only thing on your form able to receive focus
    &mdash; so it always has focus. All other components on this Lazarus form
    (if any) are unfocusable, like <code>TSpeedButton</code>.</p></li>

  <li><p>If you want to use other focusable controls, it's really up to you
    how to present it to user. In principle, you don't have to do
    anything, focus works, and it can be switched into/out of
    <code>TCastleControl</code> by the Tab key or clicking with mouse on other
    controls.</p>

    <ol>
      <li><p>You may want to create a special key shortcut to quickly shift
        focus to your control (calling <code>Control.SetFocus</code>).</p></li>

      <li><p>You may want to draw some visual indication, like a border around
        <code>TCastleControl</code>, when it's focused. Actually, our <code>TCastleControl</code>
        may contain inside our own controls (<code>TUIControl</code> class), so you may
        want to draw <code>TUIControl</code> that is focused (see
        <code>examples/3d_rendering_processing/multiple_viewports.lpr</code> for simple
        example that shows which viewport is active, having 4 viewports
        within a single OpenGL context).</p></li>

      <li><p>Finally, if you really want, you can also use standard Lazarus
        features like <code>TForm.KeyPreview</code> and
        <code>TForm.OnKeyDown</code> / <code>TForm.OnKeyUp</code> to
        capture some keys at form level and pass them directly to chosen
        control.</p></li>
    </ol>
  </li>
</ol>

<?php
tutorial_footer();
?>
