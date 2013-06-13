<?php
require_once 'castle_engine_functions.php';
tutorial_header('OpenGL context', 'Creating a window (' .
api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html', false) .
') or a Lazarus component (' .
api_link('TCastleControl', 'CastleControl.TCastleControl.html', false) .
')');
?>

<p>First of all, you need to initialize a rectangular area on the screen
that will be able to display 3D content. This is called OpenGL
context. There are two ways of doing this:

<ol>
  <li><p><b>You can use our own <?php api_link('CastleWindow', 'CastleWindow.html'); ?>  unit</b>, that defines window class
    <?php api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html'); ?>.
    This gives you the best OpenGL capabilities, avoids some problems
    with Lazarus application loop (for example, mouse look is smooth),
    and is usually best for 3D games.
    <!-- This way you don't get native-looking
    controls (only menu bar and simple dialogs), but you can use OpenGL controls
    (<?php api_link('CastleControls', 'CastleControls.html'); ?> unit),
    which is usually OK for games. -->
    </p>

    <p>You can develop such programs using Lazarus, or any other text
    editor. The only real requirement is having a
    <a href="http://www.freepascal.org/">Free Pascal Compiler</a> installed.

    <!--
    The left column of this tutorial will follow this way. We'll still be
    using Lazarus to compile our program, simply because it's the easiest
    and most popular method. But we will not use Lazarus form designer. As
    far as Lazarus is concerned, we're creating a "Custom Application"
    project.-->

    <p><b>Create new project using Lazarus "New Project" menu item.
    Choose "Custom Application".</b></p>

    <p>Place this source code in your program file (lpr).</p>

<?php echo pascal_highlight(
'uses CastleWindow;
var
  Window: TCastleWindow;
begin
  Window := TCastleWindow.Create(Application);
  Window.OpenAndRun;
end.'); ?>

    <p>Press "Run" and behold, a window! (without anything inside,
    so you will just see the default black background).</p>

    <p>Above we set global <?php api_link('Application',
    'CastleWindow.html#Application'); ?>
    as the owner (first constructor parameter) of <tt>Window</tt>. This way
    we don't have to care about freeing it later. When the <tt>Application</tt> is
    freed (which is done automatically by <tt>CastleWindow</tt> unit finalization),
    the <tt>Window</tt> will be freed too. You can pass <tt>nil</tt> instead of
    <tt>Application</tt> if you really want to avoid this automatic memory
    management, and free things yourself. This whole "owner" mechanism is
    actually a normal behavior of ObjectPascal components.</p>

  </li>

  <li><p><b>Alternative is to use our
    <?php api_link('TCastleControl', 'CastleControl.TCastleControl.html'); ?>
    inside normal Lazarus
    form</b>. Just pick <tt>TCastleControl</tt> from the component palette (tab
    <i>"Castle"</i>) and drop it on a regular Lazarus form. This allows for
    seamless integration with the normal application design using
    Lazarus, as you have a normal Lazarus form where you can place
    normal buttons and such.
    <!--
     The downside is that our TCastleControl
    inherits from Lazarus TOpenGLComponent, and it has some limitations
    (sometimes mouse look may
    stutter a little because of Lazarus event loop processing).
    -->

    <p>To develop such programs you use Lazarus, period.

    <p>This tutorial doesn't try to suggest any particular method
    (<?php api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html'); ?> or
    <?php api_link('TCastleControl', 'CastleControl.TCastleControl.html'); ?>).
    Both approaches make sense, really, and it's your
    choice. You should choose Lazarus method (<tt>TCastleControl</tt>) if you want
    to integrate game context with normal GUI (Lazarus forms, controls) or
    if you're just more comfortable with dropping components on a Lazarus
    form. If you want best OpenGL features, or if you're just more
    comfortable working outside of Lazarus (like with custom editor and
    only calling FPC through command-line) then probably you want to choose
    <tt>TCastleWindow</tt> method. The difference is only how you start: create a
    <tt>TCastleWindow</tt> instance by code, or drop <tt>TCastleControl</tt>
    on a Lazarus
    form. Everything else goes <i>almost</i> the same, as the <tt>TCastleControl</tt>
    and <tt>TCastleWindow</tt> are designed to be very similar
    (for example they both have <tt>Controls</tt> list, where
    we add 2D and 3D stuff of our engine).

    <p>In case of using Lazarus forms, you will usually want to place the
    initialization code inside your form's methods. In the simplest case,
    just place it inside the
    <?php api_link('OnGLContextOpen event', 'CastleControl.TCastleControlBase.html#OnGLContextOpen'); ?>
    event (you
    could also move non-OpenGL parts into <tt>TForm.OnCreate</tt>, that happens
    earlier).

    <p><b>Create new project using Lazarus "New Project" menu item. Choose
    "Application". Drop TCastleControl on your form and resize it to
    fit the window. Press "Run" and behold :)</b>

    <p>Note about key handling (applies only to <tt>TCastleControl</tt>): Like every
    proper Lazarus control, our <tt>TCastleControl</tt> receives the keys only when
    it has <i>focus</i>. The control <i>does not</i> capture all the keys
    pressed over the form (this would be bad, as other controls, maybe
    even other <tt>TCastleControl</tt> on the same form, may want to handle
    them). To make sure that controlling the camera by keys (arrow keys,
    and/or AWSD and other keys, more about keys later) works, make sure
    that your control has a focus.

    <p>In the simplest case, just call

<?php echo pascal_highlight(
'Control.SetFocus;'); ?>

    <p>whenever you want.

    <p>There's no visual indicator when <tt>TCastleControl</tt> has focus
    (as there's
    no standard way to show it, that would be pleasing for various game
    applications).

    <p>How you deal with focus really depends on your application:

    <ol>
      <li><p>In the simplest cases, the problem simply doesn't exist, as
        <tt>TCastleControl</tt> is the only thing on your form able to receive focus
        &mdash; so it always has focus. All other components on this Lazarus form
        (if any) are unfocusable, like <tt>TSpeedButton</tt>.</p></li>

      <li><p>If you want to use other focusable controls, it's really up to you
        how to present it to user. In principle, you don't have to do
        anything, focus works, and it can be switched into/out of
        <tt>TCastleControl</tt> by the Tab key or clicking with mouse on other
        controls.</p>

        <ol>
          <li><p>You may want to create a special key shortcut to quickly shift
            focus to your control (calling <tt>Control.SetFocus</tt>).</p></li>

          <li><p>You may want to draw some visual indication, like a border around
            <tt>TCastleControl</tt>, when it's focused. Actually, our <tt>TCastleControl</tt>
            may contain inside our own controls (<tt>TUIControl</tt> class), so you may
            want to draw <tt>TUIControl</tt> that is focused (see
            <tt>examples/3d_rendering_processing/multiple_viewports.lpr</tt> for simple
            example that shows which viewport is active, having 4 viewports
            within a single OpenGL context).</p></li>

          <li><p>Finally, if you really want, you can also use standard Lazarus
            features like <tt>TForm.KeyPreview</tt> and
            <tt>TForm.OnKeyDown</tt> / <tt>TForm.OnKeyUp</tt> to
            capture some keys at form level and pass them directly to chosen
            control.</p></li>
        </ol>
      </li>
    </ol>
  </li>
</ol>

<?php
tutorial_footer();
?>
