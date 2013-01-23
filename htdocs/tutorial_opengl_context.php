<?php
  require_once 'tutorial_common.php';
  tutorial_header('OpenGL context', 'Creating a window (TCastleWindow) or a Lazarus component (TCastleControl)');
?>

<p>First of all, you need to initialize a rectangular area on the screen
that will be able to display 3D content. This is called OpenGL
context. There are two ways of doing this:

<ol>
  <li><p><b>You can use our own CastleWindow unit</b>, that defines window class
    TCastleWindow. This gives you the most OpenGL capabilities (our
    TCastleWindow allows to request some funny stuff from OpenGL, not
    possible otherwise) and is usually best for 3D games.</p>

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
'var
  Window: TCastleWindow;
begin
  Window := TCastleWindow.Create(Application);
  Window.OpenAndRun;
end.'); ?>

    <p>Press "Run" and behold, a window! (without anything inside,
    so you will just see the default black background).</p>

    <p>As you see, we give Application (this is defined in our CastleWindow
    unit) as the owner (first constructor parameter) of Window. This way
    we don't have to care about freeing it later. When the Application is
    freed (which is done automatically by CastleWindow unit finalization),
    the Window will be freed too. You can pass "nil" instead of
    "Application" if you really want to avoid this automatic memory
    management, and free things yourself. This whole "owner" mechanism is
    actually a normal behavior of ObjectPascal components.</p>

  </li>

  <li><p><b>Alternative is to use our TCastleControl inside normal Lazarus
    form</b>. Just pick TCastleControl from the component palette (tab
    "Castle") and drop it on a regular Lazarus form. This allows for
    seamless integration with the normal application design using
    Lazarus, as you have a normal Lazarus form where you can place
    normal buttons and such. The downside is that our TCastleControl
    inherits from Lazarus TOpenGLComponent, and it has some limitations
    (not all OpenGL features can be requested, sometimes mouse look may
    stutter a little because of Lazarus event loop processing).

    <p>To develop such programs you use Lazarus, period.

    <p>This tutorial doesn't suggest any particular method (TCastleWindow or
    TCastleControl). Both approaches make sense, really, and it's your
    choice. You should choose Lazarus method (TCastleControl) if you want
    to integrate game context with normal GUI (Lazarus forms, controls) or
    if you're just more comfortable with dropping components of a Lazarus
    form. If you want best OpenGL features, or if you're just more
    comfortable working outside of Lazarus (like with custom editor and
    only calling FPC through a script) then probably you want to choose
    TCastleWindow method. The difference is only how you start: create a
    TCastleWindow instance by code, or drop TCastleControl on a Lazarus
    form. Everything else goes *almost* the same, as the TCastleControl
    and TCastleWindow are designed specifically to resemble each other, so
    they both share similar methods and properties (like "Controls", where
    we add our engine 2D and 3D stuff).

    <p>In case of using Lazarus forms, you will usually want to place the
    initialization code inside your form's methods. In the simplest case,
    just place it inside the TCastleControl.OnGLContextInit event (you
    could also move non-OpenGL parts into TForm.OnCrease, that happens
    earlier).

    <p>Create new project using Lazarus "New Project" menu item. Choose
    "Application". Drop TCastleControl on your form and resize it to
    fit the window. Press "Run" and behold :)

    <p>Note about key handling (applies only to TCastleControl): Like every
    proper Lazarus control, our TCastleControl receives the keys only when
    it has <i>focus</i>. The control <i>does not</i> capture all the keys
    pressed over the form (this would be bad, as other controls, maybe
    even other TCastleControl on the same form, may want to handle
    them). To make sure that controlling the camera by keys (arrow keys,
    and/or AWSD and other keys, more about keys later) works, make sure
    that your control has a focus.

    <p>In the simplest case, just call

<?php echo pascal_highlight(
'Control.SetFocus;'); ?>

    <p>whenever you want.

    <p>There's no visual indicator when TCastleControl has focus (as there's
    no standard way to show it, that would be pleasing for various game
    applications).

    <p>How you deal with focus really depends on your application:

    <ol>
      <li><p>In the simplest cases, the problem simply doesn't exist, as
        TCastleControl is the only thing on your form able to receive focus
        &mdash; so it always has focus. You may limit yourself to use only
        unfocusable things, like TSpeedButton, on the Lazarus form &mdash; to
        keep it simple.</p></li>

      <li><p>If you want to use other focusable controls, it's really up to you
        how to present it to user. In princible, you don't have to do
        anything, focus works, and it can be switched into/out of
        TCastleControl by the Tab key or clicking with mouse on other
        controls.</p>

        <ol>
          <li><p>You may want to create a special key shortcut to quickly shift
            focus to your control (calling Control.SetFocus).</p></li>

          <li><p>You may want to draw some visual indication, like a border around
            TCastleControl, when it's focused. Actually, our TCastleControl
            may contain inside our own controls (TUIControl class), so you may
            want the draw TUIControl that is focused (see
            examples/3d_rendering_processing/multiple_viewports.lpr for simple
            example that shows which viewport is active, having 4 viewports
            within a single OpenGL context).</p></li>

          <li><p>Finally, if you really want, you can also use standard Lazarus
            features like TForm.KeyPreview and TForm.OnKeyDown / OnKeyUp to
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
