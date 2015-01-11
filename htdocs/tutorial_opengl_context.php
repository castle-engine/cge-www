<?php
require_once 'castle_engine_functions.php';
tutorial_header('First program: display an empty window');
?>

<p>You need to initialize a rectangular area on the screen
that will be able to display 3D content. This is called an <i>OpenGL
context</i>. There are two ways of doing this:

<ol>
  <li><p><b>Integration inside Lazarus form:</b>
    Use our
    <?php api_link('TCastleControl', 'CastleControl.TCastleControl.html'); ?>
    inside normal Lazarus form.
    You should choose this method if you want
    to integrate game context with normal GUI (Lazarus forms, controls) or
    if you're just more comfortable with dropping components on a Lazarus
    form.
    <!--
    This allows for integration with the normal application design using
    Lazarus, as you have a normal Lazarus form where you can place
    normal buttons and such.
    -->

    <ul>
      <li>Create new project (using Lazarus <i>"New Project"</i> menu item).
        Choose <i>"Application"</i>.
      <li>Pick <code>TCastleControl</code> from the component palette (tab
        <i>"Castle"</i>) and drop it on a regular Lazarus form.
      <li>Press "Run" :)
    </ul>

    <!--
     The downside is that our TCastleControl
    inherits from Lazarus TOpenGLComponent, and it has some limitations
    (sometimes mouse look may
    stutter a little because of Lazarus event loop processing).
    -->

    <!--p>To develop such programs you use Lazarus, period.-->

    <p>There is no example code to show here, as you don't need to write
    any code to make this work :)
    In the following tutorial chapters we will show some code to run
    your game. You will want to place it inside the
    <?php api_link('OnOpen event', 'CastleControl.TCastleControlCustom.html#OnOpen'); ?>.

  <li><p><b>Without using Lazarus forms:</b>
    Use our own <?php api_link('CastleWindow', 'CastleWindow.html'); ?>  unit,
    that defines window class
    <?php api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html'); ?>.
    This avoids some problems
    with Lazarus application loop (for example, mouse look is smooth) and
    is suitable for desktops as well as Android/iOS.
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

    <p>Place this source code in your program file (lpr).</p>

<?php echo pascal_highlight(
'{$mode objfpc}{$H+} // you can also set ObjFpc and long strings in project compiler options

uses CastleWindow;
var
  Window: TCastleWindow;
begin
  Window := TCastleWindow.Create(Application);
  Window.OpenAndRun;
end.'); ?>

    <p>See the previous chapter for description how to compile your project,
    so that FPC can find our units like <code>CastleWindow</code>.
    Press "Run" and behold, a black window :)</p>

<!--
    <p><i>Note</i>: Above we set global <?php api_link('Application',
    'CastleWindow.html#Application'); ?>
    as the owner (first constructor parameter) of <code>Window</code>. This way
    we don't have to care about freeing it later. When the <code>Application</code> is
    freed (which is done automatically by <code>CastleWindow</code> unit finalization),
    the <code>Window</code> will be freed too. You can pass <code>nil</code> instead of
    <code>Application</code> if you really want to avoid this automatic memory
    management, and free things yourself. This whole "owner" mechanism is
    actually a normal behavior of ObjectPascal components.</p>
-->

  </li>
</ol>

<?php
tutorial_footer();
?>
