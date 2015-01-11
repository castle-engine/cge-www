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
    Choose "Custom Application" (or "Project-&gt;Simple Program").
    Using "Project->Project Inspector" window add a "New Requirement"
    and choose <code>castle_base</code> package.
    Then add another requirement and choose <code>castle_window</code>
    package. (You should have <code>castle_xxx</code> packages compiled
    in a previous step).</b></p>

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

    <p>Press "Run" and behold, a window! (without anything inside,
    so you will just see the default black background).</p>

    <p>Above we set global <?php api_link('Application',
    'CastleWindow.html#Application'); ?>
    as the owner (first constructor parameter) of <code>Window</code>. This way
    we don't have to care about freeing it later. When the <code>Application</code> is
    freed (which is done automatically by <code>CastleWindow</code> unit finalization),
    the <code>Window</code> will be freed too. You can pass <code>nil</code> instead of
    <code>Application</code> if you really want to avoid this automatic memory
    management, and free things yourself. This whole "owner" mechanism is
    actually a normal behavior of ObjectPascal components.</p>

  </li>

  <li><p><b>Alternative is to use our
    <?php api_link('TCastleControl', 'CastleControl.TCastleControl.html'); ?>
    inside normal Lazarus
    form</b>. Just pick <code>TCastleControl</code> from the component palette (tab
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
    choice. You should choose Lazarus method (<code>TCastleControl</code>) if you want
    to integrate game context with normal GUI (Lazarus forms, controls) or
    if you're just more comfortable with dropping components on a Lazarus
    form. If you want best OpenGL features, or if you're just more
    comfortable working outside of Lazarus (like with custom editor and
    only calling FPC through command-line) then probably you want to choose
    <code>TCastleWindow</code> method. The difference is only how you start: create a
    <code>TCastleWindow</code> instance by code, or drop <code>TCastleControl</code>
    on a Lazarus
    form. Everything else goes <i>almost</i> the same, as the <code>TCastleControl</code>
    and <code>TCastleWindow</code> are designed to be very similar
    (for example they both have <code>Controls</code> list, where
    we add 2D and 3D stuff of our engine).

    <p>In case of using Lazarus forms, you will usually want to place the
    initialization code inside your form's methods. In the simplest case,
    just place it inside the
    <?php api_link('OnGLContextOpen event', 'CastleControl.TCastleControlBase.html#OnGLContextOpen'); ?>
    event (you
    could also move non-OpenGL parts into <code>TForm.OnCreate</code>, that happens
    earlier).

    <p><b>Create new project using Lazarus "New Project" menu item. Choose
    "Application". Drop TCastleControl on your form and resize it to
    fit the window. Press "Run" and behold :)</b>
  </li>
</ol>

<?php
tutorial_footer();
?>
