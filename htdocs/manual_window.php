<?php
require_once 'castle_engine_functions.php';
manual_header('Start: Display a window');
?>

<p>You need to initialize a rectangular area on the screen
that will be able to display rendered content. There are two ways of doing this:

<ol>
  <li>
    <p>Use our own window class
    <?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?>.

    <p>The <i>"New Project"</i> templates used by our <a href="manual_editor.php">CGE editor</a>
    set this up by default.
    You use <?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?>
    if you just follow the (advised) code organization created by <a href="manual_editor.php">CGE editor</a>.

    <p>So <b>you don't need to do anything to make this happen.
    Read below only if you are interested in manually controlling the window creation.</b>

    <p>To manually create a window and display it you can use such simple program:

<?php echo pascal_highlight(
'{$mode objfpc}{$H+} // you can also set ObjFpc and long strings in project compiler options

uses CastleWindow;
var
  Window: TCastleWindowBase;
begin
  Window := TCastleWindowBase.Create(Application);
  Window.OpenAndRun;
end.'); ?>

    <p>You can compile such application using
    <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">CGE build tool</a>,
    just set <code>standalone_source=...</code> to point to this program file.
    The <a href="manual_editor.php">CGE editor</a> will also be able to compile it,
    as our editor just calls our build tool.

    <p>You can also compile such application using Lazarus.
    Create in Lazarus new application using <i>"Project-&gt;Simple Program"</i> menu item,
    and inside <i>"Project->Project Inspector"</i> window add 2 new requirements:
    <code>castle_base</code> and <code>castle_window</code>.

    <p>You can develop such programs using Lazarus, or any other text
    editor. The only real requirement is having a
    <a href="http://www.freepascal.org/">Free Pascal Compiler</a> installed.

    <p>We advise using
    <?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?>
    over
    <?php api_link('TCastleControlBase', 'CastleControl.TCastleControlBase.html'); ?> because:

    <ol>
      <li>
        <p>Our <?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?>
        avoids some problems with Lazarus application loop (for example, to make mouse look perfectly smooth).
      <li>
        <p>Our <?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?>
        works on all CGE platforms (desktop,
        mobile - Android and iOS,
        consoles - <a href="https://github.com/castle-engine/castle-engine/wiki/Nintendo-Switch">Nintendo Switch</a>).
    </ol>

    <!-- This way you don't get native-looking
    controls (only menu bar and simple dialogs), but you can use OpenGL controls
    (<?php api_link('CastleControls', 'CastleControls.html'); ?> unit),
    which is usually OK for games. -->
    </p>

<!--
    <p>Compile and run this program and behold, a black window :)</p>
-->

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

  <li>
    <p><b>Integration inside Lazarus form:</b>
    Use our
    <?php api_link('TCastleControlBase', 'CastleControl.TCastleControlBase.html'); ?>
    inside normal Lazarus form.
    You should choose this method if you want
    to integrate engine rendering with normal LCL GUI (Lazarus forms, controls) or
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
      <li>Pick <code>TCastleControlBase</code> from the component palette (tab
        <i>"Castle"</i>) and drop it on a regular Lazarus form.
      <li>Press "Run" :)
    </ul>

    <!--
     The downside is that our TCastleControlBase
    inherits from Lazarus TOpenGLComponent, and it has some limitations
    (sometimes mouse look may
    stutter a little because of Lazarus event loop processing).
    -->

    <!--p>To develop such programs you use Lazarus, period.-->

<!--
    <p>There is no example code to show here, as you don't need to write
    any code to make this work :)
    In the following manual chapters we will show some code to run
    your game. You will usually want to place it inside the
    <code>OnCreate</code> event of the Lazarus form.
-->
</ol>

<?php
manual_footer();
?>
