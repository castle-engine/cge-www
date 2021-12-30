<?php
require_once 'castle_engine_functions.php';
castle_header('Start: Display a window');
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
    <a href="https://castle-engine.io/build_tool">CGE build tool</a>,
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
