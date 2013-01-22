<?php
  require_once 'tutorial_common.php';
  tutorial_header('Logging');
?>

Easy: Use CastleLog unit, call InitializeLog, and you have your log. Log by default goes to standard error output (StdErr), which is standard on Unixes and avoids users asking questions "where is the log file". Be careful on Windows, for GUI application it may be undefined --- you can either suggest users to run your program from command-line to get log, or you can pass a parameter to InitializeLog to generate log to any stream (like a temporary file).

A lot of engine components will immediately use the log, if you initialized it. To use it yourself, in the simplest cases just do

[[
WritelnLog('Category', 'My Log Message');
]]

('Category' is useful to easily spot your messages later. There are no hard rules about it, i.e. you're free to just invent your own category names here, like 'Creatures' or 'Rendering'.)

<?php
  tutorial_footer();
?>
