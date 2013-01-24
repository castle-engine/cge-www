<?php
require_once 'castle_engine_functions.php';
tutorial_header('Logging');
?>

<p>Easy: Use CastleLog unit, call InitializeLog, and you have your
log. Log by default goes to standard error output (StdErr), which is
standard on Unixes and avoids users asking questions "where is the log
file". Be careful on Windows, for GUI application it may be undefined
&mdash; you can either suggest users to run your program from
command-line to get log, or you can pass a parameter to InitializeLog
to generate log to any stream (like a temporary file).

A lot of engine components will immediately use the log, if you
initialized it. To use it yourself, in the simplest cases just do

<?php echo pascal_highlight(
'uses ..., CastleLog;

...
InitializeLog(\'1.0.0\'); // parameter describes program version, will be logged

...
WritelnLog(\'My Category\', \'My Log Message\');'); ?>

<p><tt>'My Category'</tt> is useful to easily spot your messages later. There are
no hard rules about it, i.e. you're free to just invent your own
category names here, like <tt>'Creatures'</tt> or <tt>'Rendering'</tt>.

<?php
tutorial_footer();
?>
