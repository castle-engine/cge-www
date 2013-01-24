<?php
require_once 'castle_engine_functions.php';
tutorial_header('Logging');
?>

<p>Simply use <?php api_link('CastleLog', 'CastleLog.html'); ?> unit and call
<?php api_link('InitializeLog', 'CastleLog.html#InitializeLog'); ?>
 to have logging enabled.
Log by default goes to <i>standard error output</i> (<tt>StdErr</tt>,
also known as <tt>ErrOutput</tt> in Pascal), which is
standard on Unixes (and avoids users asking questions <i>"where is the log
file"</i>). Be careful on Windows, for GUI applications the <tt>StdErr</tt>
may be undefined &mdash; you can either suggest users to run your program from
command-line to get the log, or you can pass a parameter to <tt>InitializeLog</tt>
to generate log to any stream (like to a temporary file).

<p>A lot of engine components automatically use this log.
To use it yourself, just call
<?php api_link('WritelnLog', 'CastleLog.html#WritelnLog'); ?>
 (or one of it's friends, see
<?php api_link('CastleLog', 'CastleLog.html'); ?> unit).

<p>To summarize:

<?php echo pascal_highlight(
'uses ..., CastleLog;

...
InitializeLog(\'1.0.0\'); // parameter describes program version, will be logged

...
WritelnLog(\'My Category\', \'My Log Message\');'); ?>

<p><tt>'My Category'</tt> is useful to easily spot your messages later. There are
no hard rules about it, so you're free to just invent your own
category names here, whatever works for you.

<?php
tutorial_footer();
?>
