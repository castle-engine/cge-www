<?php
require_once 'castle_engine_functions.php';
manual_header('Logging');
?>

<p>Simply use <?php api_link('CastleLog', 'CastleLog.html'); ?> unit and call
<?php api_link('InitializeLog', 'CastleLog.html#InitializeLog'); ?>
 to have logging enabled.

<ul>
  <li><p><i>On Unix (Linux, Mac OS X...), and on Windows console applications</i>,
    logging by default goes to the <i>standard error output</i>.
    This is called <code>StdErr</code>, also known as <code>ErrOutput</code> in Pascal.
    This is the standard behaviour for Unix and console apps.
    The nice thing about it is that it avoids users asking questions <i>"where is the log
    file"</i> &mdash; because they choose the name and location of the log file themselves.

  <li><p><i>On Windows GUI applications</i>
    <!-- the standard error is (usually) not available, so -->
    we log to the file <code>xxx.log</code> in the current directory
    (where <code>xxx</code> is your <code>ApplicationName</code>).

  <li><p><i>On Windows GUI libraries (like NPAPI plugins)</i>
    <!-- the standard error is (usually) not available, so -->
    we log to the file <code>xxx.log</code> in the user's private directory
    (where <code>xxx</code> is your <code>ApplicationName</code>).
    <i>User's private directory</i> is determined by standard FPC functions,
    that in turn use standard Windows functions,
    it's something like
    <code>c:/Documents and Settings/USERNAME/Local settings/Application Data/xxx/xxx.log</code> or
    <code>c:/Users/USERNAME/AppData/Local/xxx/xxx.log</code> depending on your Windows version.

  <li><p><i>On Android</i> it goes to the standard device log.
    It that can be viewed using various Android tools,
    like <code>adb logcat</code>.

  <li><p>You can pass a parameter to <code>InitializeLog</code>
    to generate log to any stream.
</ul>

<p>A lot of engine components automatically use this log.
To use it yourself, just call
<?php api_link('WritelnLog', 'CastleLog.html#WritelnLog'); ?>
 (or one of it's friends, see
<?php api_link('CastleLog', 'CastleLog.html'); ?> unit).

<p>To summarize:

<?php echo pascal_highlight(
'uses ..., CastleLog;

... // place this in your initialization:
InitializeLog;

... // then use this wherever you want:
WritelnLog(\'My Log Message\');
WritelnLog(\'My Category\', \'My Log Message\');
WritelnWarning(\'My Warning\');'); ?>

<p><code>'My Category'</code> is useful to easily spot your messages later. There are
no hard rules about it, you're free to just invent your own category names here,
whatever works for you.

<?php
manual_footer();
?>
