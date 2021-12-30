<?php
require_once 'castle_engine_functions.php';
castle_header('Logging');
?>

<h2>Usage</h2>

<p>Use <?php api_link('CastleLog', 'CastleLog.html'); ?> unit and call
<?php api_link('InitializeLog', 'CastleLog.html#InitializeLog'); ?>
 to have logging enabled.
Call <?php api_link('WritelnLog', 'CastleLog.html#WritelnLog'); ?>
 or <?php api_link('WritelnWarning', 'CastleLog.html#WritelnWarning'); ?> to log stuff.

<p>Some engine functions also automatically write to this log.
For example, initializing the OpenGL context logs the OpenGL and GPU features detected.

<p>An example:

<?php echo pascal_highlight_file('code-samples/logging.lpr'); ?>

<p><code>'My Category'</code> is useful to easily spot your messages later. There are
no hard rules about it, you're free to just invent your own category names here,
whatever works for you.

<h2>Where is the log stored?</h2>

<ul>
  <li><p>When running from <a href="manual_editor.php">CGE editor</a>, the log is displayed
    in the bottom editor panel.

  <li><p>When running using <a href="https://castle-engine.io/build_tool">CGE
    build tool <code>castle-engine run</code> command</a>, the log is displayed
    as the standard output.

  <li><p>When running the application as a normal user:

    <ul>
      <li><p><i>On Unix (Linux, macOS...), and for Windows console applications</i>,
        logging by default goes to the <i>standard output</i>.
        This is the standard behavior for Unix and console apps.

        <p>Users can redirect it by running the application from
        the command-line like <code>my_game &gt; log_file.log</code>.
        The nice thing about it is that it avoids users asking questions <i>"where is the log
        file"</i>.

        <p>You can avoid this by setting
        <?php api_link('LogEnableStandardOutput', 'CastleLog.html#LogEnableStandardOutput'); ?>
        to <code>false</code>.

      <li><p><i>For Windows GUI applications</i>,
        or if you set <code>LogEnableStandardOutput := false</code>,
        we log to the file in the user config directory.

        <ul>
          <li><p>On Windows the file name looks like
            <code>C:\Users\&lt;user-name&gt;\AppData\Local\&lt;application-name&gt;\&lt;application-name&gt;.log</code>.

          <li><p>On Unix the file name looks like
            <code>$HOME/.config/&lt;application-name&gt;\&lt;application-name&gt;.log</code>.
        </ul>

        <p>The exact logic to determine the <i>user config directory</i> follows FPC
        <code>GetAppConfigDir</code>. This uses the suitable OS-specific mechanism
        (e.g. asks the Windows API function, or follows
        <a href="https://freedesktop.org/wiki/Software/xdg-user-dirs/">xdg-user-dirs</a> conventions).

        <p>You can display the <code>LogOutput</code> value to show user on screen
        where is the log file.

      <li><p><i>On Android</i> the log goes to the standard device log.
        It that can be viewed using various Android tools,
        like <code>adb logcat</code>.

      <li><p><i>On iOS, Nintendo Switch</i> it goes to the standard log facility for these devices.
  </ul>

  <li><p>You can pass a parameter to <code>InitializeLog</code>
    to generate log to any stream.

    <p>You can also set <code>LogFileName</code> variable before calling the <code>InitializeLog</code>.
    This way you force using specific filename for logging,
    overriding the OS-specific auto-detection mechanism described above.

    <p>User can also call the application with <code>--log-file=c:/tmp/my_log_name.txt</code> command-line
    option to set the location (and filename) explicitly.
    In fact, this way user sets the <code>LogFileName</code> variable.
    It will work if you call in your main program file <code>Application.ParseStandardParameters;</code>.
</ul>

<?php
castle_footer();
?>
