<?php
require_once 'castle_engine_functions.php';
tutorial_header('Key / mouse shortcuts');
?>

<p>We have global key/mouse shortcuts in some global <tt>Input_Xxx</tt> variables:

<ul>
  <li>In <tt>CastleSceneManager</tt> unit we have <tt>Input_Interact</tt>
    and some game operations.
  <li>In <tt>CastlePlayer</tt> unit we have <tt>PlayerInput_Xxx</tt>
    global variables, these override some camera shortcuts when Player is used.
</ul>

Global shortcuts are gathered inside <?php api_link('InputsAll',
'CastleInputs.html#InputsAll'); ?>
 global map. Thanks to this global map, you can let user to configure
game controls, you can detect key conflicts and handle them however
you like, etc.

<p>There are also local inputs, local to a specific camera instance,
inside <tt>Input_Xxx</tt> properties of
<?php api_link('TWalkCamera', 'CastleCameras.TWalkCamera.html'); ?> and
<?php api_link('TExamineCamera', 'CastleCameras.TExamineCamera.html'); ?>.
<b>If you use <?php api_link('TPlayer', 'CastlePlayer.TPlayer.html'); ?></b> then
<?php api_link('TPlayer', 'CastlePlayer.TPlayer.html'); ?> uses it's own
<?php api_link('TWalkCamera', 'CastleCameras.TWalkCamera.html'); ?>,
 and overrides (most of) camera inputs with global
<tt>PlayerInput_Xxx</tt>, so (most of) the <tt>TWalkCamera.Input_Xxx</tt> controls
should not be accessed directly, instead change only the global
<tt>PlayerInput_Xxx</tt>. <?php api_link('TPlayer', 'CastlePlayer.TPlayer.html'); ?>
 sets typical AWSD key
controls scheme, and they are also state-sensitive: they change appropriately
when player is dead or blocked (see <tt>TPlayer.Dead</tt> and
<tt>TPlayer.Blocked</tt>).

<p>Every key/mouse shortcut is a <?php api_link('TInputShortcut', 'CastleInputs.TInputShortcut.html'); ?>.
They are configurable,
you can change the shortcuts whenever you want (see
<tt>TInputShortcut.Assign</tt> and other methods). You can also detect
conflicts in the keymap and handle them however you like (see
<tt>GameControlsMenu</tt> unit in <?php echo a_href_page('"The Castle 1"', 'castle'); ?> sources
for example).

<p>The state of keys on the global keymap (<tt>CastleInputs.InputsAll</tt>) is
automatically saved/loaded to the config file, if you use
<tt>Config.Save</tt> / <tt>Config.Load</tt> mentioned in chapter <?php echo a_href_page('User preferences',
'tutorial_user_prefs'); ?>.

<p>You can also create new <tt>TInputShortcut</tt> descendants, global (added to
InputsAll, saved to config file and such) or local. See <tt>CastleInputs</tt>
unit API docs. You can easily handle them by overriding
<tt>TUIControl.Press</tt> or <tt>TCastleControl.EventPress</tt> or
<tt>TCastleWindow.EventPress</tt> methods, or assigning <tt>TCastleControl.OnPress</tt>
or <tt>TCastleWindow.OnPress</tt> callbacks. You will find then useful methods
<tt>TInputShortcut.IsEvent(TInputPressRelease)</tt> (to detect press/release of
input) and <tt>TInputShortcut.IsPressed(IUIContainer)</tt> (to detect holding
(keeping pressed) of input).

<?php
  tutorial_footer();
?>
