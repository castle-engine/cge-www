<?php
require_once 'castle_engine_functions.php';
manual_header('Key / mouse shortcuts');
?>

<p>We have global key/mouse shortcuts in some global <code>Input_Xxx</code> variables:

<ul>
  <li>In <code>CastleSceneManager</code> unit we have <code>Input_Interact</code>
    and some game operations.
  <li>In <code>CastlePlayer</code> unit we have <code>PlayerInput_Xxx</code>
    global variables, these override some camera shortcuts when Player is used.
</ul>

Global shortcuts are gathered inside <?php api_link('InputsAll',
'CastleInputs.html#InputsAll'); ?>
 global map. Thanks to this global map, you can let user to configure
game controls, you can detect key conflicts and handle them however
you like, etc.

<p>There are also local inputs, local to a specific camera instance,
inside <code>Input_Xxx</code> properties of
<?php api_link('TWalkCamera', 'CastleCameras.TWalkCamera.html'); ?> and
<?php api_link('TExamineCamera', 'CastleCameras.TExamineCamera.html'); ?>.
<b>If you use <?php api_link('TPlayer', 'CastlePlayer.TPlayer.html'); ?></b> then
<?php api_link('TPlayer', 'CastlePlayer.TPlayer.html'); ?> uses it's own
<?php api_link('TWalkCamera', 'CastleCameras.TWalkCamera.html'); ?>,
 and overrides (most of) camera inputs with global
<code>PlayerInput_Xxx</code>, so (most of) the <code>TWalkCamera.Input_Xxx</code> controls
should not be accessed directly, instead change only the global
<code>PlayerInput_Xxx</code>. <?php api_link('TPlayer', 'CastlePlayer.TPlayer.html'); ?>
 sets typical AWSD key
controls scheme, and they are also state-sensitive: they change appropriately
when player is dead or blocked (see <code>TPlayer.Dead</code> and
<code>TPlayer.Blocked</code>).

<p>Every key/mouse shortcut is a <?php api_link('TInputShortcut', 'CastleInputs.TInputShortcut.html'); ?>.
They are configurable,
you can change the shortcuts whenever you want (see
<code>TInputShortcut.Assign</code> and other methods). You can also detect
conflicts in the keymap and handle them however you like (see
<code>GameControlsMenu</code> unit in <?php echo a_href_page('"The Castle 1"', 'castle'); ?> sources
for example).

<p>The state of keys on the global keymap (<code>CastleInputs.InputsAll</code>) can
be saved/loaded to the config file, if you call
<?php api_link('InputsAll.SaveToConfig(UserConfig)', 'CastleInputs.TInputShortcutList.html#SaveToConfig'); ?> and
<?php api_link('InputsAll.LoadFromConfig(UserConfig)', 'CastleInputs.TInputShortcutList.html#LoadFromConfig'); ?>.
See the chapter <?php echo a_href_page('User preferences',
'manual_user_prefs'); ?> for more information about this.

<p>You can also create new <code>TInputShortcut</code> descendants.
Then can be global (added to
InputsAll, saved to config file and such) or local. See <code>CastleInputs</code>
unit API docs. You can easily handle them by overriding
<code>TUIControl.Press</code> or <code>TCastleControl.EventPress</code> or
<code>TCastleWindow.EventPress</code> methods, or assigning <code>TCastleControl.OnPress</code>
or <code>TCastleWindow.OnPress</code> callbacks. You will find then useful methods
<code>TInputShortcut.IsEvent(TInputPressRelease)</code> (to detect press/release of
input) and <code>TInputShortcut.IsPressed(IUIContainer)</code> (to detect holding
(keeping pressed) of input).

<?php
  manual_footer();
?>
