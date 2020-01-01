<?php
require_once 'castle_engine_functions.php';
manual_header('Key / mouse shortcuts');
?>

<p>We have global key/mouse shortcuts in some global <code>Input_Xxx</code> variables:

<ul>
  <li>In <code>CastleViewport</code> unit we have <code>Input_Interact</code>.
  <li>In <code>CastleLevels</code> unit we have more <code>Input_Xxx</code> shortcuts
    useful for <a href="manual_high_level_3d_classes.php">typical 3D games</a>.
  <li>In <code>CastlePlayer</code> unit we have <code>PlayerInput_Xxx</code>
    global variables, these override some navigation shortcuts when <code>TPlayer</code> is used.
</ul>

Global shortcuts are gathered inside <?php api_link('InputsAll',
'CastleInputs.html#InputsAll'); ?>
 global map. Thanks to this global map, you can let user to configure
game controls, you can detect key conflicts and handle them however
you like, etc.

<p>There are also local inputs, local to a specific navigation instance,
inside <code>Input_Xxx</code> properties of
<?php api_link('TCastleWalkNavigation', 'CastleCameras.TCastleWalkNavigation.html'); ?> and
<?php api_link('TCastleExamineNavigation', 'CastleCameras.TCastleExamineNavigation.html'); ?>.
<b>If you use <?php api_link('TPlayer', 'CastlePlayer.TPlayer.html'); ?></b> then
<?php api_link('TPlayer', 'CastlePlayer.TPlayer.html'); ?> uses it's own
<?php api_link('TCastleWalkNavigation', 'CastleCameras.TCastleWalkNavigation.html'); ?>,
 and overrides (most of) navigation inputs with global
<code>PlayerInput_Xxx</code>, so (most of) the <code>TCastleWalkNavigation.Input_Xxx</code> controls
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
<code>TCastleUserInterface.Press</code> or <code>TCastleControlBase.EventPress</code> or
<code>TCastleWindow.EventPress</code> methods, or assigning <code>TCastleControlBase.OnPress</code>
or <code>TCastleWindowBase.OnPress</code> callbacks. You will find then useful methods
<code>TInputShortcut.IsEvent(TInputPressRelease)</code> (to detect press/release of
input) and <code>TInputShortcut.IsPressed(IUIContainer)</code> (to detect holding
(keeping pressed) of input).

<p>An example application that creates a set of custom
<?php api_link('TInputShortcut', 'CastleInputs.TInputShortcut.html'); ?> instances,
and saves/loads them to config file, is in
<a href="https://github.com/castle-engine/castle-engine/blob/master/examples/3d_rendering_processing/custom_input_shortcuts_saved_to_config.lpr">examples/3d_rendering_processing/custom_input_shortcuts_saved_to_config.lpr</a>.

<?php
  manual_footer();
?>
