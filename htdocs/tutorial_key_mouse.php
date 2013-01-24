<?php
  require_once 'tutorial_common.php';
  tutorial_header('Key / mouse shortcuts');
?>

<p>We have global controls in global variables: InputPlayer_Xxx and
Input_Interact and more. They are gather inside CastleInputs.InputsAll
global map. Thanks to this global map, you can let user to configure
game controls, you can detect key conflicts and handle them however
you like, etc.

<p>There are also local inputs, like TWalkCamera/TExamineCamera.Input_Xxx
properties. <b>Onlyi f you use TPlayer</b>: then TPlayer sets it's own
TWalkCamera, and overrides (most of) camera inputs with global
InputPlayer_Xxx, so (most of) the TWalkCamera.Input_Xxx controls
should not be accessed directly, instead change only the global
InputPlayer_Xxx matter. TPlayer controls set up typical AWSD keys
scheme, and they are also state-sensitive: they change appropriately
when player is dead or blocked (see TPlayer.Dead and TPlayer.Blocked),

<p>Every key/mouse shortcut is a TInputShortcut. They are configurable,
you can change the shortcuts whenever you want (see
TInputShortcut.Assign and other methods). You can also detect
conflicts in the keymap and handle them however you like (see
GameControlsMenu in castle1 for example).

<p>The state of keys on the global keymap (CastleInputs.InputsAll) is
automatically saved/loaded to the config file, if you use
Config.Save/Load mentioned in chapter "User preferences".

<p>You can also create new TInputShortcut descendants, global (added to
InputsAll, saved to config file and such) or local. See CastleInputs
unit API docs. You can easily handle them by overriding
TUIControl.Press or TCastleControl.EventPress or
TCastleWindow.EventPress methods, or assigning TCastleControl.OnPress
or TCastleWindow.OnPress callbacks. You will find then useful methods
TInputShortcut.IsEvent(TInputPressRelease) (to detect press/release of
input) and TInputShortcut.IsPressed(IUIContainer) (to detect holding
(keeping pressed) of input).

<?php
  tutorial_footer();
?>
