<?php
  require_once 'tutorial_common.php';
  tutorial_header('Notifications');
?>

You can also show on-screen notifications by TCastleNotifications component. This is a TUIControl descendant, so you just create it and add to Window.Controls list, just like other TUIControl descendants mentioned in previous chapters.

You can actually just use the global Notifications instance created for you in the CastleGameNotifications unit (just add it to Window.Controls list). Some engine game features already make notifications to it, and it is also automatically set up to cooperate with CastleScript writeln() function, so you can print to it from VRML/X3D scripts. But you're not forced to it, you may as well create your own TCastleNotifications and ignore our default notifications instance in CastleGameNotifications, if you don't like them (you can still cooperate with CastleScript writeln() by handling OnScriptMessage yourself, see CastleGameNotifications sources for example).

<?php
  tutorial_footer();
?>
