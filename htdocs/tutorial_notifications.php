<?php
require_once 'castle_engine_functions.php';
tutorial_header('Notifications');
?>

<p>You can also show on-screen notifications by TCastleNotifications
component. This is a TUIControl descendant, so you just create it and
add to Window.Controls list, just like other TUIControl descendants
mentioned in previous chapters.

<p>You can actually just use the global Notifications instance created
for you in the CastleGameNotifications unit (just add it to
Window.Controls list). Some engine game features already make
notifications to it, and it is also automatically set up to cooperate
with CastleScript writeln() function, so you can print to it from
VRML/X3D scripts.

<?php echo pascal_highlight(
'uses ..., CastleGameNotifications;

...
Window.Controls.Add(Notifications);

...
Notifications.Show(\'Show some message\');'); ?>

<p>But you're not forced to use global <tt>Notifications</tt>
from unit <tt>CastleGameNotifications</tt>. You may as well create
your own TCastleNotifications instance, using only <tt>CastleNotifications</tt> unit.
You can ignore our CastleGameNotifications, if you don't like the
default notifications.
<!-- (you can
still cooperate with CastleScript writeln() by handling
OnScriptMessage yourself, see CastleGameNotifications sources for
example).
-->

<?php
tutorial_footer();
?>
