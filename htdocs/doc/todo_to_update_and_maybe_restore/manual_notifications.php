<?php
require_once 'castle_engine_functions.php';
castle_header('Notifications');
?>

<p>You can show on-screen notifications by
<?php echo cgeRef('TCastleNotifications'); ?>
 <i>user interface control</i>
 (<?php echo cgeRef('TUIControl'); ?> descendant).

<p>You can just create an instance of this class,
and add it to the <code>Window.Controls</code> list,
as you do with any other UI control.
But you can also use the global <code>Notifications</code> instance already created
for you in the <?php echo cgeRef('CastleGameNotifications'); ?>
 unit (just add it to
<code>Window.Controls</code> list). Some engine components already make
notifications to it, and it is also automatically set up to cooperate
with the <?php echo a_href_page('CastleScript writeln()', 'castle_script'); ?>
 function, so you can print to it from VRML/X3D scripts.

<?php echo pascal_highlight_file('code-samples/notifications.lpr'); ?>

<p>Remember that you're not forced to use the global <code>Notifications</code>
from unit <code>CastleGameNotifications</code>, especially if you don't like the
default notifications. You may always create
your own <code>TCastleNotifications</code> instance,
using only the <code>CastleNotifications</code> unit.
<!-- (you can
still cooperate with CastleScript writeln() by handling
OnScriptMessage yourself, see CastleGameNotifications sources for
example).
-->

<?php
castle_footer();
?>
