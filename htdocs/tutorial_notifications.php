<?php
require_once 'castle_engine_functions.php';
tutorial_header('Notifications');
?>

<p>You can also show on-screen notifications by
<?php api_link('TCastleNotifications', 'CastleNotifications.TCastleNotifications.html'); ?>
 component. This is a <code>TUIControl</code> descendant, so you just create it and
add to <code>Window.Controls</code> list, just like other <code>TUIControl</code> descendants
mentioned in previous chapters.

<p>You can actually just use the global <code>Notifications</code> instance created
for you in the <?php api_link('CastleGameNotifications', 'CastleGameNotifications.html'); ?>
 unit (just add it to
<code>Window.Controls</code> list). Some engine game features already make
notifications to it, and it is also automatically set up to cooperate
with <?php echo a_href_page('CastleScript writeln()', 'castle_script'); ?>
 function, so you can print to it from VRML/X3D scripts.

<?php echo pascal_highlight(
'uses ..., CastleGameNotifications;

...
Window.Controls.Add(Notifications);

...
Notifications.Show(\'Show some message\');'); ?>

<p>But you're not forced to use global <code>Notifications</code>
from unit <code>CastleGameNotifications</code>. You may as well create
your own <code>TCastleNotifications</code> instance,
using only <code>CastleNotifications</code> unit.
You can ignore our <code>CastleGameNotifications</code>, if you don't like the
default notifications.
<!-- (you can
still cooperate with CastleScript writeln() by handling
OnScriptMessage yourself, see CastleGameNotifications sources for
example).
-->

<?php
tutorial_footer();
?>
