<?php
require_once 'castle_engine_functions.php';
tutorial_header('Notifications');
?>

<p>You can also show on-screen notifications by
<?php api_link('TCastleNotifications', 'CastleNotifications.TCastleNotifications.html'); ?>
 component. This is a <tt>TUIControl</tt> descendant, so you just create it and
add to <tt>Window.Controls</tt> list, just like other <tt>TUIControl</tt> descendants
mentioned in previous chapters.

<p>You can actually just use the global <tt>Notifications</tt> instance created
for you in the <?php api_link('CastleGameNotifications', 'CastleGameNotifications.html'); ?>
 unit (just add it to
<tt>Window.Controls</tt> list). Some engine game features already make
notifications to it, and it is also automatically set up to cooperate
with <?php echo a_href_page('CastleScript writeln()', 'castle_script'); ?>
 function, so you can print to it from VRML/X3D scripts.

<?php echo pascal_highlight(
'uses ..., CastleGameNotifications;

...
Window.Controls.Add(Notifications);

...
Notifications.Show(\'Show some message\');'); ?>

<p>But you're not forced to use global <tt>Notifications</tt>
from unit <tt>CastleGameNotifications</tt>. You may as well create
your own <tt>TCastleNotifications</tt> instance,
using only <tt>CastleNotifications</tt> unit.
You can ignore our <tt>CastleGameNotifications</tt>, if you don't like the
default notifications.
<!-- (you can
still cooperate with CastleScript writeln() by handling
OnScriptMessage yourself, see CastleGameNotifications sources for
example).
-->

<?php
tutorial_footer();
?>
