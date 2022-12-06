<?php
require_once 'castle_engine_functions.php';
castle_header('On-screen menu');

echo castle_thumbs(array(
  array('filename' => 'on_screen_menu_castle_screen_0.png', 'titlealt' => 'On-screen menu in "The Castle" - main menu'),
  array('filename' => 'on_screen_menu_castle_screen_1.png', 'titlealt' => 'On-screen menu in "The Castle" - configure controls'),
  array('filename' => 'on_screen_menu_castle_screen_2.png', 'titlealt' => 'On-screen menu in "The Castle" - pause menu'),
  array('filename' => 'lights_editor_shadow_maps.png', 'titlealt' => 'Lights editor in view3dscene - also an on-screen menu'),
  array('filename' => 'terrain1.png', 'titlealt' => 'Terrain parameters (from engine "terrain" demo) are also an on-screen menu'),
));
?>

<p>The <?php echo cgeRef('TCastleOnScreenMenu'); ?>
 is a <i>user interface control</i>
 (<?php echo cgeRef('TUIControl'); ?> descendant)
 displaying an on-screen menu.
All the menu items are displayed vertically on the screen.
You can click on menu items, or choose them using the keyboard.
Each menu item is a full-featured UI control.
In the common case, a menu item is an instance of
<?php echo cgeRef('TCastleMenuButton'); ?>,
 which descends from <?php echo cgeRef('TCastleButton') ?>
 so you have available the event <?php echo cgeRef('OnClick') ?>.
 In general, menu item is <i>any</i> UI control.

<p>Menu items may also have attached an <i>"accessory"</i> which
is often used as an extra label (like a <i>"Yes"</i> / <i>"No"</i> state of some configuration option),
a slider (e.g. to control sound volume or texture quality).
In general, an <i>"accessory"</i> is just a child UI control of
the menu-item, and can be any UI control
(<?php echo cgeRef('TUIControl'); ?>).
So you can really insert any convoluted things inside the on-screen menu:)

<p>You create and insert the on-screen menu instance
 <a href="user_interface">just like all other UI controls</a>.
 <!--When using Lazarus forms, you can also drop the <code>TCastleOnScreenMenu</code> on the form.-->

<p>Once created, you should add menu items using the
<?php echo cgeRef('TCastleOnScreenMenu.Add'); ?>
 method. It has a couple of overloaded versions. In the simplest case,
 you can use the <code>Add(string)</code> or
 <code>Add(string, TNotifyEvent)</code>
 methods, that add a simplest menu item, and optionally register your
 callback to handle it's <i>click</i> event.

<p>A simplest example program:</p>

<?php echo pascal_highlight_file('code-samples/on_screen_menu.lpr'); ?>

<p>There are examples of using this class in
<ul>
  <li><code>examples/lazarus/model_3d_with_2d_controls/</code></li>
  <li><code>examples/user_interface/</code>
  <li><code>examples/terrain/</code>
  <li>Inside <a href="https://github.com/castle-engine/castle-game/">The Castle</a> game.</li>
</ul>

<h2>On-screen menu over a 3D world</h2>

<p>You can use various UI controls on top of each other.
So you can have
<?php echo cgeRef('TCastleOnScreenMenu'); ?>
 displayed on top of a TCastleViewport. You can control the existence of any UI control
either by removing/adding it from the <code>Controls</code> list,
or by changing it's <?php echo cgeRef('Exists'); ?>
 property.</p>

<p>If the game is already started, in single player games,
you usually want to pause the game when the on-screen
menu is displayed. You can do this easily by
<?php echo cgeRef('Items.Paused'); ?> property.

Use the <a href="states">TUIState</a>
to create a state like <code>TStateOptions</code> that will contain
the "options" user-interface (e.g. using <code>TCastleOnScreenMenu</code>),
and underneath pause the game by setting <code>Viewport.Items.Paused := true</code>.

<h2>Background under on-screen menu</h2>

<p>In addition to previous ideas, you may want to change the menu
<code>TCastleOnScreenMenu.FullSize</code> to <code>true</code>. Otherwise, menu receives input
only when mouse hovers over it. When <code>FullSize = true</code>, the menu obscures
completely controls under it as far as key processing is concerned
(although controls behind are still visible as a background).</p>

<ul>
  <li><p>So if you want your menu to be displayed and used orthogonally to
    the "live" 3D world underneath, leave <code>FullSize = false</code>.

  <li><p>For initial game menu with items like <i>"New Game"</i>, you probably want
    to disable the camera input for this scene. This allows you to
    display interactive 3D scene in the background, but block user from
    interacting with it (after all, the user should only interact with
    your menu on the initial screen). To do this simply set <code>FullSize =
    true</code>.
</ul>

<!--
<p>An alternative method to achieve (part) of the 2nd choice is to set
background level camera's <code>Input := []</code>. This also blocks user from
moving in the scene (although it doesn't make input passed to menu
regardless of mouse position).
-->

<p>If you want to place a static 2D image under menu, you can use
<code>TCastleImageControl</code> underneath, instead of a 3D scene.

<?php
castle_footer();
?>
