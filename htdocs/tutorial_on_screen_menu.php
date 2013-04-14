<?php
require_once 'castle_engine_functions.php';
tutorial_header('On-screen menu');
?>

<p>Our <?php api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html'); ?> and
 <?php api_link('TCastleControl', 'CastleControl.TCastleControl.html'); ?>
 have a list of 2D controls visible
of the screen. By default, the only thing present there is a scene
manager (since scene manager acts as a 2D viewport through which you
see the 3D world).<!--; that's right &mdash; the 3D stuff is "within" the 2D
stuff--> This way the scene manager (it's viewport) is visible on the
window, which in turn means that the 3D world is visible too.

<p>You can add your own 2D controls using the <tt>Window.Controls.Add</tt>
method. There are many predefined GUI controls available in our engine,
look for <?php api_link('TUIControl', 'CastleUIControls.TUIControl.html'); ?>
 descendants, for example in
<?php api_link('CastleControls', 'CastleControls.html'); ?>
 unit. You can also derive your own controls with ease.

<p>For a simple on-screen menu, where all the menu items are displayed
vertically on the screen, use the
 <?php api_link('TCastleOnScreenMenu', 'CastleOnScreenMenu.TCastleOnScreenMenu.html'); ?>
 control. For
Lazarus: drop <tt>TCastleOnScreenMenu</tt> on the form. For TCastleWindow: just
create <tt>TCastleOnScreenMenu</tt> instance. Fill it's
<?php api_link('TCastleOnScreenMenu.Items', 'CastleOnScreenMenu.TCastleOnScreenMenu.html#Items'); ?>
 property (each
line is a menu entry), and assign a handler for the
<?php api_link('TCastleOnScreenMenu.OnClick', 'CastleOnScreenMenu.TCastleOnScreenMenu.html#OnClick'); ?>
 event (to react when user chose menu item, by clicking or pressing enter
key). Inside <tt>OnClick</tt> event, the
<?php api_link('TCastleOnScreenMenu.CurrentItem', 'CastleOnScreenMenu.TCastleOnScreenMenu.html#CurrentItem'); ?>
 property tells you which item was clicked. You
also have to add this to controls list.

<?php echo pascal_highlight(
'uses ..., CastleOnScreenMenu;

var
  Window: TCastleWindow;
  OnScreenMenu1: TCastleOnScreenMenu;

procedure TEventHandler.OnScreenMenu1Click(Sender: TObject);
begin
  case OnScreenMenu1.CurrentItem of
    0: // ... load new game
    1: // ... quit
  end;
end;

...
{ Note: if you use Lazarus, you can create TCastleOnScreenMenu by dropping
  it on a form and you can initialize many properties by Object Inspector. }
OnScreenMenu1 := TCastleOnScreenMenu.Create(Application);
OnScreenMenu1.Items.Add(\'New game\');
OnScreenMenu1.Items.Add(\'Quit\');
OnScreenMenu1.OnClick := @EventHandler.OnScreenMenu1Click;
OnScreenMenu1.Position := Vector2Integer(100, 100);
// Maybe also adjust OnScreenMenu1.PositionRelativeMenu*

Window.Controls.Insert(0, OnScreenMenu1);'); ?>

<p>There is an example of this in <tt>examples/lazarus/model_3d_with_2d_controls/</tt> example in
engine sources.</p>

<h2>On-screen menu over a 3D world</h2>

<p>You can use various UI controls on top of each other.
So you can have
<?php api_link('TCastleOnScreenMenu', 'CastleOnScreenMenu.TCastleOnScreenMenu.html'); ?>
 displayed on top of a 3D world (by default, scene manager already acts as
a viewport). You can control the existence of any UI control
either by removing/adding it from the <tt>Controls</tt> list,
or by changing it's <?php api_link('Exists', 'CastleUIControls.TUIControl.html#Exists'); ?>
 property.</p>

<p>If the game is already started, in single player games,
you usually want to pause the game when the on-screen
menu is displayed. You can do this easily by
<?php api_link('SceneManager.Paused', 'CastleSceneManager.TCastleAbstractViewport.html#Paused'); ?> property.
Like this:</p>

<?php echo pascal_highlight(
'...
{ global / static variables }
var
  GameMenu: TCastleOnScreeMenu;
  GameMenuClosed: boolean;
...
  { somewhere at the beginning prepare the menu }
  GameMenu := TCastleOnScreeMenu.Create(...);
  { see example above for how to initialize and implement TCastleOnScreeMenu.
    Make sure that one of the menu items, like "Back",
    sets GameMenuClosed := true when clicked. }

...
{ when you want to actually show it }
SceneManager.Paused := true;
GameMenuClosed := false;
Window.Controls.Add(GameMenu);
repeat
  Application.ProcessMessage(true, true);
until GameMenuClosed;
Window.Controls.Remove(GameMenu);
SceneManager.Paused := false;'); ?>

<p>As the scene manager handles a lot of stuff automatically,
processing events and calling Update methods of all 3D objects periodically,
pausing it effectively pauses your whole 3D world, while still allowing
it to be displayed as a background under the on-screen menu.
Alternatively you could also hide the 3D world entirely,
by changing <tt>SceneManager.Exists</tt> property to <tt>false</tt> &mdash;
the <tt>SceneManager</tt> with <tt>Exists=false</tt> is not only paused,
it's also invisible.</p>

<h2>Background under on-screen menu</h2>

<p>In addition to previous ideas, you may want to change the menu
<tt>TCastleOnScreenMenu.FullSize</tt> to <tt>true</tt>. Otherwise, menu receives input
only when mouse hovers over it. When <tt>FullSize = true</tt>, the menu obscures
completely controls under it as far as key processing is concerned
(although controls behind are still visible as a background).</p>

<ul>
  <li><p>So if you want your menu to be displayed and used orthogonally to
    the "live" 3D world underneath, leave <tt>FullSize = false</tt>.

  <li><p>For initial game menu with items like <i>"New Game"</i>, you probably want
    to disable the camera input for this scene. This allows you to
    display interactive 3D scene in the background, but block user from
    interacting with it (after all, the user should only interact with
    your menu on the initial screen). To do this simply set <tt>FullSize =
    true</tt>.
</ul>

<!--
<p>An alternative method to achieve (part) of the 2nd choice is to set
background level camera's <tt>Input := []</tt>. This also blocks user from
moving in the scene (although it doesn't make input passed to menu
regardless of mouse position).
-->

<p>If you want to place a static 2D image under menu, you can use
<tt>TCastleImageControl</tt> underneath, instead of a 3D scene.

<?php
tutorial_footer();
?>
