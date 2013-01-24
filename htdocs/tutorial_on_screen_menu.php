<?php
require_once 'castle_engine_functions.php';
tutorial_header('On-screen menu');
?>

<p>Our <?php api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html'); ?> and
 <?php api_link('TCastleControl', 'CastleControl.TCastleControl.html'); ?>
 have a list of 2D controls visible
of the screen. By default, the only thing present there is a scene
manager (since scene manager acts as a 2D viewport through which you
see the 3D world; that's right &mdash; the 3D stuff is "within" the 2D
stuff). This way the scene manager (it's viewport) is visible on the
window, which in turn means that all the 3D stuff we will add next is
visible too.

<p>You can add your own 2D controls using the Window.Controls.Add
call. There are many predefined GUI controls available in our engine,
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
key). Inside OnClick event, the
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
OnScreenMenu1 := TCastleOnScreenMenu.Create(Application);
OnScreenMenu1.Items.Add(\'New game\');
OnScreenMenu1.Items.Add(\'Quit\');
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
 property).</p>

<p>If the game is already started, in single player games,
you usually want to pause the game when the on-screen
menu is displayed. You can do this easily by
<?php api_link('SceneManager.Paused', 'CastleSceneManager.TCastleSceneManager.html#Paused'); ?> property.
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
processing events and calling Idle methods of all 3D objects periodically,
pausing it effectively pauses your whole 3D world, while still allowing
it to be displayed as a background under the on-screen menu.
Alternatively you could also hide the 3D world entirely,
by changing SceneManager.Exists property to false &mdash;
the SceneManager with Exists=false is not only paused, it's also not visible.</p>

<h2>Background under on-screen menu</h2>

<p>In addition to previous point, you may want to change the menu
TCastleOnScreenMenu.FullSize to true. Otherwise, menu receives input
only when mouse hovers over it. When FullSize, the menu obscures
completely controls under it as far as key processing is concerned
(although controls behind are still visible as a background).</p>

<ul>
  <li><p>So if you want your menu to be displayed and used orthogonally to
    the "live" 3D world underneath, leave FullSize = false.

  <li><p>For initial game menu with items like "New Game", you probably want
    to disable the camera input for this scene. This allows you to
    display interactive 3D scene in the background, but block user from
    interacting with it (after all, the user should only interact with
    your menu on the initial screen). To do this simply set FullSize =
    true.
</ul>

<p>An alternative method to achieve (part) of 2nd choice is to set
background level camera's Input := []. This also blocks user from
moving in the scene (although it doesn't make input passed to menu
regardless of mouse position).

<p>If you want to place a static 2D image under menu, you can use
TCastleImageControl underneath, instead of 3D scene.

<?php
tutorial_footer();
?>
