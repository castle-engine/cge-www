<?php
require_once 'castle_engine_functions.php';
tutorial_header('On-screen menu');
?>

<p>Our TCastleWindow or TCastleControl have a list of 2D controls visible
of the screen. By default, the only thing present there is a scene
manager (since scene manager acts as a 2D viewport through which you
see the 3D world; that's right &mdash; the 3D stuff is "within" the 2D
stuff). This way the scene manager (it's viewport) is visible on the
window, which in turn means that all the 3D stuff we will add next is
visible too.

<p>You can add your own 2D controls using the Window.Controls.Add
call. There are many predefined GUI controls available in our engine,
look for TUIControl descendants, for example in CastleControls
unit. You can also derive your own controls with ease.

<p>For a simple on-screen menu, where all the menu items are displayed
vertically on the screen, use the TCastleOnScreenMenu control. For
Lazarus: drop TCastleOnScreenMenu on the form. For TCastleWindow: just
create TCastleOnScreenMenu instance. Fill it's Items property (each
line is a menu entry), and assign a handler for the OnClick event (to
react when user chose menu item, by clicking or pressing enter
key). Inside OnClick event, the CurrentItem property of your
TCastleOnScreenMenu instance tells you which item was clicked. You
still have to add a code to TForm.OnCreate to add this to controls
list, like

<?php echo pascal_highlight(
'Browser.Controls.Insert(0, OnScreenMenu1);'); ?>

<p>You may also want to change the position, like

<?php echo pascal_highlight(
'OnScreenMenu1.Position := Vector2Integer(100, 100);
 { you may also want to set PositionRelativeMenu* (through object inspector or code) }'); ?>

<p>There is an example of this in model_3d_with_2d_controls example in
engine sources, look for OnScreenMenu1 references in code.</p>

------------------------------------------------------------------------------
TODO: merge above and below, I wrote about OnScreenMenu two times...

<p>A similar approach works for displaying in-game menu too.
We provide a flexible on-screen menu as TCastleOnScreeMenu component.
Examples how to use it are in examples/lazarus/model_3d_with_2d_controls/
and a lot of examples are in castle1 code.
It descends from TUIControl, so it can be added and removed from the scene
just like every other control (by adding or removing from Window.Controls,
or by changing it's TUIControl.Exists property).</p>

<p>If the game is already started, in single player games,
you usually want to pause the game when the on-screen
menu is displayed. You can do this easily by SceneManager.Paused property.
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
  { see examples for how to initialize and implement TCastleOnScreeMenu.
    Make sure that one of the menu items, like "Back",
    sets GameMenuClosed := true. }

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
processing events and calling Idle methods of all 3D objects periodically.
Pausing it effectively pauses your whole 3D world, while stil allowing
it to be displayed as a background under the on-screen menu.
Alternatively you could also hide the 3D world entirely,
by changing SceneManager.Exists property to false &mdash;
the SceneManager with Exists=false is not only paused, it's also not visible.
You could use TCastleImageControl to show a special image underneath
an on-screen menu.</p>

<h2>Background under on-screen menu</h2>

<p>In addition to previous point, you may want to change the menu
TCastleOnScreenMenu.FullSize to true. Otherwise, menu receives input
only when mouse hovers over it. When FullSize, the menu obscures
completely controls under it for key processing (although they are
still visible as a background).</p>

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
