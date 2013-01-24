<?php
require_once 'castle_engine_functions.php';
tutorial_header('Display 2D controls: player HUD');
?>

<p>You will probably want to draw some custom 2D controls on your screen,
for example to display player life or inventory. The engine does manage them
automatically for you (see properties in
<?php api_link('TPlayer', 'CastlePlayer.TPlayer.html'); ?> ancestors like
<?php api_link('T3DAliveWithInventory.Inventory', 'CastleItems.T3DAliveWithInventory.html#Inventory'); ?> and
<?php api_link('T3DAlive.Life', 'Castle3D.T3DAlive.html#Life'); ?>).
But by default the engine doesn't display them in any way,
since various games have wildly different needs.

<p>You can do it by defining a new
<?php api_link('TUIControl', 'CastleUIControls.TUIControl.html'); ?>
 descendant, where you can draw anything you want in overridden
<?php api_link('TUIControl.Draw', 'CastleUIControls.TUIControl.html#Draw'); ?>
 method. A simple example:

<?php echo pascal_highlight(
'uses ..., CastleUIControls;

type
  TGame2DControls = class(TUIControl)
  public
    procedure Draw; override;
    function DrawStyle: TUIControlDrawStyle; override;
  end;

function TGame2DControls.DrawStyle: TUIControlDrawStyle;
begin
  Result := ds2D;
end;

procedure TGame2DControls.Draw;
var
  Player: TPlayer;
begin
  Player := Window.SceneManager.Player;
  { ... }
end;

{ When starting your game, create TGame2DControls instance
  and add it to Window.Controls }
var
  Controls2D: TGame2DControls;
...
  Controls2D := TGame2DControls.Create(Application);
  Window.Controls.Add(Controls2D);'); ?>

<p>Inside <tt>TGame2DControls.Draw</tt> you have the full knowledge about the world,
about the <tt>Player</tt> and <tt>Player</tt>'s inventory.
And you can draw them however you like. Basically, you can use
direct OpenGL commands (with some care taken, see
<?php api_link('TUIControl.Draw', 'CastleUIControls.TUIControl.html#Draw'); ?>
 docs about what you can change carelessly; the rest should be secured
using <tt>glPushAttrib</tt> / <tt>glPopAttrib</tt>).
But we give you a lot of helpers:

<ul>
  <li><p><?php api_link('SetWindowPos', 'CastleGLUtils.html#SetWindowPos'); ?>
    (from <?php api_link('CastleGLUtils', 'CastleGLUtils.html'); ?> unit)
    sets the raster position in screen coordinates (used for subsequent
    bitmap font and 2D image drawing).

    <p>Our projection has (0,0) in lower-left corner (as is standard
    for 2D OpenGL). You can look at the size, in pixels,
    of the current 
    <!--2D control in
    <?php api_link('TUIControl.Width', 'CastleUIControls.TUIControl.html#Width'); ?> x
    <?php api_link('TUIControl.Height', 'CastleUIControls.TUIControl.html#Height'); ?>,
    and the size of the current--> OpenGL screen in
    <?php api_link('TUIControl.ContainerWidth', 'CastleUIControls.TUIControl.html#ContainerWidth'); ?> x
    <?php api_link('TUIControl.ContainerHeight', 'CastleUIControls.TUIControl.html#ContainerHeight'); ?>.
    The container size is also available as container properties, like
    <?php api_link('TCastleWindow.Width', 'CastleWindow.TCastleWindowBase.html#Width'); ?> x
    <?php api_link('TCastleWindow.Height', 'CastleWindow.TCastleWindowBase.html#Height'); ?>.

  <li><p>You have ready global bitmap fonts
    <?php api_link('UIFont', 'CastleControls.html#UIFont'); ?> and
    <?php api_link('UIFontSmall', 'CastleControls.html#UIFontSmall'); ?>
    (in <?php api_link('CastleControls', 'CastleControls.html'); ?> unit).
    These are instances of <?php api_link('TGLBitmapFont', 'CastleGLBitmapFonts.TGLBitmapFont.html'); ?>
    that
    you can use to draw text. (Of course you can also create your own instances
    of <?php api_link('TGLBitmapFont', 'CastleGLBitmapFonts.TGLBitmapFont.html'); ?>
    to have more fonts.)
    For example, you can show player's health like this:

<?php echo pascal_highlight(
'SetWindowPos(10, 10);
UIFont.Print(Format(\'Player life: %f / %f\', [Player.Life, Player.MaxLife]));'); ?>

  <li><p>Every inventory item has already loaded image (defined in <tt>resource.xml</tt>),
    as <?php api_link('TCastleImage', 'CastleImages.TCastleImage.html'); ?>
    (image stored in normal memory, see
    <?php api_link('CastleImages', 'CastleImages.html'); ?> unit) and
    <?php api_link('TGLImage', 'CastleGLImages.TGLImage.html'); ?>
    (image stored in GPU memory, easy to render, see
    <?php api_link('CastleGLImages', 'CastleGLImages.html'); ?> unit).
    For example, you can iterate over inventory
    list and show them like this:

<?php echo pascal_highlight(
'for I := 0 to Player.Inventory.Count - 1 do
begin
  SetWindowPos(I * 100, 0);
  Player.Inventory[I].Resource.GLImage.Draw;
end;'); ?>

  <li><p>For simple screen fade effects, you have procedures inside
    <?php api_link('CastleGLUtils', 'CastleGLUtils.html'); ?> unit
    like <?php api_link('GLFadeRectangle', 'CastleGLUtils.html#GLFadeRectangle'); ?>.
    This allows you to draw
    a rectangle representing fade out (when player is in pain).
    And <?php api_link('TPlayer', 'CastlePlayer.TPlayer.html'); ?>
    instance already has properties
    <?php api_link('Player.FadeOutColor', 'CastlePlayer.TPlayer.html#FadeOutColor'); ?>,
    <?php api_link('Player.FadeOutIntensity', 'CastlePlayer.TPlayer.html#FadeOutIntensity'); ?>
    representing when player is in pain (and the pain color).
    <?php api_link('Player.Dead', 'Castle3D.T3DAlive.html#Dead'); ?>
    says when player is dead (this is simply when <tt>Life <= 0</tt>).

    <p>For example you can visualize pain and dead states like this:

<?php echo pascal_highlight(
'if Player.Dead then
  GLFadeRectangle(0, 0, ContainerWidth, ContainerHeight, Red3Single, 1.0) else
  GLFadeRectangle(0, 0, ContainerWidth, ContainerHeight,
    Player.FadeOutColor, Player.FadeOutIntensity);'); ?>

    <p>Note that <tt>Player.FadeOutIntensity</tt> will be 0 when there is no pain, which cooperates
    nicely with <tt>GLFadeRectangle</tt> definition that will do nothing when 4th parameter is 0.
    That is why we carelessly always call <tt>GLFadeRectangle</tt> &mdash; when player is not dead,
    and is not in pain (<tt>Player.FadeOutIntensity</tt> = 0) then nothing will actually happen.

  <li><p><?php api_link('CastleGLUtils', 'CastleGLUtils.html'); ?>,
    and many other units, provide many other helpers.
</ul>

<p>See <tt>examples/fps_game</tt> for a working and fully-documented
demo of such <tt>TGame2DControls</tt> implementation.
See <?php echo a_href_page('"The Castle 1"', 'castle'); ?> sources (unit <tt>GamePlay</tt>)
for an example implementation that shows
more impressive player's life indicator and inventory and other things on the screen.

<p>You can use any 2D engine controls like this, just add them to <tt>Window.Controls</tt>.
See <?php api_link('CastleControls', 'CastleControls.html'); ?>
 unit for some standard buttons and panels and images.
But for a specific game you will probably want a specialized UI,
done like the example above.

<?php
tutorial_footer();
?>
