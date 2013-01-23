<?php
  require_once 'tutorial_common.php';
  tutorial_header('Display 2D controls: player HUD');
?>

You will often want to draw a 2D controls on your screen, for example to display player life or inventory (the engine does manage the player's inventory automatically for you, in TPlayer.Inventory; however, it's not displayed automatically, since various games have wildly different needs). You can do it by defining a new TUIControl descendant, where you can draw anything you want in overridden Draw method. You can use the ready UIFont instance to draw a 2D font on the screen. A simple example:

<?php echo pascal_highlight(
'type
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

Inside TGame2DControls.Draw you have the full knowledge about the world,
about the Player and Player's inventory.
And you can draw them however you like. Basically, you can use
direct OpenGL commands (with some care taken, see TUIControl.Draw docs
about what you can change carelessly; the rest will have to be secured
using glPushAttrib / glPopAttrib). But we also give you a lot of helpers:

- SetWindowPos (CastleGLUtils unit) sets the raster position in screen coordinates.

- You have ready UIFont and UIFontSmall instances of TGLBitmapFont that
  you can use to draw text. Of course you can also create your own instances
  of TGLBitmapFont to have more fonts. See CastleGLBitmapsFonts unit.
  For example, you can show player's health like this:

<?php echo pascal_highlight(
'SetWindowPos(10, 10);
UIFont.Print(Format(\'Player life: %f / %f\', [Player.Life, Player.MaxLife]));'); ?>

- Every inventory item has already loaded image (defined in resource.xml),
  as TCastleImage (image stored in normal memory, see CastleImages unit) and
  TGLImage (image stored in GPU memory, easy to render, see CastleGLImages).
  For example, you can iterate over inventory
  list and show them like this:

<?php echo pascal_highlight(
'for I := 0 to Player.Inventory.Count - 1 do
begin
  SetWindowPos(I * 100, 0);
  Player.Inventory[I].Resource.GLImage.Draw;
end;'); ?>

- CastleGLUtils contains ready procedures to draw
  a rectangle represending fade out (when player is in pain).
  And Player instance already has properties Player.FadeOutColor, Player.FadeOutIntensity
  representing when player is in pain (and the pain color).
  Player.Dead says when Player is Dead.

  So you could visualize pain and dead states like this:

<?php echo pascal_highlight(
'if Player.Dead then
  GLFadeRectangle(0, 0, ContainerWidth, ContainerHeight, Red3Single, 1.0) else
  GLFadeRectangle(0, 0, ContainerWidth, ContainerHeight,
    Player.FadeOutColor, Player.FadeOutIntensity);'); ?>

  Note that Player.FadeOutIntensity will be 0 when there is no pain, which cooperates
  nicely with GLFadeRectangle definition that will do nothing when 4th parameter is 0.
  That is why we carelessly always call GLFadeRectangle &mdash; when player is not dead,
  and is not in pain (Player.FadeOutIntensity = 0) then nothing will actually happen.

- CastleGLUtils, and many other modules, provide many other helpers.

See fps_game for a working and fully-documented demo of such TGame2DControls
implementation, with some extra information.
See castle1 sources (unit GamePlay) for an example implementation that shows
more impressive player's life indicator and inventory and other things on the screen.

You can use any 2D engine controls like this, just add them to Window.Controls.
See CastleControls unit for some standard buttons and panels and images.
But for a specific game you will probably want a specialized UI, done like the example above.

<?php
  tutorial_footer();
?>
