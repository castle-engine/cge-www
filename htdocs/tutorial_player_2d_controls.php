<?php
require_once 'castle_engine_functions.php';
tutorial_header('Display 2D controls: player HUD');
?>

<p>You will probably want to draw some 2D controls on your screen,
even if your game is 3D.
For example you will want to display some player information,
like current life or inventory, as a HUD (<i>heads-up display</i>).

Note that you can use the standard
 <?php api_link('TPlayer', 'CastlePlayer.TPlayer.html'); ?>
 class to automatically store and update your player's life and inventory
(see <?php api_link('TPlayer', 'CastlePlayer.TPlayer.html'); ?> and ancestors properties, like
<?php api_link('T3DAliveWithInventory.Inventory', 'CastleItems.T3DAliveWithInventory.html#Inventory'); ?> and
<?php api_link('T3DAlive.Life', 'Castle3D.T3DAlive.html#Life'); ?>).
But this information is not <i>displayed</i> automatically in any way,
since various games have wildly different needs.

<p>In the simple cases, to display something in 2D,
just place the appropriate drawing code in <code>OnRender</code> event
(see <?php api_link('TCastleWindowCustom.OnRender', 'CastleWindow.TCastleWindowCustom.html#OnRender'); ?>,
<?php api_link('TCastleControlCustom.OnRender', 'CastleControl.TCastleControlCustom.html#OnRender'); ?>).

<p>In the not-so-simple cases, when your game grows larger and you
want to easily manage the stuff you display,
it is best to define a new
<?php api_link('TUIControl', 'CastleUIControls.TUIControl.html'); ?>
 descendant, where you can draw anything you want in the overridden
<?php api_link('TUIControl.Render', 'CastleUIControls.TUIControl.html#Render'); ?>
 method. A simple example:

<?php echo pascal_highlight(
'uses ..., CastleUIControls;

type
  TGame2DControls = class(TUIControl)
  public
    procedure Render; override;
  end;

procedure TGame2DControls.Render;
begin
  { ... }
end;

{ When starting your game, create TGame2DControls instance
  and add it to Window.Controls }
var
  Controls2D: TGame2DControls;
...
  Controls2D := TGame2DControls.Create(Application);
  Window.Controls.Add(Controls2D);'); ?>

<p>Inside <code>TGame2DControls.Render</code> you have the full knowledge about the world.
For example, you can access the player of your 3D game using <code>Window.SceneManager.Player</code>
(see <?php echo a_href_page('tutorial about Player for FPS game', 'tutorial_player'); ?>).
You can draw them using our 2D drawing API:

<ul>
  <li><p>To draw a <b>text</b>, you can use ready global font
    <?php api_link('UIFont', 'CastleControls.html#UIFont'); ?>
    (in <?php api_link('CastleControls', 'CastleControls.html'); ?> unit).
    This is an instance of <?php api_link('TCastleFont', 'CastleFonts.TCastleFont.html'); ?>.
    For example, you can show player's health like this:

<?php echo pascal_highlight(
'UIFont.Print(10, 10, Yellow,
  Format(\'Player life: %f / %f\', [Player.Life, Player.MaxLife]));'); ?>

    <p>Of course you can also create your own instances
    of <?php api_link('TCastleFont', 'CastleFonts.TCastleFont.html'); ?>
    to have more fonts. See
    <?php echo a_href_page('the tutorial chapter about text and fonts for more',
    'tutorial_text'); ?>.

    <p><b>Note that in simple cases, you don't need to render the text this way.
    You can instead use ready control
    <?php api_link('TCastleLabel', 'CastleControls.TCastleLabel.html'); ?></b>
    (see <?php echo a_href_page('the tutorial about text', 'tutorial_text'); ?>).
    But drawing font yourself is often more flexible.

  <li><p>To draw an <b>image</b>, use
    <?php api_link('TGLImage', 'CastleGLImages.TGLImage.html'); ?>.
    It has methods <code>Draw</code> and <code>Draw3x3</code> to draw the image,
    intelligently stretching it, optionally preserving unstretched corners.

    <p><b>Note that in simple cases, you don't need to render the image this way.
    You can instead use ready control
    <?php api_link('TCastleImageControl', 'CastleControls.TCastleImageControl.html'); ?>.</b>
    But drawing image yourself is often more flexible.

    <p>Here's a simple example of
    <?php api_link('TGLImage', 'CastleGLImages.TGLImage.html'); ?> creation and destruction:

<?php echo pascal_highlight(
'uses ..., CastleGLImages, CastleUIControls;

type
  TGame2DControls = class(TUIControl)
  private
    FMyImage: TGLImage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
  end;

constructor TGame2DControls.Create(AOwner: TComponent);
begin
  inherited;
  FMyImage := TGLImage.Create(ApplicationData(\'map_tile.png\'));
end;

destructor TGame2DControls.Destroy;
begin
  FreeAndNil(FMyImage);
  inherited;
end;

procedure TGame2DControls.Render;
begin
  inherited;
  FMyImage.Draw(100, 200);
end;'); ?>

  <li><p>Note about <b>UI scaling</b> and <code>TGLImage</code>.
    In engine version 5.3.0, we add to UI system parents, anchoring,
    and automatic UI scaling.

    <p>If you would like your own 2D controls to honor this system,
    your <code>Render</code> method will need to take them into account,
    and you should also override the <code>Rect</code> method.
    Use helpers like <code>ScreenRect</code> or <code>UIScale</code>
    to translate/scale your control correctly.

<?php echo pascal_highlight(
'function TGame2DControls.Rect: TRectangle;
begin
  Result := Rectangle(Left, Bottom, FMyImage.Width, FMyImage.Height);
  Result := Result.ScaleAround0(UIScale);
end;

procedure TGame2DControls.Render;
begin
  inherited;
  FMyImage.Draw(ScreenRect);
end;'); ?>

  <li><p><?php api_link('DrawRectangle', 'CastleGLUtils.DrawRectangle.html'); ?>
    allows to easily draw 2D <b>rectangle</b> filled with color.
    Blending is automatically used if you pass color with alpha &lt; 1.

  <li><p>Every <b>inventory item</b> has already loaded image (defined in <code>resource.xml</code>),
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
  Player.Inventory[I].Resource.GLImage.Draw(I * 100, 0);'); ?>

  <li><p>To adjust your code to window size, note that
    our projection has (0,0) in lower-left corner (as is standard
    for 2D OpenGL). You can look at the size, in pixels, of the current
    <!--2D control in
    <?php api_link('TUIControl.Width', 'CastleUIControls.TUIControl.html#Width'); ?> x
    <?php api_link('TUIControl.Height', 'CastleUIControls.TUIControl.html#Height'); ?>,
    and the size of the current--> OpenGL container (window, control) in
    <?php api_link('TUIControl.ContainerWidth', 'CastleUIControls.TUIControl.html#ContainerWidth'); ?> x
    <?php api_link('TUIControl.ContainerHeight', 'CastleUIControls.TUIControl.html#ContainerHeight'); ?>
    or (as a rectangle) as
    <?php api_link('TUIControl.ContainerRect', 'CastleUIControls.TUIControl.html#ContainerRect'); ?>.
    The container size is also available as container properties, like
    <?php api_link('TCastleWindow.Width', 'CastleWindow.TCastleWindowBase.html#Width'); ?> x
    <?php api_link('TCastleWindow.Height', 'CastleWindow.TCastleWindowBase.html#Height'); ?>
    or (as a rectangle)
    <?php api_link('TCastleWindow.Rect', 'CastleWindow.TCastleWindowBase.html#Rect'); ?>.

  <li><p>For <b>simple screen fade effects</b>, you have procedures inside
    <?php api_link('CastleGLUtils', 'CastleGLUtils.html'); ?> unit
    like <?php api_link('GLFadeRectangleDark', 'CastleGLUtils.html#GLFadeRectangleDark'); ?>
    and <?php api_link('GLFadeRectangleLight', 'CastleGLUtils.html#GLFadeRectangleLight'); ?>.
    This allows you to draw
    a rectangle representing fade out (when player is in pain).
    And <?php api_link('TPlayer', 'CastlePlayer.TPlayer.html'); ?>
    instance already has properties
    <?php api_link('Player.FadeOutColor', 'CastlePlayer.TPlayer.html#FadeOutColor'); ?>,
    <?php api_link('Player.FadeOutIntensity', 'CastlePlayer.TPlayer.html#FadeOutIntensity'); ?>
    representing when player is in pain (and the pain color).
    <?php api_link('Player.Dead', 'Castle3D.T3DAlive.html#Dead'); ?>
    says when player is dead (this is simply when <code>Life <= 0</code>).

    <p>For example you can visualize pain and dead states like this:

<?php echo pascal_highlight(
'if Player.Dead then
  GLFadeRectangleDark(ContainerRect, Red, 1.0) else
  GLFadeRectangleDark(ContainerRect, Player.FadeOutColor, Player.FadeOutIntensity);'); ?>

    <p>Note that <code>Player.FadeOutIntensity</code> will be 0 when there is no pain, which cooperates
    nicely with <code>GLFadeRectangle</code> definition that will do nothing when 4th parameter is 0.
    That is why we carelessly always call <code>GLFadeRectangle</code> &mdash; when player is not dead,
    and is not in pain (<code>Player.FadeOutIntensity</code> = 0) then nothing will actually happen.

    <p>There is also a full-featured UI control that draws an effect with
    blending (possibly modulated by an image):
    <?php api_link('TCastleFlashEffect', 'CastleFlashEffect.TCastleFlashEffect.html'); ?>.

  <li><p>Use the <code>Theme</code> global variable
    (instance of <?php api_link('TCastleTheme', 'CastleControls.TCastleTheme.html'); ?>)
    to draw using a predefined set of images. For example,
    image type <code>tiActiveFrame</code> is a general-purpose frame that you can
    use to mark a specific region on the screen. You draw it like this:

<?php echo pascal_highlight(
'Theme.Draw(Rectangle(10, 10, 100, 100), tiActiveFrame);'); ?>

    <p>You can change all the theme images. You can change them to one
    of the predefined images in <code>CastleControlsImages</code> unit.
    Like this:

<?php echo pascal_highlight(
'Theme.Images[tiActiveFrame] := FrameYellow;
Theme.Corners[tiActiveFrame] := Vector4Integer(1, 1, 1, 1);'); ?>

    <p>Or you can change them to one of your own images. Like this:

<?php echo pascal_highlight(
'Theme.Images[tiActiveFrame] := LoadImage(ApplicationData(\'frame.png\'));
Theme.OwnsImages[tiActiveFrame] := true;
Theme.Corners[tiActiveFrame] := Vector4Integer(1, 1, 1, 1);'); ?>

    <p>All our standard 2D controls are drawn using theme images.
    This way the look of your game is defined by a set of images,
    that can be easily changed by artists.

    <p>If the set of predefined images in <code>Theme</code> is too limiting,
    then use <?php api_link('TGLImage', 'CastleGLImages.TGLImage.html'); ?>
    directly.

  <li><p><?php api_link('CastleGLUtils', 'CastleGLUtils.html'); ?>,
    and some other units, provide other drawing helpers.

    <p>As a last resort, if you know OpenGL,
    you can just use direct OpenGL commands to render. This requires OpenGL knowledge
    and some extra care &mdash; you cannot change some state carelessly
    (see <?php api_link('TUIControl.Render', 'CastleUIControls.TUIControl.html#Render'); ?>),
    and you must be careful if you want your code to be portable to OpenGLES
    (Android,iOS).
</ul>

<p>See <code>examples/fps_game</code> for a working and fully-documented
demo of such <code>TGame2DControls</code> implementation.
See <?php echo a_href_page('"The Castle 1"', 'castle'); ?> sources (unit <code>GamePlay</code>)
for an example implementation that shows
more impressive player's life indicator and inventory and other things on the screen.

<p>You can use any 2D engine controls like this, just add them to <code>Window.Controls</code>.
See <?php api_link('CastleControls', 'CastleControls.html'); ?>
 unit for some standard buttons and panels and images.
But for a specific game you will probably want a specialized UI,
done like the example above.

<?php
tutorial_footer();
?>
