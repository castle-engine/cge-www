<?php
require_once 'castle_engine_functions.php';
tutorial_header('Custom drawn 2D controls: player HUD');

$toc = new TableOfContents(
  array(
    new TocItem('Introduction', 'introduction'),
    new TocItem('Initial code', 'initial_code'),
    new TocItem('Drawing stuff', 'drawing'),
    new TocItem('Text', 'text', 1),
    new TocItem('Rectangles, circles, other shapes', 'shapes', 1),
    new TocItem('Images', 'images', 1),
    new TocItem('Player inventory', 'inventory', 1),
    new TocItem('Screen fade effects', 'screen_fade', 1),
    new TocItem('Coordinates and window (container) sizes', 'coordinates_and_sizes'),
    new TocItem('Take into account UI scaling and anchors', 'scaling_and_anchors'),
    new TocItem('Examples', 'examples'),
  )
);

echo castle_thumbs(array(
  array('filename' => 'ui_custom_drawn.png', 'titlealt' => 'Player HUD we are going to create'),
  array('filename' => 'fps_game_screen_15.png', 'titlealt' => 'fps_game demo player HUD showing inventory'),
  array('filename' => 'castle_items_tower_screen_0.png', 'titlealt' => '&quot;The Castle&quot; inventory'),
));
?>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>While you can <a href="tutorial_2d_user_interface.php">compose
user interface from existing UI controls</a>,
sometimes it's more flexible to create a control that renders what you want
using lower-level utilities for 2D drawing.

<p>An example that we will consider in this chapter is designing
a HUD (<i>heads-up display</i>) that displays some player information,
like current life and inventory. In the <a href="tutorial_player.php">chapter about "Player"</a> we shown how to easily use the standard
 <?php api_link('TPlayer', 'CastlePlayer.TPlayer.html'); ?>
 class to automatically store and update your player's life and inventory
(see <?php api_link('TPlayer', 'CastlePlayer.TPlayer.html'); ?> and ancestors properties, like
<?php api_link('T3DAliveWithInventory.Inventory', 'CastleItems.T3DAliveWithInventory.html#Inventory'); ?> and
<?php api_link('T3DAlive.Life', 'Castle3D.T3DAlive.html#Life'); ?>).
But this information is not <i>displayed</i> automatically in any way,
since various games have wildly different needs.
<i>Let's draw the information about player ourselves</i>.</p>

<p>There are two places where you can draw:</p>

<ol>
  <li><p>You can just place the appropriate drawing code in
    <code>OnRender</code> event
    (see <?php api_link('TCastleWindowCustom.OnRender', 'CastleWindow.TCastleWindowCustom.html#OnRender'); ?>,
    <?php api_link('TCastleControlCustom.OnRender', 'CastleControl.TCastleControlCustom.html#OnRender'); ?>).
    This is simple to use, and works OK for simple applications.
  </li>

  <li><p>In the long-term, it's usually better to create your own
    <?php api_link('TUIControl', 'CastleUIControls.TUIControl.html'); ?>
    descendant. This way you wrap the rendering
    (and possibly other processing) inside your own class.
    You can draw anything you want in the overridden
    <?php api_link('TUIControl.Render', 'CastleUIControls.TUIControl.html#Render'); ?>
     method.</p>
   </li>
</ol>

<?php echo $toc->html_section(); ?>

<p>Here's a simple start of a 2D control class definition and usage.
It shows the player health by simply writing it out as text.</p>

<?php echo pascal_highlight(file_get_contents('code-samples/custom_2d_control.lpr')); ?>

<?php echo $toc->html_section(); ?>

<p>Inside <code>TMyPlayerHUD.Render</code> you can draw using our
2D drawing API.

<?php echo $toc->html_section(); ?>

<p>To draw a text, you can use ready global font
<?php api_link('UIFont', 'CastleControls.html#UIFont'); ?>
 (in <?php api_link('CastleControls', 'CastleControls.html'); ?> unit).
This is an instance of <?php api_link('TCastleFont', 'CastleFonts.TCastleFont.html'); ?>.
For example, you can show player's health like this:

<?php echo pascal_highlight(
'UIFont.Print(10, 10, Yellow,
  Format(\'Player life: %f / %f\', [Player.Life, Player.MaxLife]));'); ?>

<p>You can also create your own instances
of <?php api_link('TCastleFont', 'CastleFonts.TCastleFont.html'); ?>
 to have more fonts. See
<?php echo a_href_page('the tutorial chapter about text and fonts for more',
'tutorial_text'); ?>.

<p><i>Note: Drawing a text this way means that you manually do something
similar to the
<?php api_link('TCastleLabel', 'CastleControls.TCastleLabel.html'); ?>
 control</i>.

<?php echo $toc->html_section(); ?>

<p>To <b>draw a rectangle</b> use the
<?php api_link('DrawRectangle', 'CastleGLUtils.html#DrawRectangle'); ?>
 method. Blending is automatically used if you pass color with alpha &lt; 1.</p>

<p>For example, we can show a nice health bar showing the player's life:</p>

<?php echo pascal_highlight(
'procedure TMyPlayerHUD.Render;
var
  R: TRectangle;
begin
  inherited;

  R := Rectangle(10, 10, 400, 50);
  { draw background of health bar with a transparent red }
  DrawRectangle(R, Vector4Single(1, 0, 0, 0.5));
  { calculate smaller R, to only include current life }
  R := R.Grow(-3);
  R.Width := Round(R.Width * Player.Life / Player.MaxLife);
  { draw the inside of health bar with an opaque red }
  DrawRectangle(R, Vector4Single(1, 0, 0, 1));

  UIFont.Print(20, 20, Yellow,
    Format(\'Player life: %f / %f\', [Player.Life, Player.MaxLife]));
end;'); ?>

<p><i>Note: Drawing a rectangle this way means that you manually do something
similar to the
<?php api_link('TCastleRectangleControl', 'CastleControls.TCastleRectangleControl.html'); ?>
 control</i>.

<p>To <b>draw a circle</b> use the
<?php api_link('DrawCircle', 'CastleGLUtils.html#DrawCircle'); ?>.
 There are also procedures to draw only an outline:
 <?php api_link('DrawRectangleOutline', 'CastleGLUtils.html#DrawRectangleOutline'); ?>
 <?php api_link('DrawCircleOutline', 'CastleGLUtils.html#DrawCircleOutline'); ?>.

<p><i>Note: Drawing shapes this way means that you manually do something
similar to the
<?php api_link('TCastleShape', 'CastleControls.TCastleShape.html'); ?>
 control</i>.

<p>To <b>draw an arbitrary 2D primitive</b> use the
<?php api_link('DrawRectangle', 'CastleGLUtils.html#DrawRectangle'); ?>
 method. Blending is automatically used if you pass color with alpha &lt; 1.</p>

<?php echo $toc->html_section(); ?>

<p>To draw an image, use the
<?php api_link('TGLImage', 'CastleGLImages.TGLImage.html'); ?> class.
 It has methods
 <?php api_link('Draw', 'CastleGLImages.TGLImageCore.html#Draw'); ?> and
 <?php api_link('Draw3x3', 'CastleGLImages.TGLImageCore.html#Draw3x3'); ?>
 to draw the image, intelligently stretching it,
 optionally preserving unstretched corners.

<p>Here's a simple example of
<?php api_link('TGLImage', 'CastleGLImages.TGLImage.html'); ?> usage
to display a hero's face. You can use an image below,
if you're old enough to recognize it:)
(<a href="http://doom.wikia.com/wiki/File:Doomfaces.png">Source</a>.)

<p><a href="images/doom_face.png"><img src="images/doom_face.png" alt="DOOM hero face" /></a>

<?php echo pascal_highlight(
'uses ..., Classes, CastleFilesUtils, CastleGLImages;

type
  TMyPlayerHUD = class(TUIControl)
  private
    FMyImage: TGLImage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
  end;

constructor TMyPlayerHUD.Create(AOwner: TComponent);
begin
  inherited;
  FMyImage := TGLImage.Create(ApplicationData(\'face.png\'));
end;

destructor TMyPlayerHUD.Destroy;
begin
  FreeAndNil(FMyImage);
  inherited;
end;

procedure TMyPlayerHUD.Render;
begin
  inherited;

  // ... previous TMyPlayerHUD.Render contents ...

  FMyImage.Draw(420, 10);
end;'); ?>

<p><i>Note: Drawing images this way means that you manually do something
similar to the
<?php api_link('TCastleImageControl', 'CastleControls.TCastleImageControl.html'); ?>
 control</i>.

<?php echo $toc->html_section(); ?>

<p>The <code>TPlayer</code> class manages the player inventory.
Each <i>inventory item</i> may already have a default image associated with it.
It is defined in the <code>resource.xml</code> file of the item,
see <a href="tutorial_resources.php">the chapter about using creatures / items</a>
and see <a href="creating_data_resources.php">the chapter about defining creatures / items resource.xml files</a>
and see the <code>examples/fps_game/data/item_medkit/</code> for an example
item definition.

<p>The image is available as a
 <?php api_link('TGLImage', 'CastleGLImages.TGLImage.html'); ?>
 instance ready for drawing.
For example, you can iterate over the inventory list and show the items like this:</p>

<?php echo pascal_highlight(
'for I := 0 to Player.Inventory.Count - 1 do
  Player.Inventory[I].Resource.GLImage.Draw(I * 100, 0);'); ?>

 <p>See the <code>examples/fps_game/</code> for a working example of this.</p>

<?php echo $toc->html_section(); ?>

<p>For simple screen fade effects, you have procedures inside the
 <?php api_link('CastleGLUtils', 'CastleGLUtils.html'); ?> unit
 called <?php api_link('GLFadeRectangleDark', 'CastleGLUtils.html#GLFadeRectangleDark'); ?>
 and <?php api_link('GLFadeRectangleLight', 'CastleGLUtils.html#GLFadeRectangleLight'); ?>.
 These allow you to draw
 a rectangle representing fade out (when player is in pain).
 And <?php api_link('TPlayer', 'CastlePlayer.TPlayer.html'); ?>
 instance already has properties
 <?php api_link('Player.FadeOutColor', 'CastlePlayer.TPlayer.html#FadeOutColor'); ?>,
 <?php api_link('Player.FadeOutIntensity', 'CastlePlayer.TPlayer.html#FadeOutIntensity'); ?>
 representing when player is in pain (and the pain color).
 <?php api_link('Player.Dead', 'Castle3D.T3DAlive.html#Dead'); ?>
 says when player is dead (this is simply when <code>Life &lt;= 0</code>).

 <p>For example you can visualize pain and dead states like this:

<?php echo pascal_highlight(
'if Player.Dead then
  GLFadeRectangleDark(ContainerRect, Red, 1.0) else
  GLFadeRectangleDark(ContainerRect, Player.FadeOutColor, Player.FadeOutIntensity);'); ?>

 <p>Note that <code>Player.FadeOutIntensity</code> will be 0 when there is no pain, which cooperates
 nicely with <?php api_link('GLFadeRectangleDark', 'CastleGLUtils.html#GLFadeRectangleDark'); ?> definition that will do nothing when 4th parameter is 0.
 That is why we carelessly always call <?php api_link('GLFadeRectangleDark', 'CastleGLUtils.html#GLFadeRectangleDark'); ?> &mdash; when player is not dead,
 and is not in pain (<code>Player.FadeOutIntensity</code> = 0) then nothing will actually happen.

 <p><i>Note: There is also a full-featured UI control that draws an effect with
 blending (possibly modulated by an image):
 <?php api_link('TCastleFlashEffect', 'CastleFlashEffect.TCastleFlashEffect.html'); ?>.</i>

<?php echo $toc->html_section(); ?>

<p>To adjust your code to window size, note that
 our projection has (0,0) in lower-left corner (as is standard
 for 2D OpenGL). You can look at the size, in pixels, of the current
 <!--2D control in
 <?php api_link('TUIControl.Width', 'CastleUIControls.TUIControl.html#Width'); ?> x
 <?php api_link('TUIControl.Height', 'CastleUIControls.TUIControl.html#Height'); ?>,
 and the size of the current--> OpenGL container (window, control) in
 <?php api_link('ContainerWidth', 'CastleUIControls.TInputListener.html#ContainerWidth'); ?> x
 <?php api_link('ContainerHeight', 'CastleUIControls.TInputListener.html#ContainerHeight'); ?>
 or (as a rectangle) as
 <?php api_link('ContainerRect', 'CastleUIControls.TInputListener.html#ContainerRect'); ?>.
 The container size is also available as container properties, like
 <?php api_link('TCastleWindow.Width', 'CastleWindow.TCastleWindowCustom.html#Width'); ?> x
 <?php api_link('TCastleWindow.Height', 'CastleWindow.TCastleWindowCustom.html#Height'); ?>
 or (as a rectangle)
 <?php api_link('TCastleWindow.Rect', 'CastleWindow.TCastleWindowCustom.html#Rect'); ?>.

<?php echo $toc->html_section(); ?>

<p>So far, we have simply carelessly drawn our contents over the window.
<ul>
  <li>We used absolute pixel positions to draw.</li>
  <li>We did not use the control position (<?php api_link('Left', 'CastleUIControls.TUIControl.html#Left'); ?> and
    <?php api_link('Bottom', 'CastleUIControls.TUIControl.html#Bottom'); ?>).
    Nor did we take into account parent control position.</li>
  <li>We did not use the control size.
    In fact, our control has always empty size.
    The <?php api_link('TUIControl.Rect', 'CastleUIControls.TUIControl.html#Rect'); ?>
    is by default empty. We should override it, or descend from something like
    <?php api_link('TUIControlSizeable', 'CastleUIControls.TUIControlSizeable.html'); ?>.</li>
  <li>We do not honor the anchors set by
    <?php api_link('TUIControl.Anchor', 'CastleUIControls.TUIControl.html#Anchor'); ?>.</li>
  <li>We do not honor UI scaling set by the
    <?php api_link('Window.UIScaling', 'CastleUIControls.TUIContainer.html#UIScaling'); ?>.</li>
</ul>

<p>Note that it is OK to ignore (some) of these issues, if you design a UI control
specifically for your game, and you know that it's only going to be used
in a specific way.</p>

<p>To have more full-featured UI control, we could solve these issues "one by one", but as you can see
there are quite a few features that are missing.
The easiest way to handle all the features listed above is to
get inside the <code>Render</code> method the values of
<code>ScreenRect</code> and <code>UIScale</code>.
Just scale your drawn contents to always fit within the <code>ScreenRect</code>
rectangle. And scale all user size properties by <code>UIScale</code>
before applying to pixels.

<p>You have to also define a size for your control,
by overriding the <code>Rect</code> method of <code>TUIControl</code>.
(Alternatively, if you want the size to be configurable by user,
derive your control from the
<?php api_link('TUIControlSizeable', 'CastleUIControls.TUIControlSizeable.html'); ?>.)

<p>Like this:</p>

<?php echo pascal_highlight(
'function TMyImageControl.Rect: TRectangle;
begin
  Result := Rectangle(Left, Bottom, FMyImage.Width, FMyImage.Height);
  Result := Result.ScaleAround0(UIScale);
end;

procedure TMyImageControl.Render;
begin
  inherited;
  FMyImage.Draw(ScreenRect);
end;'); ?>

<?php echo $toc->html_section(); ?>

<p>See <code>examples/fps_game</code> for a working and fully-documented
demo of such <code>TMyPlayerHUD</code> implementation.
See <?php echo a_href_page('"The Castle"', 'castle'); ?> sources (unit <code>GamePlay</code>)
for an example implementation that shows
more impressive player's life indicator and inventory and other things on the screen.

<?php
tutorial_footer();
?>
