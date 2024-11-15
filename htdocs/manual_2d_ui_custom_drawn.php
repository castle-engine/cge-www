<?php
require_once 'castle_engine_functions.php';
castle_header('Advanced: custom drawn 2D controls');

$toc = new TableOfContents(
  array(
    new TocItem('Introduction', 'introduction'),
    new TocItem('Initial code', 'initial_code'),
    new TocItem('Drawing stuff', 'drawing'),
    new TocItem('Text', 'text', 1),
    new TocItem('Rectangles, circles, other shapes', 'shapes', 1),
    new TocItem('Images', 'images', 1),
    new TocItem('Complete example code showing above features', 'comple_code_finished', 1),
    new TocItem('Animations from images (movies, sprite sheets)', 'animations', 1),
    new TocItem('Screen fade effects', 'screen_fade', 1),
    new TocItem('Coordinates and window (container) sizes', 'coordinates_and_sizes'),
    new TocItem('Take into account UI scaling and anchors', 'scaling_and_anchors'),
  )
);

echo castle_thumbs(array(
  array('filename' => 'ui_custom_drawn.png', 'titlealt' => 'Player HUD we are going to create'),
  array('filename' => 'fps_game_screen_15.png', 'titlealt' => 'fps_game demo player HUD showing inventory'),
  array('filename' => 'castle_items_tower_screen_0.png', 'titlealt' => '"The Castle" inventory'),
));
?>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>While you can <a href="user_interface">compose
user interface from existing UI controls</a>,
sometimes it's more flexible to create a control that renders what you want
using lower-level utilities for 2D drawing.

<p><b>Warning: Drawing 2D things manually, as documented on this page,    is possible but not advised.</b> In our experience, manual drawing often results in more work while at the same time the resulting solution is less efficient and has less features (compared to composing your UI from components). The page below lists various ways to draw things manually but also points to higher-level components that achieve the same, and often are easier to use and more powerful. So, instead of this page, better read about <a href="user_interface">higher-level UI controls</a> and use them.

<p>An example that we will consider in this chapter is designing
a HUD (<i>heads-up display</i>) that displays some player information,
like current life.</p>

<p>Place your own drawing code in an overridden <code>Render</code> method of <?php echo cgeRef('TCastleUserInterface'); ?> descendants. It is simplest to use your <a href="views">view class</a> (which is also a <?php echo cgeRef('TCastleUserInterface'); ?> descendant) for this, just add a <code>Render</code> method to your view. But you can also create your own <?php echo cgeRef('TCastleUserInterface'); ?> descendants.</p>

<?php echo $toc->html_section(); ?>

<p>Here's a simple start of a 2D control class definition and usage.
It shows the player health by simply writing it out as text.</p>

<?php echo pascal_highlight_file('code-samples/custom_2d_control.lpr'); ?>

<?php echo $toc->html_section(); ?>

<p>Inside <code>TPlayerHud.Render</code> you can draw using our
2D drawing API.

<?php echo $toc->html_section(); ?>

<p>To draw a text, you can use ready global font
<?php echo cgeRef('UIFont'); ?>
 (in <?php echo cgeRef('CastleControls'); ?> unit).
This is an instance of <?php echo cgeRef('TCastleFont'); ?>.
For example, you can show player's health like this:

<?php echo pascal_highlight(
'UIFont.Print(10, 10, Yellow, Format(\'Player life: %f / %f\', [
  PlayerInformation.Life,
  PlayerInformation.MaxLife
]));'); ?>

<p>You can also create your own instances
of <?php echo cgeRef('TCastleFont'); ?>
 to have more fonts. See
<?php echo a_href_page('the manual chapter about "Text and fonts" for more',
'manual_text'); ?>.

<p><b>Note: It is more advised (easier, more flexible) to use
<?php echo cgeRef('TCastleLabel'); ?>
 control than to draw text like above.</b>

<?php echo $toc->html_section(); ?>

<p>To <b>draw a rectangle</b> use the
<?php echo cgeRef('DrawRectangle'); ?>
 method. Blending is automatically used if you pass color with alpha &lt; 1.</p>

<p>For example, we can show a nice health bar showing the player's life:</p>

<?php echo pascal_highlight(
'procedure TPlayerHud.Render;
var
  R: TFloatRectangle;
begin
  inherited;

  R := FloatRectangle(10, 10, 400, 50);
  { draw background of health bar with a transparent red }
  DrawRectangle(R, Vector4(1, 0, 0, 0.5));
  { calculate smaller R, to only include current life }
  R := R.Grow(-3);
  R.Width := R.Width * PlayerInformation.Life / PlayerInformation.MaxLife;
  { draw the inside of health bar with an opaque red }
  DrawRectangle(R, Vector4(1, 0, 0, 1));

  UIFont.Print(20, 20, Yellow, Format(\'Player life: %f / %f\', [
    PlayerInformation.Life,
    PlayerInformation.MaxLife
  ]));
end;'); ?>

<p><b>Note: It is more advised (easier, more flexible) to use
<?php echo cgeRef('TCastleRectangleControl'); ?>
 control than to draw rectangle like above.</b>

<p>To <b>draw a circle</b> use the
<?php echo cgeRef('DrawCircle'); ?>.
 There are also procedures to draw only an outline:
 <?php echo cgeRef('DrawRectangleOutline'); ?>
 <?php echo cgeRef('DrawCircleOutline'); ?>.

<p><b>Note: It is more advised (easier, more flexible) to use
<?php echo cgeRef('TCastleShape'); ?>
 control than to draw shapes like above.</b>

<p>To <b>draw an arbitrary 2D primitive</b> use the
<?php echo cgeRef('DrawPrimitive2D'); ?>
 method. Blending is automatically used if you pass color with alpha &lt; 1.</p>

<?php echo $toc->html_section(); ?>

<p>To draw an image, use the
<?php echo cgeRef('TDrawableImage'); ?> class.
 It has methods
 <?php echo cgeRef('TDrawableImage.Draw'); ?> and
 <?php echo cgeRef('TDrawableImage.Draw3x3'); ?>
 to draw the image, intelligently stretching it,
 optionally preserving unstretched corners.

<p>Here's a simple example of
<?php echo cgeRef('TDrawableImage'); ?> usage
to display a hero's face. You can use an image below,
if you're old enough to recognize it:)
(<a href="http://doom.wikia.com/wiki/File:Doomfaces.png">Source</a>.)

<p><a href="images/doom_face.png"><img src="images/doom_face.png" alt="DOOM hero face" /></a>

<?php echo pascal_highlight(
'uses ..., Classes, CastleFilesUtils, CastleGLImages;

type
  TPlayerHud = class(TCastleUserInterface)
  private
    FMyImage: TDrawableImage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
  end;

constructor TPlayerHud.Create(AOwner: TComponent);
begin
  inherited;
  FMyImage := TDrawableImage.Create(\'castle-data:/face.png\');
end;

destructor TPlayerHud.Destroy;
begin
  FreeAndNil(FMyImage);
  inherited;
end;

procedure TPlayerHud.Render;
begin
  inherited;

  // ... previous TPlayerHud.Render contents ...

  FMyImage.Draw(420, 10);
end;'); ?>

<p><b>Note: It is more advised (easier, more flexible) to use
<?php echo cgeRef('TCastleImageControl'); ?>
 control than to draw image like above.</b>

<?php echo $toc->html_section(); ?>

<p>Here's a complete source code that shows the above features.
You can download and compile it right now!

<?php echo pascal_highlight_file('code-samples/custom_2d_control_finished.lpr'); ?>

<?php echo $toc->html_section(); ?>

<p>If you would like to display a series of images, not a static image, you can use <?php echo cgeRef('TGLVideo2D'); ?> (show image sequence from many separate images or a video).

<p>See e.g. <a href="https://gitlab.com/michaliskambi/muuu">our game "Muuu"</a> for a demo of using sprite animations.

<p><b>Note: We advise to use instead <a href="sprite_sheets">sprite sheets</a> for animations, as they are more efficient and easier to use.</b>

<?php echo $toc->html_section(); ?>

<p>For simple screen fade effects, you have procedures inside the
 <?php echo cgeRef('CastleGLUtils'); ?> unit
 called <?php echo cgeRef('GLFadeRectangleDark'); ?>
 and <?php echo cgeRef('GLFadeRectangleLight'); ?>.
 These allow you to draw
 a rectangle representing fade out (when player is in pain).

 <p>For example you can visualize pain and dead states like this:

<?php echo pascal_highlight(
'if Player.Dead then
  GLFadeRectangleDark(ContainerRect, Red, 1.0)
else
  GLFadeRectangleDark(ContainerRect, Player.FadeOutColor, Player.FadeOutIntensity);'); ?>

 <p>Note that <code>Player.FadeOutIntensity</code> will be 0 when there is no pain, which cooperates
 nicely with <?php echo cgeRef('GLFadeRectangleDark'); ?> definition that will do nothing when 4th parameter is 0.
 That is why we carelessly always call <?php echo cgeRef('GLFadeRectangleDark'); ?> &mdash; when player is not dead,
 and is not in pain (<code>Player.FadeOutIntensity</code> = 0) then nothing will actually happen.

 <p><b>Note: There is also a full-featured UI control that draws an effect with
 blending (possibly modulated by an image), and we advise to use it instead:
 <?php echo cgeRef('TCastleFlashEffect'); ?>.</b>

<?php echo $toc->html_section(); ?>

<p>To adjust your code to window size, note that
 our projection has (0,0) in lower-left corner (as is standard
 for 2D OpenGL). You can look at the size, in pixels, of the current
 <!--2D control in
 <?php echo cgeRef('TCastleUserInterface.Width'); ?> x
 <?php echo cgeRef('TCastleUserInterface.Height'); ?>,
 and the size of the current--> OpenGL container (window, control) in
 <?php echo cgeRef('TCastleUserInterface.ContainerWidth'); ?> x
 <?php echo cgeRef('TCastleUserInterface.ContainerHeight'); ?>
 or (as a rectangle) as
 <?php echo cgeRef('TCastleUserInterface.ContainerRect'); ?>.
 The container size is also available as container properties, like
 <?php echo cgeRef('TCastleWindow.Width'); ?> x
 <?php echo cgeRef('TCastleWindow.Height'); ?>
 or (as a rectangle)
 <?php echo cgeRef('TCastleWindow.Rect'); ?>.

<?php echo $toc->html_section(); ?>

<p>So far, we have simply carelessly drawn our contents over the window.
<ul>
  <li>We used absolute pixel positions to draw.</li>
  <li>We did not use the control position (<?php echo cgeRef('TCastleUserInterface.Translation'); ?>).
    Nor did we take into account parent control position.</li>
  <li>We did not use the control size.
    We should use position and size defined by
    <?php echo cgeRef('TCastleUserInterface'); ?>.</li>
  <li>We do not honor the anchors set by
    <?php echo cgeRef('TCastleUserInterface.Anchor'); ?>.</li>
  <li>We do not honor UI scaling set by the
    <?php echo cgeRef('TCastleContainer.UIScaling'); ?>.</li>
</ul>

<p>Note that it is OK to ignore (some) of these issues, if you design a UI control
specifically for your game, and you know that it's only going to be used
in a specific way.</p>

<p>To have more full-featured UI control, we could solve these issues "one by one", but as you can see
there are quite a few features that are missing.
The easiest way to handle all the features listed above is to
get inside the <code>Render</code> method the values of
<code>RenderRect</code> and <code>UIScale</code>.
Just scale your drawn contents to always fit within the <code>RenderRect</code>
rectangle. And scale all user size properties by <code>UIScale</code>
before applying to pixels.

<p>Like this:</p>

<?php echo pascal_highlight(
'procedure TMyImageControl.Render;
begin
  inherited;
  FMyImage.Draw(RenderRect);
end;

var
  MyControl: TMyImageControl;
begin
  MyControl := TMyImageControl.Create(Application);
  MyControl.Left := 100;
  MyControl.Bottom := 200;
  MyControl.Width := 300;
  MyControl.Height := 400;
  Window.Controls.InsertFront(MyControl);
end;'); ?>

<?php
castle_footer();
?>
