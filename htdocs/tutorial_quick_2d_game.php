<?php
require_once 'castle_engine_functions.php';
tutorial_header('Quick 2D game (getting to know window events)');

$toc = new TableOfContents(
  array(
    new TocItem('Loading image (TGLImage class)', 'load'),
    new TocItem('Drawing image (OnRender event)', 'draw'),
    new TocItem('Moving image (OnUpdate event)', 'update'),
    new TocItem('Reacting to user input (OnPress event)', 'press'),
    new TocItem('Further reading', 'further'),
  )
);

echo castle_thumbs(array(
  array('filename' => 'basketball_2d_game_lazarus.png', 'titlealt' => 'Simple game where you move an image on the screen - you will learn how to do it very soon!'),
  array('filename' => 'basketball_2d_game_window.png', 'titlealt' => 'The simplest basketball game ever created'),
  array('filename' => 'castle_spine_screen_9.png', 'titlealt' => '2D game with animations done in Spine'),
  array('filename' => 'fly_over_river_screen_26.png', 'titlealt' => 'Simple &quot;River Ride&quot; clone done in 1-hour gamejam'),
));
?>

<div class="jumbotron">
<p><span class="label label-warning">Warning</span> This tutorial page uses features available only in the <b>unstable <a href="https://github.com/castle-engine/castle-engine">engine version on GitHub</a></b>. Do not read this if you use the <b>stable engine version</b> (downloaded as zip or tar.gz from our pages), or be prepared to make some modifications.

<p>In particular, in the stable engine version, the <code>TGLImage</code> class is a little more difficult to use. It needs to be created / destroyed in <code>OnGLContextOpen</code> / <code>OnGLContextClose</code>. <a href="http://castle-engine.sourceforge.net/tutorial_player_2d_controls.php">Details are explained here</a>.
</div>

<p>Before we dive into 3D, we can take a quick stab at basic stuff
you can do with our new context. Let's draw some images and handle basic
inputs.

<p>Many engine examples demonstrate this.
You can take a look e.g. at <a href="https://github.com/castle-engine/one-hour-gamejam-fly-over-river">our "River Ride" clone
done in a 1-hour game-jam</a> ! :)

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>Create an instance of <?php api_link('TGLImage', 'CastleGLImages.TGLImage.html'); ?>, and load an image there. <?php api_link('TGLImage', 'CastleGLImages.TGLImage.html'); ?> allows to load and display the image on screen.

<ol>
  <li><p><b>If you use Lazarus form with
    <?php api_link('TCastleControl', 'CastleControl.TCastleControl.html'); ?>:</b>
    Create and destroy the image in the form's <code>OnCreate</code> and
    <code>OnDestroy</code> events, like this:
    <ul>
      <li>Select the form in Lazarus (click on it in the <i>form
        designer</i> or <i>object inspector</i> &mdash; be sure to <b>select the
        form, not the TCastleControl instance</b>).
      <li>Then double click
        on appropriate events to create code for <code>OnCreate</code> and <code>OnDestroy</code>.
        Put there the following code:
    </ul>

<?php echo pascal_highlight(
'// Also: add to your uses clause: CastleGLImages, CastleFilesUtils

// Also: add to your form private section a declaration of: "Image: TGLImage"

procedure TForm1.FormCreate(Sender: TObject);
begin
  Image := TGLImage.Create(ApplicationData(\'my_image.png\'));
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Image);
end;'); ?>

    <p>If effect, your whole unit code should look like this:

<?php echo pascal_highlight(
'unit laz_unit1;
{$mode objfpc}{$H+}
interface

uses Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  CastleControl, CastleGLImages, CastleFilesUtils;

type
  TForm1 = class(TForm)
    CastleControl1: TCastleControl;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Image: TGLImage;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Image := TGLImage.Create(ApplicationData(\'my_image.png\'));
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Image);
end;

end.'); ?>

  <li><p><b>If you use
    <?php api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html'); ?></b>:
    In the simplest case, just create and destroy the image like this:

<?php echo pascal_highlight(
'uses SysUtils, CastleWindow, CastleGLImages, CastleFilesUtils;
var
  Window: TCastleWindow;
  Image: TGLImage;
begin
  Image := TGLImage.Create(ApplicationData(\'my_image.png\'));
  try
    Window := TCastleWindow.Create(Application);
    Window.Open;
    Application.Run;
  finally FreeAndNil(Image) end;
end.'); ?>
</ol>

<?php echo $toc->html_section(); ?>

<p>Next we want to draw this image. To do this, we want to call
<?php api_link('TGLImage.Draw', 'CastleGLImages.TGLImage.html#Draw'); ?> method within the <code>OnRender</code> callback of our window.

<ol>
  <li><p><b>If you use Lazarus form with
    <?php api_link('TCastleControl', 'CastleControl.TCastleControl.html'); ?>:</b>
    Select the <code>TCastleControl</code> instance,
    and double click to create code for an event <code>OnRender</code>.
    Put there the following code:

<?php echo pascal_highlight(
'// Also: add to your form private section a declaration of: "X, Y: Single;"

procedure TForm1.CastleControl1Render(Sender: TObject);
begin
  Image.Draw(X, Y);
end;'); ?>

  <li><p><b>If you use
    <?php api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html'); ?></b>:
    Change your program like this:

<?php echo pascal_highlight(
'uses SysUtils, CastleWindow, CastleGLImages, CastleFilesUtils;
var
  Window: TCastleWindow;
  Image: TGLImage;
  X: Single = 0.0;
  Y: Single = 0.0;

procedure WindowRender(Container: TUIContainer);
begin
  Image.Draw(X, Y);
end;

begin
  Image := TGLImage.Create(ApplicationData(\'my_image.png\'));
  try
    Window := TCastleWindow.Create(Application);
    Window.OnRender := @WindowRender;
    Window.Open;
    Application.Run;
  finally FreeAndNil(Image) end;
end.'); ?>
</ol>

<p>As you can guess, we can now move the image by simply changing the <code>X</code>,
<code>Y</code> variables. Note that we defined
<code>X</code>, <code>Y</code> as floating-point values
(<code>Single</code> type), not just integers, because floating-point values
are more comfortable to animate (you can easily change them at any speed).
When necessary for rendering, they will internally be rounded to whole pixels anyway.

<?php echo $toc->html_section(); ?>

<p>The event <code>OnUpdate</code> is continously called by the engine.
You should use it to update the state of your world as time passes.

<p>Use the <code>Fps.UpdateSecondsPassed</code> to know how much time has passed
since the last frame. You should scale all your movement by it, to adjust
to any computer speed. For example, to move by 100 pixels per second,
we will increase our position by <code>CastleControl1.Fps.UpdateSecondsPassed * 100.0</code>.

<ol>
  <li><p><b>If you use Lazarus form with
    <?php api_link('TCastleControl', 'CastleControl.TCastleControl.html'); ?>:</b>
    double click to create an event <code>OnUpdate</code>
    on <code>TCastleControl</code>, and put there the following code:

<?php echo pascal_highlight(
'procedure TForm1.CastleControl1Update(Sender: TObject);
begin
  Y := Y + CastleControl1.Fps.UpdateSecondsPassed * 100.0;
end;'); ?>

  <li><p><b>If you use
    <?php api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html'); ?></b>:
    Assign a <code>Window.OnUpdate</code> callback (analogous to
    <code>Window.OnRender</code> above):

<?php echo pascal_highlight(
'procedure WindowUpdate(Container: TUIContainer);
begin
  Y := Y + Container.Fps.UpdateSecondsPassed * 100.0;
end;

// ... at initialization, right after assigninig Window.OnRender, add:
  Window.OnUpdate := @WindowUpdate;'); ?>
</ol>

<?php echo $toc->html_section(); ?>

<p>The react to one-time key or mouse press, use the <code>OnPress</code> event.
You can also check which keys are pressed inside the <code>OnUpdate</code> event,
to update movement constantly. Examples below show both ways.

<ol>
  <li><p><b>If you use Lazarus form with
    <?php api_link('TCastleControl', 'CastleControl.TCastleControl.html'); ?>:</b>
    double click to create an event <code>OnPress</code>
    on <code>TCastleControl</code>. Change the <code>OnPress</code> and
    <code>OnUpdate</code> like below.

<?php echo pascal_highlight(
'procedure TForm1.CastleControl1Press(Sender: TObject; const Event: TInputPressRelease);
begin
  if Event.IsKey(K_Space) then
    Y -= 200.0;
end;

// new extended OnUpdate handler
procedure TForm1.CastleControl1Update(Sender: TObject);
var
  SecondsPassed: Single;
begin
  SecondsPassed := CastleControl1.Fps.UpdateSecondsPassed;
  Y := Y + SecondsPassed * 100.0;
  if CastleControl1.Pressed[K_Left] then
    X := X - SecondsPassed * 200.0;
  if CastleControl1.Pressed[K_Right] then
    X := X + SecondsPassed * 200.0;
end;');

  // PRO TIP: scale the SecondsPassed now to make the whole game go faster/slower:)
?>

  <li><p><b>If you use
    <?php api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html'); ?></b>:
    Assign a <code>Window.OnPress</code> callback (analogous to
    <code>Window.OnRender</code> above). Change the <code>OnPress</code> and
    <code>OnUpdate</code> like below.

<?php echo pascal_highlight(
'// Also add to the uses clause unit CastleKeysMouse

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.IsKey(K_Space) then
    Y -= 200.0;
end;

// new extended OnUpdate handler
procedure WindowUpdate(Container: TUIContainer);
var
  SecondsPassed: Single;
begin
  SecondsPassed := Container.Fps.UpdateSecondsPassed;
  Y := Y + SecondsPassed * 100.0;
  if Container.Pressed[K_Left] then
    X := X - SecondsPassed * 200.0;
  if Container.Pressed[K_Right] then
    X := X + SecondsPassed * 200.0;
end;

// ... at initialization, right after assigninig Window.OnRender, do this:
  Window.OnUpdate := @WindowUpdate;
  Window.OnPress := @WindowPress;');

// PRO TIP: scale the SecondsPassed now to make the whole game go faster/slower:)
?>
</ol>

<?php echo $toc->html_section(); ?>

<p>If you want to go more into the direction of 2D games, here are some starting points:

<ul>
  <li><p>See the <?php echo a_href_page('tutorial about displaying 2D controls - player HUD', 'tutorial_player_2d_controls'); ?>. It has a nice overview of 2D drawing capabilities.

    <p>It also shows a more flexible way to handle drawing and inputs, by creating new descendants of <?php api_link('TUIControl', 'CastleUIControls.TUIControl.html'); ?> (instead of simply attaching to window callbacks).

  <li><p>If you would like to display a series of images, not a static image, you can use <?php api_link('TGLVideo2D', 'CastleGLImages.TGLVideo2D.html'); ?> (show image sequence from many separate images or a video) or <?php api_link('TSprite', 'CastleGLImages.TSprite.html'); ?> (show image sequence from a sprite sheet &mdash; one large image containing many animation frames).

  <li><p>If you want to use smooth and efficient animations, instead of using a series of images, you can load a 2D model (and animation) from an <?php echo a_href_page('X3D', 'vrml_x3d'); ?> or <a href="https://github.com/castle-engine/castle-engine/wiki/Spine">Spine</a> or other format supported by our engine. To do this, create a <?php api_link('T2DSceneManager', 'Castle2DSceneManager.T2DSceneManager.html'); ?>, and inside it add <?php api_link('T2DScene', 'Castle2DSceneManager.T2DScene.html'); ?> instance. <?php api_link('T2DScene', 'Castle2DSceneManager.T2DScene.html'); ?> descends from our powerful <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>, you can load a 2D or 3D model there, you can transform it using <?php api_link('T3DTransform', 'Castle3D.T3DTransform.html'); ?> and do many other fancy stuff with it.

    <p>See the <i>Web3d2015 Castle Game Engine tutorial</i> (<a href="http://castle-engine.sourceforge.net/miscella/cge_tutorial_slides.pdf">the slides are here</a>, and <a href="https://github.com/castle-engine/cge-tutorial">the examples (sample data and code) are here</a>). It's 2nd part shows nicely this.

    <p>See also the example <code>castle_game_engine/examples/2d_dragon_spine_android_game/</code>.
</ul>

<p>To make inputs user-configurable, you could wrap them in <?php api_link('TInputShortcut', 'CastleInputs.TInputShortcut.html'); ?> instance. This will store whether the input is a key press or a mouse click, and you can check and change it at runtime. More information is in <?php echo a_href_page('tutorial about key / mouse shortcuts', 'tutorial_key_mouse'); ?>. This will allow you to replace the check <code>Event.IsKey(K_Space)</code> with something much more powerful:)

<?php
tutorial_footer();
?>
