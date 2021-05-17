<?php
require_once 'castle_engine_functions.php';
manual_header('Quick 2D game (basic window events)');

$toc = new TableOfContents(
  array(
    new TocItem('Finished version', 'finished'),
    new TocItem('Loading image (TDrawableImage class)', 'load'),
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

<?php /*
<div class="jumbotron">
<p><span class="label label-warning">Warning</span> This manual page uses features available only in the <b>unstable <a href="https://github.com/castle-engine/castle-engine">engine version on GitHub</a></b>. Do not read this if you use the <b>stable engine version</b> (downloaded as zip or tar.gz from our pages), or be prepared to make some modifications.

<p>In particular, in the stable engine version, the <code>TDrawableImage</code> class is a little more difficult to use. It needs to be created / destroyed in <code>OnGLContextOpen</code> / <code>OnGLContextClose</code>. <a href="manual_player_2d_controls.php">Details are explained here</a>.
</div>
*/ ?>

<p>Before we dive into full-featured viewports, scenes and 3D,
let's take a quick look at the simple things you can do with our window.
Let's draw some images and handle inputs.

<div class="panel panel-primary">
  <div class="panel-heading">TODO: Outdated manual page</div>
  <div class="panel-body">
    <p>Admittedly this manual page is outdated.
    It shows a way that <i>still works</i> but it is no longer what we advise
    as a most comfortable way to do such things in the engine.
    Have patience as we update our documentation, and in the meantime:

    <p>We advise you now to:</p>

    <ol>
      <li>
        <p>Create a <i>"New Project"</i> in <a href="manual_editor.php">CGE editor</a> and start with the <i>Empty</i> template.
      <li>
        <p>It has a code in <code>code/gamestatemain.pas</code> that shows how to handle keys in <code>TStateMain.Press</code>
          and do something continuously in <code>TStateMain.Update</code>.
      <li>
        <p>You can render an image directly (as described in this manual page, using
          <?php api_link('TDrawableImage', 'CastleGLImages.TDrawableImage.html'); ?>)
          by overriding <code>TStateMain.Render</code>.
          Alternatively, you could design (using CGE editor) an image as <code>TCastleImageControl</code>
          and only move it.
    </ol>
  </div>
</div>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>You can check out a finished version of the example presented in this chapter:

<ul>
  <li>Lazarus version: <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/lazarus/quick_2d_game">Engine examples/lazarus/quick_2d_game</a>
  <li>CastleWindow version: <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/user_interface/quick_2d_game">Engine examples/user_interface/quick_2d_game</a>
</ul>

<p>For a larger demo of a game using simplest 2D image drawing,
take a look at <a href="https://github.com/castle-engine/one-hour-gamejam-fly-over-river">our "River Ride" clone done in a 1-hour game-jam</a> ! :)

<?php echo $toc->html_section(); ?>

<p>Create an instance of <?php api_link('TDrawableImage', 'CastleGLImages.TDrawableImage.html'); ?>, and load an image there. <?php api_link('TDrawableImage', 'CastleGLImages.TDrawableImage.html'); ?> allows to load and display the image on screen.

<ol>
  <li><p><b>If you use
    <?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?></b>:
    In the simplest case, just create and destroy the image like this:

<?php echo pascal_highlight(
'uses SysUtils, CastleWindow, CastleGLImages, CastleFilesUtils;
var
  Window: TCastleWindowBase;
  Image: TDrawableImage;
begin
  Image := TDrawableImage.Create(\'castle-data:/my_image.png\');
  try
    Window := TCastleWindowBase.Create(Application);
    Window.Open;
    Application.Run;
  finally FreeAndNil(Image) end;
end.'); ?>
  <li><p><b>If you use Lazarus form with
    <?php api_link('TCastleControlBase', 'CastleControl.TCastleControlBase.html'); ?>:</b>
    Create and destroy the image in the form's <code>OnCreate</code> and
    <code>OnDestroy</code> events, like this:
    <ul>
      <li>Select the form in Lazarus (click on it in the <i>form
        designer</i> or <i>object inspector</i> &mdash; be sure to <b>select the
        form, not the TCastleControlBase instance</b>).
      <li>Then double click
        on appropriate events to create code for <code>OnCreate</code> and <code>OnDestroy</code>.
        Put there the following code:
    </ul>

<?php echo pascal_highlight(
'// Also: add to your uses clause: CastleGLImages, CastleFilesUtils

// Also: add to your form private section a declaration of: "Image: TDrawableImage"

procedure TForm1.FormCreate(Sender: TObject);
begin
  Image := TDrawableImage.Create(\'castle-data:/my_image.png\');
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
    CastleControl1: TCastleControlBase;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Image: TDrawableImage;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Image := TDrawableImage.Create(\'castle-data:/my_image.png\');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Image);
end;

end.'); ?>

</ol>

<?php echo $toc->html_section(); ?>

<p>Next we want to draw this image. To do this, we want to call
<?php api_link('TDrawableImage.Draw', 'CastleGLImages.TDrawableImage.html#Draw'); ?> method within the <code>OnRender</code> callback of our window.

<ol>
  <li><p><b>If you use
    <?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?></b>:
    Change your program like this:

<?php echo pascal_highlight(
'uses SysUtils, CastleWindow, CastleGLImages, CastleFilesUtils;
var
  Window: TCastleWindowBase;
  Image: TDrawableImage;
  X: Single = 0.0;
  Y: Single = 0.0;

procedure WindowRender(Container: TUIContainer);
begin
  Image.Draw(X, Y);
end;

begin
  Image := TDrawableImage.Create(\'castle-data:/my_image.png\');
  try
    Window := TCastleWindowBase.Create(Application);
    Window.OnRender := @WindowRender;
    Window.Open;
    Application.Run;
  finally FreeAndNil(Image) end;
end.'); ?>
  <li><p><b>If you use Lazarus form with
    <?php api_link('TCastleControlBase', 'CastleControl.TCastleControlBase.html'); ?>:</b>
    Select the <code>TCastleControlBase</code> instance,
    and double click to create code for an event <code>OnRender</code>.
    Put there the following code:

<?php echo pascal_highlight(
'// Also: add to your form private section a declaration of: "X, Y: Single;"

procedure TForm1.CastleControl1Render(Sender: TObject);
begin
  Image.Draw(X, Y);
end;'); ?>
</ol>

<p>As you can guess, we can now move the image by simply changing the <code>X</code>,
<code>Y</code> variables. Note that we defined
<code>X</code>, <code>Y</code> as floating-point values
(<code>Single</code> type), not just integers, because floating-point values
are more comfortable to animate (you can easily change them at any speed).
When necessary for rendering, they will internally be rounded to whole pixels anyway.

<?php echo $toc->html_section(); ?>

<p>The event <code>OnUpdate</code> is continuously called by the engine.
You should use it to update the state of your world as time passes.

<p>Use the <code>Fps.SecondsPassed</code> to know how much time has passed
since the last frame. You should scale all your movement by it, to adjust
to any computer speed. For example, to move by 100 pixels per second,
we will increase our position by <code>CastleControl1.Fps.SecondsPassed * 100.0</code>.

<ol>
  <li><p><b>If you use
    <?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?></b>:
    Assign a <code>Window.OnUpdate</code> callback (analogous to
    <code>Window.OnRender</code> above):

<?php echo pascal_highlight(
'procedure WindowUpdate(Container: TUIContainer);
begin
  Y := Y + Container.Fps.SecondsPassed * 100.0;
end;

// ... at initialization, right after assigninig Window.OnRender, add:
  Window.OnUpdate := @WindowUpdate;'); ?>
  <li><p><b>If you use Lazarus form with
    <?php api_link('TCastleControlBase', 'CastleControl.TCastleControlBase.html'); ?>:</b>
    double click to create an event <code>OnUpdate</code>
    on <code>TCastleControlBase</code>, and put there the following code:

<?php echo pascal_highlight(
'procedure TForm1.CastleControl1Update(Sender: TObject);
begin
  Y := Y + CastleControl1.Fps.SecondsPassed * 100.0;
end;'); ?>
</ol>

<?php echo $toc->html_section(); ?>

<p>The react to one-time key or mouse press, use the <code>OnPress</code> event.
You can also check which keys are pressed inside the <code>OnUpdate</code> event,
to update movement constantly. Examples below show both ways.

<ol>
  <li><p><b>If you use
    <?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?></b>:
    Assign a <code>Window.OnPress</code> callback (analogous to
    <code>Window.OnRender</code> above). Change the <code>OnPress</code> and
    <code>OnUpdate</code> like below.

<?php echo pascal_highlight(
'// Also add to the uses clause unit CastleKeysMouse

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.IsKey(K_Space) then
    Y := Y - 200.0;
end;

// new extended OnUpdate handler
procedure WindowUpdate(Container: TUIContainer);
var
  SecondsPassed: Single;
begin
  SecondsPassed := Container.Fps.SecondsPassed;
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
  <li><p><b>If you use Lazarus form with
    <?php api_link('TCastleControlBase', 'CastleControl.TCastleControlBase.html'); ?>:</b>
    double click to create an event <code>OnPress</code>
    on <code>TCastleControlBase</code>. Change the <code>OnPress</code> and
    <code>OnUpdate</code> like below.

<?php echo pascal_highlight(
'procedure TForm1.CastleControl1Press(Sender: TObject; const Event: TInputPressRelease);
begin
  if Event.IsKey(K_Space) then
    Y := Y - 200.0;
end;

// new extended OnUpdate handler
procedure TForm1.CastleControl1Update(Sender: TObject);
var
  SecondsPassed: Single;
begin
  SecondsPassed := CastleControl1.Fps.SecondsPassed;
  Y := Y + SecondsPassed * 100.0;
  if CastleControl1.Pressed[K_Left] then
    X := X - SecondsPassed * 200.0;
  if CastleControl1.Pressed[K_Right] then
    X := X + SecondsPassed * 200.0;
end;');

  // PRO TIP: scale the SecondsPassed now to make the whole game go faster/slower:)
?>
</ol>

<?php echo $toc->html_section(); ?>

<p>If you want to go more into the direction of 2D games:

<ul>
  <li><p>See the <?php echo a_href_page('manual about drawing your own 2D controls', 'manual_2d_ui_custom_drawn'); ?>. It has a nice overview of 2D drawing capabilities. You can also use the <?php echo a_href_page('standard 2D controls', 'manual_2d_user_interface'); ?> with a lot of ready functionality.

    <p>It also shows a more flexible way to handle drawing and inputs, by creating new descendants of <?php api_link('TCastleUserInterface', 'CastleUIControls.TCastleUserInterface.html'); ?> (instead of simply attaching to window callbacks).

  <li><p>If you want to use smooth and efficient animations, you can load a 2D model (and animation) from <a href="creating_data_model_formats.php">any supported format (like X3D or glTF or Spine)</a>. To do this:

    <ol>
      <li>Create a <?php api_link('TCastleViewport', 'CastleViewport.TCastleViewport.html'); ?>.
      <li>Call <?php api_link('TCastleViewport.Setup2D', 'CastleViewport.TCastleViewport.html#Setup2D'); ?>.
      <li>Create <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?> instance.
      <li>Call <?php api_link('TCastleScene.Setup2D', 'CastleScene.TCastleScene.html#Setup2D'); ?>.
    </ol>

    <p>The following manual chapters focus on <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?> usage, and apply for both 3D and 2D games.

    <p>See the example code <code>castle_game_engine/examples/2d_dragon_spine_android_game/</code> inside the engine.

  <li><p>You can also make inputs user-configurable. To do this, wrap each input in a <?php api_link('TInputShortcut', 'CastleInputs.TInputShortcut.html'); ?> instance. This will store whether the input is a key press or a mouse click, and you can check and change it at runtime. More information is in <?php echo a_href_page('manual about key / mouse shortcuts', 'manual_key_mouse'); ?>.
</ul>

<?php
manual_footer();
?>
