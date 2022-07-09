<?php
require_once 'castle_engine_functions.php';
castle_header('Designing user interface and handling events (press, update) within the state', array(
  'social_share_image' => 'basic_state_events_screen.png',
));

$toc = new TableOfContents(
  array(
    new TocItem('Overview'),
    new TocItem('Create empty project'),
    new TocItem('Add the background image (mountains)'),
    new TocItem('Add the player image (plane)'),
    new TocItem('Access designed components in the code'),
    new TocItem('Move the player in the Update method'),
    new TocItem('React to a key press'),
    new TocItem('React to a mouse click or touch'),
  )
);

echo cgeImg('float', array(
  array('filename' => 'state_events_screen.png', 'titlealt' => 'Plane flying on the mountain background - game'),
  array('filename' => 'state_events_biplane_4_resized.png', 'titlealt' => 'Plane flying on the mountain background - design'),
));
?>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p><i>State</i> in <i>Castle Game Engine</i> is a class that descends from <?php echo cgeRef('TUIState'); ?> and manages <i>what you display on the screen and how you react to basic events (user input, updates)</i>.

<p>While it is not required to put everything in some <i>state</i>, we highly advise to organize your application into a number of states. They usually divide your application code into a number of smaller pieces in a natural way. If you have used Lazarus LCL or Delphi VCL for visual designing previosly, you will recognize that our <code>TUIState</code> is a similar concept to <code>TForm</code> from LCL and VCL.

<p>In this chapter we will learn how to use the basic state features. We will create a simple toy that displays some images and allows to move them. You can follow this chapter and do it yourself, or you can look at the ready version in <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/user_interface/state_events">examples/user_interface/state_events</a>.

<?php echo $toc->html_section(); ?>

<p>We assume you have already <a href="/">downloaded Castle Game Engine</a> and <a href="install">installed it</a>.

<p>Start by creating a new project using the "Empty" template.

<?php
echo cgeImg('block', array(
  array('filename' => 'state_events_new_project.png', 'titlealt' => 'Castle Game Engine Editor New Project - State Events'),
));
?>

<p>When you create a new project, we create initial states for you. The <i>"Empty"</i> template creates a single state, by default called just <i>"Main"</i>. It is

<ul>
  <li><p>a Pascal class called <code>TStateMain</code>,
  <li><p>implemented in the unit <code>GameStateMain</code> (file <code>code/gamestatemain.pas</code>),
  <li><p>it has a single instance (singleton) <code>StateMain</code>,
  <li><p>and it displays a design (user interface you can visually create in the editor) from file <code>data/gamestatemain.castle-user-interface</code>.
</ul>

<p>We will edit this state (code and design) in the following steps.

<?php echo $toc->html_section(); ?>

<ol>
  <li>
    <p>Download the image <a href="https://raw.githubusercontent.com/castle-engine/castle-engine/master/examples/user_interface/state_events/data/mountains_background.png">mountains_background.png</a> and put it inside your project's <code>data</code> subdirectory. You can right-click in the editor <i>"Files"</i> panel to easily open your file manager inside the current project. Then just copy the file into the <code>data</code> subdirectory.

    <p>After copying the file, you can see it inside the editor. If you select it, editor will show a preview in the bottom-right corner.

    <?php
    echo cgeImg('block', array(
      array('filename' => 'state_events_mountains_background.png', 'titlealt' => 'Mountains background'),
      array('filename' => 'state_events_open_containing.png', 'titlealt' => 'Open Containing Folder'),
      array('filename' => 'state_events_editor_mountains_background.png', 'titlealt' => 'Mountains background loaded in Castle Game Engine editor')
    ));
    ?>

    <p>If you want to experiment with graphics at this point, go ahead. The sample image we propose here is actually constructed from multiple layers in GIMP. If you know your way around, you can create a variation of this image easily. We also have alternative "industrial" background ready. See the <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/user_interface/state_events/data">examples/user_interface/state_events/data</a> directory.

  <li>
    <p>Double-click on the <code>gamestatemain.castle-user-interface</code> file in the <code>data</code> subdirectory in the editor. It will open the user interface design, where we'll add new controls.

    <?php
    echo cgeImg('block', array(
      array('filename' => 'state_events_empty_state.png', 'titlealt' => 'Empty user interface design'),
    ));
    ?>

  <li>
    <p>Right-click on the <code>Group1</code> (the "root" component of your design) to select it and show a context menu. Then choose <i>Add User Interface -&gt; Image (TCastleImageControl)</i> from the context menu that appears.

    <?php
    echo cgeImg('block', array(
      array('filename' => 'state_events_new_image.png', 'titlealt' => 'New Image (TCastleImageControl)'),
    ));
    ?>

  <li>
    <p>Load the image by editing the URL property. Click on the small button with 3 dots (<code>...</code>) on the right side of <code>URL</code> property to invoke a standard "Open File" dialog box where you should select your image.

    <p>Note that once you confirm, the <code>URL</code> will change to something like <code>castle-data:/mountains_background.png</code>. The design saves the file location <a href="https://castle-engine.io/manual_data_directory.php">relative to a special "data" directory</a>. In a typical game, you will want to reference all your data files like this. The special <code>data</code> directory will be always properly packaged and available in your application.

    <?php
    echo cgeImg('block', array(
      array('filename' => 'state_events_image_url1.png', 'titlealt' => 'Set Image URL'),
      array('filename' => 'state_events_image_url2.png', 'titlealt' => 'Set Image URL'),
    ));
    ?>

  <li>
    <p>Set the new image <i>name</i> to <code>ImageBackground</code>.

    <p>You can adjust the component name by editing the <code>Name</code> in the inspector on the right. You can alternatively click in the hierarchy on the left, or press F2, to edit the name inside the hierarchy panel.

    <p>We advise to set useful <i>names</i> for all new components, to easier recognize the components when designing. The <code>Name</code> can also be later used to find this component from code (you will see an example of it later).

    <?php
    echo cgeImg('block', array(
      array('filename' => 'state_events_name1.png', 'titlealt' => 'Editing Name property'),
      array('filename' => 'state_events_name2.png', 'titlealt' => 'Editing Name property in the hierarchy'),
    ));
    ?>

  <li>
    <p>The image is very small at the beginning. The new project by default uses <i>UI scaling</i> that simulates window size of 1600x900, while our image has size 272 x 160. By default <code>TCastleImageControl</code> follows the image size.

    <p>Set on the new <code>TCastleImageControl</code> property <code>Stretch</code> to <code>true</code> (allow to resize the <code>TCastleImageControl</code> freely).

      <!--li><p><code>ProportionalScale</code> to <code>psEnclose</code> (the displayed image keep aspect ratio of the original)-->

    <p><i>Test</i> that you can move and resize the image freely now (by dragging using the left mouse button), test making the image larger. We will adjust the position and size more precisely in the next step, for now just test that you could play with it manually. Note that you could set <code>ProportionalScale</code> to <code>psEnclose</code> to always keep aspect ratio of the original image.

    <?php
    echo cgeImg('block', array(
      array('filename' => 'state_events_image_resize.png', 'titlealt' => 'Resize the image'),
    ));
    ?>

  <li>
    <p>Switch to the <i>Layout</i> tab and set image <code>Width</code> to <code>1600</code> and <code>Height</code> to <code>900</code>. This will make the image fit perfectly inside the game window.

    <p><i>Explanation:</i> The project uses <i>UI (user interface) scaling</i> to 1600x900 by default, so it is completely valid to just set sizes and positions to any hardcoded values. They will be adjusted to follow the actual window size correctly. You can take a look at <a href="manual_castle_settings.php">data/CastleSettings.xml</a> file &mdash; it allows to adjust how UI scaling works.

    <p><i>Alternative:</i> We can get <i>exactly</i> the same behavior by setting <code>WidthFraction</code> to <code>1.0</code>, <code>HeightFraction</code> to <code>1.0</code>, and <code>ProportionalScale</code> to <code>psFit</code>. This will also make the image keep nicely within the window, and automatically follows whatever reference window size is used by the UI scaling.

    <?php
    echo cgeImg('block', array(
      array('filename' => 'state_events_mountains_resize.png', 'titlealt' => 'Set the image Width and Height'),
    ));
    ?>

  <li>
    <p>To make the image stay at the window center, anchor it to the middle of the window (both horizontall and vertically):

    <ul>
      <li><p>go to the <i>Layout</i> tab and click the middle button in the 3x3 buttons grid. This sets the anchor to the middle.

      <li><p>Click the <i>"Move to the anchor"</i> button to change image position <i>right now</i> to be at the center.
    </ul>

    <p>When done, resize the game window (by dragging the "splitters", i.e. bars between the game window and hierarchy (on the left) or inspector (on the right)). Notice how image always stays within the window, with the image center in the window center.

    <?php
    echo cgeImg('block', array(
      array('filename' => 'state_events_anchor1.png', 'titlealt' => 'Adjust background anchor'),
      array('filename' => 'state_events_anchor2.png', 'titlealt' => 'Adjust background anchor - Move to anchor'),
      array('filename' => 'state_events_resized1.png', 'titlealt' => 'Testing background anchor by resizing window'),
      array('filename' => 'state_events_resized2.png', 'titlealt' => 'Testing background anchor by resizing window'),
    ));
    ?>

  <li>
    <p>The background image is a <i>pixel-art</i>. It has low resolution, and if you make it larger (as we did) &mdash; it is better to scale it <i>without</i> smoothing the colors, to keep the result sharp.

    <p>To do this, set <code>SmoothScaling</code> property of the image to <code>false</code>.

    <?php
    echo cgeImg('block', array(
      array('filename' => 'state_events_smooth_scaling.png', 'titlealt' => 'Changing image SmoothScaling'),
    ));
    ?>

  <li>
    <p>As a final touch, drag the <code>LabelFps</code> in the hierarchy on the left to be <i>below</i> the newly added <code>ImageControl1</code>. This will make the <code>LabelFps</code> displayed <i>in front</i> of the background image.

    <p>This matters when game window aspect ratio is close to 1600x900, the yellow text <i>"FPS: xxx"</i> should then be displayed <i>in front</i>, not hidden <i>behind</i> the background image. The code will update this label to display <i>frames per second</i> when you run the game. This is a basic metric of the performance of your game.

    <?php
    echo cgeImg('block', array(
      array('filename' => 'state_events_label_front.png', 'titlealt' => 'Moved the LabelFps to the front'),
    ));
    ?>
</ol>

<?php echo $toc->html_section(); ?>

<ol>
  <li>
    <p>Download the player (plane) image from <a href="https://raw.githubusercontent.com/castle-engine/castle-engine/master/examples/user_interface/state_events/data/biplane.png">biplane.png</a>. Just as before, add it to your project's <code>data</code> subdirectory.

    <?php
    echo cgeImg('block', array(
      array('filename' => 'state_events_biplane.png', 'titlealt' => 'Plane image'),
      array('filename' => 'state_events_biplane_1.png', 'titlealt' => 'Plane image loaded in Castle Game Engine editor'),
    ));
    ?>

  <li>
    <p>Add a new <code>TCastleImageControl</code> as a child of <code>ImageBackground</code>.

    <p>If you made a mistake and placed it under some other parent (like <code>Group1</code>) then simply drag it (in the hierarchy tree) to be a child of <code>ImageBackground</code>. Just drag the new control over the right side of the <code>ImageBackground</code> in the hierarchy (it will show a right-arrow &mdash; indicating that you will drag the component to be a child of <code>ImageBackground</code>).

    <p>This relationship means that <code>ImagePlayer</code> position is relative to parent <code>ImageBackground</code> position. So the player image will keep at the same place of the background, regardless of how do you position/resize the background.

    <?php
    echo cgeImg('block', array(
      array('filename' => 'state_events_biplane_add_child.png', 'titlealt' => 'Add TCastleImageControl as a child of ImageBackground'),
      array('filename' => 'state_events_biplane_add_child_2.png', 'titlealt' => 'Added TCastleImageControl as a child of ImageBackground'),
    ));
    ?>

  <li>
    <p>Adjust new image control:

    <ul>
      <!--li>
        <p>Place it behind the <code>LabelFps</code> but in front of our background image <code>ImageControl1</code>.
      -->

      <li>
        <p>Set the new image <code>Name</code> to <code>ImagePlayer</code>.

      <li>
        <p>Set it's <code>URL</code> to point to the plane image.

      <li>
        <p>The plane image is quite large. Set <code>Stretch</code> to <code>true</code>, <code>ProportionalScale</code> to <code>psEnclose</code>, and move and resize it manually to a nice position and size.
    </ul>

    <?php
    echo cgeImg('block', array(
      array('filename' => 'state_events_biplane_2_added.png', 'titlealt' => 'Adding plane image'),
      array('filename' => 'state_events_biplane_3_set_url.png', 'titlealt' => 'Setting the plane image URL'),
      array('filename' => 'state_events_biplane_4_resized.png', 'titlealt' => 'Resizing the plane image'),
    ));
    ?>

<?php /* // we do not want this, we want ground to be level 0, for Update demo later
  <li>
    <p>To behave nicer when the game window is resized, set the anchor of the new player image to be center too. Click on the middle button in the 3x3 grid. Do not click on the <i>"Move to the anchor"</i>, there's no need. When you resize the game window now, the plane will keep at the same position relative to the background.

    < ?php
    echo cgeImg('block', array(
      array('filename' => 'state_events_biplane_anchor.png', 'titlealt' => 'Setting anchor of the plane image'),
    ));
    ? >
*/ ?>

  <li>
    <p>Remember to save your design! Press Ctrl + S (menu item <i>Design -&gt; Save</i>).
</ol>

<p>So far we didn't write any code, we just modified the file <code>data/gamestatemain.castle-user-interface</code>. You can run the application to see that it displays 2 images, in whatever place you put them.

<?php echo $toc->html_section(); ?>

<p>Now we will write some Pascal code. You can use any Pascal editor you like &mdash; by default we use Lazarus, but you can configure it in the editor <i>Preferences</i>.

<p>Look at our <a href="https://castle-engine.io/modern_pascal_introduction.html">Modern Object Pascal Introduction for Programmers</a> to learn more about Pascal, the programming language we use.

<p>To access (from code) the components you have designed, you need to manually declare and initialize their fields. We will manually add and initialize field <code>ImagePlayer</code>, as we want to modify its properties by Pascal code. This process will be automated in the future.

<ol>
  <li>
    <p>Use our editor menu item <i>Code -&gt; Open Project in Code Editor</i>
    to make sure that Lazarus has loaded the appropriate project.

    <p>Or just open Lazarus yourself, and use Lazarus menu item <i>Project -&gt; Open Project</i>
    and choose the <code>xxx_standalone.lpi</code> in the created project directory.

  <li>
    <p>Double-click on the <code>code/gamestatemain.pas</code> unit to open it in your Pascal code editor.

  <li>
    <p>Find the <code>TStateMain</code> class declaration and add a field <code>ImagePlayer: TCastleImageControl;</code>
    near the comment <code>{ Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }</code>.

    <p>So it looks like this:

<?php echo pascal_highlight(
'type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
    LabelFps: TCastleLabel;
    ImagePlayer: TCastleImageControl; // NEW LINE WE ADDED
    ...'); ?>

  <li>
    <p>Find the <code>TStateMain.Start</code> method implementation and add there code to initialize the
    <code>ImagePlayer</code> by <code>ImagePlayer := DesignedComponent('ImagePlayer') as TCastleImageControl;</code>.
    The end result should look like this:

<?php echo pascal_highlight(
'procedure TStateMain.Start;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent(\'LabelFps\') as TCastleLabel;
  ImagePlayer := DesignedComponent(\'ImagePlayer\') as TCastleImageControl;
end;'); ?>

</ol>

<p>Pascal code within <code>TStateMain</code> can now use the field <code>ImagePlayer</code>
the change the properties of the designed image.
We will use it in the following sections, to change the player position and color.

<?php echo $toc->html_section(); ?>

<p>The state has an <code>Update</code> method that is continuously called by the engine.
You should use it to update the state of your game as time passes.
In this section, we will make the plane fall down by a simple gravity, by moving the plane down
each time the <code>Update</code> method is called.

<ol>
  <li>
    <p>Add unit <code>Math</code> to the uses clause.

    <p>You can extend the <i>uses clause</i>
    of the <code>interface</code> or the <code>implementation</code> of the <code>GameStateMain</code> unit.
    It doesn't matter in this simple example,
    but it is easier to extend the <code>interface</code> section (in case you will need to use some
    type in the interface). So extend the uses clause in the interface, so it looks like this:

<?php echo pascal_highlight(
'unit GameStateMain;

interface

uses Classes, Math,
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleVectors;'); ?>

  <li>
    <p>Find the <code>TStateMain.Update</code> method implementation and change it into this:

<?php echo pascal_highlight(
'procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var
  PlayerPosition: TVector2;
begin
  inherited;
  LabelFps.Caption := \'FPS: \' + Container.Fps.ToString;

  { update player position to fall down }
  PlayerPosition := ImagePlayer.AnchorDelta;
  PlayerPosition.Y := Max(PlayerPosition.Y - SecondsPassed * 400, 0);
  ImagePlayer.AnchorDelta := PlayerPosition;
end;'); ?>

    <p>We use the <code>SecondsPassed</code> parameter to know how much time has passed
    since the last frame. You should scale all your movement by it, to adjust
    to any computer speed. For example, to move by 100 pixels per second,
    we would increase our position by <code>SecondsPassed * 100.0</code>.

    <p>We use the <code>TVector2</code> in this code, which is a 2D vector, that is: just 2 floating-point
    fields <code>X</code> and <code>Y</code> (of standard Pascal type <code>Single</code>).
    We modify the <code>Y</code> to make the plane fall down, and use <code>Max</code> (from standard <code>Math</code> unit)
    to prevent it from falling too much (below the game window).

    <p>We get and set the <a href="https://castle-engine.io/apidoc-unstable/html/CastleUIControls.TCastleUserInterface.html#AnchorDelta">ImagePlayer.AnchorDelta</a>
    which changes the image position. The anchors are relative to the parent
    (<code>ImageBackground</code>) and, since the image is anchored by default to the left-bottom of the parent,
    the anchor value (0,0) means that the left-bottom corner of <code>ImagePlayer</code> matches
    the left-bottom corner of <code>ImageBackground</code>. This is what we want.
</ol>

<p>Run the application now to see that the plane falls down.
Note that this is a rather naive approach to implement gravity
&mdash; for a realistic gravity you should rather use <a href="manual_physics.php">physics engine</a>.
But it is enough for this demo, and it shows you how to do <i>anything</i>
that needs to be done (or tested) <i>"all the time when the game is running"</i>.

<?php echo $toc->html_section(); ?>

<p>The react to one-time key press, use the <code>TStateMain.Press</code> method.
You can also check which keys are pressed inside the <code>TStateMain.Update</code> method,
to update movement constantly. Examples below shows both ways.

<ol>
  <li>
    <p>Extend the <code>TStateMain.Update</code> method implementation into this:

<?php echo pascal_highlight(
'procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
const
  MoveSpeed = 800;
var
  PlayerPosition: TVector2;
begin
  inherited;
  LabelFps.Caption := \'FPS: \' + Container.Fps.ToString;

  PlayerPosition := ImagePlayer.AnchorDelta;

  // NEW CODE WE ADD:
  if Container.Pressed[keyArrowLeft] then
    PlayerPosition := PlayerPosition + Vector2(-MoveSpeed * SecondsPassed, 0);
  if Ctontainer.Pressed[keyArrowRight] then
    PlayerPosition := PlayerPosition + Vector2( MoveSpeed * SecondsPassed, 0);
  if Container.Pressed[keyArrowDown] then
    PlayerPosition := PlayerPosition + Vector2(0, -MoveSpeed * SecondsPassed);
  if Container.Pressed[keyArrowUp] then
    PlayerPosition := PlayerPosition + Vector2(0,  MoveSpeed * SecondsPassed);

  { update player position to fall down }
  PlayerPosition.Y := Max(PlayerPosition.Y - SecondsPassed * 400, 0);
  ImagePlayer.AnchorDelta := PlayerPosition;
end;'); ?>

    <p>The new code looks whether user has pressed one of the arrow keys
    by <code>if Container.Pressed[keyArrowXxx] then</code>.
    If yes, we modify the <code>PlayerPosition</code> variable accordingly.

    <p>Note that we could also modify directly <code>ImagePlayer.AnchorDelta</code>,
    like <code>ImagePlayer.AnchorDelta := ImagePlayer.AnchorDelta + Vector2(...);</code> .
    This would also work perfectly. But since we already had a variable <code>PlayerPosition</code>,
    it seemed even better to use it, as it has a self-explanatory name.

    <p>Just as with gravity, we scale all the movement by <code>SecondsPassed</code>.
    This way the movement will be equally fast, regardless of whether the game runs
    at 60 FPS (<i>frames per second</i>) or slower or faster.
    This also means that <code>MoveSpeed</code> constant defines a <i>"movement per 1 second"</i>.

    <p><i>You can now move the plane by arrow keys!</i>

  <li>
    <p>To handle a <i>key press</i> find the <code>TStateMain.Press</code> method implementation
    and change it into this:

<?php echo pascal_highlight(
'function TStateMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  // NEW CODE WE ADD:
  if Event.IsKey(keySpace) then
  begin
    ImagePlayer.Color := Vector4(Random, Random, Random, 1);
    Exit(true); // event was handled
  end;
end;'); ?>

    <p>The <code>Event.IsKey(keySpace)</code> checks whether this is a press of the <code>space</code>
    key.

    <p>As a demo, we modify the <a href="https://castle-engine.io/apidoc-unstable/html/CastleControls.TCastleImageControl.html#Color">ImagePlayer.Color</a>,
    which is an RGBA value multiplied by the original image color.
    This allows to easily "tint" the image, e.g. setting it to <code>Vector4(0.5, 0.5, 1, 1)</code>
    means that <i>red</i> and <i>green</i> color components are darker (multiplied by 0.5)
    and thus the image appears more <i>blueish</i>.
    In this case we use random values for the all <i>red</i>, <i>green</i> and <i>blue</i>
    channels (the standard <code>Random</code> returns a random float in the 0..1 range),
    just for test. So each time you press the <i>space</i> key, the player image will look
    a bit different.

    <p>Note that we keep the 4th <code>ImagePlayer.Color</code> component (alpha) at 1.0.
    Lower <i>alpha</i> would make image partially-transparent.

    <p>Note that you can experiment with changing the <code>ImagePlayer.Color</code> effects
    also visually, in the editor.
</ol>

<p>Run the application now to test the key handling.

<p>When to use <code>Press</code> to handle a single key press,
and when to use <code>Update</code> to watch the key state? This depends on the need.
If the action caused by the key is a single, instant, uninterruptible operation &mdash; then do it in <code>Press</code>.
If the key causes an effect that is somehow applied more and more over time  &mdash; then watch the key and apply it in <code>Update</code>.

<?php echo $toc->html_section(); ?>

<p>The mouse press is also handled in the <code>Press</code> method. In general,
<code>Press</code> method receives key press, or a mouse press, or a mouse wheel use
(see the documentation of <a href="https://castle-engine.io/apidoc-unstable/html/CastleKeysMouse.TInputPressRelease.html">TInputPressRelease</a>).

<p>Everywhere in the engine, the mouse events also work on touch devices, when they correspond to
the movement / touches of the fingers. When you use a touch device, then we only report <i>left</i> mouse button clicks
(<a href="https://castle-engine.io/apidoc-unstable/html/CastleKeysMouse.TInputPressRelease.html#MouseButton">TInputPressRelease.MouseButton</a>
will be <code>buttonLeft</code>).
When you use use the actual mouse on the desktop, then we only report touches by the 1st finger (<a href="https://castle-engine.io/apidoc-unstable/html/CastleKeysMouse.TInputPressRelease.html#FingerIndex">TInputPressRelease.FingerIndex</a>
will be <code>0</code>). The example code below checks for <code>if Event.IsMouseButton(buttonLeft) then</code>
and thus it will work on both desktop (detecting mouse click) and mobile (detecting touch).

<p>Extend the <code>TStateMain.Press</code> method implementation into this:

<?php echo pascal_highlight(
'function TStateMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keySpace) then
  begin
    ImagePlayer.Color := Vector4(Random, Random, Random, 1);
    Exit(true); // event was handled
  end;

  // NEW CODE:
  if Event.IsMouseButton(buttonLeft) then
  begin
    ImagePlayer.AnchorDelta := ImagePlayer.Parent.ContainerToLocalPosition(Event.Position);
    Exit(true); // event was handled
  end;
end;'); ?>

<p>The <code>Event.Position</code> contains the mouse/touch position. It is expressed in the <i>container</i>
coordinates, which means it is not affected by UI scaling or the UI hierarchy and anchors.
It's easiest to convert it to a position relative to some UI control using the <a href="https://castle-engine.io/apidoc-unstable/html/CastleUIControls.TCastleUserInterface.html#ContainerToLocalPosition">ContainerToLocalPosition</a>
method. In this case, we use <code>ImagePlayer.Parent.ContainerToLocalPosition</code>, to use
the resulting position to set <code>ImagePlayer.AnchorDelta</code>. The <code>ImagePlayer.Parent</code>
is just another way to access <code>ImageBackground</code> in this case. We want to calculate new player
position, in the coordinates of <code>ImagePlayer</code> parent, because that's what
<code>ImagePlayer.AnchorDelta</code> expects.

<?php
castle_footer();
?>
