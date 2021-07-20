<?php
require_once 'castle_engine_functions.php';
manual_header('Designing user interface and handling events (press, update) within the state', array(
  'social_share_image' => 'basic_state_events_screen.png',
));

$toc = new TableOfContents(
  array(
    new TocItem('Overview'),
    new TocItem('Create empty project'),
    new TocItem('Add the background image (mountains)'),
    new TocItem('Add the player image (plane)'),
    new TocItem('Move the player in the Update method'),
    new TocItem('React to a key press'),
    new TocItem('React to a mouse click or touch'),
    new TocItem('Using multiple states'),
    new TocItem('Examples'),
  )
);

echo castle_thumbs(array(
  array('filename' => 'basic_state_events_screen.png', 'titlealt' => 'Plane flying on the mountain background - game'),
  array('filename' => 'cge_editor_biplane_4_resized.png', 'titlealt' => 'Plane flying on the mountain background - design'),
//   array('filename' => 'basketball_2d_game_lazarus.png', 'titlealt' => 'Simple game where you move an image on the screen - you will learn how to do it very soon!'),
//   array('filename' => 'basketball_2d_game_window.png', 'titlealt' => 'The simplest basketball game ever created'),
//   array('filename' => 'castle_spine_screen_9.png', 'titlealt' => '2D game with animations done in Spine'),
//   array('filename' => 'fly_over_river_screen_26.png', 'titlealt' => 'Simple &quot;River Ride&quot; clone done in 1-hour gamejam'),
 ));
?>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p><i>State</i> in <i>Castle Game Engine</i> is a class that descends from <a href="https://castle-engine.io/apidoc-unstable/html/CastleUIState.TUIState.html">TUIState</a> and manages <i>what you display on the screen and how you react to basic events (user input, updates)</i>.

<p>While it is not required to put everything in some <i>state</i>, we highly advise to organize your application into a number of <i>states</i>. They organize your application into a number of smaller pieces in a natural way. If you have used Lazarus LCL or Delphi VCL for visual designing previosly, you will recognize that our <code>TUIState</code> is a similar concept to <code>TForm</code> from LCL and VCL.

<p>In this chapter we will learn how to use the basic state features. We will create a simple toy that displays some images and allows to move them. You can follow this chapter and do it yourself, or you can look at the ready version in <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/user_interface/basic_state_events">examples/user_interface/basic_state_events</a>.

<?php echo $toc->html_section(); ?>

<p>Start by creating a new project using the "Empty" template.

<?php
echo castle_thumbs(array(
  array('filename' => 'cge_editor_new_project.png', 'titlealt' => 'Castle Game Engine Editor New Project'),
), 'auto', 'left');
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
    <p>Download the image <a href="https://raw.githubusercontent.com/castle-engine/castle-engine/master/examples/user_interface/basic_state_events/data/mountains_background.png">mountains_background.png</a> and put it inside your project's <code>data</code> subdirectory. You can right-click in the editor <i>"Files"</i> panel to easily open your file manager inside the current project. Then just copy the file into the <code>data</code> subdirectory.

    <p>After copying the file, you can see it inside the editor. If you select it, editor will show a preview in the bottom-right corner.

    <?php
    echo castle_thumbs(array(
      array('filename' => 'mountains_background.png', 'titlealt' => 'Mountains background'),
      array('filename' => 'cge_editor_open_containing.png', 'titlealt' => 'Open Containing Folder'),
      array('filename' => 'cge_editor_mountains_background.png', 'titlealt' => 'Mountains background loaded in Castle Game Engine editor')
    ), 'auto', 'left');
    ?>

    <p>If you want to experiment with graphics at this point, go ahead. The sample image we propose here is actually constructed from multiple layers in GIMP. If you know your way around, you can create a variation of this image easily. We also have alternative "industrial" demo. See the <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/user_interface/basic_state_events/data">examples/user_interface/basic_state_events/data</a> directory.

  <li>
    <p>Double-click on the <code>gamestatemain.castle-user-interface</code> file in the <code>data</code> subdirectory in the editor. It will open the user interface design, where we'll add new controls.

    <?php
    echo castle_thumbs(array(
      array('filename' => 'cge_editor_empty_state.png', 'titlealt' => 'Empty user interface design'),
    ), 'auto', 'left');
    ?>

  <li>
    <p>Right-click on the <code>Group1</code> (the "root" component of your design) to select it and show a context menu. Then choose <i>Add User Interface -&gt; Image (TCastleImageControl)</i> from the context menu that appears.

    <?php
    echo castle_thumbs(array(
      array('filename' => 'cge_editor_new_image.png', 'titlealt' => 'New Image (TCastleImageControl)'),
    ), 'auto', 'left');
    ?>

  <li>
    <p>Set the new image <i>name</i> to <code>ImageBackground</code>.

    <p>You can adjust the component name by editing the <code>Name</code> in the inspector on the right. You can alternatively click in the hierarchy on the left, or press F2, to edit the name inside the hierarchy panel.

    <p>We advise to set useful <i>names</i> for all new components, to easier recognize the components when designing. The <code>Name</code> can also be later used to find this component from code (you will see an example of it later).

    TODO update screens to show rename at this point

    <?php
    echo castle_thumbs(array(
      array('filename' => 'cge_editor_name1.png', 'titlealt' => 'Editing Name property'),
      array('filename' => 'cge_editor_name2.png', 'titlealt' => 'Editing Name property in the hierarchy'),
    ), 'auto', 'left');
    ?>

  <li>
    <p>Load the image by editing the URL property. Just click on the 3 dots button on the right side of <code>URL</code> property to invoke a standard "Open File" dialog box where you should select your image.

    <p>Note that once you confirm, the <code>URL</code> will change to something like <code>castle-data:/mountains_background.png</code>. The design saves the file location <a href="https://castle-engine.io/manual_data_directory.php">relative to a special "data" directory</a>. In a typical game, you will want to reference all your data files like this. The special <code>data</code> directory will be always properly packaged and available in your application.

    <?php
    echo castle_thumbs(array(
      array('filename' => 'cge_editor_image_url1.png', 'titlealt' => 'Set Image URL'),
      array('filename' => 'cge_editor_image_url2.png', 'titlealt' => 'Set Image URL'),
    ), 'auto', 'left');
    ?>

  <li>
    <p>The image is very small at the beginning. The new project by default uses <i>UI scaling</i> that simulates window size of 1600x900, while our image has size 272 x 160. By default <code>TCastleImageControl</code> follows the image size.

    <p>Set on the new <code>TCastleImageControl</code> property <code>Stretch</code> to <code>true</code> (allow to resize the <code>TCastleImageControl</code> freely).

      <!--li><p><code>ProportionalScale</code> to <code>psEnclose</code> (the displayed image keep aspect ratio of the original)-->

    <p><i>Test</i> that you can move and resize the image freely now (by dragging using the left mouse button), test making the image larger. We will adjust the position and size more precisely in the next step, for now just test that you could play with it manually. Note that you could set <code>ProportionalScale</code> to <code>psEnclose</code> to always keep aspect ratio of the original image.

    <?php
    echo castle_thumbs(array(
      array('filename' => 'cge_editor_image_resize.png', 'titlealt' => 'Resize the image'),
    ), 'auto', 'left');
    ?>

  <li>
    <p>Switch to the <i>Layout</i> tab and set image <code>Width</code> to <code>1600</code> and <code>Height</code> to <code>900</code>. This will make the image fit perfectly inside the game window.

    <p><i>Explanation:</i> The project uses <i>UI (user interface) scaling</i> to 1600x900 by default, so it is completely valid to just set sizes and positions to any hardcoded values. They will be adjusted to follow the actual window size correctly. You can take a look at <a href="manual_castle_settings.php">data/CastleSettings.xml</a> file &mdash; it allows to adjust how UI scaling works.

    TODO screen with

    <?php
    echo castle_thumbs(array(
      array('filename' => 'cge_editor_mountains_resize.png', 'titlealt' => 'Set the image Width and Height'),
    ), 'auto', 'left');
    ?>

  <li>
    <p>To make the image stay at the window center, anchor it to the middle of the window (both horizontall and vertically):

    <ul>
      <li><p>go to the <i>Layout</i> tab and click the middle button in the 3x3 buttons grid. This sets the anchor to the middle.

      <li><p>Click the <i>"Move to the anchor"</i> button to change image position <i>right now</i> to be at the center.
    </ul>

    <p>When done, resize the game window (by dragging the "splitters", i.e. bars between the game window and hierarchy (on the left) or inspector (on the right)). Notice how image always stays within the window, with the image center in the window center.

    TODO update screens, the image is inside now

    <?php
    echo castle_thumbs(array(
      array('filename' => 'cge_editor_anchor1.png', 'titlealt' => 'Adjust background anchor'),
      array('filename' => 'cge_editor_anchor2.png', 'titlealt' => 'Adjust background anchor - Move to anchor'),
      array('filename' => 'cge_window_resized1.png', 'titlealt' => 'Testing background anchor by resizing window'),
      array('filename' => 'cge_window_resized2.png', 'titlealt' => 'Testing background anchor by resizing window'),
    ), 'auto', 'left');
    ?>

  <li>
    <p>The background image is a <i>pixel-art</i>. It has low resolution, and if you make it larger (as we did) &mdash; it is better to scale it <i>without</i> smoothing the colors, to keep the result sharp.

    <p>To do this, just set <code>SmoothScaling</code> property of the image to <code>false</code>.

    <?php
    echo castle_thumbs(array(
      array('filename' => 'state_events_smooth_scaling.png', 'titlealt' => 'Changing image SmoothScaling'),
    ), 'auto', 'left');
    ?>

  <li>
    <p>As a final touch, drag the <code>LabelFps</code> in the hierarchy on the left to be <i>below</i> the newly added <code>ImageControl1</code>. This will make the <code>LabelFps</code> displayed <i>in front</i> of the background image.

    <?php
    echo castle_thumbs(array(
      array('filename' => 'cge_editor_label_front.png', 'titlealt' => 'Moved the LabelFps to the front'),
    ), 'auto', 'left');
    ?>
</ol>

<?php echo $toc->html_section(); ?>

<ol>
  <li>
    <p>Download the player (plane) image from <a href="https://github.com/castle-engine/castle-engine/blob/master/examples/user_interface/basic_state_events/data/biplane.png">biplane.png</a>. Just as before, add it to your project's <code>data</code> subdirectory.

    <?php
    echo castle_thumbs(array(
      array('filename' => 'biplane.png', 'titlealt' => 'Plane image'),
      array('filename' => 'cge_editor_biplane_1.png', 'titlealt' => 'Plane image loaded in Castle Game Engine editor'),
    ), 'auto', 'left');
    ?>

  <li>
    <p>Add a new <code>TCastleImageControl</code> as another child of <code>Group1</code>. Place it behind the <code>LabelFps</code> but in front of our background image <code>ImageControl1</code>.

    <p>Set the new image <code>Name</code> to <code>ImagePlayer</code>.

    <p>Set it's <code>URL</code> to point to the plane image.

    <p>The plane image is quite large. Set <code>Stretch</code> to <code>true</code>, <code>ProportionalScale</code> to <code>psEnclose</code>, and move and resize it manually to a nice position and size.

    <?php
    echo castle_thumbs(array(
      array('filename' => 'cge_editor_biplane_2_added.png', 'titlealt' => 'Adding plane image'),
      array('filename' => 'cge_editor_biplane_3_set_url.png', 'titlealt' => 'Setting the plane image URL'),
      array('filename' => 'cge_editor_biplane_4_resized.png', 'titlealt' => 'Resizing the plane image'),
    ), 'auto', 'left');
    ?>

  <li>
    <p>To behave nicer when the game window is resized, set the anchor of the new player image to be center too. Click on the middle button in the 3x3 grid. Do not click on the <i>"Move to the anchor"</i>, there's no need. When you resize the game window now, the plane will keep at the same position relative to the background.

    <?php
    echo castle_thumbs(array(
      array('filename' => 'cge_editor_biplane_anchor.png', 'titlealt' => 'Setting anchor of the plane image'),
    ), 'auto', 'left');
    ?>

  <li>
    <p>Remember to save your design! Press Ctrl + S (menu item <i>Design -&gt; Save</i>).
</ol>

<p>So far we didn't write any code, we just modified the file <code>data/gamestatemain.castle-user-interface</code>. You can run the application to see that it displays 2 images, in whatever place you put them.

<?php echo $toc->html_section(); ?>

<p>The state has an <code>Update</code> method that is continuously called by the engine.
You should use it to update the state of your game as time passes.
In this section, we will make the plane fall down by a simple gravity, by moving the plane down
each time the <code>Update</code> method is called.

<ol>
  <li>
    <p>Double-click on the <code>code/gamestatemain.pas</code> unit to open it in your Pascal code editor
    (Lazarus by default). You should also use once menu item <i>Code -&gt; Open Project in Code Editor</i>
    to make sure that Lazarus has loaded the appropriate project.

  <li>
    <p>Find the <code>TStateMain</code> class declaration and add a field <code>ImagePlayer: TCastleImageControl;</code>
    at the place of the comment <code>{ Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }</code>.

    <p>So it looks like this:

<?php echo pascal_highlight(
'type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
    LabelFps: TCastleLabel;
    ImagePlayer: TCastleImageControl;
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

  <li>
    <p>Add units <code>Math</code> and <code>CastleVectors</code> to the uses clause.

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
    fields `X` and `Y` (of standard Pascal type `Single`).
    We modify the `Y` to make the plane fall down, and use `Max` (from standard <code>Math</code> unit)
    to prevent it from falling too much (below the game window).
</ol>

<p>Run the application now to see that the plane falls down.
Note that this is a rather naive approach to implement gravity
&mdash; for a realistic gravity you should rather use <a href="manual_physics.php">physics engine</a>.
But it is enough for this demo, and it shows you how to do <i>anything</i>
that needs to be done (or tested) <i>"all the time when the game is running"</i>.

<?php echo $toc->html_section(); ?>

<!-- TODO test this -->

<p>The react to one-time key or mouse press, use the <code>TStateMain.Press</code> method.
You can also check which keys are pressed inside the <code>TStateMain.Update</code> method,
to update movement constantly. Examples below shows both ways.

<?php echo pascal_highlight(
'function TStateMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keySpace) then
    TODO
  if Event.IsMouseButton(mbLeft) then // this also handles touch on mobile
    TODO
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  TODO previous code, and handle some

  if Container.Pressed[keyArrowLeft] then
    X := X - SecondsPassed * 200.0;
  if Container.Pressed[keyArrowRight] then
    X := X + SecondsPassed * 200.0;
end;'); ?>

<p>Run the application now to test the key handling.

<?php echo $toc->html_section(); ?>

TODO

<?php echo $toc->html_section(); ?>

<p>You can add new states to your application using the menu item <i>Code -&gt; New Unit -&gt; Unit With State...</i>. It is equivalent to just creating a new Pascal unit that defines a new <code>TUIState</code> descendant and loads a new user interface design.

<p>At runtime, you can change from one state into another using <a href="https://castle-engine.io/apidoc-unstable/html/CastleUIState.TUIState.html#Current">TUIState.Current := StateXxx</a> or <a href="https://castle-engine.io/apidoc-unstable/html/CastleUIState.TUIState.html#Push">TUIState.Push</a> / <a href="https://castle-engine.io/apidoc-unstable/html/CastleUIState.TUIState.html#Push">TUIState.Pop</a> class methods.

<?php echo $toc->html_section(); ?>

<p>Explore the <i>"3D FPS game"</i> and <i>"2D game"</i> templates, by creating 2 new projects from these templates. Each of these templates creates 2 states, <i>"MainMenu"</i> and <i>"Play"</i>. They follow the same pattern as above:

<ul>
  <li>
    <p>Class <code>TStateMainMenu</code>, unit <code>code/statemainmenu.pas</code>, instance <code>StateMainMenu</code>, design <code>data/statemainmenu.castle-user-interface</code>.

  <li>
    <p>Class <code>TStatePlay</code>, unit <code>code/stateplay.pas</code>, instance <code>StatePlay</code>, design <code>data/stateplay.castle-user-interface</code>.
</ul>

<p>We have multiple examples showing more complicates states:

<p>Platformer demo in <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/platformer">examples/platformer/</a> has states for:

<ul>
  <li>main menu,
  <li>options (with volume configuration),
  <li>pause,
  <li>credits,
  <li>game over,
  <li>and of course the actual game.
</ul>

<p>Strategy game demo in <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/tiled/strategy_game">examples/tiled/strategy_game</a> and "zombie fighter" demo in <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/user_interface/zombie_fighter">examples/user_interface/zombie_fighter</a> also feature multiple states.

<?php
manual_footer();
?>
