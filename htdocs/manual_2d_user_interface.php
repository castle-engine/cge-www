<?php
require_once 'castle_engine_functions.php';
manual_header('User interface, standard controls, viewports', array(
  'social_share_image' => 'zombie_fighter_0.png',
));

$toc = new TableOfContents(
  array(
    new TocItem('Introduction', 'introduction'),
    new TocItem('Using the 2D controls', 'usage'),
    new TocItem('Parents and anchors', 'parents_and_anchors'),
    new TocItem('User-interface scaling (Container.UIScaling)', 'scaling'),
    new TocItem('Query sizes', 'sizes'),
    new TocItem('Adjust theme', 'theme'),
    new TocItem('Taking control of the viewport', 'viewport'),
    new TocItem('Insert animation (in a viewport) into a button', 'button_with_viewport', 1),
    new TocItem('Wrapping it up (in a custom TCastleUserInterface descendant)', 'wrapping'),
    new TocItem('User-interface state (TUIState)', 'ui_state'),
  )
);
?>

<p>Using our engine you can create nice user-interface for your applications.
The look and behavior of everything is very customizable,
as you often want a special-looking UI in your games.
Our user-interface is rendered inside a container like
<?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?> or
<?php api_link('TCastleControlBase', 'CastleControl.TCastleControlBase.html'); ?>,
and of course works without any modifications on all the platforms
we support &mdash; desktop, mobile...

<!-- It's appearance and behavior is very customizable,
as you often want a custom-looking UI in your games. -->

<p><b>A complete program using the concepts shown here is in the engine
examples</b>, in the <code>examples/user_interface/zombie_fighter/</code>
directory.</p>

<?php
echo castle_thumbs(array(
  array('filename' => 'zombie_fighter_0.png', 'titlealt' => 'Dialog box composed from simple UI elements'),
  //array('filename' => 'two_viewports.png', 'titlealt' => 'Two viewports'),
  //array('filename' => 'view3dscene_viewports.png', 'titlealt' => 'Multiple viewports with a DOOM level in view3dscene'),
  array('filename' => 'button_with_2d_scene_manager.png', 'titlealt' => '2D animated scene inside a button'),
  //array('filename' => 'zombie_fighter_1.png', 'titlealt' => 'Multiple viewports and basic game UI'),
  array('filename' => 'zombie_fighter_2.png', 'titlealt' => 'UI dialog, in a state over the game UI'),
), 'auto', 'left');
?>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>Our game window is covered by <i>2D user-interface controls</i>.
All of the classes representing a user-interface control descend from
the <?php api_link('TCastleUserInterface', 'CastleUIControls.TCastleUserInterface.html'); ?> class.
They can render, they can handle inputs, they can perform any time-dependent task, and much more.

<p>All of the currently used controls are added to the list of
<?php api_link('TCastleWindowBase.Controls', 'CastleWindow.TCastleWindowBase.html#Controls'); ?>
 (if you use <?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?>) or
<?php api_link('TCastleControlBase.Controls', 'CastleControl.TCastleControlBase.html#Controls'); ?>
 (if you use Lazarus forms with <?php api_link('TCastleControlBase', 'CastleControl.TCastleControlBase.html'); ?>).
The controls on this list are ordered from the back to front. But usually you don't need to remember that,
as you add them using the aptly-named methods
<?php api_link('InsertFront', 'CastleUIControls.TChildrenControls.html#InsertFront'); ?> or
<?php api_link('InsertBack', 'CastleUIControls.TChildrenControls.html#InsertBack'); ?>,
indicating the visual "front" (<i>in front of other controls</i>) or "back".
The <?php api_link('TCastleUserInterface', 'CastleUIControls.TCastleUserInterface.html'); ?> can be arranged
 in hierarchy: one instance of <?php api_link('TCastleUserInterface', 'CastleUIControls.TCastleUserInterface.html'); ?>
 can be a child of another, which makes it visually "contained" inside the parent,
and moved along with parent.

<p>Note that you can also render and handle inputs using the
<?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?> or
<?php api_link('TCastleControlBase', 'CastleControl.TCastleControlBase.html'); ?> callbacks,
like <?php api_link('Window.OnRender', 'CastleWindow.TCastleWindowBase.html#OnRender'); ?>,
<?php api_link('Window.OnPress', 'CastleWindow.TCastleWindowBase.html#OnPress'); ?> and
<?php api_link('Window.OnUpdate', 'CastleWindow.TCastleWindowBase.html#OnUpdate'); ?>.
But usually most of your rendering and input handling should be done inside some
<?php api_link('TCastleUserInterface', 'CastleUIControls.TCastleUserInterface.html'); ?>
 descendant, most importantly inside
 <?php api_link('TIUState', 'CastleUIState.TUIState.html'); ?> descendant
 (<code>TUIState</code> descends from <code>TCastleUserInterface</code>).

<p>Note that <?php api_link('TCastleViewport', 'CastleViewport.TCastleViewport.html'); ?>
 is also a descendant of <?php api_link('TCastleUserInterface', 'CastleUIControls.TCastleUserInterface.html'); ?>.
 So the organization follows what you see:
 inside a 2D window, you create a 2D viewport, that allows to view the 3D world inside.
So, almost everything is "inside" some <?php api_link('TCastleUserInterface', 'CastleUIControls.TCastleUserInterface.html'); ?>
 in our engine.<!--, and we advice you to follow this approach when creating your own games.-->

<p>While <?php api_link('TCastleUserInterface', 'CastleUIControls.TCastleUserInterface.html'); ?> is basically
 a way to render <i>anything</i>, in this chapter we will focus on the most traditional
use of this class: to create simple 2D user interfaces. Our engine includes a large number of
<?php api_link('TCastleUserInterface', 'CastleUIControls.TCastleUserInterface.html'); ?> descendants for this. The most
often used controls are:

<ul>
  <li><?php api_link('TCastleLabel', 'CastleControls.TCastleLabel.html'); ?> - Label with text. As with all our text controls, the font family and size is customizable (see <a href="manual_text.php">chapter about text</a>). May have a frame. May be multiline. May contain some <?php api_link('HTML tags', 'CastleControls.TCastleLabel.html#Html'); ?>.</li>

  <li><?php api_link('TCastleButton', 'CastleControls.TCastleButton.html'); ?> - Clickable button. The look is highly configurable with custom images and tint colors. May contain an icon inside (actually, by inserting other UI controls as children, it may contain <i>anything</i> inside). The size may be automatically adjusted to the inside caption (and icon), or may be explicitly given.</li>

  <li><?php api_link('TCastleImageControl', 'CastleControls.TCastleImageControl.html'); ?> - Image. May stretch the content, or adjust to the content size. Image may be partially transparent, with blending or alpha-testing. Image can be rotated or clipped by an arbitrary line. Image may be "tinted" (multiplied) by given color. Underneath, the image is a full-featured <?php api_link('TCastleImage', 'CastleImages.TCastleImage.html'); ?>, so you can process it in a myriad of ways.<!--, and load from a myriad sources (e.g. you can load here a dynamically generated <a href="">screenshot, not only an image file).--></li>

  <li><?php api_link('TCastleRectangleControl', 'CastleControls.TCastleRectangleControl.html'); ?> - Rectangle filled with solid color. Good for a general-purpose background. The color may be partially transparent, in which case the content underneath is still visible.</li>

  <li><?php api_link('TCastleUserInterface', 'CastleUIControls.TCastleUserInterface.html'); ?> itself is also very useful - General-purpose container (or ancestor) for other UI controls. Does not do or show anything by itself, but it has a configurable position and size.</li>
</ul>

<p>These are just the basic UI classes. Find the <code>TCastleUserInterface</code> in our <?php api_link('class hierarchy', 'ClassHierarchy.html'); ?> diagram and look at all it's descendants to discover more:)

<?php echo $toc->html_section(); ?>

<p>You simply create an instance of any class you like, and add it as a children
of <?php api_link('Window.Controls', 'CastleWindow.TCastleWindowBase.html#Controls'); ?>,
 <?php api_link('CastleControl.Controls', 'CastleControl.TCastleControlBase.html#Controls'); ?>
 or another (parent) <?php api_link('TCastleUserInterface', 'CastleUIControls.TCastleUserInterface.html'); ?>.
 Note that <?php api_link('TCastleUserInterface', 'CastleUIControls.TCastleUserInterface.html'); ?> is a descendant
 of the standard <code>TComponent</code> property, so you can use the standard "ownership"
mechanism to take care of freeing the UI control. In simple cases, you can make the <code>Application</code>
or <code>Window</code> a parent of your UI control.

<p>An example below shows a simple button and a label:

<?php echo pascal_highlight_file('code-samples/standard_2d_ui.lpr'); ?>

<p>Remember that at any time, you can add and remove the controls.
You can also make a control temporarily "not existing"
(not visible, not handling inputs and so on &mdash;
just like it would not be present on the controls list at all) by flipping it's
<?php api_link('Exists', 'CastleUIControls.TCastleUserInterface.html#Exists'); ?>
 property.

<p>Here's the previous example expanded, showing how to handle button click,
to toggle the visibility of a rectangle:

<?php echo pascal_highlight_file('code-samples/standard_2d_ui_toggle_exists.lpr'); ?>

<?php echo $toc->html_section(); ?>

<?php
echo castle_thumbs(array(
  array('filename' => 'zombie_fighter_0.png', 'titlealt' => 'Dialog box composed from simple UI elements'),
));
?>

<p>Every UI control may have children, which are positioned relative to their parent.
The children are always drawn on top of their parent.
(Although the parent has an additional chance to draw over the children in it's
<?php api_link('RenderOverChildren', 'CastleUIControls.TCastleUserInterface.html#RenderOverChildren'); ?> method, but that's rarely used.)

<ul>
  <li><p>The <b>children receive input events (key and mouse presses) before their parent</b>.
    So the innermost children get the first chance to process an event
    (like a click), and only if they do not handle it (their
    <?php api_link('Press', 'CastleUIControls.TInputListener.html#Press'); ?> method will return <code>false</code>)
    then the event is passed to the parent. If no UI control processes a press event,
    it is passed to the
    <?php api_link('TCastleWindowBase.OnPress', 'CastleWindow.TCastleWindowBase.html#OnPress'); ?> or
    <?php api_link('TCastleControlBase.OnPress', 'CastleControl.TCastleControlBase.html#OnPress'); ?>.

    <p>Note that the <b>input events are only send to the controls under the pointer</b>
    (mouse or touch position). Although this is configurable using
    the <?php api_link('CapturesEventsAtPosition', 'CastleUIControls.TCastleUserInterface.html#CapturesEventsAtPosition'); ?>
     method, but usually it's saner to leave it at default.
    A useful trick to capture the events from the whole window is to use a
    <?php api_link('TCastleUserInterface', 'CastleUIControls.TCastleUserInterface.html'); ?>
     with
    <?php api_link('FullSize', 'CastleUIControls.TCastleUserInterface.html#FullSize'); ?>
     = <code>true</code>, this will capture mouse clicks and key presses from everything.

  <li><p><b>Any control can be a parent</b>. For example, you can insert arbitrary images and labels
    inside a <?php api_link('TCastleButton', 'CastleControls.TCastleButton.html'); ?>, to make it's content look in any way you want.
    If you want to group a couple of controls, but don't have a natural "parent" control,
    it's often a good idea to use a new instance of an
    <?php api_link('TCastleUserInterface', 'CastleUIControls.TCastleUserInterface.html'); ?>
     as a parent.

  <li><p><b>Controls are positioned relative to the parent</b>, using just the
    <?php api_link('Left', 'CastleUIControls.TCastleUserInterface.html#Left'); ?> and
    <?php api_link('Bottom', 'CastleUIControls.TCastleUserInterface.html#Bottom'); ?> properties
    by default. Remember that our engine in 2D uses a coordinate system
    where the Y grows from zero (bottom) to maximum height (at the top).
    This is contrary to a convention of various GUI libraries that designate <code>Top</code> as zero,
    and make Y grow downward. We decided to follow the convention <i>Y grows up</i>
    for a couple of reasons, mostly because it naturally matches the 3D situation too
    (in 3D, our engine also follows the convention that <i>Y grows up</i> by default;
    so in 3D you get <i>one additional dimension</i>, Z, going "outside" of the screen,
    while X and Y axes are oriented the same in 2D and 3D).

    <p>Usually, instead of assigning the positions using the
    <?php api_link('Left', 'CastleUIControls.TCastleUserInterface.html#Left'); ?> and
    <?php api_link('Bottom', 'CastleUIControls.TCastleUserInterface.html#Bottom'); ?> properties,
    it's better to <b>use <i>anchors</i></b>. Anchors specify the position of some border (or center)
    of the control, relative to some border (or center) of it's parent.
    When the parent control is resized (e.g. when user resizes the window),
    children are automatically repositioned correctly.
    This usually avoids the need to react to window size changes in callbacks like
     <?php api_link('Window.OnResize', 'CastleWindow.TCastleWindowBase.html#OnResize'); ?>
     or <?php api_link('TCastleUserInterface.Resize', 'CastleUIControls.TInputListener.html#Resize'); ?> implementations.</p>
  </li>

  <li><p>Note that <b>the parent does not clip the visibility of the children</b>.
    That is, we assume that you will set the size of children small enough to make them fit
    within the parent. If you don't, the result will be a little unintuitive: the overflowing
    contents of children will be drawn outside of the rectangle of the parent, but they will
    not receive input (like mouse clicks). For this reason, it's best to make children actually fit
    within the parent.

    <p>If you actually want to clip the children, set the
    <?php api_link('ClipChildren', 'CastleUIControls.TCastleUserInterface.html#ClipChildren'); ?> to <code>true</code>.
  </li>
</ul>

<p>With all this knowledge about parents and anchors, let's make a simple dialog box,
showing off what we learned, and something extra (note the use of <code>TCastleLabel.Html</code>
and <code>HexToColor</code> below).
Below are the contents of the <code>ApplicationInitialize</code> procedure,
you can just use this code to setup UI in the main program block
(like in the simple examples above on the same page),
or you can set it as the <code>Application.OnInitialize</code> callback following the
<a href="manual_cross_platform.php">chapter about developing cross-platform applications</a>.

<p>If in doubt, <b>take a look at the <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/user_interface/zombie_fighter">examples/user_interface/zombie_fighter/</a>
code that contains the final application we will make in this manual!</b>
It uses the <code>ApplicationInitialize</code> procedure.</p>

<?php echo pascal_highlight('uses SysUtils, CastleControls, CastleUtils, CastleFilesUtils,
  CastleColors, CastleUIControls;

var
  Rect: TCastleRectangleControl;
  InsideRect: TCastleRectangleControl;
  Image: TCastleImageControl;
  LabelStats: TCastleLabel;
  ButtonRun, ButtonFight: TCastleButton;

procedure ApplicationInitialize;
begin
  Rect := TCastleRectangleControl.Create(Application);
  Rect.Width := 400;
  Rect.Height := 500;
  Rect.Color := HexToColor(\'5f3939\'); // equivalent: Vector4(95/255, 57/255, 57/255, 1.0);
  Rect.Anchor(hpMiddle);
  Rect.Anchor(vpMiddle);
  Window.Controls.InsertFront(Rect);

  InsideRect := TCastleRectangleControl.Create(Application);
  InsideRect.Width := Rect.EffectiveWidth - 10;
  InsideRect.Height := Rect.EffectiveHeight - 10;
  InsideRect.Color := Silver;
  InsideRect.Anchor(hpMiddle);
  InsideRect.Anchor(vpMiddle);
  Rect.InsertFront(InsideRect);

  Image := TCastleImageControl.Create(Application);
  Image.URL := \'castle-data:/Female-Zombie-300px.png\';
  Image.Anchor(hpMiddle);
  Image.Anchor(vpTop, -10);
  InsideRect.InsertFront(Image);

  LabelStats := TCastleLabel.Create(Application);
  LabelStats.Color := Black;
  LabelStats.Html := true;
  { anything, just to show off the HTML :) }
  LabelStats.Caption := \'Statistics:\' + NL +
    \'Life: <font color="#ff0000">12%</font>\' + NL +
    \'Stamina: <font color="#ffff00">34%</font>\' + NL +
    \'Mana: <font color="#0000ff">56%</font>\';
  LabelStats.Anchor(hpMiddle);
  LabelStats.Anchor(vpBottom, 100);
  InsideRect.InsertFront(LabelStats);

  ButtonRun := TCastleButton.Create(Application);
  ButtonRun.Caption := \'Run\';
  ButtonRun.Anchor(hpLeft, 10);
  ButtonRun.Anchor(vpBottom, 10);
  ButtonRun.PaddingHorizontal := 40;
  InsideRect.InsertFront(ButtonRun);

  ButtonFight := TCastleButton.Create(Application);
  ButtonFight.Caption := \'Fight\';
  ButtonFight.Anchor(hpRight, -10);
  ButtonFight.Anchor(vpBottom, 10);
  ButtonFight.PaddingHorizontal := 40;
  InsideRect.InsertFront(ButtonFight);
end;'); ?>

<p>Note that <a href="manual_editor.php">we have a visual designer for UI</a>.
It is described in the separate chapter.

<?php echo $toc->html_section(); ?>

<p>We advise using the <b>user interface scaling</b> to automatically adjust all the user interface controls, depending on the window size of the final application. Underneath it scales the UI coordinates (and then renders using the final coordinates, so all rendering remains crisp), adjusting to your "target" resolution but keeping the aspect ratio of the current window. It relies on properly set anchors to make everything look good on all aspect ratios.

<p><b>If this description sounds complicated, just try it</b>. It is easy to realize what happens visually, all <a href="manual_editor.php">"New Project" templates created by editor</a> use it automatically.

<p>It is advised to activate it by creating a file called <code>CastleSettings.xml</code> in the
<a href="manual_data_directory.php"><code>data</code> subdirectory of your project</a>.
The sample <code>CastleSettings.xml</code> contents look like this:

<?php echo xml_full_highlight('<?xml version="1.0" encoding="utf-8"?>
<castle_settings>
  <ui_scaling
    mode="EncloseReferenceSize"
    reference_width="1024"
    reference_height="768"
  />
</castle_settings>'); ?>

<p>Then in your <code>Application.OnInitialize</code> callback just call
<code>Window.Container.LoadSettings('castle-data:/CastleSettings.xml');</code>.
This will set <code>UIScaling</code>.
Remember that <a href="manual_editor.php">"New Project" templates created by editor</a> already do it by default.

<p>The advantage of using the <code>CastleSettings.xml</code> file is that
the <a href="manual_editor.php">editor</a> reads this file too,
and can apply the same scaling, default font and so on when you edit the UI.
Read <a href="manual_castle_settings.php">more about the <code>CastleSettings.xml</code> file here.</a>

<p>An alternative way to activate UI scaling is without the <code>CastleSettings.xml</code> file. Do this:

<?php echo pascal_highlight('Window.Container.UIReferenceWidth := 1024;
Window.Container.UIReferenceHeight := 768;
Window.Container.UIScaling := usEncloseReferenceSize;'); ?>

<p>Using UI scaling is incredibly important if you want your game to work on
various window sizes. Which is especially important on mobile devices,
where the sizes of the screen (in pixels) vary wildly.

<p>This means that the whole user interface will be scaled, by the
same ratio as if we would try to fit a <code>1024 x 768</code> area
inside the user's window. The proportions will not be distorted, but
things will get smaller or larger as necessary to accommodate to the
larger window size. If you use anchors correctly, things will
accommodate nicely to the various aspect ratios too.

<p>The fact that things are scaled is largely hidden from you. You get and set
the
 <?php api_link('Left', 'CastleUIControls.TCastleUserInterface.html#Left'); ?>,
 <?php api_link('Bottom', 'CastleUIControls.TCastleUserInterface.html#Bottom'); ?>,
 <?php api_link('Anchor', 'CastleUIControls.TCastleUserInterface.html#Anchor'); ?>,
 <?php api_link('EffectiveWidth', 'CastleUIControls.TCastleUserInterface.html#EffectiveWidth'); ?>,
 <?php api_link('EffectiveHeight', 'CastleUIControls.TCastleUserInterface.html#EffectiveHeight'); ?>,
 <?php api_link('EffectiveRect', 'CastleUIControls.TCastleUserInterface.html#EffectiveRect'); ?>,
 <?php api_link('FontSize', 'CastleControls.TCastleUserInterfaceFont.html#FontSize'); ?>
 and many other properties in the <i>unscaled</i> pixels. Which
basically means that you can hardcode them, and they will still look
right everywhere. Only a few properties uncover the final (<i>real</i>
or <i>scaled</i>) control size. In particular the
<?php api_link('RenderRect', 'CastleUIControls.TCastleUserInterface.html#RenderRect'); ?>
 method (very useful for custom drawing) returns the control rectangle
in the real device pixels (and with the anchors and parent
transformations already applied). More about this in the
<a href="manual_2d_ui_custom_drawn.php">chapter about custom-drawn UI controls</a>.

<?php echo $toc->html_section(); ?>

<p>You can check the resulting size of the control
with <?php api_link('EffectiveWidth', 'CastleUIControls.TCastleUserInterface.html#EffectiveWidth'); ?>
 and <?php api_link('EffectiveHeight', 'CastleUIControls.TCastleUserInterface.html#EffectiveHeight'); ?>.

<p>Beware: Many controls,
like <?php api_link('TCastleButton', 'CastleControls.TCastleButton.html'); ?>, expose also properties
called <?php api_link('Width', 'CastleControls.TCastleButton.html#Width'); ?> and
 <?php api_link('Height', 'CastleControls.TCastleButton.html#Height'); ?>, but they are only to set
an <i>explicit</i> size of the control (if you have disabled
auto-sizing using <?php api_link('TCastleButton.AutoSize', 'CastleControls.TCastleButton.html#AutoSize'); ?>
 or <?php api_link('TCastleButton.AutoSizeWidth', 'CastleControls.TCastleButton.html#AutoSizeWidth'); ?>
 or such). They will not be updated when the control auto-sizing mechanism calculates the actual
size. So <i>do not use <code>Width</code> or <code>Height</code> properties to query the size of a button.
Always use the <code>EffectiveWidth</code> or <code>EffectiveHeight</code> properties instead.</i>

<p>You can also use the <?php api_link('EffectiveRect', 'CastleUIControls.TCastleUserInterface.html#EffectiveRect'); ?>
 property, it contains the control position and size. But note that it's
valid only after the control, and all it's parents, is part of a window that already
received a <code>Resize</code> event. So you may need to wait for the <code>Resize</code> event
to actually use this value.</p>

<?php echo $toc->html_section(); ?>

<!--li><p>You can use <a>KeepInFront</a> to force a control to be in
front of other controls, even the ones added with <a>InsertFront</a>
(unless they will also have KeepInFront = true...)</p>
</li-->

<p>To adjust the look of some controls, you can adjust the theme.
All of the standard 2D controls are drawn using theme images.
This way the look of your game is defined by a set of images,
that can be easily customized.

<!-- Note that each button has already highly configurable look,
even without using the theme, by it's custom images.</p-->

<p>Use the <code>Theme</code> global variable
(instance of <?php api_link('TCastleTheme', 'CastleControls.TCastleTheme.html'); ?>).
For example, image type <code>tiButtonNormal</code> is the normal
(not pressed or disabled) button look.

<p>You can change it to one of your own images. Like this:

<?php echo pascal_highlight(
'Theme.Images[tiButtonNormal] := LoadImage(\'castle-data:/custom_button_normal.png\');
Theme.OwnsImages[tiButtonNormal] := true;
Theme.Corners[tiButtonNormal] := Vector4Integer(1, 1, 1, 1);'); ?>

<p>Note that we also adjust the <code>Corners</code>.
The image will be drawn stretched, using the
<?php api_link('Draw3x3', 'CastleGLImages.TDrawableImage.html#Draw3x3'); ?>
 to intelligently stretch taking the corners into account.</p>

<p>You can see the default images used in the engine sources, in <code>src/ui/opengl/gui-images/</code>
subdirectory. Feel free to base your images on them.</p>

<p>If you prefer to embed the image inside your application
executable, you can do it using the <code>image-to-pascal</code>
tool (compile it from the engine sources in <code>tools/image-to-pascal/</code>).
You can then assign new image like this:

<?php echo pascal_highlight(
'Theme.Images[tiButtonNormal] := CustomButtonNormal;
Theme.OwnsImages[tiButtonNormal] := false;
Theme.Corners[tiButtonNormal] := Vector4Integer(1, 1, 1, 1);'); ?>

<p>Note that we set <code>OwnsImages</code> to <code>false</code>
in this case. The instance of <code>CustomButtonNormal</code>
will be automatically freed in the <code>finalization</code>
section of the unit generated by the <code>image-to-pascal</code>.

<!--?php echo pascal_highlight(
'Theme.Draw(Rectangle(10, 10, 100, 100), tiActiveFrame);'); ?-->

<p>To adjust the initial <i>"Loading"</i> image (visible when you open
the application window) you want to adjust the <code>Theme.Images[tiLoading]</code>
image. <a href="https://github.com/eugeneloza/decoherence/issues/22#issuecomment-317891488">The process is described in detail here.</a>

<?php echo $toc->html_section(); ?>

<?php
echo castle_thumbs(array(
  array('filename' => 'multiple_viewports_dynamic_world.png', 'titlealt' => 'Multiple viewports, interactive scene, shadow volumes and cube-map reflections'),
  array('filename' => 'view3dscene_viewports.png', 'titlealt' => 'Multiple viewports with a DOOM level in view3dscene'),
), 'auto', 'left');
?>

<p>You should use
 <?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?> or
 <?php api_link('TCastleControlBase', 'CastleControl.TCastleControlBase.html'); ?>
 to create an area where <i>Castle Game Engine</i> can render.
 And you should create an instance of
 <?php api_link('TCastleViewport', 'CastleViewport.TCastleViewport.html'); ?>
 and add it to the <code>Controls</code> list
 (<code>TCastleWindowBase.Controls</code> or
 <code>TCastleControlBase.Controls</code>).

<p>It is allowed to have multiple
 <?php api_link('TCastleViewport', 'CastleViewport.TCastleViewport.html'); ?>
 instances in your game, even visible at the same time.
 They can even show the same world (but from different cameras)
 if they share the same <code>Viewport.Items</code> value.

<p>The example below creates one viewport, showing the world from the player perspective,
and then adds another viewport that observes the same world from another perspective.

<p>Note that, since the viewport is a 2D control,
you can place it as child of other UI controls.
The example below demonstrates this technique, inserting
 <?php api_link('TCastleViewport', 'CastleViewport.TCastleViewport.html'); ?>
 inside a <?php api_link('TCastleRectangleControl', 'CastleControls.TCastleRectangleControl.html'); ?>.

<?php
echo castle_thumbs(array(
  array('filename' => 'two_viewports.png', 'titlealt' => 'Two viewports'),
), 'auto', 'left');
?>

<?php echo pascal_highlight_file('code-samples/two_viewports.lpr'); ?>

<?php echo $toc->html_section(); ?>

<?php
echo castle_thumbs(array(
  array('filename' => 'button_with_2d_scene_manager.png', 'titlealt' => '2D animated scene inside a button'),
));
?>

<p>As the viewport may contain a <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>
 with animation, and a viewport
is just a 2D user-interface control,<!-- that can be children of other controls,-->
you can mix user-interface with animations freely.
For example, you can design an animation in Spine, load it to <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>,
insert it to <?php api_link('TCastleViewport', 'CastleViewport.TCastleViewport.html'); ?>, which you can then insert inside a
<?php api_link('TCastleButton', 'CastleControls.TCastleButton.html'); ?>. Thus you can have a button
 with any crazy animation inside:)

<?php echo pascal_highlight_file('code-samples/button_with_viewport.lpr'); ?>

<?php echo $toc->html_section(); ?>

<?php
echo castle_thumbs(array(
  array('filename' => 'zombie_fighter_0.png', 'titlealt' => 'Dialog box composed from simple UI elements'),
));
?>

<p>When you make a non-trivial composition of UI controls, it's a good
idea to wrap them in a parent UI control class.

<p>For this, you can derive a
new descendant of your top-most UI class. The top-most UI class can be

<ol>
  <li>something specific, like the <?php api_link('TCastleRectangleControl', 'CastleControls.TCastleRectangleControl.html'); ?> if your whole UI
    is inside a simple rectangle,
  <li>or it can be our universal <i>"UI control with position and size"</i>:
    <?php api_link('TCastleUserInterface', 'CastleUIControls.TCastleUserInterface.html'); ?>.
</ol>

<p>In the constructor of your new class, you
initialize and add all the child controls. You can even register
private methods to handle the events of private controls inside,
e.g. you can internally handle button clicks inside your new class.

<p>This way you get a new class, like <code>TZombieDialog</code>, that is a
full-featured UI control. It hides the complexity of the UI inside,
and it exposes only as much as necessary to the outside world. It has
a fully functional <code>Update</code> method to react to time passing, it can
handle inputs and so on.

<p>The new UI control can be inserted directly to the
<code>Window.Controls</code>, or it can be used as a child of other UI controls, to
create even more complex stuff. It can be aligned within parent using
the normal <?php api_link('Anchor', 'CastleUIControls.TCastleUserInterface.html#Anchor'); ?> methods.

<p>Example below implements the <code>TZombieDialog</code> class, which is a reworked
version of the previous UI example, that now wraps the dialog UI inside
a neat reusable class.

<?php echo pascal_highlight('uses SysUtils, Classes, CastleControls, CastleUtils, CastleFilesUtils,
  CastleColors, CastleUIControls;

type
  TZombieDialog = class(TCastleRectangleControl)
  private
    InsideRect: TCastleRectangleControl;
    Image: TCastleImageControl;
    LabelStats: TCastleLabel;
    ButtonRun, ButtonFight: TCastleButton;
  public
    constructor Create(AOwner: TComponent); override;
  end;

constructor TZombieDialog.Create(AOwner: TComponent);
begin
  inherited;

  Width := 400;
  Height := 500;
  Color := HexToColor(\'5f3939\');

  InsideRect := TCastleRectangleControl.Create(Self);
  InsideRect.Width := EffectiveWidth - 10;
  InsideRect.Height := EffectiveHeight - 10;
  InsideRect.Color := Silver;
  InsideRect.Anchor(hpMiddle);
  InsideRect.Anchor(vpMiddle);
  InsertFront(InsideRect);

  Image := TCastleImageControl.Create(Self);
  // ... see previous example for the rest of Image initialization
  InsideRect.InsertFront(Image);

  LabelStats := TCastleLabel.Create(Self);
  // ... see previous example for the rest of LabelStats initialization
  InsideRect.InsertFront(LabelStats);

  ButtonRun := TCastleButton.Create(Self);
  // ... see previous example for the rest of ButtonRun initialization
  InsideRect.InsertFront(ButtonRun);

  ButtonFight := TCastleButton.Create(Self);
  // ... see previous example for the rest of ButtonFight initialization
  InsideRect.InsertFront(ButtonFight);
end;

var
  SimpleBackground: TCastleSimpleBackground;
  Dialog: TZombieDialog;

procedure ApplicationInitialize;
begin
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;

  SimpleBackground := TCastleSimpleBackground.Create(Application);
  SimpleBackground.Color := Black;
  Window.Controls.InsertFront(SimpleBackground);

  Dialog := TZombieDialog.Create(Application);
  Dialog.Anchor(hpMiddle);
  Dialog.Anchor(vpMiddle);
  Window.Controls.InsertFront(Dialog);
end;'); ?>

<?php echo $toc->html_section(); ?>

<?php
echo castle_thumbs(array(
  array('filename' => 'zombie_fighter_1.png', 'titlealt' => 'Multiple viewports and basic game UI'),
  array('filename' => 'zombie_fighter_2.png', 'titlealt' => 'UI dialog, in a state over the game UI'),
));
?>

<p>To go one step further, consider organizing larger
games into <i>"states"</i>. The idea is that the game is always within some
state, and the state is also reflected by some user-interface. We have
a ready class <?php api_link('TIUState', 'CastleUIState.TUIState.html'); ?>
 in our engine that helps you take care of that.

<p>In the typical usecase, you create many descendants of the class
<?php api_link('TIUState', 'CastleUIState.TUIState.html'); ?>. Each descendant represents a different state,
like <code>TStateMainMenu</code>,
<code>TStatePlay</code>, <code>TStatePause</code> and so on. Usually you create a
single instance for each of these classes, at the beginning of your
game (e.g. in <?php api_link('Application.OnInitialize', 'CastleWindow.TCastleApplication.html#OnInitialize'); ?> handler).

<p>Each such class contains the user-interface appropriate in the
given state. As <?php api_link('TIUState', 'CastleUIState.TUIState.html'); ?>
 is itself a special <?php api_link('TCastleUserInterface', 'CastleUIControls.TCastleUserInterface.html'); ?>
 descendant, it
can act as a parent (always filling the whole window) for other UI
controls. You can add children controls:

<ul>
  <li><p>In the state constructor.</p></li>

  <li><p>Or you can add them in every <?php api_link('Start', 'CastleUIState.TUIState.html#Start'); ?>
    call, overriding it. In this case, you
    should remove the controls in the <?php api_link('Stop', 'CastleUIState.TUIState.html#Stop'); ?> method.
    Or you can set the controls' owner
    to a special <?php api_link('FreeAtStop', 'CastleUIState.TUIState.html#FreeAtStop'); ?> component,
    to make them freed and removed automatically at the next
    <?php api_link('Stop', 'CastleUIState.TUIState.html#Stop'); ?> call.</p></li>

  <li><p>For advanced uses, if you will use the state stack, you can
    also add / remove children in the
    <?php api_link('Resume', 'CastleUIState.TUIState.html#Resume'); ?> and
    <?php api_link('Pause', 'CastleUIState.TUIState.html#Pause'); ?> calls.</p></li>
</ul>

<p>During the game you use <?php api_link('TIUState', 'CastleUIState.TUIState.html'); ?>
 class methods and properties to change the
current state. Most importantly, you can simply change to a new state
by setting "<?php api_link('TUIState.Current', 'CastleUIState.TUIState.html#Current'); ?> := NewState;". This will call
<?php api_link('Stop', 'CastleUIState.TUIState.html#Stop'); ?> on the
old state, and <?php api_link('Start', 'CastleUIState.TUIState.html#Start'); ?>
 on the new state (these are methods that you can override to do something useful).

<p>For advanced uses, you can also have a <i>"state stack"</i>. This is
perfectly useful when one user-interface is displayed on top of
another, for example when the <code>TStatePause</code> shows a dimmed state of the
game underneath. Be sure to actually pause the game underneath; you
can make a "dimmed" look by adding a fullscreen
<?php api_link('TCastleRectangleControl', 'CastleControls.TCastleRectangleControl.html'); ?>
 with a transparent color (that has alpha between 0 and 1,
like 0.5). If you don't want the underlying state to also receive the
inputs, be sure to set <?php api_link('InterceptInput', 'CastleUIState.TUIState.html#InterceptInput'); ?> on the top state
(<code>TStatePause</code> in this example).

<p>To actually change the state using the "stack" mechanism, use the
<?php api_link('TUIState.Push', 'CastleUIState.TUIState.html#Push'); ?> and
<?php api_link('TUIState.Pop', 'CastleUIState.TUIState.html#Pop'); ?> methods.

<p>The example game <code>zombie_fighter</code> shows a simple implementation of <code>TStateMainMenu</code>,
<code>TStatePlay</code>, <code>TStateAskDialog</code>. The <code>TStateAskDialog</code> is activated when you
click the zombie sprite in the <code>TStatePlay</code>, it then shows our
<code>TZombieDialog</code> created above.

<div class="download jumbotron">
    <a class="btn btn-primary btn-lg" href="https://github.com/castle-engine/castle-engine/tree/master/examples/user_interface/zombie_fighter">Explore the "zombie_fighter" example on GitHub</a>
</div>

<?php
manual_footer();
?>
