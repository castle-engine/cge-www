<?php
require_once 'castle_engine_functions.php';
tutorial_header('Standard 2D controls: user interface', array(
  'social_share_image' => 'zombie_fighter_0.png',
));

$toc = new TableOfContents(
  array(
    new TocItem('Introduction', 'introduction'),
    new TocItem('Using the 2D controls', 'usage'),
    new TocItem('Parents and anchors', 'parents_and_anchors'),
    new TocItem('User-interface scaling (Container.UIScaling)', 'scaling'),
    new TocItem('Other TUIControl features', 'other_features'),
    new TocItem('Taking control of the 2D viewport and scene manager', 'viewport'),
    new TocItem('Wrapping it up (in a custom TUIControl descendant)', 'wrapping'),
    new TocItem('User-interface state (TUIState)', 'ui_state'),
  )
);

echo castle_thumbs(array(
  array('filename' => 'zombie_fighter_0.png', 'titlealt' => 'Dialog box composed from simple UI elements'),
), 'auto', 'left');

// TODO all the <a> below fix and check all links
?>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>The whole rectangle of our game window is handled by <i>2D user-interface controls</i>.
All of the classes representing a user-interface control descend from
the <?php api_link('TUIControl', 'CastleUIControls.TUIControl.html'); ?> class.
They can render, they can handle inputs, they can perform any time-dependent task, and much more.

<p>All of the currently used controls are added to the list of
<?php api_link('Window.Controls', 'CastleWindow.TCastleWindowCustom.html#Controls'); ?>
 (if you use <code>TCastleWindow</code>) or
<?php api_link('CastleControl1.Controls', 'CastleWindow.TCastleWindowCustom.html#Controls'); ?>
 (if you use Lazarus forms with <code>TCastleControl</code>).
They are ordered from the back to front. But usually you don't need to remember that,
as you add them using the aptly-named methods <code>InsertFront</code> or <code>InsertBack</code>,
indicating the visual "front" (<i>in front of other controls</i>) or "back".
The <?php api_link('TUIControl', 'CastleUIControls.TUIControl.html'); ?> can be arranged
 in hierarchy: one instance of <?php api_link('TUIControl', 'CastleUIControls.TUIControl.html'); ?>
 can be a children of another, which makes it visually "contained" inside the parent,
and moved along with parent.

<p>Note that you can also render and handle inputs using the
<?php api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html'); ?> or
<a>TCastleControl callbacks, like Window.OnRender, Window.OnPress and Window.OnUpdate.
But this is usually only useful for simple cases.
Most of your rendering and input handling should be done inside some
<?php api_link('TUIControl', 'CastleUIControls.TUIControl.html'); ?>
 descendant. Note that a <i>scene manager</i> by default acts also as a <i>viewport</i>
into the 3D world, and it's also a descendant of <?php api_link('TUIControl', 'CastleUIControls.TUIControl.html'); ?>.
It makes sense, if you think about it: inside a 2D screen, you create a 2D viewport, that allows
to view the 3D world inside.
So, truly, everything is a descendant of <?php api_link('TUIControl', 'CastleUIControls.TUIControl.html'); ?>
 is our engine, and we advice you to follow this approach when creating your own games.

<p>While <?php api_link('TUIControl', 'CastleUIControls.TUIControl.html'); ?> is basically
 an entry to render <i>anything</i>, in this chapter we will focus in the most traditional
use of this class: to create simple 2D user interfaces. Our engine includes a large number of
<?php api_link('TUIControl', 'CastleUIControls.TUIControl.html'); ?> to this end. The most
often used controls are:

<ul>
  <li><?php api_link('TCastleLabel', 'CastleControls.TCastleLabel.html'); ?> - Label with text. As with all our text controls, the font family and size is customizable (see <a href="tutorial_text.php">chapter about text</a>). May have a frame. May be multiline. May contain some <?php api_link('HTML tags', 'CastleControls.TCastleLabel.html#Html'); ?>.</li>

  <li><?php api_link('TCastleButton', 'CastleControls.TCastleButton.html'); ?> - Clickable button. The look is highly configurable with custom images and tint colors. May contain an icon inside. The size may be automatically adjusted to the inside caption (and icon), or may be explicitly given.</li>

  <li><?php api_link('TCastleImageControl', 'CastleControls.TCastleImageControl.html'); ?> - Image. May stretch the content, or adjust to the content size. Image may be partially transparent, with blending or alpha-testing. Image can be rotated or clipped by an arbitrary line. Image may be "tinted" (multiplied) by given color. Undernath, the image is a full-featured <?php api_link('TCastleImage', 'CastleImages.TCastleImage.html'); ?>, so you can process it in a myriad of ways.<!--, and load from a myriad sources (e.g. you can load here a dynamically generated <a href="">screenshot, not only an image file).--></li>

  <li><?php api_link('TCastleRectangleControl', 'CastleControls.TCastleRectangleControl.html'); ?> - Rectangle filled with solid color. Good for a general-purpose background. The color may be partially transparent, in which case the content underneath is still visible.</li>

  <li><?php api_link('TUIControlSizeable', 'CastleUIControls.TUIControlSizeable.html'); ?> - General-purpose container (or ancestor) for other UI controls. Does not do or show anything by itself, but it has a configurable position and size.</li>
</ul>

<p>These are just the basic UI classes. Find the <code>TUIControl</code> in our <?php api_link('class hierarchy', 'ClassHierarchy.html'); ?> diagram and look at all it's descendants to discover more:)

<?php echo $toc->html_section(); ?>

<p>You simply create an instance of any class you like, and add it as a children
of <?php api_link('Window.Controls', 'CastleWindow.TCastleWindowCustom.html#Controls'); ?>
 or <?php api_link('CastleControl1.Controls', 'CastleWindow.TCastleWindowCustom.html#Controls'); ?>
 or another (parent) <?php api_link('TUIControl', 'CastleUIControls.TUIControl.html'); ?>.
 Note that <?php api_link('TUIControl', 'CastleUIControls.TUIControl.html'); ?> is a descendant
 of the standard <code>TComponent</code> property, so you can use the standard "ownership"
mechanism to take care of freeing the UI control. In simple cases, you can make the <code>Application</code>
or <code>Window</code> a parent of your UI control.

<p>An example below shows a simple button and a label:

<?php echo pascal_highlight(file_get_contents('code-samples/standard_2d_ui.lpr')); ?>

<p>Remember that at any time, you can add and remove the controls.
You can also make a control temporarily "not existing"
(not visible, not handling inputs and so on &mdash;
just like it would not be present on the controls list at all) by flipping it's
<a>Exists</a> property.

<p>Here's the previous example expanded, showing how to handle button click,
to toggles the visibility of a rectangle:

<?php echo pascal_highlight(file_get_contents('code-samples/standard_2d_ui_toggle_exists.lpr')); ?>

<?php echo $toc->html_section(); ?>

<p>Every UI control may have children, which are positioned relative to their parent.
The children are always drawn on top of their parent.
(Although the parent has an additional chance to draw over the children in it's
RenderOverChildren method, but that's only for really special purposes.)

<p>The children also receive input events (key and mouse presses) first.
So the innermost children get the first chance to process an event
(like a click), and only if they do not (their <a>Press method will return <code>false</code>)
then the event is passed to the parent. If no UI control processes a press event,
it is passed to the <a>TCastleWindow.OnPress</a> or <a>TCastleControl.OnPress</a>.

<p>Note that the input events are only send to the controls under the pointer
(mouse or touch position). Although this is configurable using
the <a>CapturesEventsAtPosition</a> method, but usually it's saner to leave it at default.
A useful trick to capture the events from the whole window is to use a TUIControlSizeable
with FullScreen = true, this will capture mouse clicks and key pressed from everything.

<p>Any control can be a parent. For example, you can insert arbitrary images and labels
inside a <a>TCastleButton</a>, to make it's content look in any way you want.
If you want to group a couple of controls, but don't have a natural "parent" control,
it's often a good idea to use a new instance of an
<?php api_link('TUIControlSizeable', 'CastleUIControls.TUIControlSizeable.html'); ?>
 as a parent.

<p>Controls are positioned relative to parent, using the <a>Left</a> and <a>Bottom</a> properties
by default. Remember that our engine in 2D uses a mathematical coordinate system,
where the Y grows from zero (bottom) to maximum height (at the top).
This is contrary to a common convention of various GUI libraries to designate <code>Top</code> as zero,
and make Y grow downward. We decided to follow the convention <i>Y grows up</i>
for a couple of reasons, mostly because it more naturally matches the 3D axis
(in 3D, our engine also follows the convention that <i>Y grows up</i> by default).

<p>Usually, instead of assignining the positions using the <a>Left</a> and <a>Bottom</a> properties,
it's better to use <i>anchors</i>. Anchors specify the position of some border (or center)
of the control, relative to some border (or center) of it's parent.
When the parent control is resized (e.g. when user resizes the window),
children are automatically repositioned correctly.
This usually avoids the need to react to window size changes in callbacks like <a>Window.OnResize
or <a>TUIControl.Resize</a> implementations.

<p>Note that the parent does <b>not</b> clip the visibility of the children.
That is, we assume that you will set the size of children small enough to make them fit
within the parent. If you don't, the result will be a little unintuitive: the overflowing
contents of children will be drawn outside of the rectangle of the parent, but they will
not receive input (like clicks). For this reason, it's best to make children actually fit
within the parent.

<p>If you actually want to clip the children, use the <a>TScissor</a> .
The <a>TCastleScrollView</a> implementation
is a good example of how to use it properly.

<p>With all this knowledge about parents and anchors, let's make a simple dialog box,
showing off what we learned, and something extra (note the use of <code>TCastleLabel.Html</code>
and <code>HexToColor</code> below).
Below are the contents of the <code>ApplicationInitialize</code> procedure,
you can just use this code to setup UI in the main program block
(like in the simple examples above on the same page),
or you can set it as the <code>Application.OnInitialize</code> callback following the
<a>chapter about developing cross-platform applications</a>.

<p>If in doubt, <b>take a look at the <code>examples/2d_standard_ui/zombie_fighter/game.pas</code>
code that contains the final application we will make in this tutorial!</b></p>

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
  Rect.Color := HexToColor(\'5f3939\'); // equivalent: Vector4Single(95/255, 57/255, 57/255, 1.0);
  Rect.Anchor(hpMiddle);
  Rect.Anchor(vpMiddle);
  Window.Controls.InsertFront(Rect);

  InsideRect := TCastleRectangleControl.Create(Application);
  InsideRect.Width := Rect.CalculatedWidth - 10;
  InsideRect.Height := Rect.CalculatedHeight - 10;
  InsideRect.Color := Silver;
  InsideRect.Anchor(hpMiddle);
  InsideRect.Anchor(vpMiddle);
  Rect.InsertFront(InsideRect);

  Image := TCastleImageControl.Create(Application);
  Image.URL := ApplicationData(\'Female-Zombie-300px.png\');
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

<p>As you probably noticed, <b>we do not have a visual UI designer
yet</b>. It's on the roadmap to make a visual designer integrated with
Lazarus, so you could design the inside the <a>TCastleControl and
even <a>TCastleWindow</a> in the same comfortable way as when you design
a Lazarus form with standard Lazarus LCL components. Contributions
or <a href="">donations</a> towards this goal are welcome!

<?php echo $toc->html_section(); ?>

<p>You can use the <a>UIScaling</a> to automatically adjust all the UI
controls sizes. You activate it like this:

<?php echo pascal_highlight('Window.Container.UIReferenceWidth := 1024;
Window.Container.UIReferenceHeight := 768;
Window.Container.UIScaling := usEncloseReferenceSize;'); ?>

<p>This is incredibly important if you want your game to work on
various window sizes. Which is especially important on mobile devices,
where the sizes of the screen (in pixels) vary wildly.

<p>This means that the whole user interface will be scaled, by the
same ratio as if we would try to fit a <code>1024 x 768</code> area
inside the user's window. The proportions will not be distorted, but
things will get smaller or larger as necessary to accomodate to the
larger window size. If you use anchors correctly, things will
accomodate nicely to the various aspect ratios too.

<p>The fact that things are scaled is largely hidden from you. You get and set
the <a>Left</a>, <a>Bottom</a>, <a>Anchor</a>, <a>CalculatedWidth</a>,
<a>CalculatedHeight</a>, <a>CalculatedRect</a>, <a>FontSize</a>
and many other properties in the <i>unscaled</i> pixels. Which
basically means that you can hardcode them, and they will still look
right everywhere. Only a few properties uncover the final (<i>real</i>
or <i>scaled</i>) control size. In particular the <a>ScreeRect</a>
method (very useful for custom drawing) returns the control rectangle
in the real device pixels (and with the anchors and parent
transformations already applied). More about this in the later
chapter.

<?php echo $toc->html_section(); ?>

<ul>
  <li><p>You can use <a>KeepInFront</a> to force a control to be in
    front of other controls, even the ones added with <a>InsertFront</a>
    (unless they will also have KeepInFront = true...)</p>
  </li>

  <li><p>To adjust the look of some controls, you can adjust the theme
    images. Note that each button has already highly configurable look,
    even without using the theme, by it's custom images.</p>
  </li>

  <li><p>Always check the resulting size of the control
    with <a>CalculatedWidth</a> <a>CalculatedHeight</a>. Many controls,
    like <a>TCastleButton</a>, expose also properties
    called <a>Width</a> and <a>Height</a>, but they are only to set
    an <i>explicit</i> size of the control (if you have disabled
    auto-sizing using <a>TCastleButton.AutoSize
    or</a> <a>TCastleButton.AutoSizeWidth</a> or such). They will not be
    updated when the control auto-sizing mechanism calculates the actual
    size. There is also the <a>CalcultedRect</a> property, that also
    contains the control position (but note that it's valid only after
    the control, and all it's parents, is part of a window that already
    received a Resize event; you may need to wait for the Resize event
    to actually use this value).</p>
  </li>
</ul>

----------
TODO:
show update() method too,
----

<?php echo $toc->html_section(); ?>

<p>Note that by default, the <?php api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html'); ?> and
 <?php api_link('TCastleControl', 'CastleControl.TCastleControl.html'); ?>
 already contain one item on their <code>Controls</code> list: the default <code>SceneManager</code>
 instance. For non-trivial games, you may prefer to start with a truly empty window (or control),
 and create your scene managers and viewports yourself. To do this, simply use the
 <?php api_link('TCastleWindowCustom', 'CastleWindow.TCastleWindowCustom.html'); ?> or
 <?php api_link('TCastleControlCustom', 'CastleControl.TCastleControlCustom.html'); ?> instead.

<p>The example below creates a scene manager, showing the world from the player perspective,
and then adds a viewport that observes the same world from another perspective.
The key to understand this is that <i>the scene manager serves by default two purposes</i>:

<ol>
  <li>It's a central keeper of the information about your game world (in the <a>SceneManager.Items</a>).

  <li>It is also a default viewport, that instantiates a camera to view this world.
    This works because <a>TCastleSceneManager</a> is a descendant of the <a>TAbstractCastleViewport</a>.
    You can deactivate this feature of scene manager by setting <a>SceneManager.DefaultViewport</a>
    to <code>false</code>, then the scene manager is used <i>only</i> to store the information about your
    world, and you <i>must</i> use at least one <a>TCastleViewport</a> to actually see anything.
  </li>
</ol>

<p>On the other hand, <a>TCastleViewport</a> always serves only one purpose: it's a viewport,
and it always refers to some <a>TCastleSceneManager</a> instance to know what to show inside.

<p>Often is makes sense to keep <a>SceneManager.DefaultViewport</a> as true, if you have something
like a <i>primary</i> viewport of the user. And then use <a>TCastleViewport</a> only for <i>secondary</i>
viewports. Sometimes, if's better to set <a>SceneManager.DefaultViewport</a> to false,
and use only a number of <a>TCastleViewport</a> instances to show it.
It really depends on what is easier for you conceptually, in a given game.

<p>Note that, since the scene manager and viewports are 2D controls,
you can place them as children of other UI controls.
The example below demonstrates this technique, inserting <a>TCastleViewport</a> inside a <a>TCastleRectangleControl</a>.

<p>// TODO: example


<p>This means that you can mix static user-interface with animations freely. The viewport may contain
a TCastleScene with animation. For example, you can design an animation in Spine, load it to TCastleScene,
insert it to T2DSceneManager, which you can then insert inside a TCastleButton. Thus you can have a button
with any crazy animation inside you want:)

<p>// TODO: example

<?php echo $toc->html_section(); ?>

<p>When you make a non-trivial composition of UI controls, it's a good
idea to wrap them in a parent UI control class. For this, you derive a
new descendant of your top-most UI class. The top-most UI class can be
something specific, like the TCastleRectangleControl if your whole UI
is a parent of a simple rectangle, or it can be our universal "UI
control with size" class TUIControlSizeable, or it can be even our
abstract TUIControl (in this last case, you will have full
flexibility, but you will need to carefully define it's Rect
overridden method). In the constructor of your new class, you
initialize and add all the child controls. You can even register
private methods to handle the events of private controls inside,
e.g. you can internally handle button clicks inside your new class.

<p>This way you get a new class, like TMyZombieDialog, that is a
full-featured UI control. It hides the complexity of the UI inside,
and it exposes only as much as necessary to the outside world. It has
a fully functional Update method to react to time passing, it can
handle inputs and so on.

<p>The new UI control can be then be inserted directly to the
Window.Controls, or it can be used as a child of other UI controls, to
create even more complex stuff. It can be aligned within parent using
the normal Anchor() methods.

<p>Example below shows the TZombieDialog class. , which is a reworked
version of the previous code, that now wraps

<p>// TODO: example

<?php echo $toc->html_section(); ?>

<p>To go even one step further, you may consider organizing larger
games into "states". The idea is that the game is always within some
state, and the state is also reflected by some user-interface. We have
a ready class TUIState in our engine that helps you take care of that.

<p>The idea is that you create many descendants of the class
TUIState. Each one represents a different state, like TMainMenuState,
TGameState, TPauseState and so on. In the usual scenario, you create a
single instance of each of these classes, at the beginning of your
game (e.g. in Application.OnInitialize handler).

<p>Each such class contains the user-interface appropriate in the
given state. As TUIState is itself a special TUIControl descendant, it
can act as a parent (always filling the whole window) for other UI
controls. You can add children controls there

<ul>
  <li><p>in the constructor,</p></li>

  <li><p>or you can add them in every Start call (in which case, you
    should remove them in the Stop call; you can set the controls' owner
    to special FreeAtStop component, to make them freed and removed
    automatically at next Stop call),</p></li>

  <li><p>for advanced uses, if you will use the state stack, you can
    also add / remove stuff in the Resume and Pause calls.</p></li>
</ul>

<p>During the game you use class methods and properties to change the
current state. Most importantly, you can simply change to a new state
by setting "TUIState.Current := NewState;". This will call Stop on the
old state, and Start on the new state (these are methods that you can
override to do anything useful).

<p>For advanced uses, you can also have a "state stack". This is
perfectly useful when one user-interface is displayed on top of
another, for example when the TPauseState shows a dimmed state of the
game underneath. Be sure to actually pause the game underneath; you
can make a "dimmed" look by adding a fullscreen
TCastleRectangleControl with a color that some alpha between 0 and 1,
like 0.5. If you don't want the underlying state to also receive the
inputs, be sure to set InterceptInput on the top state (TPauseState in
this example).

<p>To actually change the state using the "stack" mechanism, use the
Push() and Pop() methods. See the API reference for details.

<p>The example below shows a simple implementation of TMainMenuState,
TGameState, TAskDialogState. The TAskDialogState is activated when you
press the zombie head in the TGameState, it then shows our
TMyZombieDialog created previously.

<p>// TODO: example

<?php
tutorial_footer();
?>
