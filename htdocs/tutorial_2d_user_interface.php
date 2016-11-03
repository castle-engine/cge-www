<?php
require_once 'castle_engine_functions.php';
tutorial_header('Standard 2D controls: user interface');
?>

TODO: finish this.


<p>Our <?php api_link('TCastleWindow', 'CastleWindow.TCastleWindow.html'); ?> and
 <?php api_link('TCastleControl', 'CastleControl.TCastleControl.html'); ?>
 have a list of 2D controls visible
of the screen. By default, the only thing present there is a scene
manager (since scene manager acts as a 2D viewport through which you
see the 3D world).<!--; that's right &mdash; the 3D stuff is "within" the 2D
stuff--> This way the scene manager (it's viewport) is visible on the
window, which in turn means that the 3D world is visible too.

<p>You can add your own 2D controls using the <code>Window.Controls.Add</code>
method. There are many predefined GUI controls available in our engine,
look for <?php api_link('TUIControl', 'CastleUIControls.TUIControl.html'); ?>
 descendants, for example in
<?php api_link('CastleControls', 'CastleControls.html'); ?>
 unit. You can also derive your own controls with ease.


Design simple window with an image and two buttons. "Attack the zombie?"

- parents hierarchy, relative coords
- tuicontrolrectangle as parent invisible
- tcastlelabel, TCastleImage control, tcastlebutton, tcastleshape and tcastlerectangle as common ui controls

Note that *Every* control can be a parent for more controls. So you can place children controls e.g. inside the TCastleBytton, to make a button contain any UI you like.

Also note that the parent by default does *not* cut the visibility of children, I.e. it doesn't vmake a "scissor" for children. It is your responsibility to put children inside the parent (otherwise some things will not work intuitively, e.g. mouse clicks may not be passed to children OK). Or you can activate the scissor, to cut children visibility to parent area, see the tscrollviw for example.
castlebutton is very flexible, you can really adjust it
- you can adjust theme, and lots of their properties. Label may be HTML.
- position relative to parent using anchor, usually. Ev. In resize adjust as needed, modify left or bottom or call Align then.
- check size with calculated width/height. Do not just use button width/height, it is left as set when using autosize. Also, for some controls it's not available until the first resize event.
- use ui scaling as needed. ScreeRect gives rect after scaling, Rect gives size after scaling too, and font has size after scaling too, generally everything else is unscaled. Everything you set is unscaled.

<h2>Viewport is also a 2D control</h2>

- remember that Tabstract castle viewport (tcastl scene manager, t2dscene manager, tcastleviewport) are 2d controls too. You CAN go wild and insert this way animations from x3d / spine on top of your UI, it works as always, and we have no problems with multiple vscene managers.

Example with adding scene manager by hand. Two times.

Example with adding 2d scene manager. Inside a button?

<h2>Wrapping it up</h2>

<p>When you make a non-trivial composition of UI controls, it's a good idea to wrap them in a parent UI control class. For this, you derive a new descendant of your top-most UI class. The top-most UI class can be something specific, like the TCastleRectangleControl if your whole UI is a parent of a simple rectangle, or it can be our universal "UI control with size" class TUIControlSizeable, or it can be even our abstract TUIControl (in this last case, you will have full flexibility, but you will need to carefully define it's Rect overridden method). In the constructor of your new class, you initialize and add all the child controls. You can even register private methods to handle the events of private controls inside, e.g. you can internally handle button clicks inside your new class.

<p>This way you get a new class, like TMyZombieDialog, that is a full-featured UI control. It hides the complexity of the UI inside, and it exposes only as much as necessary to the outside world. It has a fully functional Update method to react to time passing, it can handle inputs and so on.

<p>The new UI control can be then be inserted directly to the Window.Controls, or it can be used as a child of other UI controls, to create even more complex stuff. It can be aligned within parent using the normal Anchor() methods.

<p>Example below shows TMyZombieDialog class

----
TODO
----

<h2>User-interface state</h2>

<p>To go even one step further, you may consider organizing larger games into "states". The idea is that the game is always within some state, and the state is also reflected by some user-interface. We have a ready class TUIState in our engine that helps you take care of that.

<p>The idea is that you create many descendants of the class TUIState. Each one represents a different state, like TMainMenuState, TGameState, TPauseState and so on. In the usual scenario, you create a single instance of each of these classes, at the beginning of your game (e.g. in Application.OnInitialize handler).

<p>Each such class contains the user-interface appropriate in the given state. As TUIState is itself a special TUIControl descendant, it can act as a parent (always filling the whole window) for other UI controls. You can add children controls there
- in the constructor,
- or you can add them in every Start call (in which case, you should remove them in the Stop call; you can set the controls' owner to special FreeAtStop component, to make them freed and removed automatically at next Stop call),
- for advanced uses, if you will use the state stack, you can also add / remove stuff in the Resume and Pause calls.

<p>During the game you use class methods and properties to change the current state. Most importantly, you can simply change to a new state by setting "TUIState.Current := NewState;". This will call Stop on the old state, and Start on the new state (these are methods that you can override to do anything useful).

<p>For advanced uses, you can also have a "state stack". This is perfectly useful when one user-interface is displayed on top of another, for example when the TPauseState shows a dimmed state of the game underneath. Be sure to actually pause the game underneath; you can make a "dimmed" look by adding a fullscreen TCastleRectangleControl with a color that some alpha between 0 and 1, like 0.5. If you don't want the underlying state to also receive the inputs, be sure to set InterceptInput on the top state (TPauseState in this example).

<p>To actually change the state using the "stack" mechanism, use the Push() and Pop() methods. See the API reference for details.

<p>The example below shows a simple implementation of TMainMenuState, TGameState, TAskDialogState. The TAskDialogState is activated when you press the zombie head in the TGameState, it then shows our TMyZombieDialog created previously.

----
TODO:example
----

- Special UIs:
  - you may find it useful to use TCastleuistate to organize your application states. In this case, you will usually create ui in start, and pass Free AtStop as owner (or free manually in stop).
  - you can use tuicontrolrectangle fullscreen to capture keys

<?php
tutorial_footer();
?>
