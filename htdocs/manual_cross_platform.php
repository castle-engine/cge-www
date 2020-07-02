<?php
require_once 'castle_engine_functions.php';
manual_header('Cross-platform (desktop, mobile, consoles...) games');

$toc = new TableOfContents(
  array(
    new TocItem('Initializing a cross-platform game', 'initialize'),
    new TocItem('Optionally create a standalone program file', 'program'),
    new TocItem('Compiling and debugging on mobile platforms', 'compiling_debuggng'),
    new TocItem('Differences in input handling between mobile (touch) and desktop (mouse) platforms', 'input'),
    new TocItem('Things to avoid in cross-platform games', 'avoid'),
  )
);

echo castle_thumbs(array(
  array('filename' => 'android12glued.png', 'titlealt' => 'Various Android applications developed using Castle Game Engine'),
));
?>

<p><i>Castle Game Engine</i> supports many platforms:
desktop (Windows, Linux, Mac OS X, FreeBSD, Raspberry Pi...),
mobile (Android, iOS),
<a href="https://github.com/castle-engine/castle-engine/wiki/Nintendo-Switch">Nintendo Switch</a>.
The engine hides as much as possible differences between these platforms,
exposing a nice cross-platform API.

<!--
Still, there are some
facts you have to know when developing a game for a mobile platform
(or developing a game both for mobile and standalone platforms).
-->
<!--
Now that you've seen how to create a simple game, with level and player,
it seems a good moment to talk about them.
-->

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>To create a game that works on all platforms, write a simple
platform-independent main game unit.
This unit is usually called <code>GameInitialize</code> (in file
<code>gameinitialize.pas</code>) in our example projects,
although you can of course use any name you like.

<p>See the simplest initial project code in engine examples:
<a href="https://github.com/castle-engine/castle-engine/tree/master/examples/portable_game_skeleton">castle_game_engine/examples/portable_game_skeleton/</a> .
In particular see <a href="https://github.com/castle-engine/castle-engine/blob/master/examples/portable_game_skeleton/gameinitialize.pas">the GameInitialize unit inside, showing a cross-platform initialization</a>.
You can use it as a start of your projects.

<!--
Many other engine demos use the same approach to be cross-platform
Other examples of this approach can be found
in most new projects/examples of our engine. For example see
<code>castle_game_engine/examples/2d_spine_game/</code> code.
Or <a href="darkest_before_dawn.php">Darkest Before the Dawn</a>
source code (see <a href="https://github.com/castle-engine/darkest-before-dawn/blob/master/code/game.pas">game.pas of Darkest Before the Dawn</a>).
-->

<p>This is a short implementation of a <b>cross-platform "Hello world!" application</b>:</p>

<?php echo pascal_highlight_file('code-samples/gameinitialize.pas'); ?>

<p>The <code>initialization</code> section at the bottom of the <code>GameInitialize</code>
unit should only assign a callback to <?php api_link('Application.OnInitialize', 'CastleWindow.TCastleApplication.html#OnInitialize'); ?>,
and create and assign <code>Application.MainWindow</code>.
Most of the actual initialization (loading images, resources, setting up player
and such) should happen in the callback you assigned to <?php api_link('Application.OnInitialize', 'CastleWindow.TCastleApplication.html#OnInitialize'); ?>.
At that point you know that your program is ready to load and prepare resources.

<!--
<p>The initialization <b>must assign the <?php api_link('Applcation.MainWindow', 'CastleWindow.TCastleApplication.html#MainWindow'); ?></b> instance,
that will be used by platform-specific program/library code.
It should be a <?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?> class
instance (it may be a descendant of this class, of course).
-->

<p>This <code>GameInitialize</code> unit can be included by the main program or library
(the <code>.lpr</code> file for Lazarus, <code>.dpr</code> file for Delphi).
The <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">build tool</a>
will automatically generate a main program or library code using this unit,
you only need to indicate it by writing <code>game_units="GameInitialize"</code> in the
<a href="https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples">CastleEngineManifest.xml</a>.

<p>Usually, all other game units are (directly or indirectly) used by this
initialization unit. Although you can also extend the <code>game_units</code> attribute
to include more units.

<p>Create a <a href="https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples">CastleEngineManifest.xml</a> file to compile your project using the <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">build tool</a>. It can be as simple as this:

<?php echo xml_full_highlight(
'<?xml version="1.0" encoding="utf-8"?>
<project name="my-cool-game" game_units="GameInitialize">
</project>'); ?>

<p>Compile and run it on your desktop using this on the command-line:

<pre>castle-engine compile
castle-engine run</pre>

<p>If you have installed <a href="https://github.com/castle-engine/castle-engine/wiki/Android">Android SDK, NDK and FPC cross-compiler for Android</a> then you can also build and run for Android:

<pre>castle-engine package --target=android
castle-engine install --target=android
castle-engine run --target=android</pre>

<p>If you have installed <a href="https://github.com/castle-engine/castle-engine/wiki/iOS">FPC cross-compiler for iOS</a> then you can also build for iOS:

<pre>castle-engine package --target=iOS
# Now open in XCode the project inside
# castle-engine-output/ios/xcode_project/
# to compile and run on device or simulator.</pre>

<?php echo $toc->html_section(); ?>

<p>This is <b>not necessary</b>, but optionally,
to be able to run and debug the project from Lazarus,
you can create a desktop program file like <code>my_fantastic_game_standalone.lpr</code>
to run your game from Lazarus.

<p>It may be as simple as this:

<?php echo pascal_highlight(
'{$mode objfpc}{$H+}
{$apptype GUI}
program my_fantastic_game_standalone;
uses CastleWindow, GameInitialize;
begin
  Application.MainWindow.OpenAndRun;
end.'); ?>

<p>You can even generate a simple program skeleton (<code>lpr</code> and <code>lpi</code> files)
using

<pre>castle-engine generate-program</pre>

<p>You can customize the desktop <code>xxx_standalone.lpr</code>
file to do some desktop-specific things.
For example initialize window size or fullscreen or read command-line parameters. See examples how to do it:
<a href="https://github.com/castle-engine/darkest-before-dawn/blob/master/code/darkest_before_dawn_standalone.lpr">darkest_before_dawn program file (simple)</a>
or <a href="https://github.com/castle-engine/hotel-nuclear/blob/master/code/hotel_nuclear.lpr">hotel_nuclear (more complicated)</a>.

<p>To make our build tool use your customized program file (instead of the auto-generated
one), be sure to set <code>standalone_source</code> in the <code>CastleEngineManifest.xml</code>.

<p>Note that <b>you can edit and run the desktop version using <i>Lazarus</i></b>,
to benefit from Lazarus editor, code tools, integrated debugger...
Using our build tool does not prevent using Lazarus at all!
<ul>
  <li>If you did not create the <code>lpi</code> file using
    <code>castle-engine generate-program</code>, you can create it manually:
    Simply create in Lazarus a new project using the <i>New -&gt; Project -&gt; Simple Program</i>
    option. Or (if you already have the <code>xxx.lpr</code> file) create
    the project using <i>Project -&gt; New Project From File...</i>.
  <li>Add to the project requirements packages <code>castle_base</code> and <code>castle_window</code>
    (from <i>Project -&gt; Project Inspector</i>, you want to <i>Add</i> a <i>New Requirement</i>).
  <li>Save the project as <code>my_fantastic_game_standalone.lpi</code>.
  <li>...and develop and run as usual.
  <li>Edit the main <code>my_fantastic_game_standalone.lpr</code>
    file using the <i>Project -&gt; View Project Source</i> option in Lazarus.
</ul>

<?php echo $toc->html_section(); ?>

<p>Developing for mobile platforms requires installing
some special tools. Everything is explained on these platform-specific pages:

<ul>
  <li><a href="https://github.com/castle-engine/castle-engine/wiki/Android">Developing for Android</a>
  <li><a href="https://github.com/castle-engine/castle-engine/wiki/iOS">Developing for iOS (iPhone, iPad)</a>
</ul>

<p>Compiling and packaging cross-platform games is greatly
simplified if you use our <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">build tool</a>.
For Android and iOS, our build tool nicely hides from you
a lot of complexity with creating a platform-specific application.
<ul>
  <li>For Android, you get a ready working <code>xxx.apk</code> file.
  <li>For iOS, you get a ready project that can be installed using XCode. Or, with some additional options, you can get an IPA file or upload the application straight to the AppStore (see <a href="https://github.com/castle-engine/castle-engine/wiki/iOS">the docs</a>).
</ul>

<!--p>Rendering on mobile platforms uses OpenGLES. Our OpenGLES renderer
can handle almost everything the same as a desktop OpenGL renderer,
but <a href="https://github.com/castle-engine/castle-engine/wiki/OpenGL-ES,-Android-and-iOS-TODOs">there
are some things not implemented yet in the OpenGLES version</a>.
-->

<?php echo $toc->html_section(); ?>

<p>To create portable games you have to think about different types
of inputs available on mobile platforms vs desktop.
The engine gives you various helpers, and abstracts various things
(for example, mouse clicks and touches can be handled using the same API,
you just don't see multi-touches on desktop).
But it's not possible to 100% hide the differences,
because some concepts just cannot work &mdash; e.g. mouse look cannot work
on touch interfaces (since we don't get motion events when you don't press...),
keyboard is uncomfortable on touch devices,
multi-touch doesn't work on desktops with a single mouse and so on.

<p>To account for this, you can adjust your input handling depending on the
<?php api_link('ApplicationProperties.TouchDevice', 'CastleApplicationProperties.TCastleApplicationProperties.html#TouchDevice'); ?> value.

<?php /*

<p>For example, to make 3D navigation working using touch controls on mobile
or "mouse look" on desktops, you can

<ol>
  <li><p>Create an instance of
    <code>TCastleWindowTouch</code>
    (instead of the simpler
    <?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?>).

    <p>TODO: TCastleWindowTouch is deprecated now. These instructions
    will be updated when we implement a replacement.

  <li><p>Create <code>Player</code> instance
    (see the <?php echo a_href_page('manual chapter about the Player', 'manual_player'); ?>)

  <li><p>At initialization (e.g. inside <code>Application.OnInitialize</code> callback)
    do this:

<?php echo pascal_highlight(
'Window.AutomaticTouchInterface := ApplicationProperties.TouchDevice;
Player.Camera.MouseLook := not ApplicationProperties.TouchDevice;'); ?>

</ol>

*/ ?>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p>Do not call <code>Window.Open</code> or <code>Window.Close</code> or
    <code>Application.Run</code>
    inside the cross-platform unit like <code>gameinitialize.pas</code>.

    <p>These methods should never be explicitly called on non-desktop platforms.
    Even on the desktop platforms, they should only be called from the main program file
    (<code>xxx_standalone.lpr</code>), which may be auto-generated by the build tool.

  <li><p>Do not call <code>Application.Terminate</code> on platforms
    where users don't expect it. Use
    <?php api_link('ApplicationProperties.ShowUserInterfaceToQuit', 'CastleApplicationProperties.TCastleApplicationProperties.html#ShowUserInterfaceToQuit'); ?>
    to show or hide the appropriate user interface,
    like a "<i>Quit Game</i>" button.
    Mobile applications generally don't have
    a buttton to quit &mdash; instead, mobile users just switch
    to a different application (or desktop) using the standard buttons.

    <p>Also, the <code>Application.Terminate</code> may not be implemented
    on some platforms where <code>ShowUserInterfaceToQuit</code> is <code>false</code>.

  <li><p>Do not create more than one <code>TCastleWindowBase</code> instance.
    If you want your game to be truly portable to <b>any</b> device &mdash;
    you have to limit yourself to using only one window.
    For normal games that's probably natural anyway.

    <p>Note that the engine still supports, and will always support,
    multiple-window programs.
    See e.g.<code>castle_game_engine/examples/window/multi_window.lpr</code> example.
    However, it only works on normal desktop systems.
    It is not possible to do portably (to seamlessly work on mobile and console systems)
    since other platforms don't have a concept of "window" that works like on desktops.
</ul>

<?php
manual_footer();
?>
