<?php

array_push($news,
    array('title' => 'Development: Android and iOS, new game release "Darkest Before the Dawn", more',
          'year' => 2014,
          'month' => 1,
          'day' => 1,
          'short_description' => '',
          'guid' => '2014-01-01',
          'description' =>
castle_thumbs(array(
  array('filename' => 'darkest_before_dawn_1.png'),
  array('filename' => 'darkest_before_dawn_2.png'),
)) .
'<p>Hello everyone in 2014 :)

<p>First of all, Michalis would like to present a small game done
during a weekend "gamejam" at the end of November 2013:

<h3><a href="darkest_before_dawn.php">Darkest Before the Dawn</a></h3>

<p>The game is free to download for Android, Linux or Windows.
Of course the game uses our <a href="' . CURRENT_URL . 'engine.php">Castle
Game Engine</a> for everything. The complete game code and data are
available in our SVN repository.

<p>Next, we have many exciting news about the engine development:
<ul>
  <li>Yes, you have heard right: <b>the engine supports Android and iOS
    (iPhone, iPad)</b>.

    <p>The rendering is done using an OpenGL ES 2.0 renderer.
    You can compile the whole engine for OpenGLES (as opposed to normal desktop
    OpenGL) by defining symbol<tt>OpenGLES</tt> in <tt>base/castleconf.inc</tt> file.
    The <tt>OpenGLES</tt> symbol is defined automatically
    when we compile for Android or iOS,
    but you can also test OpenGLES renderer on normal systems like Windows
    and Linux (it\'s a nice way to test your mobile game on a desktop system).

    <p>A lot of the rendering features work on OpenGLES.
    Rendering 3D shapes with shaders, textures, lighting is ready.
    The OpenGLES shaders (I can them "mobile shaders" in some places) use
    the Gouraud shading for speed.
    Rendering 2D images (TGLImage, with possible alpha test or blending)
    is also done, and since it\'s the basis for all our 2D controls
    &mdash; most of 2D GUI works already (except fonts...).

    <p>We can initialize OpenGLES context using the EGL library.
    This is useful to initialize OpenGLES on Android or Xlib (with glX+EGL)
    or Windows (with wgl+EGL).

    <p>The Android port uses
    <a href="http://developer.android.com/reference/android/app/NativeActivity.html">NativeActivity</a>,
    available since Android 2.3. It requires FPC 2.7.1 to cross-compile
    code to Android ARM, see
    <a href="://wiki.freepascal.org/Android">FPC wiki about Android</a>.

    <p>Detailed instructions about compiling for Android and iOS will follow.
    We also plan (although not sure will this be ready for next release)
    to make an engine build tool, to make it possible to build cross-target
    games with ease (with a single source code that can be compiled
    into various platforms, like standalone, Android, iOS etc.).
    See <a href="https://sourceforge.net/p/castle-engine/wiki/Planned:%20build%20tool/">Planned: build tool</a> wiki page.
    I would like to make this "the most comfortable way to compile
    programs using our engine" &mdash; your comments about this
    are much appreciated.﻿

http://www.youtube.com/watch?v=8u7DggGe_Uk

- In another news, thanks to Jan Adamec, our engine can be compiled and used as a shared library (castleengine.dll on Windows, libcastleengine.so on Linux), with API accessible from C or C++. We provide a C header src/library/castlelib.h for this purpose. See code in src/library/ and tests in examples/library/ . This is generally useful, and in particular will be a way to use our engine on iOS from non-Pascal programs.

- gles-123 screenshots
- Changed default TWalkCamera keys to be equal to TPlayer keys, in particular to honour common AWSD combo. This way:
  - You can move using AWSD by default (e.g. in view3dscene).
  - Space / c make jump / crouch. Also, we no longer have separate inputs for jump / crouch (when gravity works) or flying up / down (when gravity doesn\'t work).
  - This avoids switching the meaning or left/right arrows in mouse look mode in view3dscene.
  This makes keys in all our programs and games more consistent, and just better --- everyone knows AWSD, while previous shortcuts for strafing (comma and dot) were uncommon.
  new_walk_shortcuts.png
- Background rewrite, TextureBackground, possibility to MovieTexture as background
- GLES : whole engine compiles, also view3dscene and castle1 and most examples.
- Context resource sharing (so that many windows/controls with context work Ok, sharing textures and fonts etc.) implemented for CastleWindow Xlib+GLX and GTK  ackend too.
- GL ES renderer: light ambient, color per vertex (including Background colors).
- Support for png and gz without any external libraries, by using FpRead/WritePng and PasZlib (where suitable).
- Support for Android applications, through Android NativeAcivity and EGL. Integrated with Android: using Android\'s log facility, using Android\'s assets (URLs like assets:/my_texture.png are supported, ApplicationData returns assets:/).
- CastleEnumeratedFiles API much changed and renamed to FindFiles. It supports searching for files inside Android assets now, so you can look for level.xml / resource.xml files inside assets just like with local games.
- --hide-menu option for view3dscene. Especially useful for fullscreen presentations, where you may want to hide all UI.
- tutorial_transformation_hierarchy.php docs

')
);
