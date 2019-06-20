<?php

array_push($news,
    array('title' => 'Castle Game Engine 5.1.1 release (Spine, build tool, more), view3dscene 3.15.0 release',
          'year' => 2014,
          'month' => 12,
          'day' => 30,
          'short_description' => '',
          'guid' => '2014-12-30',
          'images' => array(
            array('filename' => 'goblins-ffd_0.png', 'titlealt' => 'Spine animation with female skin'),
            array('filename' => 'android_emulator.png', 'titlealt' => '&quot;Dragon Spine&quot; running in an Android emulator'),
            array('filename' => 'spine_spinosaurus_0.png', 'titlealt' => 'GUI in Spine'),
            array('filename' => 'android1.png', 'titlealt' => 'Various Android applications developed using Castle Game Engine'),
          //  array('filename' => 'android_text.png', 'titlealt' => 'Text node on Android'),
            array('filename' => 'primitives_3.png', 'titlealt' => 'Text with shadow'),
            array('filename' => 'android_little_things.png', 'titlealt' => '&quot;Little Things&quot; game on Android'),
            array('filename' => 'android_tasty_reflections.png', 'titlealt' => '&quot;Tasty Reflections&quot; game on Android'),
          //  array('filename' => 'named_animation_knight.png', 'titlealt' => 'Named animations on 3D model'),
            array('filename' => 'play_named_animation_fixed.png', 'titlealt' => 'Named animations on example Spine model'),
            array('filename' => 'venice_named_animation.png', 'titlealt' => 'Named animations on Venice character'),
          //  array('filename' => 'mountains_of_fire_screen_4.png', 'titlealt' => '&quot;Mountains Of Fire&quot; game screen'),
          //  array('filename' => 'hotel_nuclear_screen_5.png', 'titlealt' => '&quot;Hotel Nuclear&quot; game screen'),
            array('filename' => 'sunny_street_final_0.png', 'titlealt' => 'New selection indicator'),
          ),
          'description' =>
'<p>We proudly present, after 7 months of work, the next official release of our engine:)</p>
<!-- teaser -->

<div class="download jumbotron">
<a href="' . CURRENT_URL . 'engine.php" class="btn btn-primary btn-lg">Download Castle Game Engine 5.1.1!</a>
</div>

<p>As usual, we also release <a href="' . CURRENT_URL . 'view3dscene.php">view3dscene</a> 3.15.0, our 3D/2D model browser and converter.

<p><b>New features of the engine and view3dscene:</b></p>

<ol>
  <li>Extensive support for <a href="https://github.com/castle-engine/castle-engine/wiki/Spine">Spine</a> 2D models and animations. <a href="http://esotericsoftware.com/">Spine</a> is a great program to design 2D skeletal (but not only) animations for games. It can be used to design animated characters, backgrounds, GUIs... Our engine and view3dscene can now effortlessly load and play Spine JSON format.
  <li>New <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">build tool of Castle Game Engine</a>, able to easily compile and package programs developed using our engine. Especially useful for Android, where it hides a lot of complications related to creating Android apk from your code.
  <li><a href="https://castle-engine.io/x3d_implementation_text.php">X3D Text node</a> gets a new implementation, very fast, portable to OpenGLES (Android/iOS), and easy to use (shares tools and classes with 2D text rendering). You can also control <a href="https://castle-engine.io/x3d_implementation_text_extensions.php#section_ext_fontstyle_blending">whether it uses alpha blending or alpha test</a>.
  <li>Text can now handle international text (in UTF-8). <code>texturefont2pascal</code> can read font with extra international characters.
  <li>Many improvements for 2D games. Besides Spine support, there\'s also a comfortable <a href="https://castle-engine.io/apidoc/html/Castle2DSceneManager.T2DSceneManager.html">T2DSceneManager</a> and <a href="https://castle-engine.io/apidoc/html/Castle2DSceneManager.T2DScene.html">T2DScene</a>. See <code>castle_game_engine/examples/2d_spine_game/</code> for example of use.
  <li>Many improvements for Android. See <a href="https://castle-engine.io/tutorial_mobile.php">new tutorial page about developing mobile games</a>. Try our "portable games skeleton" code in <code>castle_game_engine/examples/portable_game_skeleton</code>.
  <!-- This includes <a href="https://castle-engine.io/apidoc/html/CastleWindow.TCastleApplication.html#OnInitialize">Application.OnInitialize</a> improvements (it now happens when GL context is ready, so you can really initialize your whole game there, with progress bar), fixes to <a href="https://castle-engine.io/apidoc/html/CastleWindowTouch.TCastleWindowTouch.html#AutomaticTouchInterface">AutomaticTouchInterface</a> (it was not cooperating with our progress bar tricks previously). --> Also Android emulator is working now with our programs.
  <li>New X3D extensions (used by our Spine loader, but may also be useful for other purposes too):
    <ul>
      <li><a href="https://castle-engine.io/x3d_implementation_navigation_extensions.php#section_ext_blending_sort">NavigationInfo.blendingSort</a>
      <li><a href="https://castle-engine.io/x3d_implementation_interpolation_extensions.php#section_cubic_bezier_interpolator">CubicBezierXxx interpolators</a>
      <li><a href="https://castle-engine.io/x3d_implementation_texturing_extensions.php#section_texture_properties_gui_texture">TextureProperties.guiTexture </a>
      <li><a href="https://castle-engine.io/x3d_implementation_eventutilities_extensions.php#section_forceContinuousValue_Changed">X3DSequencerNode.forceContinuousValue_Changed</a>
    </ul>
  <li>High-quality image scaling, using interpolation modes like Mitchell, Blackman, Lanczos, Gaussian. See <a href="https://castle-engine.io/apidoc/html/CastleImages.TCastleImage.html#MakeResized">TCastleImage.MakeResized</a> and <a href="https://castle-engine.io/apidoc/html/CastleImages.html#TResizeNiceInterpolation">TResizeNiceInterpolation</a>. The implementation uses excellent fcl-image library underneath.
  <li>Named animations: Our engine detects named animations by looking at X3D TimeSensor node names. Name starting with "Animation_xxx" indicates an animation, with name "xxx". Some converters (right now, our Spine to X3D conversion) follow this convention, so our engine immediately "knows" the Spine animation names and can play them. The benefits:
    <ul>
      <li>view3dscene displays a user-friendly menu in <i>"Animation->Named Animations->..."</i> to run animation by name.</li>
      <li>TCastleScene offers a simple API for programmers to browse and run animation by name, see <a href="https://castle-engine.io/apidoc/html/CastleSceneCore.TCastleSceneCore.html#PlayAnimation">TCastleScene.PlayAnimation</a>.
    </ul>
</ol>

<p><b>Also be sure to install on your Android device a demo of Castle Game Engine + Spine</b>. It\'s completely free of course, and the source code is one of the examples in Castle Game Engine sources.

<div>
<a href="https://play.google.com/store/apps/details?id=net.sourceforge.castleengine.castlespine">
  <img alt="Android app on Google Play"
       src="https://developer.android.com/images/brand/en_app_rgb_wo_60.png" />
</a>
</div>

<p><i>"Dragon Spine"</i> is also <a href="https://sourceforge.net/projects/castle-engine/files/castle_spine/">available for Windows or Linux</a>. You can also <a href="https://www.youtube.com/watch?v=AuI4zgmT-YQ">watch the video!</a>.

<!-- <div class="download"> -->
<!-- <a href="' . CURRENT_URL . 'view3dscene.php">Download view3dscene</a> -->
<!-- </div> -->

<p><b>Check out also some games using <a href="' . CURRENT_URL . 'engine.php">Castle Game Engine</a></b>:

<ul>
  <li><a href="https://www.facebook.com/Venicethegame">Venice &mdash; new 2D adventure game with superb graphics and storyline, in the making!</a>
  <li><a href="https://castle-engine.io/mountains_of_fire.php">Mountains Of Fire &mdash; split-screen cooperative survival game</a>
  <li><a href="https://github.com/michaliskambi/frogger3d">Frogger 3D (initial version done during 1-hour gamejam by Michalis!:)</a>
</ul>
'),

    array('title' => 'Castle Game Engine 5.0.0 release (Android, iOS, more), view3dscene 3.14.0 release',
          'year' => 2014,
          'month' => 5,
          'day' => 1,
          'short_description' => '',
          'guid' => '2014-05-01',
          'images' => array(
            array('filename' => 'darkest_before_dawn_1.png', 'titlealt' => 'Screenshot from &quot;Darkest Before the Dawn&quot;'),
            array('filename' => 'darkest_before_dawn_2.png', 'titlealt' => 'Screenshot from &quot;Darkest Before the Dawn&quot;'),
            array('filename' => 'darkest_before_dawn_ui.png', 'titlealt' => '&quot;Darkest Before the Dawn&quot; - title screen'),

            array('filename' => 'font_from_image_screen_0.png', 'titlealt' => 'Font from colorful image'),
            array('filename' => 'font_from_texture.png', 'titlealt' => 'Font from image and ttf files'),
            array('filename' => 'compare_anti_aliasing.png', 'titlealt' => 'Compare font with and without antialiasing'),

            array('filename' => 'android_cubemap_1.png', 'titlealt' => 'Cubemap reflections on Android'),
            array('filename' => 'android_progress_bar.png', 'titlealt' => 'Loading progress bar on Android'),
            array('filename' => 'android_message.png', 'titlealt' => 'Modal message box on Android'),

            array('filename' => 'little_things_screen_0.png', 'titlealt' => 'Screenshot from &quot;Little Things&quot;'),
            array('filename' => 'little_things_screen_10.png', 'titlealt' => 'Screenshot from &quot;Little Things&quot;'),
            array('filename' => 'little_things_screen_2.png', 'titlealt' => 'Screenshot from &quot;Little Things&quot;'),
            array('filename' => 'little_things_screen_4.png', 'titlealt' => 'Screenshot from &quot;Little Things&quot;'),
            array('filename' => 'little_things_screen_5.png', 'titlealt' => 'Screenshot from &quot;Little Things&quot;'),
            array('filename' => 'little_things_screen_7.png', 'titlealt' => 'Screenshot from &quot;Little Things&quot;'),
            array('filename' => 'little_things_screen_8.png', 'titlealt' => 'Screenshot from &quot;Little Things&quot;'),

            array('filename' => 'new_walk_shortcuts.png', 'titlealt' => 'Tooltip with summary of new view3dscene AWSD controls'),

            array('filename' => 'orcs_and_volcanoes_screen-intro.png', 'titlealt' => 'Orcs and Volcanoes - intro screen'),
            array('filename' => 'orcs_and_volcanoes_08.png', 'titlealt' => 'Orcs and Volcanoes'),
            array('filename' => 'orcs_and_volcanoes_04.png', 'titlealt' => 'Orcs and Volcanoes'),

            array('filename' => 'fps_game_fancy_item_box_2.png', 'titlealt' => 'Custom image under selected item, 2.'),
            array('filename' => 'view3dscene_tooltip_rounded.png', 'titlealt' => 'view3dscene tooltips with rounded corners.'),
            array('filename' => 'view3dscene_message_buttons.png', 'titlealt' => 'Message dialog with nice buttons.'),
            array('filename' => 'castle_scrollbar.png', 'titlealt' => 'New dialog look with a nice scrollbar and button.'),
            array('filename' => 'view3dscene_scrollbar.png', 'titlealt' => 'New dialog look with a nice scrollbar and button.'),
          ),
          'description' =>
'<p>We\'re proud to announce the release of <i>Castle Game Engine</i> version 5.0.0:)</p>
<!-- teaser -->

<p>We also release <a href="' . CURRENT_URL . 'view3dscene.php">view3dscene</a> 3.14.0, our 3D model browser (packed with many graphic effects) and converter. And, for fun, checkout also a new 1.2.0 release of <a href="' . CURRENT_URL . 'darkest_before_dawn.php">Darkest Before the Dawn</a>, a small scary game for Android, Linux and Windows &mdash; of course done using our engine.

<p><b>Major new features of this release:</b></p>

<ol>
  <li><p><b>Android support</b></p>

    <p>Using OpenGLES 2.0 and NativeActivity underneath. Works on any Android &gt;= 2.3 with OpenGL ES 2.0. Most of the normal 2D and 3D rendering features are working under Android, including fun stuff like shader effects, screen effects (with depth, like SSAO), multi-texturing, cubemaps, fog... The integration with Android is very comfortable &mdash; although the code is compiled in a library (controlled by the main activity) you still have normal message loop (so you can use modal functions like from <a href="https://castle-engine.io/apidoc/html/CastleMessages.html">CastleMessages</a> unit). Logging is also integrated with Android logging. Reading from Android assets (files packed inside Android apk) is immediately available, just use URL like <code>assets:/my_texture.png</code>. Multi-touch is fully supported.</p>

    <p>More information about using the Android port on the <a href="https://github.com/castle-engine/castle-engine/wiki/Android">Android development</a> wiki page.</p>

  <li><p><b>iOS (iPhone, iPad) support</b></p>

    <p>Like on Android, rendering is done using OpenGLES 2.0, and most of the normal rendering features just work :) The engine is wrapped in a library controlled by the main iOS application.</p>

    <p>More information about using the iOS port is on <a href="https://github.com/castle-engine/castle-engine/wiki/iOS">iOS development</a> wiki page.</p>

  <li><p><b>Comfortable API for 2D rendering</b></p>

    <p>Our 2D rendering functions and components were reworked, to provide much more flexible (and also simpler) API for game developers. Making a 2D game, or just adding 2D HUD on top of your 3D game, is now a breeze :) This includes a couple of new features:

    <ul>
      <li><p><a href="https://castle-engine.io/apidoc/html/CastleGLImages.TGLImage.html">TGLImage</a> is now the main class to draw images in 2D. It renders 2D images as non-power-of-two textures underneath, which makes it suitable for modern OpenGL and GLES, and better for anti-aliasing. The <a href="https://castle-engine.io/apidoc/html/CastleGLImages.TGLImage.html#Draw">Draw</a> method can stretch the image, and the <a href="https://castle-engine.io/apidoc/html/CastleGLImages.TGLImage.html#Draw3x3">Draw3x3</a> method can even intelligently stretch the image while preserving constant-size corners. It also automatically uses alpha channel (for alpha test or alpha blending, you can also force specific treatment using <a href="https://castle-engine.io/apidoc/html/CastleGLImages.TGLImage.html#Alpha">TGLImage.Alpha</a> property). Property <a href="https://castle-engine.io/apidoc/html/CastleGLImages.TGLImage.html#Color">TGLImage.Color</a> allows to easily color the rendered image. <a href="https://castle-engine.io/tutorial_player_2d_controls.php">Tutorial chapter about 2D controls contains various usage examples</a>.

      <li><p>All <a href="https://castle-engine.io/apidoc/html/CastleControls.html">CastleControls</a> components use now <code>TGLImage</code> for all of their rendering. They use <a href="https://castle-engine.io/apidoc/html/CastleControls.TCastleTheme.html">TCastleTheme</a> (as <code>Theme</code> global instance) to get their images, this way you can easily change the look of all controls defined in <a href="https://castle-engine.io/apidoc/html/CastleControls.html">CastleControls</a>.

      <li><p>Dialog boxes in <a href="https://castle-engine.io/apidoc/html/CastleMessages.html">CastleMessages</a> unit are also using <code>TGLImage</code> for the rendering. They also use normal buttons at the bottom, so they are clickable and generally look and behave like normal dialog windows with normal buttons :) Same thing for the progress bar.

      <li><p>We consistently use <code>TCastleColor</code> (alias for <code>TVector4Single</code>) for all the color properties. The alpha value of the color is honored everywhere.

      <li><p>Image and text drawing routines discourage now from using global state, like "current color" or "current raster position". You should use <a href="https://castle-engine.io/apidoc/html/CastleGLImages.TGLImage.html#Draw">TGLImage.Draw</a> versions that take explicit draw position, and <a href="https://castle-engine.io/apidoc/html/CastleFonts.TCastleFont.html#Print">TCastleFont.Print</a> versions that take explicit draw position and color.
    </ul>

  <li><p><b>Modern font loading and rendering</b></p>

     <p>We now use the FreeType library for loading fonts, which allows us to load at runtime a font from a ttf file, and use it with any size, with or without anti-aliasing. This makes the font rendering modern (no display lists, just a single texture), and working with GLSL and OpenGLES20 (Android, iOS), and suitable both for anti-aliased and non-anti-aliased text (resulting in alpha blending or alpha testing).

     <p>It is also possible to convert ttf font to a Pascal code, to easily embed the fonts inside Pascal program, and avoid the need for FreeType library at runtime (which also avoids the needs to worry about linking with FreeType on Android/iOS). The program do it is texturefont2pascal (see <code>castle_game_engine/examples/fonts/</code>).

     <p>Important font classes are called now <a href="https://castle-engine.io/apidoc/html/CastleFonts.TTextureFont.html">TTextureFont</a> and (abstract) <a href="https://castle-engine.io/apidoc/html/CastleFonts.TCastleFont.html">TCastleFont</a>. TTextureFont is either loaded from ttf or from prepared data (when TTextureFontData was converted to a Pascal code). There is also new <a href="https://castle-engine.io/apidoc/html/CastleFonts.TSimpleTextureFont.html">TSimpleTextureFont</a> to draw a colorful text from glyphs in an image.

     <p>Check out new example <code>castle_game_engine/examples/fonts/font_from_texture.lpr</code> for usage examples.
</ol>

<p>By the way, on the side of this post, you can see screenshots from three games using our engine done in weekend "gamejams" (<a href="https://www.facebook.com/tsgcompo">see Ten Square Games COMPO</a>). There is <i>"Orcs and Volcanoes"</i>, roguelike with pixelart graphics, with some 3D twists and real-time action. There\'s <a href="' . CURRENT_URL . 'darkest_before_dawn.php">"Darkest Before the Dawn"</a>, a scary 3D game for Android. And there\'s <i>"Little Things"</i>, a sweet colorful tiny game about a brave boy (you can get and compile it from our SVN, just checkout <code>http://svn.code.sf.net/p/castle-engine/code/trunk/little_things/</code>).

<p><b>The rest of new engine and view3dscene features:</b></p>

<ul>
  <li><p><b>Engine can be compiled and used as a shared library</b> (<code>castleengine.dll</code> on Windows, <code>libcastleengine.so</code> on Linux), <b>with API accessible from C or C++</b>. We provide a C header in <code>src/library/castlelib.h</code> for this purpose. See code in <code>src/library/</code> and tests in <code>examples/library/</code> .

  <li><p><b>You can already develop cross-target games (for Android, iOS, or standalone  normal OSes like Linux or Windows) using the engine</b>. This means that a single source code can be used to compile multiple versions of the game. Some more plans for Android/iOS:

    <ul>
      <li>We plan to make a "build tool" for the engine to auto-generate the necessary files for the compilation on given target (although it\'s unsure if this will be ready for next release). See <a href="https://sourceforge.net/p/castle-engine/wiki/Planned%3A%20build%20tool/">Planned: build tool wiki page</a>, and drop a comment if you\'re interested!
      <li>List of <a href="https://sourceforge.net/p/castle-engine/wiki/OpengLES%2C%20Android%20and%20iOS%20TODOs/">missing features on Android and iOS is documented here</a>. Help in implementing them is very much welcome :)
    </ul>

  <li><p><b>The default TWalkCamera inputs are now equal to TPlayer inputs, in particular they honour common AWSD combo.</b></p>

  <li><p><b><a href="http://www.web3d.org/files/specifications/19775-1/V3.2/Part01/components/enveffects.html#TextureBackground">TextureBackground</a> support</b>, making it possible to use MovieTexture as skybox sides. The rendering of Background and TextureBackground uses new simplified code, that can utilize our texture cache and works on OpenGLES too.

  <li><p><b>Context resource sharing</b> (so that many windows/controls work OK, sharing textures and fonts and such) implemented for CastleWindow Xlib+GLX and GTK backends.</p>

  <li><p>Support for png and gz formats without any external libraries (using FpRead/WritePng and PasZlib underneath). This is particularly useful for Android and iOS, where linking with external libraries is not so easy.

  <li><p>CastleEnumerateFiles API much changed and renamed to CastleFindFiles. It supports searching for files inside Android assets too (although, unfortunately, not recursive &mdash; because of NDK API limitations).

  <li><p>Font lifetime is no longer limited to the OpenGL context. This means that you can create and use TCastleFont or TTextureFont instances more freely, e.g. create them once at the application initialization (no need to repeat it in every OnOpen), and measure text (e.g. calling TextWidth) at any time.

  <li><p>The <code>Send</code> method of X3D events (like <a href="https://castle-engine.io/apidoc/html/X3DFields.TSFFloatEvent.html#Send">TSFFloatEvent.Send</a>) is now safer to use. Now all EventXxx properties have a specialized type, like TSFBoolEvent or TSFStringEvent, and you can only call Send with proper parameters. <!-- (Previously all events were of TX3DEvent class, and Send() was overloaded for all types. This made mistakes in values possible to detect only at runtime, by catching EInvalidCast errors. Now they are catched at compile time.) -->

  <li><p>The ARCHITECTURE mode was renamed to TURNTABLE, following InstantReality mode that has a similar purpose.

  <li><p>The DrawStyle, OnDrawStyle, Draw, OnDraw renamed to RenderStyle and Render.

  <li><p>New class TUIContainer makes it easier to create and use containers (like the ones provided by TCastleWindow and TCastleControl). Various small fixes and improvements come as a result of that. You may need to adjust your window callbacks to take "Container: TUIContainer" parameter (although we added compatibility alias TCastleWindowBase = TUIContainer to make it possible to still compile old code; but remember that now TUIContainer and TCastleWindowCustom are totally separate classes). This also cleaner reflects that the basis for all engine rendering (2D and 3D) is now TUIControl. So all container providers (TCastleWindowCustom, TCastleControlCustom) give you Controls list with the same behaviour.

  <li><p>The Android docs contain also a section about <a href="https://sourceforge.net/p/castle-engine/wiki/Android%20development/#debugging-running-application-on-an-android-device-using-ndk-gdb">"Debugging running application (on an Android device) using ndk-gdb"</a> &mdash; yes, it works flawlessly :)

  <li><p><b>Multi-touch is implemented :)</b>

    <ol>
      <li>There is an example program in <code>castle_game_engine/examples/android/drawing_toy/</code> where you can draw on the screen, with each finger index drawing with a different color. Try drawing with 2-5 fingers simultaneously :) It\'s pretty fun, and allows you to test the multi-touch support on Android :)

      <li>Multi-touch API changes:
        <ol>
          <li><b>Press/Release</b>: <a href="https://castle-engine.io/apidoc/html/CastleKeysMouse.TInputPressRelease.html">TInputPressRelease</a> structure (which is a parameter for OnPress/OnRelease events) has a new field FingerIndex. On devices with a single normal mouse (no touch), like on desktops, FingerIndex is always 0.
          <li><b>Motions</b>: TUIControl has new method <a href="https://castle-engine.io/apidoc/html/CastleUIControls.TInputListener.html#Motion">Motion</a>, and TCastleWindow/TCastleControl have new callback <a href="https://castle-engine.io/apidoc/html/CastleWindow.TCastleWindowCustom.html#OnMotion">OnMotion</a>. They get a parameter <a href="https://castle-engine.io/apidoc/html/CastleKeysMouse.TInputMotion.html">TInputMotion</a>, that contains new and old position, a FingerIndex, and Pressed describing which buttons are pressed (for mouse, this corresponds to actual buttons pressed, may be [] if none; for touch device, it is always [mbLeft]; this way you can just detect any dragging by Pressed <> []).
          <li><b>Current state</b>: TCastleWindow and TCastleControl have a list of <a href="https://castle-engine.io/apidoc/html/CastleWindow.TCastleWindowCustom.html#Touches">Touches</a>, listing current state of the touches. It\'s length varies (can be zero if not touching). Right now, the state of touch is just it\'s position in 2D and FingerIndex, but we may extend it in the future. See comments at <a href="https://castle-engine.io/apidoc/html/CastleUIControls.TTouch.html">CastleUIControls.TTouch</a> type.
          <li>You can use MousePosition to get position of touch with FingerIndex = 0, if any (will return zeros if no such touch). Consistently, the state of mbLeft in MousePressed says whether the touch with FingerIndex = 0 currently exists. The effect is that code that looks only at MousePosition/MousePressed will somewhat work with touch devices, treating them as a device with a mouse with a single (mbLeft) button (that mysteriously doesn\'t report move events when button is not pressed).
          <li>The mouse/touch positions are changed to floats (not ints), to support devices with sub-pixel precision.
          <li>The mouse/touch position has Y going from bottom to top, just like our 2D drawing routines. No more the need to invert MouseY.
        </ol>
    </ol>
  </li>
</ul>

<p>Have fun using the engine! We hope to see some nice games in the nearest future :) And visit our <a href="https://sourceforge.net/p/castle-engine/discussion/">forums</a> if you have any questions!
'),

    array('title' => 'Development: Android and iOS, new game release "Darkest Before the Dawn", more',
          'year' => 2014,
          'month' => 1,
          'day' => 2,
          'short_description' => '',
          'guid' => '2014-01-02',
          'description' =>
castle_thumbs(array(
  array('filename' => 'darkest_before_dawn_1.png', 'titlealt' => 'Screenshot from &quot;Darkest Before the Dawn&quot;'),
  array('filename' => 'darkest_before_dawn_2.png', 'titlealt' => 'Screenshot from &quot;Darkest Before the Dawn&quot;'),
  array('filename' => 'gles-2.png', 'titlealt' => 'One of the first 2D programs to run with OpenGLES renderer - &quot;isometric_game&quot; from engine examples'),
  array('filename' => 'cge-android-demo-1.png', 'titlealt' => 'One of the first 3D programs to run with OpenGLES renderer - &quot;android_demo&quot; from engine examples'),
  array('filename' => 'navigation_controls.png', 'titlealt' => 'Summary of new view3dscene AWSD controls'),
  array('filename' => 'new_walk_shortcuts.png', 'titlealt' => 'Tooltip with summary of new view3dscene AWSD controls'),
)) .
'<p>Hello everyone in 2014 :)

<p>First of all, I would like to present a small game I did
during a weekend "gamejam" at the end of November 2013:

<h3><a href="darkest_before_dawn.php">Darkest Before the Dawn</a></h3>

<p>The game is free to download for Android, Linux or Windows.
Of course the game uses our <a href="' . CURRENT_URL . 'engine.php">Castle
Game Engine</a> for everything. The complete game code and data are
available in our SVN repository.

<p>Next, we have news about the engine development:

<ol>
  <li>Yes, you have heard right: <b>the engine supports Android and iOS (iPhone, iPad)</b>.

    <p>The rendering is done using an OpenGL ES 2.0 renderer. Most of the rendering features work (and the rest is in-progress :). Rendering 3D shapes with shaders, textures, lighting of course works. Note that mobile shaders use the Gouraud shading for speed. Rendering 2D images (<a href="https://castle-engine.io/apidoc/html/CastleGLImages.TGLImage.html">TGLImage</a>) is also done. Since <a href="https://castle-engine.io/apidoc/html/CastleGLImages.TGLImage.html">TGLImage</a> is used underneath by all our 2D controls &mdash; most of 2D GUI works (except fonts, for now...).

    <p>You can compile the whole engine for OpenGLES (as opposed to normal desktop OpenGL) by defining symbol <code>OpenGLES</code> in <code>base/castleconf.inc</code> file. This symbol is defined automatically when we compile for Android or iOS, but you can also test OpenGLES renderer on normal systems like Windows and Linux (it\'s a nice way to test your mobile game on a desktop system).

    <p>We can initialize OpenGLES context using the EGL library. This is useful to initialize OpenGLES on Android or Xlib (with glX+EGL) or Windows (with wgl+EGL).

    <p>The Android port uses <a href="http://developer.android.com/reference/android/app/NativeActivity.html">NativeActivity</a>, available since Android 2.3. It requires FPC 2.7.1 to cross-compile code to Android ARM, see <a href="http://wiki.freepascal.org/Android">FPC wiki about Android</a>. Integration with Android includes using Android\'s log facility (just use <a href="' . CURRENT_URL . 'tutorial_log.php">WritelnLog from CastleLog</a> unit) and using Android\'s assets (URLs like <code>assets:/my_texture.png</code> are supported, and <a href="https://castle-engine.io/apidoc/html/CastleFilesUtils.html#ApplicationData">ApplicationData</a> returns <code>assets:/</code> to read data from apk).

    <p>You can develop cross-target games (for Android, iOS, or standalone &mdash; normal OSes like Linux or Windows) using the engine. This means that a single source code can be used to compile multiple versions of the game. We also have plans to make a "build tool" for the engine to auto-generate the necessary files for the compilation on given target (although it\'s unsure if this will be ready for next release). See <a href="https://sourceforge.net/p/castle-engine/wiki/Planned:%20build%20tool/">Planned: build tool</a> wiki page, and drop a comment if you\'re interested!

    <p>' . (!HTML_VALIDATION ? '<iframe width="560" height="315" src="//www.youtube.com/embed/dVGZ3lGrrVc?rel=0" frameborder="0" allowfullscreen></iframe>' : '') . '

  <li><b>Engine can be compiled and used as a shared library</b> (<code>castleengine.dll</code> on Windows, <code>libcastleengine.so</code> on Linux), <b>with API accessible from C or C++</b>. We provide a C header in <code>src/library/castlelib.h</code> for this purpose. See code in <code>src/library/</code> and tests in <code>examples/library/</code> .

  <li><b>The default <a href="https://castle-engine.io/apidoc/html/CastleCameras.TWalkCamera.html">TWalkCamera</a> inputs are now equal to <a href="https://castle-engine.io/apidoc/html/CastlePlayer.TPlayer.html">TPlayer</a> inputs, in particular they honour common AWSD combo</b>.
    <ul>
      <li>You can move using AWSD by default (e.g. in <a href="https://castle-engine.io/view3dscene.php">view3dscene</a>).
      <li><i>Space / c</i> keys make <i>jump / crouch</i>. We no longer have separate inputs for jump / crouch (when gravity works) or flying up / down (when gravity doesn\'t work).
      <li>This avoids switching the meaning or left / right arrows in mouse look mode in view3dscene.
      <li>This makes keys in all our programs and games more consistent. And everyone knows AWSD, while previous shortcuts for strafing (comma and dot) were quite uncommon.
      <li>Of course, all TWalkCamera inputs remain configurable. So, if you really liked previous key shortcuts, you can restore them for your application.
    </ul>
  <li><b><a href="http://www.web3d.org/files/specifications/19775-1/V3.2/Part01/components/enveffects.html#TextureBackground">TextureBackground</a> support, making it possible to use <code>MovieTexture</code> as skybox sides</b>. The rendering of <code>Background</code> and <code>TextureBackground</code> uses new simplified code, that can utilize our texture cache and works on OpenGLES too.
  <li>Notes about <b><a href="' . CURRENT_URL . 'tutorial_transformation_hierarchy.php">transformation hierarchy</a></b> added to the documentation.
  <li>Context resource sharing (so that <b>many windows/controls work OK, sharing textures and fonts and such</b>) implemented for CastleWindow Xlib+GLX and GTK backends.
  <li>Support for <i>png</i> and <i>gz</i> formats without any external libraries (using FpRead/WritePng and PasZlib underneath). This is particularly useful for Android and iOS, where linking with external libraries is not so easy.
  <li>CastleEnumerateFiles API much changed and renamed to <a href="https://castle-engine.io/apidoc/html/CastleFindFiles.html">CastleFindFiles</a>. It supports searching for files inside Android assets too (although, unfortunately, not recursive &mdash; because of NDK API limitations).
  <li><code>--hide-menu</code> option implemented for view3dscene. Useful e.g. for fullscreen presentations, where you may want to hide all UI.
</ol>
')
);
