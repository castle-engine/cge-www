<?php

/* Next news:
font_from_image_screen_0.png
<li>New example examples/fonts/font_from_image.lpr showing how to use a font painted as an image.
<li>The <a href=Send method of X3D events</a> is now safer to use. Now all EventXxx properties have a specialized type, like TSFBoolEvent or TSFStringEvent, and you can only call Send with proper parameters. <!-- (Previously all events were of TX3DEvent class, and Send() was overloaded for all types. This made mistakes in values possible to detect only at runtime, by catching EInvalidCast errors. Now they are catched at compile time.) -->
<li>The ARCHITECTURE mode was renamed to TURNTABLE, following InstantReality mode that has a similar purpose.
*/

array_push($news,
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

    <p>The rendering is done using an OpenGL ES 2.0 renderer. Most of the rendering features work (and the rest is in-progress :). Rendering 3D shapes with shaders, textures, lighting of course works. Note that mobile shaders use the Gouraud shading for speed. Rendering 2D images (<a href="http://castle-engine.sourceforge.net/apidoc/html/CastleGLImages.TGLImage.html">TGLImage</a>) is also done. Since <a href="http://castle-engine.sourceforge.net/apidoc/html/CastleGLImages.TGLImage.html">TGLImage</a> is used underneath by all our 2D controls &mdash; most of 2D GUI works (except fonts, for now...).

    <p>You can compile the whole engine for OpenGLES (as opposed to normal desktop OpenGL) by defining symbol <tt>OpenGLES</tt> in <tt>base/castleconf.inc</tt> file. This symbol is defined automatically when we compile for Android or iOS, but you can also test OpenGLES renderer on normal systems like Windows and Linux (it\'s a nice way to test your mobile game on a desktop system).

    <p>We can initialize OpenGLES context using the EGL library. This is useful to initialize OpenGLES on Android or Xlib (with glX+EGL) or Windows (with wgl+EGL).

    <p>The Android port uses <a href="http://developer.android.com/reference/android/app/NativeActivity.html">NativeActivity</a>, available since Android 2.3. It requires FPC 2.7.1 to cross-compile code to Android ARM, see <a href="http://wiki.freepascal.org/Android">FPC wiki about Android</a>. Integration with Android includes using Android\'s log facility (just use <a href="' . CURRENT_URL . 'tutorial_log.php">WritelnLog from CastleLog</a> unit) and using Android\'s assets (URLs like <tt>assets:/my_texture.png</tt> are supported, and <a href="http://castle-engine.sourceforge.net/apidoc/html/CastleFilesUtils.html#ApplicationData">ApplicationData</a> returns <tt>assets:/</tt> to read data from apk).

    <p>You can develop cross-target games (for Android, iOS, or standalone &mdash; normal OSes like Linux or Windows) using the engine. This means that a single source code can be used to compile multiple versions of the game. We also have plans to make a "build tool" for the engine to auto-generate the necessary files for the compilation on given target (although it\'s unsure if this will be ready for next release). See <a href="https://sourceforge.net/p/castle-engine/wiki/Planned:%20build%20tool/">Planned: build tool</a> wiki page, and drop a comment if you\'re interested!

    <p>' . (!HTML_VALIDATION ? '<iframe width="560" height="315" src="//www.youtube.com/embed/8u7DggGe_Uk?rel=0" frameborder="0" allowfullscreen></iframe>' : '') . '

  <li><b>Engine can be compiled and used as a shared library</b> (<tt>castleengine.dll</tt> on Windows, <tt>libcastleengine.so</tt> on Linux), <b>with API accessible from C or C++</b>. We provide a C header in <tt>src/library/castlelib.h</tt> for this purpose. See code in <tt>src/library/</tt> and tests in <tt>examples/library/</tt> .

  <li><b>The default <a href="http://castle-engine.sourceforge.net/apidoc/html/CastleCameras.TWalkCamera.html">TWalkCamera</a> inputs are now equal to <a href="http://castle-engine.sourceforge.net/apidoc/html/CastlePlayer.TPlayer.html">TPlayer</a> inputs, in particular they honour common AWSD combo</b>.
    <ul>
      <li>You can move using AWSD by default (e.g. in <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a>).
      <li><i>Space / c</i> keys make <i>jump / crouch</i>. We no longer have separate inputs for jump / crouch (when gravity works) or flying up / down (when gravity doesn\'t work).
      <li>This avoids switching the meaning or left / right arrows in mouse look mode in view3dscene.
      <li>This makes keys in all our programs and games more consistent. And everyone knows AWSD, while previous shortcuts for strafing (comma and dot) were quite uncommon.
      <li>Of course, all TWalkCamera inputs remain configurable. So, if you really liked previous key shortcuts, you can restore them for your application.
    </ul>
  <li><b><a href="http://www.web3d.org/files/specifications/19775-1/V3.2/Part01/components/enveffects.html#TextureBackground">TextureBackground</a> support, making it possible to use <tt>MovieTexture</tt> as skybox sides</b>. The rendering of <tt>Background</tt> and <tt>TextureBackground</tt> uses new simplified code, that can utilize our texture cache and works on OpenGLES too.
  <li>Notes about <b><a href="' . CURRENT_URL . 'tutorial_transformation_hierarchy.php">transformation hierarchy</a></b> added to the documentation.
  <li>Context resource sharing (so that <b>many windows/controls work OK, sharing textures and fonts and such</b>) implemented for CastleWindow Xlib+GLX and GTK backends.
  <li>Support for <i>png</i> and <i>gz</i> formats without any external libraries (using FpRead/WritePng and PasZlib underneath). This is particularly useful for Android and iOS, where linking with external libraries is not so easy.
  <li>CastleEnumerateFiles API much changed and renamed to <a href="http://castle-engine.sourceforge.net/apidoc/html/CastleFindFiles.html">CastleFindFiles</a>. It supports searching for files inside Android assets too (although, unfortunately, not recursive &mdash; because of NDK API limitations).
  <li><tt>--hide-menu</tt> option implemented for view3dscene. Useful e.g. for fullscreen presentations, where you may want to hide all UI.
</ol>
')
);
