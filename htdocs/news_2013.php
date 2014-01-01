<?php

array_push($news,
    array('title' => 'Development: great API for 2D games and UI, our engine in Debian, on the road to OpenGL ES, Orcs and Volcanoes game screenshots',
          'year' => 2013,
          'month' => 9,
          'day' => 15,
          'short_description' => '',
          'guid' => '2013-09-15',
          'description' =>
castle_thumbs(array(
  array('filename' => 'orcs_and_volcanoes_screen-intro.png', 'titlealt' => 'Orcs and Volcanoes - intro screen'),
  array('filename' => 'orcs_and_volcanoes_07.png', 'titlealt' => 'Orcs and Volcanoes'),
  array('filename' => 'orcs_and_volcanoes_08.png', 'titlealt' => 'Orcs and Volcanoes'),
  array('filename' => 'orcs_and_volcanoes_04.png', 'titlealt' => 'Orcs and Volcanoes'),
  array('filename' => 'orcs_and_volcanoes_02.png', 'titlealt' => 'Orcs and Volcanoes'),
  array('filename' => 'orcs_and_volcanoes_10.png', 'titlealt' => 'Orcs and Volcanoes'),
  array('filename' => 'octopus_animation.png', 'titlealt' => 'Octopus exported from Blender to KAnim.'),
  array('filename' => 'model_3d_with_2d_controls_round_tooltip.png', 'titlealt' => 'Tooltips with rounded corners over partially-transparent buttons.'),
  array('filename' => 'fps_game_fancy_item_box.png', 'titlealt' => 'Custom image under selected item.'),
  array('filename' => 'fps_game_fancy_item_box_2.png', 'titlealt' => 'Custom image under selected item, 2.'),
  array('filename' => 'view3dscene_tooltip_rounded.png', 'titlealt' => 'view3dscene tooltips with rounded corners.'),
  array('filename' => 'view3dscene_message_buttons.png', 'titlealt' => 'Message dialog with nice buttons.'),
  array('filename' => 'castle_scrollbar.png', 'titlealt' => 'New dialog look with a nice scrollbar and button.'),
  array('filename' => 'view3dscene_scrollbar.png', 'titlealt' => 'New dialog look with a nice scrollbar and button.'),
)) .
'
<ol>
  <li><p>Great news for <a href="http://www.debian.org/">Debian</a> users: <a href="http://packages.debian.org/jessie/fp-units-castle-game-engine">Castle Game Engine package is available inside official Debian repositories</a>. Version 4.0.1 is already available in the <i>testing</i> release, and <a href="http://packages.qa.debian.org/c/castle-game-engine.html">version 4.1.1 is in <i>unstable</i> (on the way to testing)</a>. Installing Castle Game Engine in Debian, with documentation and Lazarus integration, is now as trivial as installing standard FPC units :)</li>

  <li><p>On the side of this post you see screenshots from <i>"Orcs and Volcanoes"</i>, a game Michalis did 2 weekends ago at <a href="http://tensquaregames.com/">Ten Square Games</a> "gamejam" :) This is a roguelike with pixelart graphics, with some 3D twists and real-time action. For a weekend project (literally 36 hours of programming, no sleep) I think the result is quite cool, the game actually works and is playable :) Of course, it uses our engine for everything.

    <p>Almost all graphics from the <i>"Badass Heroes"</i> game. Used with permission, but they are not freely redistributable, so I can\'t share the game publicly... Anyway, feast your eyes on the screenshots :)

  <li><p>A lot of engine API improvements done in the recent weeks, in particular around 2D controls and images. Most of that is caused by the desire to port complete engine to OpenGL ES, and have Android/iOS version.

    <p>The nice side-effect is that the 2D API is now generally much better. Frankly, if you want to make a pure 2D game, Castle Game Engine is now an excellent choice :) Drawing 2D images and animations is now very flexible. Details:

    <p><b><tt>TGLImage</tt> (our class to render images as 2D, for GUI and 2D games) much improved</b>:
    <ul>
      <li>Renders 2D images as npot textures underneath (suitable for modern OpenGL and GLES, better with anti-aliasing).
      <li><tt>TGLImage.Draw</tt> can display image stretched (if ScalingPossible, OpenGL bilinear filtering will stretch it nicely).
      <li><tt>TGLImage.Draw3x3</tt> can display image stretched intelligently, preserving corners and sides. These functions are the basis for implementing 2D GUI, e.g. <tt>TGLImage.Draw3x3</tt> is used for all <tt>TCastleButton</tt> states, which makes it also easier to theme (e.g. it\'s now possible to make rounded corners without any fuss, just make transparent corners in the texture). All <tt>CastleControls</tt> are now drawn using <tt>TGLImage</tt>.
      <li><tt>TGLImage</tt> drawing automatically uses alpha test or alpha blending, depending on alpha channel in the loaded image. You can always change <tt>TGLImage.Alpha</tt> to explicitly force specific alpha treatment. You can also change <tt>TCastleImageControl.AlphaChannel</tt>.
      <li><tt>TCastleTheme</tt> is now a configurable collection of images (with nice defaults).
    </ul>

    <p><a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/docs/tutorial_player_2d_controls.html">Tutorial about 2D controls (SVN version)</a>
    contains various examples about new <tt>TGLImage</tt> and <tt>Theme.Draw</tt> usage.

    <p><b>Dialog boxes (<tt>CastleMessages</tt> unit) improved</b>:
    <ul>
      <li>Drawn using images, so much more configurable.
      <li>Normal buttons at the bottom (descendants of our TCastleButton), so it\'s natural to handle them with mouse.
      <li>Theme for slider and frame much better (somewhat based on GTK 2 theme "clearlooks", esp. the scrollbar).
    </ul>

    <p><b>More 2D controls</b>:
    <ul>
      <li><tt>TCastleLabel</tt> (like <tt>Font.PrintStringsBox</tt>, but nicely wrapped in a class).
      <li><tt>TCastleDialog</tt> (to make modal or non-modal dialog boxes, with scrollbars, buttons etc).
      <li><tt>TCastleProgressBar</tt> (display progress bar as <tt>TUIControl</tt>, regardless if you use <tt>CastleWindow</tt> or <tt>CastleControl</tt>).
    </ul>

    <p>Because of the move to new TGLImage API and the rest of GLES2 changes, small compatibility breakage may happen at 4.2.0 release. This concerns you if you do direct OpenGL calls or use low-level tricks from CastleGLUtils. <b>Things to take into account to have a smooth upgrade to 4.2.0</b>:
    <ul>
      <li>Always use <tt>SetWindowPos</tt>, never directly use <tt>glRasterPos*</tt> functions. In general, forget about raster position &mdash; this concept is gone in GLES2 and modern OpenGL. Use only <tt>SetWindowPos</tt> to affect initial text position for <tt>TGLBitmapFont</tt> and <tt>TGLImage</tt>, this works in both engine 4.1.1 and in SVN (future 4.2.0). It is only for 2D, and is not affected by modelview matrix state.
      <li>Since 4.2.0, it will be advised to use cleaner <tt>TGLBitmapFont.Print(X,Y,Color,string)</tt> and <tt>TGLImage.Draw(X,Y)</tt> instead of <tt>SetWindowPos</tt>. But <tt>SetWindowPos</tt> will also work, to allow you to write code in the existing 4.1.1 API that will also work smoothly in engine &gt;= 4.2.0.
      <li>If you hope to keep your code portable to GLES2, of course be sure to avoid old OpenGL API. Immediate mode rendering cannot be used (use only VBO; usually, you should just use our <tt>TCastleScene</tt> for rendering all 3D, and <tt>TGLImage</tt> or <tt>TGLVideo2D</tt> for all 2D). <!--Push/pop of matrices and attributes is also not available.-->
    </ul>

    <p><b>Font API improved</b>:
    <ul>
      <li>The advised font drawing call is now <tt>Print(X,Y,Color)</tt>. Other versions are deprecated, as they lead to messy code &mdash; it\'s not nice to manage global state in <tt>WindowPos</tt>, it\'s not nice to manage global color in <tt>CurrentColor</tt>.
      <li>All font drawing can use blending for text (just pass text color with alpha < 1).
    </ul>

    <p><b>Consistent <tt>TCastleColor</tt> usage</b>. We now consistently use <tt>TVector4Single</tt> type to express a color, it\'s even aliased as <tt>TCastleColor</tt>.
    <ul>
      <li>We do not use TVector3Single (as we like having alpha available).
      <li>We do not use byte versions. There are numerous arguments in favor of using float-based color values (instead of byte-based): it allows for components &gt; 1 (useful for physically-correct rendering and some shaders inputs), it is natural to smoothly interpolate (you can also use functions in <tt>CastleColors</tt> to interpolate in HSV space), it is easier to express constants.

        <p>The only advantage of using byte-based colors is that this is what you get from RGB 32-bit color images, and you can compare it for exact equality. But this isn\'t universal anyway (there are images with float colors, like RGBE). When dealing with <tt>TRGBImage</tt> or <tt>TRGBAlphaImage</tt> you can still use byte-based colors and eventually convert using Vector4Single() and Vector4Byte() both ways (in practice 8-bit and 16-bit values will be expressed precisely as Single too).
      <li>Color constants as TVector4Single have simple names inside CastleColors, just "Yellow" instead of "Yellow4Single".
    </ul>
  </li>

  <li><p>Comfortable TAbstractGeometryNode.Solid, TAbstractGeometryNode.Convex.

  <li><p><a href="' . CURRENT_URL . 'blender.php">X3D and Kanim exporters for Blender</a> updated to work with latest Blender 2.68a.
</ol>
'),

    array('title' => 'Castle Game Engine 4.1.1, view3dscene 3.13.0 release and more',
          'year' => 2013,
          'month' => 8,
          'day' => 17,
          'short_description' => '',
          'guid' => 'release-411',
          'description' =>
castle_thumbs(array(
  array('filename' => 'view3dscene_macosx_1.png', 'titlealt' => 'view3dscene on Mac OS X, with nice icon, menu bar, file dialog'),
  array('filename' => 'view3dscene_macosx_2.png', 'titlealt' => 'view3dscene on Mac OS X, with nice icon, menu'),
  array('filename' => 'caffeine_x3d.png', 'titlealt' => 'Caffeine model from http://www.web3d.org/x3d/content/examples/Basic/ChemicalMarkupLanguage/index.html'),
  array('filename' => 'data_uri_0.png', 'titlealt' => 'data URI demo. All the textures, movies, sounds, scripts, linked 3D models here are embedded using data URI.'),
  array('filename' => 'functions_screen.png', 'titlealt' => 'MultiTexture.function test'),
  array('filename' => 'view3dscene_url.png', 'titlealt' => 'Model with textures loaded from network, URL dialog'),
  array('filename' => 'cad.png', 'titlealt' => 'CAD example model from http://www.web3d.org/wiki/index.php/X3DOM_CAD#X3D_Models'),
)) .
'<p>New 4.1.1 version of <a href="' . CURRENT_URL . 'engine.php">Castle Game Engine</a> is released! Along with it, as usual, we release <a href="' . CURRENT_URL . 'view3dscene.php">view3dscene</a> 3.13.0, our VRML/X3D browser, and update a couple of other tools/games.</p>

<p><b>New user-visible features (in Castle Game Engine and view3dscene):</b></p>

<ol>
  <li><b>Mac OS X native look and easy installation</b>. Our Mac OS X programs now have a native look, with typical Mac OS X theme and menu and dialogs. They are nicely packaged in a dmg file, with a Mac OS X "bundle" inside that you can drag to your Applications folder.<br/>
    This concerns both <a href="' . CURRENT_URL . 'view3dscene.php">view3dscene</a> and <a href="' . CURRENT_URL . 'glviewimage.php">glViewImage</a> (our handy image viewer, supporting some uncommon formats like DDS).
  <li><b>Network (http) support.</b> We can download everything from the Internet, everything is correctly treated as URL, we also use MIME-types more. For developers <a href="' . CURRENT_URL . 'tutorial_network.php">new chapter of our tutorial describing network support is available</a>.
  <li><b>More complete data URI support</b>. Absolutely everything can now use data URI to embed data inside a single VRML/X3D file. There is a demo <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/x3d/data_uri.x3dv">data_uri.x3dv</a> showing how you can use data URI to embed all kinds of things inside X3D file: textures, sounds, other 3D models (to Inline or Anchor to them), scripts etc.<br/>
    Engine examples contain a simple tool <tt>examples/tools/to_data_uri.lpr</tt> that can generate data URI from any file.
  <li><b>Clipboard</b> (Ctrl+C, Ctrl+V, Ctrl+X in message boxes, especially handy to copy/paste URLs). For developers: use <a href="' . CURRENT_URL . 'apidoc/html/CastleWindow.TCastleClipboard.html#AsText">Clipboard.AsText</a> property.
  <li>view3dscene interprets <i>Home</i> / <i>PageUp</i> / <i>PageDown</i> / <i>End</i> keys to switch to initial / next / previous / last viewpoint. This is consistent with other VRML / X3D browsers behavior and follows <a href="http://www.web3d.org/files/specifications/19775-1/V3.3/Part01/behaviours.html#SelectFromMulitpleViewpoints">recommended X3D shortcuts</a>, and it makes moving through viewpoints easier, using just a keyboard. Thanks to Don Brutzman for bringing this to my attention :)
  <li>Nice message on status when switching viewpoints. Together with Home/PageUp/PageDown/End combo, this makes switching viewpoints by keyboard very comfortable.
  <li><b>X3D CAD level 2 support (CADXxx nodes)</b>.
  <li><tt>MultiTexture.function</tt> support. <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/multi_texturing/functions.x3dv">Demo model</a>.
  <li><tt>NavigationInfo.transitionComplete</tt> support. <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/navigation/transition_multiple_viewpoints.x3dv">Demo model transition_multiple_viewpoints.x3dv</a> shows how to use it to make an animated transition between a couple of viewpoints.
  <li>Support for 8 and 16 samples for anti-aliasing.
  <li>If you load or save image sequences using the syntax <tt>image%d.png</tt>, for example inside our extension <a href="' . CURRENT_URL . 'x3d_extensions.php#section_ext_movie_from_image_sequence">Movies for MovieTexture can be loaded from images sequence</a>: the new syntax to indicate counter inside the URL is <tt>@counter(4)</tt>, where 4 is the padding. For example <tt>image%d.png</tt> has to be changed to <tt>image@counter(1).png</tt> and <tt>image%4d.png</tt> has to be changed to <tt>image@counter(4).png</tt>. See previous news for explanation why this change is necessary.
  <li>FullScreen switching much improved: you can now freely change <a href="' . CURRENT_URL . 'apidoc/html/CastleWindow.TCastleWindowBase.html#FullScreen">TCastleWindowBase.FullScreen</a> property at runtime. For backends that handle it (GTK, LCL) the switch may happen by resizing the window, instead of recreating it, which means it avoids (sometimes time-consuming) reinitialization of OpenGL resources. On Mac OS X, fullscreen mode hides the dock and menu, so they don\'t cover the window. Added <a href="' . CURRENT_URL . 'apidoc/html/CastleWindow.TMenuItemToggleFullScreen.html">TMenuItemToggleFullScreen</a> for comfort.
  <li>You can now load <a href="' . CURRENT_URL . 'creating_data_material_properties.php">material_properties.xml files</a> in view3dscene, will be used to enhance all subsequent materials. You can also specify alpha_channel (see <a href="' . CURRENT_URL . 'x3d_extensions.php#section_ext_alpha_channel_detection">alphaChannel extension</a>) in material properties. I expect to enhance this in the next release, so that you can add there also stuff like <tt>TextureProperties</tt> (for anisotropic filtering and more, right now you have to edit VRML/X3D to add it, which is not always comfortable when exporting VRML/X3D e.g. from Blender).
</ol>

<p><b>Other new engine features, visible only to developers:</b></p>

<ol>
  <li><a href="' . CURRENT_URL . 'apidoc/html/CastleWindow.html">CastleWindow</a> LCL backend. This was used to create native Mac OS X apps now, see <a href="' . CURRENT_URL . 'macosx_requirements.php">Mac OS X notes</a>.
  <li><a href="' . CURRENT_URL . 'apidoc/html/CastleFilesUtils.html#ApplicationData">ApplicationData</a> and <a href="' . CURRENT_URL . 'apidoc/html/CastleFilesUtils.html#ApplicationConfig">ApplicationConfig</a> to get data/config dirs as URLs. On Mac OS X <tt>ApplicationData</tt> can use data from bundle <tt>Contents/Resources/data</tt> . See also useful <tt>CastleFilesUtils.BundlePath</tt> function.
  <li>Nicer <a href="' . CURRENT_URL . 'apidoc/html/CastleUIControls.TInputListener.html#Update">TInputListener.Update</a> API, with simple HandleInput parameter. It used to be called <tt>Idle</tt>, but we renamed it to <tt>Update</tt>, since this describes its behavior correctly.
  <li>Various improvements to 2D rendering and API. Use <a href="' . CURRENT_URL . 'apidoc/html/CastleGLUtils.html#SetWindowPos">SetWindowPos</a>, this will also be portable to next engine compatible with GLES20 (Android, iOS).
  <li><tt>TCastleControl.AggressiveUpdate*</tt> are removed. The (simplified and improved) version of this mechanism is now always "on" and automatically makes mouse look work better. (Still not perfect, though. Lazarus event loop still causes problems with mouse look. Use our <a href="' . CURRENT_URL . 'apidoc/html/CastleWindow.html">CastleWindow</a> for smooth mouse look.)
  <li>In all programs (view3dscene and others created by Castle Game Engine) the Home / PageUp / PageDown keys loose their old meaning (they were used to raise/bow/straighten head in Walk, or rotate/reset in 3rd axis in Examine). I think that nowadays, people don\'t need keys to perform these actions, as rotating with mouse (like mouse look or mouse dragging or scroll wheel) is more intuitive and more discoverable. This way these keys are free to use for viewpoint navigation.
    <p>If you would like to restore the previous behavior just for your application, you can of course do it, since all the inputs of cameras remain configurable. Like this:

<pre class="sourcecode">
var
  Camera: TUniversalCamera;

....
{ Make sure to create a camera, if none was created yet
  (SceneManager.Camera is autocreated at first render).
  The default CreateDefaultCamera always creates
  a TUniversalCamera descendant, so the cast below is safe,
  unless you do something non-standard with cameras otherwise. }
if SceneManager.Camera = nil then
  SceneManager.Camera := SceneManager.CreateDefaultCamera;
Camera := SceneManager.Camera as TUniversalCamera;

{ Assign old Home/PageDown/PageUp meanings to Examine and Walk cameras. }
Camera.Examime.Input_Home.Assign(K_Home);
Camera.Examime.Inputs_Move[2, false].Assign(K_PageDown);
Camera.Examime.Inputs_Move[2, true ].Assign(K_PageUp);
Camera.Examime.Inputs_Rotate[2, false].Assign(K_PageDown);
Camera.Examime.Inputs_Rotate[2, true ].Assign(K_PageUp);
Camera.Walk.Input_GravityUp .Assign(K_Home);
Camera.Walk.Input_UpRotate  .Assign(K_PageUp);
Camera.Walk.Input_DownRotate.Assign(K_PageDown);
</pre>

  <li>Comfortably add modifiers (Ctrl, Shift, Alt) to menu item shortcuts by <a href="' . CURRENT_URL . 'apidoc/html/CastleWindow.TMenuItem.html#Modifiers">TMenuItem.Modifiers</a>. Previously it was somewhat-possible (by using CharKey like CtrlA..CtrlZ or uppercase letter, which requests Ctrl or Shift modifier). New approach is much more flexible.
</ol>

<p>Along with the <a href="' . CURRENT_URL . 'engine.php">engine</a> and <a href="' . CURRENT_URL . 'view3dscene.php">view3dscene</a>, we also release <a href="' . CURRENT_URL . 'glviewimage.php">glViewImage 1.5.0</a>, <a href="' . CURRENT_URL . 'castle.php">castle game 1.0.1</a>, <a href="' . CURRENT_URL . 'demo_models.php">demo models 3.3.0</a>.

<p><b>Future</b>: Jan Adamec has started work on iOS port, you can see his results in ios_tests/ directory inside SVN. And Michalis started work on porting engine renderer to GLES20. If all goes well, this will result in next engine release (4.2.0) being able to use GLES20 on mobile devices like iOS and Android :)
'),

    array('title' => 'Development: transitionComplete, Debian packages, network tutorial, data URI, MultiTexture tests, more',
          'year' => 2013,
          'month' => 5,
          'day' => 12,
          'short_description' => '',
          'guid' => 'devel-2013-05-12',
          'description' =>
castle_thumbs(array(
  array('filename' => 'caffeine_x3d.png', 'titlealt' => 'Caffeine model from http://www.web3d.org/x3d/content/examples/Basic/ChemicalMarkupLanguage/index.html'),
  array('filename' => 'data_uri_0.png', 'titlealt' => 'data URI demo. All the textures, movies, sounds, scripts, linked 3D models here are embedded using data URI.'),
  array('filename' => 'functions_screen.png', 'titlealt' => 'MultiTexture.function test'),
  array('filename' => 'modes_and_sources_screen.png', 'titlealt' => 'MultiTexture.mode and source test'),
  array('filename' => 'modes_blend_screen.png', 'titlealt' => 'MultiTexture blending modes test'),
  array('filename' => 'fireplace_final_0.png', 'titlealt' => 'Fireplace model, with fire rendered as animated image sequence'),
)) .
'<p>Various new features developed for next <a href="' . CURRENT_URL . 'engine.php">Castle Game Engine</a> and <a href="' . CURRENT_URL . 'view3dscene.php">view3dscene</a>:

<ol>
  <li><p><tt>NavigationInfo.transitionComplete</tt> support. <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/navigation/transition_multiple_viewpoints.x3dv">Demo model transition_multiple_viewpoints.x3dv</a> shows how to use it to make an animated transition between a couple of viewpoints.

  <li><p>Thanks to <i>Abou Al Montacir</i> we will have packages with <a href="' . CURRENT_URL . 'engine.php">Castle Game Engine</a> and <a href="' . CURRENT_URL . 'view3dscene.php">view3dscene</a> in <a href="http://www.debian.org/">Debian</a>! Most of this software was developed by Michalis using Debian, so having my software in the Debian repository would feel really great for me :) <a href="https://sourceforge.net/p/castle-engine/discussion/general/thread/377c403d/">See here for our forum thread</a>, and here is the Debian bug marking ITP (Intent To Package) for engine: <a href="http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=706408">#706408</a> and for view3dscene: <a href="http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=707932">#707932</a>.

  <li><p>For developers, <a href="' . CURRENT_URL . 'tutorial_network.php">new chapter of our tutorial describing network support is available</a>.

  <li><p>Engine examples contain a simple tool <tt>examples/tools/to_data_uri.lpr</tt> that can generate data URI (to embed your texture, audio, model, etc. inside a VRML/X3D model, or a webpage, or other documents) from any file. It gets the file and guesses MIME type using our existing CastleDownload unit, so it supports local files as well as http links, and MIME type is retrieved from server or guessed based on file extension.

    <p>There is a demo <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/x3d/data_uri.x3dv">data_uri.x3dv</a> showing how you can use data URI to embed all kinds of things inside X3D file: textures, sounds, other 3D models (to Inline or Anchor to them), scripts etc.

  <li><p><tt>MultiTexture.function</tt> support (forces shader pipeline rendering for given shape; there\'s no way to reasonably implement this using fixed-function pipeline). <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/multi_texturing/functions.x3dv">Demo in functions.x3dv</a>.

  <li><p><a href="' . CURRENT_URL . 'x3d_multi_texturing.php">A set of X3D multi-texturing tests</a> is available, testing support of view3dscene and other VRML / X3D browsers for multi-texture features. This is part of my ongoing effort to improve X3D MultiTexturing specification.

  <li><p>There is a progress bar showing the process the downloading. The download is still blocking, but at least now you see what\'s going on :)

  <li><p>If you load or save image sequences using the syntax <tt>image%d.png</tt>, for example inside our extension <a href="' . CURRENT_URL . 'x3d_extensions.php#section_ext_movie_from_image_sequence">Movies for MovieTexture can be loaded from images sequence</a>: the new syntax to indicate counter inside the URL will be <tt>@counter(4)</tt>, where 4 is the padding. For example <tt>image%d.png</tt> has to be changed to <tt>image@counter(1).png</tt> and <tt>image%4d.png</tt> has to be changed to <tt>image@counter(4).png</tt>.

    <p>For loading, you will <i>have</i> to use new syntax with <tt>@counter(&lt;padding&gt;)</tt> with new view3dscene / Castle Game Engine versions. You will have to update your VRML/X3D models, old syntax will unfortunately not work anymore (reasons below). For saving, the old syntax <tt>%d</tt> will continue to work for some time (along the new <tt>@counter(&lt;padding&gt;)</tt> syntax, and you\'re encouraged to upgrade to new syntax).

    <p>The reason for this is that <tt>MovieTexture.url</tt> is now correctly treated as an URL, and this means that percent character <tt>%</tt> needs to be escaped to <tt>%25</tt>. Inside URL the sequence <tt>%4d</tt> has to mean letter <tt>M</tt> (ASCII code 77, which is 4d in hexadecimal). So there is unfortunately no way to avoid breaking compatibility &mdash; we want to correctly support URLs, which implies that <tt>%4d</tt> must be interpreted as letter "M", not as a magic counter.

    <p>Looking back, it was an unfortunate choice to use percent character to indicate images sequence, since percent is special inside URIs. It was done for consistency with <tt>ffmpeg</tt>, that supports things like <tt>image%4d.png</tt> on the command-line (but <b>not</b> when using URLs; for example, <tt>ffplay /home/image%4d.png</tt> works, but <tt>ffplay file:///home/image%4d.png</tt> does not work, neither does <tt>ffplay file:///home/image%254d.png</tt>). So, one can say that it was <tt>ffmpeg</tt> that made a bad choice, but then <tt>ffmpeg</tt> did it for consistency with common string formatting functions (C sprintf, ObjectPascal Format)...

    <p>Comments about this change are of course welcome, through <a href="' . CURRENT_URL . 'forum.php">forum</a> or any other means. Right now, I just don\'t see a way to avoid breaking compatibility. We made a bad decision to use <tt>%d</tt> to indicate image sequence, and it has to change in order to correctly support URL encoding in new versions.

  <li><p>A couple of bugfixes. Including bugfix to a quite horrible mistake in <tt>ShaderPart</tt> node, in some circumstances the shader code would be reloaded from file at every frame, causing a horrible slowdown. It\'s fixed now of course.
</ol>'),

    array('title' => 'Development: better Mac OS X support, networking, CAD level 2, more',
          'year' => 2013,
          'month' => 4,
          'day' => 19,
          'short_description' => '',
          'guid' => 'devel-2013-04-19',
          'description' =>
castle_thumbs(array(
  array('filename' => 'view3dscene_macosx_1.png', 'titlealt' => 'view3dscene on Mac OS X, with nice icon, menu bar, file dialog'),
  array('filename' => 'view3dscene_macosx_2.png', 'titlealt' => 'view3dscene on Mac OS X, with nice icon, menu'),
  array('filename' => 'view3dscene_url.png', 'titlealt' => 'Model with textures loaded from network, URL dialog'),
  array('filename' => 'cad.png', 'titlealt' => 'CAD example model from http://www.web3d.org/wiki/index.php/X3DOM_CAD#X3D_Models'),
)) .
'<p>Hello everyone!</p>

<ol>
  <li><p>There is a contest for best open-source project on Polish portal <a href="http://www.spinacz.edu.pl/">http://www.spinacz.edu.pl/</a>. Please take a moment to <a href="http://www.spinacz.edu.pl/projects/project/castle-game-engine-proj64/">vote for our Castle Game Engine</a>!</p></li>

  <li><p>Our engine was submitted to devmaster.net, with lots of information and screenshots. <a href="http://devmaster.net/devdb/engines/castle-game-engine">You\'re welcome to review and rate us there!</a></p></li>

  <li><p>We also want to remind that <a href="https://plus.google.com/101185352355602218697">we have a Google+ page about our Castle Game Engine</a>, you can follow it to see (a little more frequent) news about our engine development.</p></li>
</ol>

<p>New engine features in development:</p>

<ol>
  <li><p><b>Mac OS X with native look</b>. This is already much more user-friendly than our previous GTK-based releases for Mac OS X. See <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/docs/macosx_requirements.html">development details</a>. Hopefully, this will allow us to release next <a href="' . CURRENT_URL . 'view3dscene.php">view3dscene</a> as normal Mac OS X application, that will be trivial to install for Mac OS X users.</p></li>

  <li><p><b>Network (http) support.</b> Most "FileName" parameters and properties are now URLs. You can use protocols like <tt>file:</tt> and <tt>http:</tt> and <tt>data:</tt> everywhere, and of course http will be automatically downloaded. Try <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/">view3dscene from snapshots</a>, enable <i>"Preferences-&gt;Download Resources From Network"</i> and then use menu item (Ctrl+L) to load URL, or pass URL on the command-line.</p>

    <p>This is a little user-unfriendly, as the downloading is blocking (the process waits for the download to finish, instead of letting you enjoy the game and download in the background; there isn\'t even any nice way to cancel the download, or even notification about it, except for --debug-log message). For this reason, it is disabled by default: you have to explicitly allow it by <i>"Preferences->Download Resources From Network"</i> (in code: <tt>CastleDownload.EnableNetwork</tt> variable).</p>

    <p>For some test scenes on the Internet, try e.g.
      <a href="http://www.web3d.org/x3d/content/examples/ConformanceNist/SpecialGroups/Inline/single-url.x3dv">Inline demo</a> or
      <a href="http://www.web3d.org/x3d/content/examples/ConformanceNist/Appearance/ImageTexture/256jpg.x3d">Texture demo</a> or
      <a href="http://www.web3d.org/x3d/content/examples/ConformanceNist/Sounds/AudioClip/default_looptrue.x3dv">AudioClip demo</a>.
      (from <a href="http://www.web3d.org/x3d/content/examples/ConformanceNist/">ConformanceNist X3D Examples Archive</a>). Or see our demo models through http, by browsing through <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/x3d/">SVN http link</a>.

    <p><a href="' . CURRENT_URL . 'view3dscene.php">view3dscene</a> automatically downloads the model, as well as all linked resources (textures, inline models, sounds, scripts etc.). You can also use VRML/X3D Anchors to jump to any URL (for example, you can jump from a local file to a model available through http). Also <a href="https://en.wikipedia.org/wiki/Data_URI_scheme">data: URI handling</a> is now more uniform. Also the engine uses now MIME types more, instead of merely file extensions. This makes us work with http transfers, and improves cooperation with data: URIs.

    <p>In a game, you could also use URLs inside files like <tt>level.xml</tt> and <tt>resource.xml</tt>. For example, you could distribute now a level.xml file that contains URLs to levels on your server, downloaded each time.

  <li><p><b>Clipboard</b> (Ctrl+C, Ctrl+V, Ctrl+X in message boxes, especially handy to copy/paste URLs). For developers: use <tt>Clipboard.AsText</tt> property. Implemented for CastleWindow WinAPI, GTK, LCL backends.

  <li><p><b>CAD level 2 support (CADXxx nodes)</b>.</p></li>

  <!--li><p>There was a silent 4.0.1 release to fix compilation of CastleGLWindowsFonts unit, and do other minor fixes/improvements around font-related units and examples.-->

  <li><p>Improvements to 2D rendering. <!-- (as it happens, they also mostly revolve around bitmap fonts). --> They also workaround crashes on some Mesa 9 drivers.

  <!--
    <ul>
      <li>use SetWindowPos instead of glRasterPos almost everywhere (cleaner, better for future strict OpenGL >= 3, and avoids crashes with Mesa 9 &mdash; at least with "OpenGL renderer string: Gallium 0.4 on AMD RV710" and "OpenGL version string: 2.1 Mesa 9.0.2" (on Ubuntu 12.10)).
      <li>use PrintAndMove instead of Print, it\'s much more optimal.
    </ul>
  -->

  <li><p>Support for 8 and 16 samples for anti-aliasing, there are (at least NVidia) GPUs supporting it. <!--By "support" I mean that you can see new options now in view3dscene <i>Preferences->Anti-Aliasing</i>, and that TCastleWindow.AntiAliasing allows this, and that our screen effects library cooperates with them (so make screen effects combined with anti-aliasing work). Everything else (like basic TCastleWindow.MultiSampling and TCastleControl.MultiSampling) was already working.-->

  <li><p>Renamed our event <tt>Idle</tt> to <tt>Update</tt>.

    <p>This reflects our implementation and usage of this event clearer. This event is for continuous tasks, called even when the application is not "idle" (when application is processing something, like mouse moves). Our <tt>Update</tt> event doesn\'t correspond 100% to normal (as used by LCL or GTK) meaning of "idle" (which is also evidenced by code if TCastleWindow LCL and GTK backends, that cannot simply use LCL/GTK "idle" concepts to implement our Update).

  <li><tt>TCastleControl.AggressiveUpdate*</tt> are removed. The (simplified and improved) version of this mechanism is now always "on" and automatically makes mouse look work better. It\'s still not perfect (it seems LCL event loop is just too slow to process events during mouse look fast enough), but it\'s better now. If you want perfectly smooth mouse look, you should still consider TCastleWindow instead of TCastleControl.
</ol>
'),

    array('title' => 'Castle Game Engine 4.0.0 release! And view3dscene 3.12.0, and castle 1.0.0, and more',
          'year' => 2013,
          'month' => 1,
          'day' => 26,
          'short_description' => '',
          'guid' => 'release-4.0.0',
          'description' =>
castle_thumbs(array(
//  array('filename' => 'fps_game_screen_19.png', 'titlealt' => 'FPS game screen'),
  array('filename' => 'fps_game_screen_18.png', 'titlealt' => 'FPS game screen'),
  array('filename' => 'fps_game_screen_17.png', 'titlealt' => 'FPS game screen'),
  array('filename' => 'control_lazarus_shared_resources.png', 'titlealt' => 'Lazarus form with 2 castle controls sharing textures'),
  array('filename' => 'resource_animations_1.png', 'titlealt' => 'resource_animations: Knight default idle animation'),
  array('filename' => 'missile_stuck_in_wall_4.png', 'titlealt' => 'Missile (arrow) stuck in a wall'),
  array('filename' => 'castle_items_tower_screen_2_ring.png', 'titlealt' => 'Testing dropping items on level'),
  array('filename' => 'barna29_water_0.png', 'titlealt' => 'German Pavillion in the Universal Expo of Barcelona of 1929 (by Victor Amat)'),
  array('filename' => 'room_arranger_3_ssao.png', 'titlealt' => 'Room Arranger with SSAO demo, shown by view3dscene'),
  array('filename' => 'room_arranger_viewer_2.png', 'titlealt' => 'Final stages of RoomArranger viewer using our engine, with more controls and SSAO'),
  array('filename' => 'lights_editor_shadow_maps.png', 'titlealt' => 'Lights Editor playing with shadow maps'),
  array('filename' => 'lights_editor_fountain_0.png', 'titlealt' => 'Lights Editor in view3dscene - fountain, shadow volumes settings'),
)) .
'<p>We proudly present the <a href="' . CURRENT_URL . 'engine.php">Castle Game Engine</a> 4.0.0 release! This is the greatest release of our engine <i>ever</i> for <a href="http://www.freepascal.org/">ObjectPascal</a> developers interested in making their own games:</p>

<ol>
  <li><p>We introduce a <b>high-level game API, to construct a fully-working 3D game really easy</b>. This includes integrated units to manage levels, creatures (with smart AI), items, player, with inventory, management of 3D resources, easy 3D sound, flexible game data layout (you can use XML files to define a lot of things without touching game code), and so on.</p>

    <p>Of course, we use <a href="' . CURRENT_URL . 'vrml_x3d.php">VRML/X3D</a> for all the 3D models, so everything can be animated and interactive, and may use features like shadows, mirrors, shaders and such. See <a href="' . CURRENT_URL . 'demo_models.php">our demo 3D models</a> (in particular, check new <tt>water/caustics/</tt> demo inside, by Victor Amat, <a href="http://youtu.be/1mUU8prDi9k">movie showing it is here</a>).</p>

    <p>There are a couple new examples in engine sources. First of all, <tt>examples/fps_game/</tt> is a must-see: it shows a complete working FPS game, with a lot of comments in the source code and data files.</p>

    ' . (!HTML_VALIDATION ? '<iframe width="640" height="480" src="http://www.youtube.com/embed/S0bA3mJ8lZc" frameborder="0" allowfullscreen></iframe>' : '') . '

    <p>The engine is very flexible, you can derive new classes and override a lot of methods to customize behavior of everything in ObjectPascal. You can define new level logic, new creatures, new items, and finally you can also use basic 3D classes to easily create any 3D entities suitable for your game. You can also easily create your own viewports, scene managers, 2D controls, use 3D sound and much more. Everything is explained in the <a href="' . CURRENT_URL . 'tutorial_intro.php">new tutorial</a>.</p>
    <!--  Of course, you can also define animations and even use scripting inside VRML/X3D files, but for a non-trivial game -->

    <!--
    <p>Another new example program is the <tt>examples/resource_animations</tt> to view creature/item animations. This is accompanied by <a href="' . CURRENT_URL . 'creating_data_resources.php">creating data guide about resources</a>, that explains new methods to define 3D animations.</p>
    -->
  </li>

  <li><p>We have <b>a lot of new documentation</b> to show you how to use the engine:</p>

    <ul>
      <li><a href="' . CURRENT_URL . 'tutorial_intro.php">Tutorial</a> - introduces most concepts of the engine, with example code snippets.</li>
      <li><a href="' . CURRENT_URL . 'tutorial_classes_overview.php">Classes overview (cheatsheet)</a> - a quick ride through most important engine classes and ideas.</li>
      <li><a href="' . CURRENT_URL . 'creating_data_intro.php">Guide to creating game data</a> - how to create your 3D models using any 3D modeling software (like open-source <a href="http://www.blender.org/">Blender</a>), and how to write various configuration files like <tt>resource.xml</tt> and <tt>level.xml</tt>.</li>
      <li><a href="' . CURRENT_URL . 'apidoc/html/index.html">As always, there is also a complete API reference.</a></li>
    </ul>

  <li><p>The full list of changes is definitely too large to fit into a normal news item. In the past I usually tried to list all the important changes in a release announcement, but there\'s just too many things this time :) New engine units include
    <a href="' . CURRENT_URL . 'apidoc/html/CastleCreatures.html">CastleCreatures</a>,
    <a href="' . CURRENT_URL . 'apidoc/html/CastleItems.html">CastleItems</a>,
    <a href="' . CURRENT_URL . 'apidoc/html/CastleLevels.html">CastleLevels</a>,
    <a href="' . CURRENT_URL . 'apidoc/html/CastlePlayer.html">CastlePlayer</a>,
    <a href="' . CURRENT_URL . 'apidoc/html/CastleMaterialProperties.html">CastleMaterialProperties</a>,
    <a href="' . CURRENT_URL . 'apidoc/html/CastleResources.html">CastleResources</a>.
    There are also countless changes in the rest of the engine to make it better and more integrated. See the <a href="' . CURRENT_URL . 'news.php">news</a> from the whole 2012 year for a complete list of details.</p>

    <!--p>This was probably the longest delay between releases of our engine, but I hope it was worth it :)</p-->

    <p><i>For developers upgrading from engine 3 version</i>: all of our unit names are now prefixed with <tt>CastleXxx</tt>, see <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/castle_game_engine/doc/naming_engine_4.0.txt">doc/naming_engine_4.0.txt</a> document. So you will almost definitely need to fix your "uses" clauses. Aside from that, the engine is quite compatible with previous version. <!-- (as the basis for controls and scene manager and 3D objects was already there; now it\'s just used much more extensively). --> Of course please <a href="' . CURRENT_URL . 'forum.php">ask of the forum</a> if you have any questions about upgrading code from engine 3 to 4 (or about anything else, for that matter :)</p>
</ol>

<p>We also release a <b>new 3.12.0 version of <a href="' . CURRENT_URL . 'view3dscene.php">view3dscene</a>, our VRML/X3D browser and viewer for other 3D model formats</b>. The most important improvements are listed below. Note that improvements 1-8 are actually in the engine, instantly available for all the games/applications using our engine. But most users will probably observe them in <a href="' . CURRENT_URL . 'view3dscene.php">view3dscene</a> for the 1st time.</p>

<ol>
  <li>Navigating in <i>Walk / Fly</i> modes by mouse dragging.</li>
  <li><a href="http://youtu.be/lsUztfdike8">Using 3D mouse devices</a>.</li>
  <li>Screen-space ambient occlusion (see menu <i>"View -> Screen Effects"</i> menu item, developers: try the ultra-simple <a href="' . CURRENT_URL . 'apidoc/html/CastleSceneManager.TCastleAbstractViewport.html#ScreenSpaceAmbientOcclusion">TCastleAbstractViewport.ScreenSpaceAmbientOcclusion</a> boolean property).</li>
  <li>All screen effects cooperate now with multi-sampling (anti-aliasing).</li>
  <li>UNIT statement from X3D 3.3 is implemented.</li>
  <li>VisibilitySensor node is supported.</li>
  <li>Many fixes to triangulating concave polygons.</li>
  <li><a href="' . CURRENT_URL . 'x3d_extensions.php#section_ext_shading">X3D extension to force Phong shading</a>.</li>
  <li>New <i>"Edit -> Lights Editor"</i> feature.</li>
</ol>

<p>Many thanks go to Jan Adamec for implementing the 1-3 features above (and more)! Our engine is used as VRML viewer for shareware <a href="http://www.roomarranger.com/">Room Arranger</a>.</p>

<p>All the other programs and data on our pages were updated to use/show new <a href="' . CURRENT_URL . 'engine.php">engine 4.0.0 version</a>:
  <a href="' . CURRENT_URL . 'castle.php">The Castle 1.0.0 (finally!)</a>,
  <a href="' . CURRENT_URL . 'malfunction.php">malfunction 1.2.8</a>,
  <a href="' . CURRENT_URL . 'kambi_lines.php">kambi_lines 1.1.7</a>,
  <a href="' . CURRENT_URL . 'view3dscene.php">view3dscene 3.12.0</a>,
  <a href="' . CURRENT_URL . 'rayhunter.php">rayhunter 1.3.4</a>,
  <a href="' . CURRENT_URL . 'glviewimage.php">glViewImage 1.4.1</a>,
  <a href="' . CURRENT_URL . 'glplotter_and_gen_function.php">glplotter 1.2.5</a>,
  <a href="' . CURRENT_URL . 'bezier_curves.php">bezier_curves 1.1.9</a>,
  <a href="' . CURRENT_URL . 'glinformation.php">glinformation 1.2.2</a>,
  <a href="' . CURRENT_URL . 'glplotter_and_gen_function.php">gen_function 1.0.5</a>,
  <a href="' . CURRENT_URL . 'demo_models.php">demo models 3.2.0</a>.
</p>

<!--
- kambiShadows, kambiShadowsMain fields renamed to shadowVolumes, shadowVolumesMain &mdash; much more sensible names. But the old names will remain to be available, for compatibility (probably for a long time). TCastleAbstractViewport.ShadowVolumes is true by default now.
-->
'),

    array('title' => 'Development: engine 4.0.0 release in a few days, many improvements this month',
          'year' => 2013,
          'month' => 1,
          'day' => 17,
          'short_description' => '',
          'guid' => '2013-01-17',
          'description' =>
castle_thumbs(array(
  array('filename' => 'fps_game_screen_19.png', 'titlealt' => 'FPS game screen'),
  array('filename' => 'fps_game_screen_18.png', 'titlealt' => 'FPS game screen'),
  array('filename' => 'fps_game_screen_17.png', 'titlealt' => 'FPS game screen'),
  array('filename' => 'fps_game_screen_16.png', 'titlealt' => 'FPS game screen'),
  array('filename' => 'fps_game_screen_15.png', 'titlealt' => 'FPS game screen'),
  array('filename' => 'fps_game_screen_14.png', 'titlealt' => 'FPS game screen'),
  array('filename' => 'fps_game_screen_13.png', 'titlealt' => 'FPS game screen'),
  array('filename' => 'fps_game_screen_12.png', 'titlealt' => 'FPS game screen'),
  array('filename' => 'fps_game_screen_11.png', 'titlealt' => 'FPS game screen'),
//  array('filename' => 'fps_game_screen_10.png', 'titlealt' => 'FPS game screen'),
//  array('filename' => 'fps_game_screen_03.png', 'titlealt' => 'FPS game screen'),
  array('filename' => 'fps_game_screen_02.png', 'titlealt' => 'FPS game screen'),
//  array('filename' => 'fps_game_screen_01.png', 'titlealt' => 'FPS game screen'),
//  array('filename' => 'fps_game_screen_00.png', 'titlealt' => 'FPS game screen'),
  array('filename' => 'debug_castle_screen_1.png', 'titlealt' => 'Debug 3D information'),
//  array('filename' => 'debug_castle_screen_0.png', 'titlealt' => 'Debug 3D information'),
)) .
'<p>Many final improvements done to the engine, to make the next 4.0.0 release really polished for developers. Below is a list of new SVN features (implemented since last news, mostly within the last 3 weeks).</p>

<p>Engine 4.0.0 release is almost ready, what remains is finishing the new tutorials. BTW, I got into habit of posting a little smaller (and more frequent) notes about engine development on <a href="https://plus.google.com/101185352355602218697" rel="publisher">our Google+ page</a>, so you\'re welcome to follow us there.</p>

<p>Latest improvements to the engine:</p>

<ol>
  <li>
    <p><b>New very important example program is available in engine source code: <tt>fps_game</tt></b>.</p>
    <p>It is a simple demo, with a lot of comments in code, of a fully-working FPS game. We have level, creatures, items (medkit and weapon), inventory, player can be hurt and die, player can shoot and kill the creatures, bump mapping, move limit, sectors/waypoints (for AI), buttons, notifications, custom viewport, sounds, sky, water, footsteps and a <b>lot</b> of comments.</p>
    <p>P.S. Various game data (knight, textures, sounds) are from <a href="http://opengameart.org/">opengameart.org</a>, countless thanks go to it\'s many contributors. Details what is from where are inside AUTHORS.txt files in sources. Only level and items were modeled by Michalis from scratch.</p>
  </li>

  <li><p><b>Improvements to game logic:</b></p>
    <ul>
      <li>Creature enemy is now configurable by overriding <tt>TWalkAttackCreature.Enemy</tt> method. The default implementation chooses the central player as the enemy (if the player is not dead yet), but you can override it to implement something more sophisticated (like creature vs creature fighting, teams etc. &mdash; not everything is tested there yet, but it all should be doable now, so go wild :).</li>
      <li>New methods to define creature/items (all resources) animations are available. You can load animation from <a href="' . CURRENT_URL . 'kanim_format.php">kanim</a>, or from multiple VRML/X3D files (one VRML/X3D for each animation), or from a single VRML/X3D file (in which case the file will have to contain different TimeSensor to activate each animation). Description how it works is inside DRAFT.modeling_tutorial.txt for now. Example of all 3 methods to define animations are inside <tt>castle_game_engine/examples/resource_animations/</tt> in sources.</li>
      <li>Many improvements to our <tt>RenderDebug3D</tt> and <tt>RenderDebugCaptions</tt> mechanisms (you can see them e.g. in fps_game demo): respect Orientation (work with games with either +Y or +Z up), sphere is displayed better,  caption is multi-line and contains "enemy distance" info.</li>
      <li><tt>TItem.Picked</tt> method can be overridden to configure what happens when you pick item. Example code in fps_game shows how to make an item that is consumed on pickup (aka "power-up").</li>
      <li><tt>TextureProperties</tt> mechanism was enhanced into <tt>MaterialProperties</tt>, that can describe much more things. In the future, we can add there more stuff about material/texture that cannot be comfortably expressed in Blender, but that is conceptually tied to material/texture. For now, the main addition is that you can define bump mapping by normal_map properties there. The hack in <a href="' . CURRENT_URL . 'blender.php">our Blender exporter</a> (to autodetect xxx_normalmap textures) will possibly be removed at some point (as it has no future, it\'s dirty).</li>
      <li><tt>player.xml</tt> must be now loaded explicitly by <tt>Player.LoadFromFile</tt> (we try to never load the file from some hardcoded location without you requesting it). <!--; previous implementation was always loading <tt>data/player.xml</tt> in <tt>TPlayer</tt> constructor). You can give your own location of <tt>player.xml</tt>. --> Also, all player properties have now sensible defaults, so in simple cases there\'s no need to use this file at all.</li>
    </ul>
  </li>

  <li><p><b>Final code renames/refactoring:</b></p>
    <ul>
      <li>A lot of unit renames, to bring all units to <tt>CastleXxx</tt> names. I admit it was a mistake trying to have non-prefixed unit names in engine 3.0.0. After some time, many unit names had to be prefixed anyway, and this created inconsistent naming &mdash; some unit names had <tt>Castle</tt> prefix, some not. Now all unit names have <tt>Castle</tt> prefix (the only exception being <tt>X3DXxx</tt> units).
        <p>It means that when upgrading your project from engine 3 to 4, you\'ll have to do appropriate renames in the "uses" clause of your units. Compiler will tell you where :) I also prepared a list of renames in <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/castle_game_engine/doc/naming_engine_4.0.txt">doc/naming_engine_4.0.txt</a> file.</li>
      <li>Many <tt>DefaultXxx</tt> global constants moved to constants inside classes, to not pollute namespaces with a lot of constants that are usually useful only as default value for corresponding property Xxx.</li>
      <li>All creature/items "kinds" renamed to "resources". Previously, terms "kinds" and "resources" were used to refer to the same thing, which could create confusion. Now it\'s simple: <tt>resource.xml</tt> file refers to the resource class (maybe it\'s creature resource, maybe item resource and so on), and creates resource instance. <tt>TCreatureResource</tt> is derived from <tt>T3DResource</tt> (as <tt>TCreature</tt> derives from <tt>T3D</tt>). <tt>TItemResource</tt> is derived from <tt>T3DResource</tt>, like <tt>TItemOnLevel</tt> derives from <tt>T3D</tt>.</li>
      <li><tt>CastleKeysMouse</tt> (formerly <tt>KeysMouse</tt>) types are no longer aliased in <tt>CastleWindow</tt> unit. This was unmaintainable, and wasn\'t available in Lazarus <tt>CastleControl</tt>. Simply add <tt>CastleKeysMouse</tt> to your uses clause if you use any identifier from it.</li>
      <li>Removed units: <tt>VideosCache</tt> (merged into <tt>CastleVideos</tt>), <tt>Rectangles</tt> (merged to <tt>CastleOnScreenMenu</tt>), <tt>BezierCurve</tt> (merged to <tt>CastleCurves</tt>), <tt>NormalizationCubeMap</tt> (moved to <tt>bump_mapping</tt> example dir, not useful for engine; and <tt>bump_mapping</tt> was actually removed from engine core, as not useful anymore, it\'s now in <a href="http://michalis.ii.uni.wroc.pl/wsvn/michalis/obscure_castle_engine_demos_and_tools/">obscure_castle_engine_demos_and_tools repo</a>).</li>
    </ul>
  </li>

  <li><p><b>Other engine improvements:</b></p>
    <ul>
      <li><tt>CastleImages</tt> supports resizing images with bilinear interpolation. <tt>TCastleImage.Resize</tt> and <tt>TCastleImage.MakeResized</tt> take <tt>Interpolation</tt> parameter, <tt>riNearest</tt> or <tt>riBilinear</tt>. Default is still <tt>riNearest</tt>, so fast (and ugly) like before. <tt>CastleWindowProgress</tt>, when it needs to scale progress background image, requests <tt>riBilinear</tt> interpolation. <a href="' . CURRENT_URL . 'glviewimage.php">glViewImage</a> allows to test both resizing methods.</li>
      <li>Fonts: Huge refactoring of font units and types.
        <ul>
          <li>We now have 4 units: <tt>CastleOutlineFonts</tt>, <tt>CastleBitmapFonts</tt>, <tt>CastleGLOutlineFonts</tt>, <tt>CastleGLBitmapFonts</tt>.
          <li><tt>TBitmapFont</tt> and <tt>TOutlineFont</tt> are classes, with Data field and some methods. This way utilities from <tt>CastleOutlineFonts</tt> become methods.
          <li><tt>font2pascal</tt> utility added, this is a very old Windows-only utility to convert fonts into our bitmap/outline font structures. It awaits eagerly to be reimplemented using freetype, contributions for this are most welcome!
        </ul>
      </li>
      <li>Memory optimization for models with many nodes (esp. using heavily prototypes), by about 10%, and another 10% possible if you define <tt>CONSERVE_TRIANGLE_MEMORY</tt> symbol.</li>
      <li>Improvements to <tt>CastleWindowProgress</tt>: <tt>Opacity</tt> property, configurable colors, using <tt>UIFontSmall</tt> when necessary, fixed to work with non-standard glViewport (like after typical TCastleViewport usage).</li>
      <li>"Home" key in Examine mode goes to initial viewpoint (configurable by VRML/X3D Viewpoint or SetInitialView in code), instead of always going to viewpoint with +Y up. This should be universally more useful and more natural than previous behavior.</li>
      <li>Fixes to viewports for ATI on Linux (one bug of Mesa, one bug of fglrx, see BuggyDepth32 and BuggySwapNonStandardViewport in CastleGLVersion).</li>
      <li>Examine camera has <tt>ExclusiveEvents</tt> = true, consistent with other TUIControl.</li>
      <li><tt>CastleLog</tt> output stream is configurable.</li>
    </ul>
  </li>

  <li><p><b>The caustics demo</b> (you seen a movie of this <a href="' . CURRENT_URL . 'news.php?id=2012-07-10">in earlier news</a>) is now committed to our <a href="' . CURRENT_URL . 'demo_models.php">demo models</a>. Many thanks to Victor Amat! I hope to use these ideas to make such water available out-of-the-box for all games using our engine (right now you will have to adjust shaders for your specific case).</p></li>
</ol>')
);
