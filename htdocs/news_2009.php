<?php

array_push($news,
    array('title' => 'News - terrain demo, large engine layout changes',
          'year' => 2009,
          'month' => 12,
          'day' => 21,
          'guid' => '2009-12-21',
          'short_description' => '',
          'description' =>

vrmlengine_thumbs(array(
  array('filename' => 'terrain_1.png', 'titlealt' => 'Terrain from noise'),
  array('filename' => 'terrain_2.png', 'titlealt' => 'Terrain from SRTM file'),
)) . '<ol>
  <li>
    <p>There\'s a new demo in engine <a href="http://vrmlengine.sourceforge.net/kambi_vrml_game_engine.php#section_svn">SVN sources</a>: <tt>terrain</tt> (look inside <tt>kambi_vrml_game_engine/examples/vrml/terrain/</tt> directory). It shows basic procedural terrain generation. Uses cosine interpolated noise, summed by <i>Fractional Brownian Motion</i> (which is a fancy way of saying "sum a few scaled noise functions"&nbsp;:)&nbsp;).</p>

    <p>It can also load a terrain data from <a href="http://www2.jpl.nasa.gov/srtm/">SRTM (.hgt files)</a> (for example, <a href="http://netgis.geo.uw.edu.pl/srtm/Europe/">sample files for Europe are here</a>). And it can display a terrain defined by mathematical expression, like <i>sin(x*10) * sin(y*10)</i> (see <a href="http://vrmlengine.sourceforge.net/kambi_script.php">KambiScript language reference</a> for full syntax and functions available for math expressions).</p>

    <p>If you\'re interested in some background, <a href="http://freespace.virgin.net/hugo.elias/models/m_perlin.htm">this is the simplest introduction to "making noise"</a> (although beware that it\'s actually not about Perlin noise :), Perlin noise is a "gradient noise", not covered there).</p>

    <p>I would like to extend this <tt>terrain</tt> demo to something much larger (infinite terrain rendering, render with normals, cover with a couple layers of textures, add water surface, maybe render with Precomputed Radiance Transfer (taken from my other demo) etc.). For now, it\'s a start :)</p>
  </li>

  <li>
    <p>Developers will note large changes in the layout of <tt>kambi_vrml_game_engine</tt> archive (and SVN directory). (links below point to appropriate <a href="http://vrmlengine.svn.sourceforge.net/viewvc/vrmlengine/">viewvc</a> URL, for your convenience)</p>

    <ol>
      <li>All "core" sources are moved to the <a href="http://vrmlengine.svn.sourceforge.net/viewvc/vrmlengine/trunk/kambi_vrml_game_engine/src/"><tt>src/</tt> subdirectory</a>, to keep them separate from other stuff (packages, doc, tests etc.).

      <li><a href="http://vrmlengine.svn.sourceforge.net/viewvc/vrmlengine/trunk/kambi_vrml_game_engine/examples/"><tt>examples/</tt> subdirectory</a> was moved to the top. I should have done this a long time ago.  If you want to look at VRML demos, now you just go to <tt>examples/vrml/</tt> subdirectory, which is hopefully obvious to new developers. (Previously, you had to dig into cryptically-named <tt>3dmodels.gl/examples/</tt>)

      <!-- teaser </li></ol></li></ol> -->

      <li>Also, some subdirectory names changes, and some units moved around.
        <ul>
          <li>We have new <a href="http://vrmlengine.svn.sourceforge.net/viewvc/vrmlengine/trunk/kambi_vrml_game_engine/src/glwindow/">subdirectory for <tt>glwindow</tt></a> specific stuff (since the distinction what is for GLWindow, and what is not, is important to many developers; e.g. if you always want to use Lazarus OpenGL control, then GLWindow isn\'t really useful for you).
          <li>We have new <a href="http://vrmlengine.svn.sourceforge.net/viewvc/vrmlengine/trunk/kambi_vrml_game_engine/src/ui/">subdirectory for <tt>ui</tt></a>. This contains now the navigator and GLMenu &mdash; both things were available already, but now are implemented as <tt>TUIControl</tt> descendants, handled in more uniform fashion. In the future, I want to extend this, to make more OpenGL controls this way. The very scene manager may be treated as such "control" one day.
          <li>I noticed that the most important directories of our engine had a little cryptic naming: 3dgraph, 3dmodels, 3dmodels.gl. After a little shuffling, the new names are <a href="http://vrmlengine.svn.sourceforge.net/viewvc/vrmlengine/trunk/kambi_vrml_game_engine/src/3d/">3d</a>, <a href="http://vrmlengine.svn.sourceforge.net/viewvc/vrmlengine/trunk/kambi_vrml_game_engine/src/vrml/">vrml</a>, <a href="http://vrmlengine.svn.sourceforge.net/viewvc/vrmlengine/trunk/kambi_vrml_game_engine/src/vrml/opengl/">vrml/opengl/</a> &mdash; this should make things much clearer.
        </ul>
      <li>A minor thing is also that units Glw_Navigated, Glw_Win, Glw_Demo are gone, along with the one-unit package kambi_glwindow_navigated. I never liked these units, they were just handy shortcuts for simple demo programs. All normal programs should declare and explicitly create TGLWindow instance themselves, this is just 2 more lines of code but gives you better understanding what TGLWindow is and is more clean IMO.
      <li>Also, new kambi_vrml_game_engine*.tar.gz archives will contain offline HTML docs by pasdoc (the same ones <a href="http://vrmlengine.sourceforge.net/apidoc/html/index.html">as available online</a>).
    </ol>
    <!-- p>Please note that because of this change, pretty much everything inside <tt>kambi_vrml_game_engine</tt> is now, well, somewhere else. E.g. some URLs here and there may temporarily not work. Of course, please submit any observed problems, so that I may fix every page to be perfect. -->
  </li>
</ol>
'),

    array('title' => 'News - LGPL, SSAO demos, White Dune, more',
          'year' => 2009,
          'month' => 10,
          'day' => 30,
          'guid' => '2009-10-30',
          'short_description' => '',
          'description' =>

vrmlengine_thumbs(array(
  array('filename' => 'ssao_barna29_on.png' , 'titlealt' => 'Barna29 (with SSAO)'),
  array('filename' => 'ssao_barna29_off.png', 'titlealt' => 'Barna29 (without SSAO)'),
  array('filename' => 'ssao_stairs_on.png' , 'titlealt' => 'Stairs (with SSAO)'),
  array('filename' => 'ssao_stairs_off.png', 'titlealt' => 'Stairs (without SSAO)'),
), 2) . '

<ul>
  <li><p><a href="http://vrmlengine.sourceforge.net/kambi_vrml_game_engine.php#section_license">The core of our engine is now available under the GNU Lesser General Public License (with "static linking exception")</a>. Basically, this allows using the engine in closed-source programs, as long as you keep open your improvements to the engine.</p>

    <p>I had a long thought before this decision, always and still being a free software fanatic :) <!-- The text <i>In the future I may change the license to more liberal than GNU GPL &mdash; most probably to modified LGPL</i> was present in the "license" section since a few years. --> I wrote a short summary of my thoughts, <span ' . (HTML_VALIDATION ? '' : 'tabindex="0"') . ' class="js_link" onclick="kambi_toggle_display(\'lgpl-thoughts\')">click here to read it</span> (hidden by default, as may be boring to most people).</p>

    <ol id="lgpl-thoughts" style="display: none">
      <li><p>The initial insight is that with strict GPL, potential proprietary users of the engine wouldn\'t "open" their product just to use our engine. Instead they would move their interest elsewhere. We\'re <a href="http://openvrml.org/">not</a> <a href="http://freewrl.sourceforge.net/">the only</a> free/open VRML/X3D engine out there, neither we\'re the <a href="http://www.ogre3d.org/">the</a> <a href="http://irrlicht.sourceforge.net/">only</a> free/open 3d/game engine out there, and everyone else is available under LGPL (or even more permissible licenses).</p></li>

      <li><p>The common answer to above argument is that "popularity of the engine is not all that matters". LGPL is, ultimately, a permission to make closed-source software.</p>

        <p>The counter-thought to this is that LGPL still protects the freedom of my engine. You still have to share modifications to the engine, so it\'s not like properietary software can get all the benefits in some "unfair" way.</p></li>
    </ol>
  </li>

  <li><p>Victor Amat implemented demos of <a href="http://en.wikipedia.org/wiki/Screen_Space_Ambient_Occlusion">Screen Space Ambient Occlusion</a> using our <tt>GeneratedShadowMap</tt>. The complete examples, with shaders, are available inside our <a href="https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/kambi_vrml_test_suite/shadow_maps/">shadow_maps/ directory in kambi_vrml_test_suite (SVN only right now)</a>. Many thanks!</p>

    <p>Be sure to test these examples with view3dscene from <a href="http://michalis.ii.uni.wroc.pl/~michalis/vrmlengine-snapshots/">nightly builds</a>, as various problems reported by Victor (related to generating shadow maps) were fixed along the way.</p>

    <p>Some demo screenshots are on the right. They show the same view with/and without SSAO. (The comparison is somewhat unfair, as "without SSAO" versions just have GLSL shaders turned off. But the point is that they don\'t have smooth shadows (occlusion)).</p></li>

  <li><p>New <a href="http://vrml.cip.ica.uni-stuttgart.de/dune/">White Dune</a> release supports all <a href="http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php">VRML/X3D extensions</a> of our engine. Thanks go to Joerg "MUFTI" Scheurich.</p></li>

  <li><p>Documentation of our "VRML / X3D implementation status" was refactored, each X3D component has now separate page with support details. This should make it easier to read and find needed things. <a href="http://michalis.ii.uni.wroc.pl/~michalis/vrmlengine-snapshots/docs/vrml_implementation_status.html">See the SVN documentation here</a>.

  <!-- Previous documentation (just one long page with stream of information) was fine a long time ago, when so little of VRML/X3D standards was implemented that it was sensible to mention only things that are actually working. Right now it makes more sense to focus on mentioning things that are missing :) -->
  </p></li>

  <li><p>Also, I noticed today that our <a href="http://michalis.ii.uni.wroc.pl/~michalis/vrmlengine-snapshots/">nightly builds</a> were down for the last 3 weeks. Sorry about that, fixed now.</p></li>
</ul>'),

    array('title' => 'Development news - more NURBS: interpolators, VRML 97 compatibility',
          'year' => 2009,
          'month' => 9,
          'day' => 7,
          'guid' => '2009-09-07',
          'short_description' => '',
          'description' =>
vrmlengine_thumbs(array(
  array('filename' => 'nurbs_curve_interpolators.png', 'titlealt' => 'Animating along the NURBS curve (NurbsPositionInterpolator and NurbsOrientationInterpolator)'),
  array('filename' => 'nurbs_surface_interpolator.png', 'titlealt' => 'Animating along the NURBS surface (NurbsSurfaceInterpolator)'),
)) . '
<p>Implementation of NURBS is progressing very nicely. In addition to previously announced nodes (rendered curves and surfaces: <tt>NurbsPatchSurface</tt> and <tt>NurbsCurve</tt>), we now also handle X3D NURBS interpolators: <tt>NurbsPositionInterpolator</tt>, <tt>NurbsSurfaceInterpolator</tt>, <tt>NurbsOrientationInterpolator</tt>. Using them you can animate movement of objects and viewpoints along the NURBS curves and surfaces.</p>

<p>Also basic VRML 97 NURBS nodes are implemented, for compatibility.</p>

<p><a href="http://vrmlengine.sourceforge.net/vrml_implementation_nurbs.php">Up-to-date documentation about supported NURBS nodes is here.</a> Some demo scenes are inside kambi_vrml_test_suite in SVN, see e.g. <a href="https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/kambi_vrml_test_suite/x3d/nurbs_curve_interpolators.x3dv">nurbs_curve_interpolators.x3dv</a> and <a href="https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/kambi_vrml_test_suite/x3d/nurbs_surface_interpolator.x3dv">nurbs_surface_interpolator.x3dv</a>.</p>

<p>You can try the new features by using the <a href="http://michalis.ii.uni.wroc.pl/~michalis/vrmlengine-snapshots/">nightly builds</a> of <tt>view3dscene</tt>. Or, of course, you can wait for the next stable view3dscene 3.5 release &mdash; later this month.</p>
'),

    array('title' => 'Development news - NURBS basics',
          'year' => 2009,
          'month' => 9,
          'day' => 5,
          'guid' => '2009-09-05',
          'short_description' => '',
          'description' =>
vrmlengine_thumbs(array(
  array('filename' => 'nurbs_lantern.png', 'titlealt' => 'Lantern composed from NURBS patches (from web3d.org examples)'),
)) . '
<p>Basic support for X3D NURBS is implemented. <tt>NurbsPatchSurface</tt> and <tt>NurbsCurve</tt> nodes are handled following X3D specification.</p>

<p>As a background info: the core of our NURBS implementation (<a href="https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/kambi_vrml_game_engine/src/3d/nurbs.pas">nurbs unit</a>) is adapted from the <a href="http://vrml.cip.ica.uni-stuttgart.de/dune/">White_dune</a> source code. (Licensed on GPL &gt;= 2, just like our engine, so no problem here.)</p>

<p>For the next engine release, this NURBS support will be extended. I would like to cover X3D NURBS component up to level 2 and also implement most important VRML 97 NURBS nodes for compatibility (they are similar but a little incompatible to X3D ones).</p>

<p>For now, you can try the new features by using the <a href="http://michalis.ii.uni.wroc.pl/~michalis/vrmlengine-snapshots/">nightly builds</a> of <tt>view3dscene</tt>.</p>
'),

    array('title' => 'view3dscene 3.4 release - advanced texturing',
          'year' => 2009,
          'month' => 8,
          'day' => 26,
          'guid' => '2009-08-26',
          'short_description' => '',
          'description' =>
vrmlengine_thumbs(array(
  array('filename' => 'trees_river_shadow_maps.png', 'titlealt' => 'Shadow maps'),
  array('filename' => 'water_reflections.png', 'titlealt' => 'Water reflections by optimized GeneratedCubeMapTexture'),
  array('filename' => 'cubemap_teapot.png', 'titlealt' => 'Teapot with cube map reflections'),
)) . '

<p>' . news_a_href_page('view3dscene 3.4', 'view3dscene') . ' is released! The codename of this release should be <i>"Everything you wanted to know about textures"</i>, as most of the new features deal with X3D advanced texturing nodes.
<!-- ' . news_a_href_page('See recent news archive', 'news') . '  -->
</p>

<ul>
  <li><p>All X3D multi-texturing nodes implemented. See also <a href="http://vrmlengine.sourceforge.net/vrml_implementation_texturing.php#section_multi_texturing_clarifications">clarifications how MultiTexture.mode/source fields work and how to separate them for rgb and alpha channel</a>.</p></li>

  <li><p>All X3D cube map nodes implemented. This includes <tt>GeneratedCubeMapTexture</tt>, very useful to make mirrors, especially with the help of <a href="http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_tex_coord_worldspace">WORLDSPACEREFLECTIONVECTOR extensions</a>.</p></li>

  <li><p>All X3D 3D texture nodes implemented.</p></li>

  <li><p>DDS (DirectDraw Surface) format is supported, for all texture types (2D, 3D in <tt>ImageTexture3D</tt>, cube map in <tt>ImageCubeMapTexture</tt>). S3TC compression, explicit mipmaps are all supported, <a href="http://vrmlengine.sourceforge.net/vrml_implementation_texturing.php#section_dds">more details here</a>. New ' . news_a_href_page('glViewImage 1.3.0', 'glviewimage') . ' supports reading, writing and even limited editing of DDS images.<!-- Many other usability fixes were done to <tt>glViewImage</tt> along the road.--></p></li>

  <li><p><a href="http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_rendered_texture">RenderedTexture</a> node is implemented: a texture rendered from a specified viewpoint.</p></li>

  <li><p>Passing to GLSL shaders various uniform value types is implemented. This includes vectors, matrices and many more. <a href="http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_viewpoint_camera_matrix">Viewpoint.camera*Matrix</a> may be very useful to feed to shaders. You can also <a href="http://vrmlengine.sourceforge.net/vrml_implementation_shaders.php#glsl_passing_uniform_textures">pass texture nodes to GLSL shader uniforms, following X3D specification</a>.</p></li>

  <li><p>New extensions to easily make <a href="http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_shadow_maps">projective texturing and shadow maps</a> within your VRML/X3D worlds.</p></li>

  <li><p>Anisotropic texture filtering (by standard X3D <tt>TextureProperties.anisotropicDegree</tt>).</p></li>

  <li><p><i>Hardware occlusion query</i> may be activated for rendering, this can speed browsing large scenes enormously. Try it by menu options <i>View -&gt; ... Occlusion Query</i>.</p></li>
</ul>

<!-- teaser -->

<ul>
  <li><p>When using single texturing, you can set environment mode to replace <a href="http://vrmlengine.sourceforge.net/vrml_implementation_status.php#default_texture_mode_modulate">(default is modulate)</a>.</p></li>

  <li><p><a href="http://vrmlengine.sourceforge.net/kambi_script.php">KambiScript</a> functions to operate on string characters: <tt>"character_from_code"</tt>, overloaded <tt>"array_set", "array_get", "array_get_count", "array_set_count"</tt> for strings.</li>

  <li><p>As usual, along with view3dscene release, we also release <a href="http://vrmlengine.sourceforge.net/kambi_vrml_game_engine.php">accompanying Kambi VRML engine (version 1.8.0)</a> for developers. Released binaries are compiled with FPC 2.2.4, sources can also be compiled with FPC from trunk (tested on 2009-08-21). I also provide binaries for Linux/x86_64 (not only 32-bit Linux/i386), as I see a demand for it.</li>
</ul>'
    ),

    array('title' => 'News from SVN - RenderedTexture node, Viewpoint.camera*Matrix events, and more',
          'year' => 2009,
          'month' => 8,
          'day' => 13,
          'guid' => '2009-08-13',
          'short_description' => '',
          'description' =>
vrmlengine_thumbs(array(
  array('filename' => 'rendered_texture.png', 'titlealt' => 'RenderedTexture demo'),
)) . '
<p>New features in SVN:</p>

<ul>
  <li><a href="http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_rendered_texture">RenderedTexture</a> node is implemented: a texture rendered from a specified viewpoint. Useful for many effects. The most straightforward use would be to make a "security camera" or a "portal", through which a player can peek what happens at the other place in 3D world. (<a href="http://vrmlengine.sourceforge.net/miscella/rendered_texture_one_file.x3dv">Simple example</a>).</li>
  <li><a href="http://michalis.ii.uni.wroc.pl/~michalis/vrmlengine-snapshots/docs/kambi_vrml_extensions.html#section_ext_viewpoint_camera_matrix">Viewpoint.camera*Matrix</a> output events are implemented, very useful for shaders.</li>
  <li><a href="http://michalis.ii.uni.wroc.pl/~michalis/vrmlengine-snapshots/docs/kambi_vrml_extensions.html#section_ext_tex_coord_worldspace">WORLDSPACEREFLECTIONVECTOR, WORLDSPACENORMAL extensions</a> are documented.</li>
  <li>We have a <a href="http://michalis.ii.uni.wroc.pl/~michalis/vrmlengine-snapshots/docs/vrml_implementation_texturing.php#section_multi_texturing">clear and precise specification how MultiTexture.mode/source fields work and how to separate them for rgb and alpha channel</a>.</li>
  <li>Texture handling code was refactored last week: we have much cleaner implementation now, various bump mapping fixes were done by the way, and all texture generating nodes use now OpenGL framebuffer (possibly faster, and texture dimensions no longer limited by window size).</li>
  <li><tt>Examine</tt> mode improved, to be more feature-rich like <tt>Walk</tt> mode: works nicely with <tt>LOD</tt> and <tt>ProximitySensor</tt> nodes, you can click on <tt>TouchSensor</tt> and such in <tt>Examine</tt> mode, you can initiate ray-tracer in view3dscene from <tt>Examine</tt> mode.</li>
</ul>

<p>Still no official release, but view3dscene 3.4 should be released Really Soon :) For now you can try new features by using the <a href="http://michalis.ii.uni.wroc.pl/~michalis/vrmlengine-snapshots/">nightly builds</a>.</p>
'),


    array('title' => 'News from SVN - 3D textures, shadow maps, hardware occlusion query and more',
          'year' => 2009,
          'month' => 5,
          'day' => 5,
          'guid' => '2009-05-05',
          'short_description' => '',
          'description' =>
vrmlengine_thumbs(array(
  array('filename' => 'trees_river_shadow_maps.png', 'titlealt' => 'Shadow maps'),
  array('filename' => 'tex3d_smoke.png', 'titlealt' => 'Fog from 3D noise'),
  array('filename' => 'anisotropic_demo.png', 'titlealt' => 'Demo how anisotropic filtering helps'),
  array('filename' => 'oq_demo.png', 'titlealt' => 'Occlusion query optimizing city view'),
  array('filename' => 'water_reflections.png', 'titlealt' => 'Water reflections by optimized GeneratedCubeMapTexture'),
), 2) . '

<p>New features implemented last month in our engine:</p>

<ul>
  <li>3D textures (full support for X3D <tt>Texturing3D</tt> component).
    In particular, <tt>ImageTexture3D</tt> supports 3D textures in DDS format.</li>
  <li>New extensions to easily make
    <a href="http://vrmlengine.sourceforge.net/kambi_vrml_extensions_shadow_maps.php">projective
    texturing and shadow maps</a> within your VRML/X3D worlds.</li>
  <li>Anisotropic texture filtering (by standard X3D <tt>TextureProperties.anisotropicDegree</tt>
    field).</li>
  <li><i>Hardware occlusion query</i> may be activated for rendering,
    this can speed browsing large
    scenes enormously. Implemented both the <a href="http://http.developer.nvidia.com/GPUGems/gpugems_ch29.html">basic method (see GPU Gems 1, Chapter 29)</a>
    and more involved algorithm <a href="http://http.developer.nvidia.com/GPUGems2/gpugems2_chapter06.html">Coherent Hierarchical Culling (see GPU Gems 2, Chapter 6)</a>.
    view3dscene has menu options (<i>View -&gt; ... Occlusion Query</i>) to try it all.
    </li>
  <li>And many other things: fixes and optimizations for <tt>GeneratedCubeMapTexture</tt>,
    glViewImage improvements (you no longer have to open files from command-line),
    S3TC compressed textures (from DDS; usable as textures, also viewable in glViewImage),
    sorting transparent shapes (for better blending),
    exit shortcut for view3dscene is Ctrl+W (escape was too error-prone).
</ul>

<p>For the brave: you can test these features already by trying the
<a href="http://michalis.ii.uni.wroc.pl/~michalis/vrmlengine-snapshots/">nightly builds</a>
(or grabbing source code from SVN and compiling yourself, of course).</p>
'),

    array('title' => 'News from SVN &mdash; X3D multi-texturing, cube maps and more',
          'year' => 2009,
          'month' => 4,
          'day' => 10,
          'guid' => '2009-04-10',
          'short_description' => '',
          'description' =>

vrmlengine_thumbs(array(
  array('filename' => 'cubemap_teapot.png', 'titlealt' => 'Teapot with cube map reflections'),
)) . '

<p>Hi! I didn\'t post a message here since some time, but rest assured that the development of next engine version continues :) Things already implemented in the SVN include:

<ul>
  <li>X3D multi-texturing.</li>
  <li>X3D cube map nodes.</li>
  <li>This also means that DDS file format (for <tt>ImageCubeMapTexture</tt>) is implemented (both reading and writing, you can even use glViewImage as a simple DDS editor).</li>
  <li>This includes generating textures on the fly (for <tt>GeneratedCubeMapTexture</tt>).</li>
  <li>As extensions, I added texture generation modes "<tt>WORLDSPACEREFLECTIONVECTOR</tt>" and "<tt>WORLDSPACENORMAL</tt>" (analogous to X3D standard modes in CAMERA space) to make simulating real reflections trivial.</li>
  <li>There is also quite cool new feature in view3dscene to catch a "screenshot" of 3D world around you as a cube map (DDS, or six separate normal images).</li>
  <li>Passing almost all possible VRML types to GLSL shaders is implemented.</li>
  <li>..and a lot of other cool features are already implemented :)</li>
</ul>

<p>The plan for the next release (view3dscene 3.4, engine 1.8) is to polish implementation of all above (yes, there are some known problems, also <tt>GeneratedCubeMapTexture</tt> implementation is severely unoptimal now), and add related texturing and GLSL features:</p>

<ul>
  <li>3D texturing (that\'s easy since we already have DDS).</li>
  <li>Basic implementation of <a href="http://www.instantreality.org/documentation/nodetype/RenderedTexture/"><tt>RenderedTexture</tt></a> from InstantReality (that\'s easy since it\'s internally simpler than <tt>GeneratedCubeMapTexture</tt>).</li>
  <li>Finish GLSL stuff by supporting X3D attributes nodes.</li>
</ul>

<p>For the impatient: <a href="http://michalis.ii.uni.wroc.pl/~michalis/vrmlengine-snapshots/">nightly builds of vrmlengine binaries (including view3dscene) are available.</a> They are build automatically every night using current SVN code. Use at your own risk, of course &mdash; they <i>do</i> contain some known bugs. For now, they are made for Linux and Windows (32-bit).</p>
'),

    array('title' => 'Dynamic Ambient Occlusion, Shadow Fields demos in the engine sources',
          'year' => 2009,
          'month' => 1,
          'day' => 24,
          'guid' => '2009-01-24',
          'short_description' => '',
          'description' =>

vrmlengine_thumbs(array(
  array('filename' => 'dyn_ao_chinchilla.png', 'titlealt' => 'Chinchilla with Dynamic Ambient Occlusion'),
  array('filename' => 'dyn_ao_chinchilla_elements.png', 'titlealt' => 'Chinchilla elements used for Dynamic Ambient Occlusion'),
  array('filename' => 'dyn_ao_peach.png', 'titlealt' => 'Peach with Dynamic Ambient Occlusion'),
  array('filename' => 'sf_1.png', 'titlealt' => 'Shadow Fields screenshot 1'),
  array('filename' => 'sf_2.png', 'titlealt' => 'Shadow Fields screenshot 2'),
  array('filename' => 'sf_3.png', 'titlealt' => 'Shadow Fields screenshot 3'),
), 2) . '

<p>This week I implemented a demo of <a href="http://http.developer.nvidia.com/GPUGems2/gpugems2_chapter14.html">Dynamic Ambient Occlusion</a> using our engine.</p>

<p>In related news, last month I also implemented a demo of <a href="http://www.kunzhou.net/#shadow-field">Shadow Fields</a>. (I forgot to brag about it earlier, so I\'m doing it now :) ).</p>

<p>An extremely short summary: both techniques strive to make low-frequency soft shadows (in other words, the kind of shadows you usually see in the Real World) in dynamic 3D environment.</p>

<p>For now, they are just implemented as demos, and are not easily available for non-programmers. You have to actually get the source and compile some example programs to try out this stuff. (Although I think I\'ll make at least dynamic ambient occlusion available as an easy option inside view3dscene in the future.) The full source code, and example models, are available in SVN, naturally. Simple instructions:</p>

<pre>
$ svn checkout https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/kambi_vrml_game_engine/

$ cd kambi_vrml_game_engine/examples/vrml/shadow_fields
$ ./shadow_fields_compile.sh
$ ./shadow_fields

$ cd ../dynamic_ambient_occlusion
$ ./dynamic_ambient_occlusion_compile.sh
$ ./dynamic_ambient_occlusion models/peach.wrl.gz
</pre>

<p>There are more sample models in the <tt>models</tt> subdirectories,
and you can test both demos with your own models.
Both techniques require highly-tesselated
models to make shadows look nice. Shadow fields require preprocessing
by included <tt>precompute_shadow_field</tt> program. Dynamic ambient
occlusion doesn\'t require any preprocessing, but it requires really good
GPU (it does ~thousands of texture fetches per pixel in GLSL fragment shader).
You can find a lot of notes and links in the README files inside the
source directories.</p>

<p>Have fun!</p>'),

    array('title' => 'view3dscene 3.3, engine 1.7 release: LOD, Collision.proxy, and more',
          'year' => 2009,
          'month' => 1,
          'day' => 3,
          'guid' => '2009-01-03',
          'short_description' => '',
          'description' => "

<p>" . news_a_href_page('view3dscene 3.3', 'view3dscene') . " is released,
just a mixture of various new features, optimizations and fixes.
Traditionally, " . news_a_href_page('underlying
Kambi VRML game engine 1.7.0', 'kambi_vrml_game_engine') . " is released along.
Changes:</p>

" .
vrmlengine_thumbs(array(
  array('filename' => 'apple_lods.png', 'titlealt' => 'Apple model with various levels of detail'),
)) . "

<ul>
  <li><b>LOD (level-of-detail)</b> node proper handling.</li>

  <li><b>Collision.proxy</b> handling (very handy, allows you to make non-collidable but visible geometry, or approximate complex geometry with simpler for collision detection).</li>

  <li><a href=\"http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_octree_properties\">KambiOctreeProperties, an extensions to specify octree limits for your scene</a>. <a href=\"" . CURRENT_URL . "vrml_engine_doc/output/xsl/html/section.octrees_dynamic.html\">Section \"Octrees for dynamic worlds\" added to the documentation</a>, to explain how octree works since 1.6.0 version. The shape octree was speed up by mailboxes.</li>

  <li>Various workarounds for <a href=\"http://mesa3d.org/\">Mesa</a> bugs (in particular on Intel GPUs) and Mesa detection improved. This should significantly improve stability for Unix users with cheaper graphic cards. Because of this, also " . news_a_href_page('castle 0.8.3', 'castle') . " and " . news_a_href_page('glcaps 1.1.4', 'glcaps') . " are released, to get these fixes too.</li>

  <li>Various frustum culling optimizations.</li>

  <li>Small improvements in the view3dscene interface: blend status text, a shapes count fix, and keeping the selected triangle when transforming shape.</li>

  <li>The path tracer honors VRML &gt;= 2.0 materials, and <a href=\"http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_material_phong_brdf_fields\">VRML &gt;= 2.0 materials have the physical fields</a>. Because of this, also " . news_a_href_page('rayhunter 1.3.0', 'rayhunter') . " is released.</li>
</ul>

<p><a href=\"http://www.archlinux.org/\">Arch Linux</a> users may now install view3dscene from SVN easily by <a href=\"http://aur.archlinux.org/packages.php?ID=22782\">view3dscene Arch Linux package</a>. Thanks to Antonio Bonifati!
")
);
