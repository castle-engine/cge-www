<?php

array_push($news,
    array('title' => 'Development news: fixed view and isometric games in engine examples, shadow maps improvements, color interpolation in HSV space',
          'year' => 2011,
          'month' => 12,
          'day' => 23,
          'short_description' => '',
          'guid' => '2011-12-23-fixed-view-examples-shadow-maps-improvements-hsv',
          'description' =>
castle_thumbs(array(
  array('filename' => 'rift_0.png', 'titlealt' => 'Example Fixed Camera Game &quot;Rift&quot; - intro'),
  array('filename' => 'rift_1.png', 'titlealt' => 'Example Fixed Camera Game &quot;Rift&quot; - game menu'),
  array('filename' => 'rift_2.png', 'titlealt' => 'Example Fixed Camera Game &quot;Rift&quot; - inside game 1'),
  array('filename' => 'rift_3.png', 'titlealt' => 'Example Fixed Camera Game &quot;Rift&quot; - inside game 2'),
  array('filename' => 'sandbox_0.png', 'titlealt' => 'Example Isometric Game &quot;Sandbox&quot; - 1'),
  array('filename' => 'sandbox_1.png', 'titlealt' => 'Example Isometric Game &quot;Sandbox&quot; - 2'),
)) .
'<p>Hi everyone, here\'s a summary of improvements done to the engine in last month.</p>

<p>Remember you can check all new features of <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a> immediately by downloading a binary from our <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/">nightly builds</a>. To get new engine examples, you can download <a href="http://castle-engine.sourceforge.net/engine.php">engine</a> from SVN. Have fun!</p>

<ol>
  <li>Two large examples added to engine sources:
    <ol>
      <li>isometric game example (sandbox).</li>
      <li>fixed camera game (rift). "Fixed camera" means you have a still 2D background + animated 3D models on top, like <i>"Alone In The Dark"</i>, <i>"Syberia"</i>, <i>"Still Life"</i> and many other adventure games.</li>
    </ol>

    <p>Both these examples were converted from an unfinished (but maintained for a long time) games in the SVN. <!-- (moving them to engine examples allows them to at least serve as examples, and be enjoyed by engine programmers). --> We also plan to add an example fps_game to engine 3.0.0 release, which will show the main purpose of the engine &mdash; full 3D games.</p>
  </li>
  <li><p>Shadow maps improvements:</p>
    <ul>
      <li><p>Shadow maps generation optimized a lot. This speeds up greatly (2 times or more in our tests) scenes when shadow maps are dynamic (recreated every frame).</p>
        <p>We now use a special mode for rendering shadow maps, that essentially only sends pure geometry and textures for shadow maps. Textures are send since they may be needed for alpha test (in the future, we may improve this to actually ignore other textures).</p></li>
      <li><p>Many <i>Variance Shadow Maps</i> improvements. They are quite usable now, if you have sufficiently good GPU (with good float texture support). Work fast, and honor alpha test textures (typically used for wired cages, tree leaves etc.).</p>

        <p>There are still some problems with VSM (which is why they aren\'t used by default):</p>
        <ul>
          <li>Unfortunately, on some older GPUs, they may exhibit really weird behavior and uncover wild OpenGL bugs.
          <li>In some cases, there are accuracy problems. These are known VSM problems (see papers around VSM describing possible improvements of the base idea).
        </ul>
        <p>You\'re most welcome to test <i>Variance Shadow Maps</i> yourself. For starters, just get <a href="http://castle-engine.sourceforge.net/demo_models.php">demo models</a>, and open models inside shadow_maps subdirectory there with <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a>. <tt>shadow_maps/sunny_street/sunny_street.x3dv</tt> is one test worth trying, <tt>projected_spotlight_*</tt> is the other. Turn on Variance Shadow Maps by menu item <i>"View -&gt; Shadow Maps -&gt; Variance Shadow Maps"</i>. Please report on <a href="http://castle-engine.sourceforge.net/forum.php">forum</a> the GPUs where they work / where they don\'t, this will help me get some idea where they can/should be enabled by default.</p>
      </li>
    </ul>
  </li>
  <li>Fill Mode <i>"Solid Wireframe"</i> and <i>"Normal with Silhouette"</i> improved: wireframe is now always white. (Previously it was textured &mdash; which was hardly visible in many cases.)</li>
  <li>Saving viewpoint in some specific configurations (when look direction was close to +Z) fixed.</li>
  <li><a href="http://castle-engine.sourceforge.net/x3d_implementation_shaders.php#section_geometry">Geometry shaders</a> fixed on ATI graphic cards fixed (see <a href="https://sourceforge.net/p/castle-engine/tickets/2/">ticket #2</a> and <a href="http://castle-engine.sourceforge.net/x3d_implementation_shaders.php#section_geometry_input_size">macro CASTLE_GEOMETRY_INPUT_SIZE</a> docs.)</li>
  <li>Fixes to <a href="http://castle-engine.sourceforge.net/demo_models.php">demo models</a> GLSL shaders to work with <a href="http://freedesktop.org/wiki/Software/gallium">Gallium3D</a> on ATI graphic cards. Finally, we have an open-source OpenGL drivers with real support for GLSL! Not everything works with Gallium 3D perfectly, but a lot of stuff does work, and works even quite fast  &mdash; which is really great, compared to state of GLSL in Mesa3D a couple of years ago.</li>
  <li><a href="http://castle-engine.sourceforge.net/x3d_implementation_interpolation.php">ColorInterpolator and ColorSetInterpolator</a> interpolate colors in HSV space now. Our engine contains simple HSV&lt;-&gt;RGB conversion routines for general use in <tt>base/castlecolors.pas</tt> unit.</li>
</ol>
'),

    array('title' => 'Development news: Geometry shaders, my Ph.D. thesis, cellular texturing',
          'year' => 2011,
          'month' => 11,
          'day' => 14,
          'short_description' => '',
          'guid' => '2011-11-14-geometry-shaders-phd-cellular',
          'description' =>
castle_thumbs(array(
  array('filename' => 'geometry_shader_fun_smoothing.png', 'titlealt' => 'Geometry shader performing a fun variant of smoothing/exploding a shape'),
  array('filename' => 'fancy_light_spot_shape_enlightenment.png', 'titlealt' => 'Plug light_scale used to implement spot light with custom intensity'),
  array('filename' => 'cellular_texturing_0.png',  'titlealt' => 'Cellular procedural texture, using our ShaderTexture extension'),
  array('filename' => 'cellular_texturing_mirror_fun.png', 'titlealt' => 'Cellular procedural texture with mirrors, again using our ShaderTexture extension'),
  array('filename' => 'cellular_texturing_sphere.png', 'titlealt' => 'Cellular procedural texture, using our ShaderTexture extension'),
)) .
'<ol>
  <li><p><a href="http://castle-engine.sourceforge.net/x3d_implementation_shaders.php#section_geometry"><b>Geometry shaders</b> are implemented in our engine and view3dscene</a>.</p>

    <p>Geometry shaders are executed between the vertex and fragment shader. They can change the primitive type (like triangles to points), explode a single primitive into many (maybe adding some intermediate vertexes), remove some primitives and more. We support geometry shaders following modern GLSL version &gt;= 1.50 (since OpenGL 3.2). See the <a href="http://castle-engine.sourceforge.net/x3d_implementation_shaders.php#section_geometry">documentation</a> for more details and links to example X3D files.</p>

    <p>Interesting demos using geometry shaders are most welcome! You can post them on our <a href="http://castle-engine.sourceforge.net/forum.php">forum</a>. Nice demos may be included in our <a href="http://castle-engine.sourceforge.net/demo_models.php">demo models</a>.</p>
  </li>

  <li><p><a href="http://castle-engine.sourceforge.net/compositing_shaders.php">Michalis Ph.D. thesis, describing the "compositing shaders" idea, is published here :)</a>. There are HTML and PDF versions (45 pages for A4) with the same content.</p>

    <p>Various improvements to the idea were implemented lately, most of all: extensions for <a href="http://castle-engine.sourceforge.net/x3d_implementation_shaders.php#section_geometry">geometry shaders</a>. They are described in the <a href="http://castle-engine.sourceforge.net/compositing_shaders_doc/html/chapter.geometry_shaders.html">Chapter 6. "Extensions for geometry shaders"</a>. This allows to write <i>robust geometry shaders</i>, that cooperate with renderer internal effects by using special <tt>geometryVertexXxx</tt> functions.</p>

    <p>As always, comments and tests of our <i>compositing shaders</i> idea are most welcome!</p>
  </li>

  <li><p>Various new demos added to <a href="http://castle-engine.sourceforge.net/demo_models.php">demo models</a> (only in SVN now), like simple demos of <i>cellular texturing</i>, see <tt>demo_models/compositing_shaders/cellular_texturing*.x3dv</tt>.</p>
  </li>

  <li><p>Various small fixes and enhancements to <a href="http://castle-engine.sourceforge.net/compositing_shaders.php">compositing shaders</a> implementation for texture and light effects. Due to bugs/omissions, previously you could only use there texture/light-specific plugs, like <tt>PLUG_light_scale</tt> (for light source) and <tt>PLUG_texture_color</tt> (for texture). Now you can also use there:</p>
    <ul>
      <li>plugs using other user-defined plugs. See new <tt>texture_effect.x3dv</tt> example (in SVN of <a href="http://castle-engine.sourceforge.net/demo_models.php">demo models</a>).</li>
      <li>plugs using shape plugs (like <tt>PLUG_vertex_object_space</tt>). See new <tt>texture_coord_generate.x3dv</tt> (in SVN of <a href="http://castle-engine.sourceforge.net/demo_models.php">demo models</a>).</li>
    </ul>
  </li>
</ol>'),

    array('title' => 'Development news: grand rename to "Castle Game Engine" finished, more',
          'year' => 2011,
          'month' => 10,
          'day' => 13,
          'short_description' => '',
          'guid' => '2011-10-13-rename',
          'description' =>
castle_thumbs(array(
  array('filename' => 'view_3d_model_advanced.png', 'titlealt' => '&quot;view_3d_model_advanced&quot; example from engine sources'),
)) .
'<ol>
  <li><p><tt>--viewpoint</tt> command-line option was implemented for <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a>. This allows to choose a different viewpoint as the default (initial) one, especially useful in combination with <tt>--screenshot</tt> option. As <a href="https://sourceforge.net/apps/phpbb/vrmlengine/viewtopic.php?f=3&amp;t=195">requested on forum</a>.</p></li>

  <li><p>Shadow map generation improved: for shadow maps, we render model with shaders turned <i>off</i>. This is consistent with <i>Variance Shadow Maps</i> (that requires using custom shader, so user shaders are also ignored at shadow map capture). It avoids problems with shaders writing to <tt>gl_FragDepth</tt> and accidentally breaking shadow map content. Thanks to Victor Amat for pointing this out!</p></li>

  <li><p>The quest to cleanup and improve our examples continues. A lot of them are simplified now (using new <tt>TCastleWindow</tt> that automatically provides a scene manager instance). Some previous examples were merged and extended into the new <tt>3d_rendering_processing/view_3d_model_advanced</tt> example.</p></li>

  <li><p>As announced, rename of our engine (<i>"Kambi VRML game engine"</i> -&gt; <i>"Castle Game Engine"</i>) will also bring renames to a lot of identifiers. Since we\'re breaking compatibility anyway, I took some time to really think about new proper names for various stuff (previous names were invented somewhat organically, often without consistency, often following the <i>implementation</i> approach instead of following the typical usage).</p>

    <p>I wrote a <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/castle_game_engine/doc/naming_engine_3.0.txt">document describing in details what renames were done, and what is the reasoning behind them</a>. This may be too much information for some of you, but I hope you appreciate this kind of transparency :) To upgrade your existing programs to new engine version, just do the same renames in your code &mdash; both Pascal sources and form resources (lfm), and all will work Ok :)</p>

    <p>In short:</p>

    <ul>
      <li><p>Our official prefix for most units is now <tt>CastleXxx</tt>. No more <tt>KambiXxx</tt>, <tt>VRMLXxx</tt>, <tt>VRMLGLXxx</tt> units. Most of them become <tt>CastleXxx</tt>, with the exception of concepts defined in the X3D specification: <tt>X3DNodes</tt>, <tt>X3DFields</tt>, <tt>X3DLexer</tt> (many other castle units in fact use or depend on X3D stuff too; the whole engine is based on X3D nodes graph after all).</p></li>

      <li><p>Our official prefix for classes is now <tt>TCastleXxx</tt>, but preferably no prefix at all. All components get <tt>TCastle</tt> prefix (these are registered on component palette, so essentially have to live in a global namespace with all other components, so prefixing them is a good idea). Most other classes have no prefix at all, not needed.</p>

        <p>Generally no more <tt>TGLXxx</tt>, <tt>TKamGLXxx</tt>, <tt>TVRMLXxx</tt>, <tt>TVRMLGLXxx</tt>. They become <tt>TCastleXxx</tt> (if it\'s a component) or just <tt>TXxx</tt>.</p>

        <p>Our most important class, <tt>TVRMLGLScene</tt>, is now called <tt>TCastleScene</tt>. This nicely removes the implementation details from it\'s name (the fact that it\'s rendered using OpenGL, and uses X3D scene graph inside). These implementation details were really excessive (for an identifier name), because everything in our engine uses OpenGL (and if other renderer will be made, it should be pluggable and will not change the name anyway), and this component can load 3D data in any format (even if internally everything is converted into X3D nodes graph).</p>
      </li>

      <li><p>Control and window names simplified and made consistent: simple control or window is now <tt>TCastleWindow</tt> or <tt>TCastleControl</tt>. Descends from <tt>TCastleWindow/ControlXxx</tt>, adds <tt>SceneManager</tt> and easy <tt>Load</tt> method. For most programs/games with 3D world and 2D controls, just use this.</p></li>
    </ul>
</ol>
'),

    array('title' => 'Rename "Kambi VRML game engine" to "Castle Game Engine", moving to SourceForge Allura platform, my slides from TPCG11',
          'year' => 2011,
          'month' => 9,
          'day' => 23,
          'short_description' => '',
          'guid' => '2011-9-23-rename',
          'description' =>
castle_thumbs(array(
  array('filename' => 'castle_sunset.png', 'titlealt' => '&quot;view3dscene&quot; rendering tower with sunset sky'),
)) .
'<p>Hi everyone,</p>

<ol>
  <li><p>The first news is that you can see <i>"Castle Game Engine"</i> title slapped everywhere around the webpages&nbsp;:), instead of old <i>"Kambi VRML game engine"</i>. That\'s right: as <a href="http://castle-engine.sourceforge.net/news.php?id=2011-8-22-engine_rename_to__castle_game_engine__soon__development_news__some_important_fixes___improvements__many_api_improvements">announced previously</a>, we rename our engine to a new, better name.</p>

    <ul>
      <li><p>Most old URLs will still work, appropriate redirects are done to go to the new castle-engine website.

      <li><p><a href="https://sourceforge.net/p/castle-engine/discussion/">We have new forum</a>,
        and <a href="https://sourceforge.net/p/castle-engine/wiki/">new wiki</a>,
        and a new Flattr button:</p>

        <p>' . flattr_button(false, false) . '

        <p>Please click this to donate, thank you :)</p>

      <li><p>New <a href="https://sourceforge.net/projects/castle-engine">SourceForge project page</a>. We use new SourceForge 2.0 <a href="https://sourceforge.net/p/allura/">Allura</a> project platform &mdash; I think it works quite nicely. And we get nice wiki and forum for free, we\'ll try to use them (instead of MediaWiki and awfully ugly+bloated PhpBB).</p>

      <li><p>The URL of SVN repository changed. You can "svn switch" your existing repository, like</p>

<pre>
svn switch --relocate https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk http://svn.code.sf.net/p/castle-engine/code/trunk
</pre>

        <p>Or you can just do a fresh "svn checkout" of course. There are <a href="https://sourceforge.net/p/castle-engine/code/">different protocols available to access the new SVN repository</a>, for maximum comfort and flexibility. <!-- (svn:// and http:// for anonymous RO, svn+ssh:// and https:// for authenticated RW).--> Links on my page will advice the http:// protocol usually (as it\'s easy to check it also in a WWW browser, without SVN client). <!-- Developers should <a href="https://sourceforge.net/p/forge/documentation/svn%20-%20Beta/">prefer svn+ssh://, it can be faster than https:// according to SF docs</a>. --></p></li>

      <li><p>We have <a href="http://cia.vc/stats/project/castle-engine">a new cia.vc</a> page (no stats there yet, <a href="http://code.google.com/p/cia-vc/issues/detail?id=50">looks like cia.vc has problems with SVN polling</a>) and <a href="https://www.ohloh.net/p/castle-engine">a new ohloh page</a>. <a href="http://cia.vc/stats/project/vrmlengine">Old cia.vc page</a> and <a href="https://www.ohloh.net/p/vrmlengine">old ohloh page</a> should not be used anymore (please update your bookmarks etc.).</p></li>

      <li><p>For now, downloads still go to the old vrmlengine project space, although this is probably not noticeable &mdash; just click the link and download, all works Ok. This will be changed with the next release of view3dscene and everything else, when I\'ll upload everything to new castle-engine space and update links.</p></li>
    </ul>
  </li>

  <li><p>I was at <a href="http://www.eguk.org.uk/TPCG11/">TPCG11</a> conference and gave a talk about <a href="http://castle-engine.sourceforge.net/compositing_shaders.php">our "Compositing Shaders in X3D" idea</a> this month. I think it went quite good :) <a href="http://castle-engine.sourceforge.net/compositing_shaders_slides.pdf">Slides from the presentation are available</a>.</p></li>

  <li><p>Various small website improvements, like nice OS icons for download links (we use <a href="http://www.webdevstuff.com/95/free-operating-system-icons.html">CC icons from here</a>), added Google +1 button on the main page.</p></li>
</ol>'
),

    array('title' => 'Engine rename to "Castle Game Engine" soon, development news: some important fixes / improvements, many API improvements',
          'year' => 2011,
          'month' => 8,
          'day' => 22,
          'short_description' => '',
          'description' =>
castle_thumbs(array(
  array('filename' => 'castle_sunset.png', 'titlealt' => '&quot;view3dscene&quot; rendering tower with sunset sky'),
  array('filename' => 'humanoid_transparent_bg.png', 'titlealt' => 'Humanoid model, captured with transparent background, replaced in GIMP with some texture and gradient'),
  array('filename' => 'collada_text.png', 'titlealt' => 'Collada 3D text logo (from collada.org/owl)'),
)) .
'<ol>
  <li><p>First, the big news: <b>Grand engine rename is coming!</b> New name of the engine is planned to be

    <p style="font-size: larger; font-weight: bold">
    "Castle Game Engine"
    </p>

    <p>(Short (Unix) name: <tt>castle-engine</tt>, with dash inside.)</p>

    <p>For two reasons, the current name of our engine, <i>"Kambi VRML game engine"</i>, feels a little uncomfortable:</p>

    <ol>
      <li><p>Our focus, since quite some time, is on X3D. Yes, X3D is really just VRML 3.0 (even the X3D header shows that X3D versions start from 3). But people from the outside don\'t know about this, and may think that we support only some old standard from ~1997. So, at the very least, the "VRML" in the name should be replaced with VRML/X3D or just X3D.</p>
        <p>This was caused by history &mdash; the engine development started when VRML 1.0 (now ancient) was still useful (e.g. <a href="http://www.blender.org/">Blender</a> had built-in VRML 1.0 exporter), and VRML 2 (97) was just gaining popularity.</p>
        <p>The problem isn\'t limited to engine name, unfortunately. Most important units and classes inside the engine start with "VRML" instead "X3D". Our SourceForge project name and URL has "vrml" inside. So the rename will be some organizational challenge, but I think that we just have to "bite the bullet".</p></li>
      <li><p>"Kambi" in the name, which is really just a shortcut of my last name, feels like this is too much of a personal project. One-man achievement. Which is, well, kind of true for now... :) But I really want to change it, and hope to get some more developers involved.</p>
        </li>
    </ol>

    <p>Looking back, I should have just made more creative name than just slapping "Kambi" (my last name) + VRML (technology) + "Game Engine" :) For example, <a href="http://www.ogre3d.org/">Ogre</a> has a nice name, that also immediately suggests the engine icon &mdash; ogre\'s head, used also in many Ogre3D examples. I like it.</p>

    <p>We already have a nice engine icon that I like very much (this tower with tentacles and moon thing). It is connected to the fact that main <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a> screenshot, used on various pages, depicts a tower (which, in turn, comes from a very, very old Blender model I made). Also, we made a game called <a href="http://castle-engine.sourceforge.net/castle.php">"The Castle"</a>. So the new name <i>"Castle Game Engine"</i> seems sensible. No more "VRML" in the name, no more "Kambi" in the name. New name has nice connection with engine icon. When googling "castle game engine", we already hit our engine page (because of "The Castle" game).</p>

    <p>Nothing is set in stone yet, so if you have an idea for an even cooler name &mdash; please write e.g. on <a href="http://castle-engine.sourceforge.net/forum.php">forum</a>.</p>

    <p>The engine rename means also renaming many identifiers (as the idea is to get rid of VRML prefix), so this will be a large breakage of API compatibility.</p>

    <p>Note that the rename doesn\'t mean that we drop support for any VRML version (we still fully support VRML 1.0 and VRML 97 (aka VRML 2.0)). It also doesn\'t mean any major redesign of the engine architecture &mdash; just a lot of renames, and of course a usual, steady progress (see below for some new engine features implemented since last release). So, although new engine release will require changes in your code (if you\'re a developer making a game using our engine), it will be relatively trivial, just renames &mdash; no architectural refactoring. I will of course publish a list of important renames (of components and units), and don\'t hesitate to ask (for example <a href="http://castle-engine.sourceforge.net/forum.php">through forum</a>) if you have any questions.</p>

    <p>Of course, old URLs will continue to work for a long time. I\'ll set appropriate redirects wherever possible.</p>

    <p>We also take this opportunity to migrate to the new SourceForge project management <a href="https://sourceforge.net/p/allura/">Allura, aka SF 2.0</a>. The new SourceForge project page is on <a href="https://sourceforge.net/p/castle-engine/">https://sourceforge.net/p/castle-engine/</a> (although old-style URL <a href="http://sourceforge.net/projects/castle-engine/">http://sourceforge.net/projects/castle-engine/</a> also works). You can see there a Wiki (hopefully will work better than HostedApp MediaWiki), new "Discussion" forum (possibly we will use it, instead of PhpBB hosted forum).
  </li>

  <li><p><b>Important user-visible improvements</b> in the <a href="http://castle-engine.sourceforge.net/kambi_vrml_game_engine.php">engine</a> and <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a>:</p>
    <ul>
      <li>Loading of many scenes is now ~2 times faster :), and is done immediately. That is because we fixed octree creation, it was mistakenly done <i>twice</i>, and the 2nd time was on-demand.</li>
      <li>More image formats supported (without the need for installing any external program or library). We use excellent <a href="http://wiki.freepascal.org/fcl-image">FPC fcl-image</a> to load and save various image formats now. This improves reading of some formats (PCX, JPG), and introduces some new formats (GIF, TGA, XPM, PSD).
        <p>Note that some formats, like PNG and DDS, are still read using our internal code (in case on PNG, this relies on libpng that is distributed with Windows binaries and present on all Unix installations). These formats already have good (optimized, and using full format features) reader inside our engine, so no need to change them.</p>
        <p>See <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/docs/glviewimage.html">glviewimage docs in SVN</a> for a current list of supported image formats.</p>
      <li>New view3dscene menu item to take a screenshot with transparent background, <i>Display -&gt; Screenshot To Image (Transparent Background)</i>. (<a href="https://sourceforge.net/apps/phpbb/vrmlengine/viewtopic.php?f=3&amp;t=164">Requested on forum</a>).</li>
      <li>Fixed <i>"Edit -&gt; Remove Selected Face"</i> to allow using it repeatedly, and to work with <tt>IndexedTriangleSet</tt>.</li>
      <li>Fixed "pure shader renderer" on shapes that have textures with full (not only 0/1) alpha channel. In some situations, they were mistakenly drawn with additional alpha test, and effectively some parts of them were invisible. This bug slipped in 3.10.0 release.</li>
      <li><a href="http://castle-engine.sourceforge.net/news.php?id=2011-7-31-development_news__huge_collada_import_improvements">Collada importer improvements, this was mentioned in detail in previous news</a>. Static stuff from Collada models is read almost fully.</li>
    </ul>
  </li>

  <li><p><b>Engine improvements</b>. Next engine version promises some work on engine API to make it simpler and leaner. Some of the larger developer-visible changes:</p>

    <ul>
      <li>Generics everywhere! We have replaced <i>all</i> of our containers with clean FPC/Lazarus generics. We heavily use FGL.TFPGObjectList (classes list), our own TGenericStructList (as <a href="http://bugs.freepascal.org/view.php?id=15480">standard FGL doesn\'t work for records/vectors</a>), and FGL.TFPGList (primitive types lists). We also use standard TFPList instead of TDynPointerArray, and TKamStringList (descendant of standard TStringList) for strings. Removed custom hacky templates objectslist.inc and dynarray.inc.</li>
      <li>Object3DAsVRML is renamed to X3DLoad, with various X3DLoadInternal* helper units.</li>
      <li>Various simplifications around (to KambiGLUtils, to VectorMath) to remove old $ifdef stuff needed for older FPC versions and such. We require now FPC &gt;= 2.2.4 (there is really no sensible way to get our generics working with earlier FPC releases). <!--; and note that 2.2.4 will make a lot of memory leaks, see src/base/fglobjectlist22.pas comments; updating to FPC 2.4.x, preferably just latest 2.4.4, is strongly advised). We also always compile with FPC objfpc mode, which removes the need for various $ifdefs. --></li>
      <li>Under GLX (Unix), we can use new GLX &gt;= 1.3 functions to init context, and we have a testing code to activate OpenGL >= 3.0 forward-compatible context. (<a href="http://bugs.freepascal.org/view.php?id=19953">Relevant patches to FPC also submitted</a>).
        <p>No, we generally don\'t fully work in 3.0 forward context yet, some deprecated features from GL &lt; 3 are still used around, e.g. for fonts. But it\'s nice that we can actually check it now, and push forward.</p>
        <!--p>Better support for using some features from OpenGL core is now done. Previously, we depended on more features being available as extensions (multi-texturing, 3D textures). Using them as extensions makes sense for newer extensions, when many GPUs don\'t have the necessary core yet.</p-->
      </li>
      <li>Camera components in Lazarus package have now icons.</li>
      <li>TBox3D and TQuaternion records remade into old-style object. Previous procedures Box3DXxx(Box, ...) have to be changed now to Box.Xxx(...). This results in nicer API, method names may be shorter and look cleaner.</li>
      <li>T3DTranslated is free from OpenGL dependency, and moves to Base3D unit. This opens the door for T3DTransformed in the near future.</li>
      <li>Removed tgAll and TransparentGroup, this was an unnecessary complication. We have now just Transparent:boolean, and our T3D.Render only renders only opaque or only transparent.</li>
      <li>Notifications (GLNotifications, these messages <i>"You picked up Potion Of Life"</i> etc. you see in FPS games) were reimplemented. There are now TUIControl descendants, so can be used just like our other 2D controls. They are also visible on the Lazarus component palette now.</li>
      <li>Examples rearranged. Many useless examples removed, many good examples visible better. Some new examples are being planned, to showcase some engine features. Also, lets_take_a_walk and rift may be moved inside engine examples.

        <p>Outdated engine examples removed:
          direct_vrmlglscene_test_*, <!--. These were really old examples, showing how to use TVRMLGLScene without the scene manager. This is very discouraged and uncomfortable (scene manager is really needed to get most of the engine), and generally has no point now.</li-->,
          dds_remove_small_mipmaps, <!--. This was a tool to remove from DDS mipmaps the levels with one of the sizes < 4. It workarounds bug in older version of GIMP-DDS, it generated invalid images for smaller mipmaps. The bug is fixed in GIMP-DDS >= 2.0.9. See http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=564111 . Since the fixed version of GIMP-DDS is available since a long time (and the tool didn\'t really serve any other purpose), it\'s removed now.</li-->
          md3tovrmlsequence, <!--. We can convert MD3 to X3D now, Kanim doesn\'t matter anymore.</li -->
          svn_to_grayscale, <!--this was really for PZA only.</li -->
          detect_alpha_simple_yes_no, <!-- test was moved into image_identify</li -->
          demo_parseparameters, <!-- (useless, many programs show this)</li-->
          demo_textreader, <!-- (trivial, and not really important for anyone)</li-->
          test_menu_change_from_keyup, <!-- (was only testing an old bug, solved long time ago)</li-->
          multi_texture_demo, <!--. It can be fully replaced by a trivial X3D model in demo_models/texturing_advanced/multi_tex_order.x3dv</li-->
          test_platform_specific_utils example, <!--moved to automatic tests in tests/testosspecific.pas</li-->
          shading_langs mode, <!-- (Cg, ARB assembly, direct TGLSLProgram usage &mdash; only the last one is really relevant to our engine, and it\'s not advised anyway, usually you should just use GLSL within VRML/X3D mpdels).</li-->
          lazarus/camera. <!-- demo. We have other demos for cameras there. And this duplicates glwindow/demo_camera anyway.</li-->
        </p>

        <!--p>Renamed/moved some examples:</p>
        <ul>
          <li>glwindow/demo_camera to vrml/custom_3d_object, to reflect the *real* purpose of this demo (for camera demo, see every 3d demo).</li>
          <li>dynamic_ambient_occlusion, radiance_transfer, terrain, other demos moved up, to be more visible.</li>
          <li>menu_test, menu_test_alternative merged into glwindow_menu demo.</li>
        </ul-->
      </li>
    </ul>
  </li>
</ol>
'),

    array('title' => 'Development news: huge Collada import improvements',
          'year' => 2011,
          'month' => 7,
          'day' => 31,
          'short_description' => '',
          'description' =>
castle_thumbs(array(
  array('filename' => 'collada_text.png', 'titlealt' => 'Collada 3D text logo (from collada.org/owl)'),
  array('filename' => 'collada_logo.png', 'titlealt' => 'Collada flat logo (from collada.org/owl)'),
  array('filename' => 'collada_faerie_forrest_day.png', 'titlealt' => 'Collada Faerie Forest (from collada.org/owl)'),
  array('filename' => 'collada_faerie_forrest_day_1.png', 'titlealt' => 'Collada Faerie Forest (from collada.org/owl), different view'),
  array('filename' => 'collada_lunar_vehicle_tris.png', 'titlealt' => 'Collada Lunar Vehicle (from collada.org/owl)'),
  array('filename' => 'collada_seymour.png', 'titlealt' => 'Collada Seymour model (from collada.org/owl)'),
  array('filename' => 'collada_turbochi_fromxsi.png', 'titlealt' => 'Collada Turbochi model (from collada.org/owl)'),
)) .
'<p>Our <a href="http://collada.org/">Collada</a> importer received a lot of improvements this week. They improve both rendering of Collada models, and converting them to X3D (as our importer simply loads Collada to X3D node graph in memory). As always, remember that you can try all the new features immediately by downloading a binary from our <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/">nightly builds</a>!</p>

<p>New Collada features handled:</p>

<ul>
  <li>Textures (texture image name, tex coords, tex coord index).</li>
  <li>Normals.</li>
  <li>Cameras (perspective, orthographic &mdash; all possible types) and their properties (field of view, znear, zfar).</li>
  <li>Lights (spot, point, directional, ambient &mdash; all possible types) and their properties (color, attenuation, falloff angle, radius (from Blender profile &lt;dist&gt;)).</li>
  <li>All primitives (trifans, tristrips, lines, linestrips, in addition to previous polygons, polylist, triangles &mdash; all possible types).</li>
  <li>Geometry with many polylists (for example exported from <a href="http://www.blender.org/">Blender</a> when you use multiple materials on a single Blender object).</li>
  <li>Collada 1.5 version is recognized and handled. We now handle any 1.3, 1.4, 1.5.</li>
  <li>Appearances, coordinates, tex coords, normals are correctly reUSEd when converting Collada to X3D.</li>
  <li>Double sided information from Collada is read. There is no standard way to express this in Collada 1.4 as far as I know, we just handle what <a href="http://www.blender.org/">Blender</a> exporter and various <a href="http://collada.org/owl/">Collada test models</a> use inside &lt;geometry&gt;:

<pre>
  &lt;extra&gt;
    &lt;technique profile="MAYA"&gt;
      &lt;double_sided&gt;1&lt;/double_sided&gt;
    &lt;/technique&gt;
  &lt;/extra&gt;
</pre>

    Any value &lt;&gt; 0 as double_sided means "true".</li>
  <li>&lt;lambert&gt; is handled (just like &lt;phong&gt;).</li>
  <li>A lot of fixes (to allow libraries in weird orders, etc.).</li>
</ul>

<p>Our Collada import was tested on many models from <a href="http://collada.org/owl/">COLLADA Test Model Bank</a> and on Collada models exported from <a href="http://www.blender.org/">Blender 2.58</a>.</p>

<p>Note that Blender currently writes incorrect normals (disregarding <i>"Auto Smooth Angle"</i>), this makes some models exported from Blender (Collada as well as X3D) have incorrect shading.<!-- (Todo: link to #bugreport).--></p>

<p>Also, <a href="http://castle-engine.sourceforge.net/demo_models.php">VRML/X3D demo models 3.0.1</a> have been released, with minor fixes and updates, including better webpage text.</p>'),

    array('title' => 'view3dscene 3.10.1, engine 2.5.1 released: XML output',
          'year' => 2011,
          'month' => 7,
          'day' => 25,
          'short_description' => '',
          'description' =>
castle_thumbs(array(
  array('filename' => 'castle_sunset.png', 'titlealt' => '&quot;view3dscene&quot; new demo screen - tower with sunset sky'),
  array('filename' => 'view3dscene_tooltip_and_smoke.png', 'titlealt' => '&quot;view3dscene&quot; new demo screen - Examine navigation tooltip, and some fog by texture layers'),
  array('filename' => 'xmlportals_to_x3d_barricade_0.png', 'titlealt' => 'Testing model for xmlportals_to_x3d'),
  array('filename' => 'xmlportals_to_x3d_barricade_2.png', 'titlealt' => 'Portals visualized by xmlportals_to_x3d'),
)) .
'<p>We present new releases of <b><a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene 3.10.1</a></b>, our VRML/X3D browser and 3D models viewer, and <b><a href="http://castle-engine.sourceforge.net/kambi_vrml_game_engine.php">Kambi VRML game engine 2.5.1</a></b>, the engine underneath.</p>

<ul>
  <li>The main new feature of <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a> is the ability to save files in X3D XML encoding (in addition to previous VRML/X3D classic encoding). This allows you to convert between X3D encodings.</li>
  <li><a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a> can also convert from VRML 2 to X3D (necessary if you want to save VRML 2 to X3D XML, optional if you want to save VRML 2 to X3D classic).</li>
  <li>All conversions are available through three <i>File -&gt; Save As...</i> menu items in view3dscene.</li>
  <li>All conversions can also be done in batch mode, by new command-line options <tt>--write</tt>, <tt>--write-encoding</tt>, <tt>--write-force-x3d</tt>. Old <tt>--write-to-vrml</tt> option becomes deprecated (it\'s a shortcut for <tt>--write --write-encoding=classic</tt>). <a href="http://castle-engine.sourceforge.net/view3dscene.php#section_converting">Documentation of conversion options is here.</a></li>
  <li>Independent <tt>tovrmlx3d</tt> binary is also provided inside view3dscene release, to perform conversion in batch mode. It\'s usually better to use this than <tt>view3dscene --write</tt>, because <tt>tovrmlx3d</tt> is not linked to GUI libraries (so can work on stripped-down systems) and has simpler command-line options (it\'s purpose is only to convert).</li>
  <li>Various small improvements to classic encoding output were also done. We better treat X3D <tt>META</tt> (adding ourselves to <tt>META</tt> <tt>generator/source</tt> fields, moving previous (different) values to <tt>generator-previous/source-previous</tt>). We have better, consistent newlines around <tt>SFNode/MFNode</tt> fields. Command-line conversion does not "expand" <tt>receiveShadows</tt> and <tt>defaultShadowMap</tt> fields.</li>
</ul>

<p>Also, various small improvements to the engine API for developers. Engine 2.6.0 is planned to have some large API improvements, this is a first small step:</p>

<ul>
  <li>Annoying suffixes <tt>_2</tt>, <tt>_3</tt> were removed from most node class names. For example, you can just write <tt>TNodeGroup</tt> or <tt>TNodeTransform</tt> to use <tt>Group</tt> or <tt>Transform</tt> from latest standard version (in these, as in most cases, "latest" node version accounts for both VRML 2.0 and X3D; only the ancient VRML 1.0 nodes need <tt>_1</tt> suffix).
    <p>For compatibility, the old names with suffixes still exist, but you should drop them (assuming of course that you want to target your work for the latest version of the standard, that is X3D &mdash; not VRML 1.0 or 2.0).</p>
  </li>
  <li>Large and useless <tt>SuggestedVRMLVersion</tt> mechanism (to auto-detect standard version before saving) was removed.</li>
  <li><tt>xmlportals_to_x3d</tt> example removed. This was a visualization (and converter to X3D) for an output (Boost serialized data) from Remigiusz Żukowski program to automatically detect portals in 3D scene. (<a href="http://www.ii.uni.wroc.pl/~anl/MGR/">You can read Remigiusz paper (in Polish) here</a>.) This program was not available publicly and is not extended anymore, as far as I know, so I don\'t think that my visualization tool is useful anymore. (It will live in SVN repository anyway.)</li>
  <li>Capturing warnings is simplified. This unfortunately breaks compatibility (otherwise we would need awfully complicated hacks only to keep compatibility). Units <tt>VRMLErrors</tt> and <tt>DataErrors</tt> no longer exist. Unit <tt>KambiWarnings</tt> is added, with <tt>OnWarning</tt> variable, that captures both VRML/X3D and other (Collada, sound files, images) warnings (warning type can be distinguished by <tt>Category</tt> string).</li>
</ul>

<p>Also minor website improvements:</p>

<ul>
  <li>Large <a href="http://castle-engine.sourceforge.net/forum.php">"Helping in the engine development" section on the forum page.</a></li>
  <li>Some new screenshots on <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a> page. The "tower" screenshot, that served as our "iconic" screenshot on various sites, is updated with a sunset lighting and sky.</li>
  <li>Separate <a href="http://castle-engine.sourceforge.net/donate.php">"Donate" page</a>.</li>
</ul>'),

    array('title' => 'view3dscene 3.10.0, engine 2.5.0, demo models 3.0.0 released',
          'year' => 2011,
          'month' => 7,
          'day' => 2,
          'short_description' => '',
          'description' =>
castle_thumbs(array(
  /* shader pipeline */
  array('filename' => 'rhan_shrine_5_everything.png', 'titlealt' => 'Shinto shrine model, from http://opengameart.org/content/shrine-shinto-japan , with multiple shadow maps enabled'),
  array('filename' => 'metallic_shiny.png', 'titlealt' => 'Shiny dark metallic material under multiple lights, with per-pixel lighting.'),
  array('filename' => 'castle_headlight_1.png', 'titlealt' => 'Castle level with sharp spot headlight'),
  array('filename' => 'castle_headlight_3.png', 'titlealt' => 'Castle level with smooth spot headlight'),
  array('filename' => 'light_street_lights_radius_shaders.png', 'titlealt' => 'Lights with radius with shader rendering'),
  /* compositing shaders */
  array('filename' => 'volumetric_animated_fog_all.png', 'titlealt' => 'Volumetric fog'),
  array('filename' => 'fancy_light_spot_shape.png', 'titlealt' => 'Textured spot light with shadow'),
  array('filename' => 'castle_overburn.png', 'titlealt' => 'Castle &quot;overburn&quot; simple effect.'),
  /* converters */
  array('filename' => 'horse_bump_from_3ds.png', 'titlealt' => 'Horse model from 3DS file with bump map'),
  /* demo_models shadows */
  array('filename' => 'shadows_chopper_and_house.png', 'titlealt' => 'Shadow volumes from chopper over a house scenery. Chopper can be moved, rotated, scaled by mouse.'),
  array('filename' => 'fountain_shadows_0.png', 'titlealt' => 'Fountain level model, with shadow volumes.'),
  array('filename' => 'fountain_shadows_1.png', 'titlealt' => 'The same fountain level model, with shadow volumes. After some interactive fun with moving/rotating stuff around :)'),
  /* ColorSetInterpolator */
  array('filename' => 'point_set_colors.png', 'titlealt' => 'PointSet, with coordinates and colors animated (the latter by our ColorSetInterpolator). From Jens van Schelve from www.ssbwindsystems.de.'),
), 2) .
'<p>We\'re proud to release a new stable versions of <b><a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a></b>, our VRML/X3D browser and other 3D models viewer, and <b><a href="http://castle-engine.sourceforge.net/kambi_vrml_game_engine.php">Kambi VRML game engine</a></b>, which is at the core of view3dscene and where all the magic is actually implemented :)</p>

<p>Important new features:</p>

<ul>
  <li><p>We have a new <b>shiny method of rendering everything through shaders</b> (OpenGL Shading Language, aka GLSL). All of the standard X3D rendering features, as well as our extensions, are available in this rendering mode. This results in much better look of many scenes.</p>

    <p>By default, the shader rendering is used only for shapes that require it &mdash; shapes that are shadow map receivers, use bump mapping, or have an explicit shader source code assigned (by <a href="http://castle-engine.sourceforge.net/x3d_implementation_shaders.php">the ComposedShader node</a>, or by the new <a href="http://castle-engine.sourceforge.net/compositing_shaders.php">Compositing Shaders extensions</a>). You can switch <i>"View -&gt; Shaders -&gt; Enable For Everything"</i> in <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a> menu, to have <i>everything</i> rendered through shaders (make sure you have a good GPU).

    <ul>
      <li><p>All the <a href="http://castle-engine.sourceforge.net/x3d_implementation_lighting.php#section_per_pixel_lighting">lighting is calculated per-pixel</a> in shader rendering (we use the <i>Phong shading</i>). This means you see much nicer specular and spot light highlights. Try to make some smooth and curvy metallic surfaces to appreciate it :) Also light\'s attenuation, radius, <tt>SpotLight.beamWidth</tt> are precisely rendered now.</p>

      <li><p>Our <a href="http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_bump_mapping">bump mapping</a> effect is very nicely unified within the new rendering process. Now bump mapping works under all lighting and texturing conditions, works with two-sided lighting, and takes all normal VRML/X3D lights into account.</p>

        <p>This also allows us to make bump mapping "enabled" by default. (You can control it by <i>View -&gt; Bump Mapping</i> menu of view3dscene of course.)</p>
      </li>

      <li><p>The <a href="http://castle-engine.sourceforge.net/x3d_extensions_shadow_maps.php">shadow maps</a> implementation is also nicely unified with new rendering. This gives a huge improvement, as now we take into account the shadows in the correct place of the lighting equation, scaling down only the contribution of the obscured light. So the shadows maps work fully correctly with multiple lights and multiple shadow maps over the same shape.</p>

        <p>Shadow maps also work now with all the multi-texturing possibilities. And, in general, they work with every VRML/X3D lights/materials/textures settings.</p>
      </li>
    </ul>

  <li><p>Another big news is our <b>compositing shaders</b> extensions. The <a href="http://castle-engine.sourceforge.net/compositing_shaders.php">short introduction to our &quot;compositing shaders&quot; idea is here</a>, including the screenshots and information where to find the demo models.</p>

    <p>My paper about this was accepted for <a href="http://www.eguk.org.uk/TPCG11/">Theory and Practice of Computer Graphics 2011 conference</a>, and will be available here publicly later.</p></li>

  <li><p><b>Converters improvements</b>:</p>
    <ul>
      <li>Conversion of 3DS, GEO, Wavefront OBJ, MD3, Collada reimplemented to produce X3D&nbsp;3.2.</li>
      <li>When loading 3DS and Wavefront OBJ, we try harder to find matching texture name.</li>
      <li>We read normalmap (aka bumpmap) information from 3DS and Wavefront OBJ models. This is naturally converted to <a href="http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_bump_mapping">our bump mapping extensions for VRML/X3D</a>.</li>
    </ul>
  </li>

  <li><p>We also release <b>grand new version of our <a href="http://castle-engine.sourceforge.net/demo_models.php">VRML/X3D demo models 3.0.0</a></b> (previously called <i>Kambi VRML test suite</i>). A ton of new models were added, and many were improved &mdash; the whole archive is 65 megabytes now (compared to previous 41 megabytes).

    <p>Most of the stuff is reorganized into directories named after features (like <tt>shadow_maps</tt>, <tt>shaders</tt> etc.) as opposed to versions (like <tt>vrml_2</tt>, <tt>x3d</tt>). Brand new models are mainly in the <tt>compositing_shaders</tt> and <tt>shadow_volumes</tt> directories, but also explore other directories &mdash; the new layout should allow you to discover some old-but-hard-to-find gems inside :) The idea is that new directory names indicate features, so you can immediately see the features of engine and view3dscene.</p>

  <li><p>We also release <b>(in source only) <a href="http://castle-engine.sourceforge.net/castle.php">castle</a> version 0.9.2</b>. This is for developers, so you have <a href="http://castle-engine.sourceforge.net/kambi_vrml_game_engine.php#section_download_src">castle sources compatible with latest engine</a>.</p></li>

  <li><p>Our <a href="http://castle-engine.sourceforge.net/blender.php">Blender X3D exporter</a> is updated to <a href="http://www.blender.org/">Blender 2.58</a>, porting our <tt>normalMap</tt> features.</p></li>

  <li><p><b>Other new features and fixes</b>:</p>
    <ul>
      <li>X3D light source "global" field is correctly supported now.</li>
      <li>You have much more options for customizing the headlight, by new <a href="http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_headlight">KambiNavigationInfo.headlightNode</a> extension. You can put any X3D light (directional, spot, point) there, and it will work.</li>
      <li><a href="http://castle-engine.sourceforge.net/x3d_implementation_interpolation.php#section_vector_interpolator"><tt>VectorInterpolator</tt> extension</a> implemented to animate MFFloat sets, for example to animate <tt>ElevationGrid.set_height</tt>. <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/animation_simple_interpolation_and_time/vector_interpolator.x3dv">Simple demo</a>.</li>
      <li><a href="http://castle-engine.sourceforge.net/x3d_implementation_interpolation.php#section_color_set_interpolator"><tt>ColorSetInterpolator</tt> extension</a> to animate MFColor (set of colors) fields.</li>
      <li><tt>LineProperties</tt> node implemented (<a href="http://castle-engine.sourceforge.net/x3d_implementation_shape.php">see Shape component support</a>).</li>
      <li>Transform animation optimizations.</li>
      <li>Various prototype speed and memory optimizations.</li>
      <li>Crude implementation of <a href="http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_bitmanagement">some BitManagement nodes</a>.</li>
      <li><a href="http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_bump_mapping">heigthMap for bump mapping</a> should be placed within alpha channel of normalmap image, and <i>parallax bump mapping</i> works now much more efficient.</li>
      <li>view3dscene has new <i>File -&gt; Preferences</i> persistent settings for line width (controls all line visualization, like wireframe, bounding box, <tt>LineSet</tt> etc.), point size and default background color.</li>
      <li>VRML 1.0 <tt>PerspectiveCamera.heightAngle</tt> and <tt>OrthographicCamera.height</tt> support.</li>
      <li>Shadow maps <i>PCF 4 bilinear</i> fixes &mdash; it was too dark.</li>
      <li>Various smaller engine fixes (some redraw, camera, fpc versions issues fixed).</li>
      <!-- Remaining bits missing from shader pipeline implemented: <a href="">our volumetric fog</a> and X3D explicit <tt>fogCoord</tt> field handling.
           No space, obvious? -->
    </ul>
  </li>
</ul>

<!--p>This long news message is still only a shortcut of new features :)
For some more in-depth information, see also previous "development news"
(<a href="http://castle-engine.sourceforge.net/news.php?id=2011-6-20-development_news__lights_improvements__shadow_volumes_in_demo_models__hunter%27s_mark__more">2011-6-20</a>,
 <a href="http://castle-engine.sourceforge.net/news.php?id=2011-6-1-development_news__many_shader_pipeline_improvements__many_3ds_wavefront_etc._converters_improvements__more">2011-6-01</a>,
 <a href="http://castle-engine.sourceforge.net/news.php?id=2011-4-21-development_news__engine_2.4.4_fixes_release__colorsetinterpolator">2011-4-21</a>,
 <a href="http://castle-engine.sourceforge.net/news.php?id=2011-4-9-development_news__finishing_shader_rendering__steep_parallax_bump_mapping_reimplemented__engine_2.4.1_fixes_release">2011-4-09</a>,
 <a href="http://castle-engine.sourceforge.net/news.php?id=2011-3-8-development_news__beautiful_shader_rendering__compositing_shaders_extensions__shadow_maps__blender_x3d_exporter_mods__and_more">2011-3-08</a>).</p>
-->
'),

    array('title' => 'Development news: lights improvements, shadow volumes in demo_models, Hunter\'s Mark, more',
          'year' => 2011,
          'month' => 6,
          'day' => 20,
          'short_description' => '',
          'description' =>
castle_thumbs(array(
  array('filename' => "headlight_per_pixel.png", 'titlealt' => 'Spot headlight with per-pixel lighting'),
  array('filename' => 'castle_headlight_1.png', 'titlealt' => 'Castle level with sharp spot headlight'),
  array('filename' => 'castle_headlight_2.png', 'titlealt' => 'Castle level with smooth spot headlight'),
  array('filename' => 'castle_headlight_3.png', 'titlealt' => 'Castle level with smooth spot headlight'),
  array('html' => ''), // empty cell
  array('filename' => 'shadows_chopper_and_house.png', 'titlealt' => 'Shadow volumes from chopper over a house scenery. Chopper can be moved, rotated, scaled by mouse.'),
  array('filename' => 'fountain_shadows_0.png', 'titlealt' => 'Fountain level model, with shadow volumes.'),
  array('filename' => 'fountain_shadows_1.png', 'titlealt' => 'The same fountain level model, with shadow volumes. After some interactive fun with moving/rotating stuff around :)'),
  array('filename' => 'hunters_mark_mainmenu.jpg', 'titlealt' => '&quot;Hunter\'s Mark&quot; - main menu'),
  array('filename' => 'hunters_mark_ingame.jpg', 'titlealt' => '&quot;Hunter\'s Mark&quot; - in game'),
  array('filename' => 'hunters_mark_ingamemenu.jpg', 'titlealt' => '&quot;Hunter\'s Mark&quot; - menu in game'),
  array('filename' => 'hunters_mark_credits.jpg', 'titlealt' => '&quot;Hunter\'s Mark&quot; - credits'),
), 2) .
'<p>Work on next <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a> and <a href="http://castle-engine.sourceforge.net/kambi_vrml_game_engine.php">engine</a> release continues. I hope to release the next versions before the end of this month (it\'s already much delayed according to my plans, sorry :). Some highlights from this month\'s work:</p>

<ul>
  <li><p>All the lights, including headlight, are now correctly passed to the shader pipeline renderer. This means that using spot light as a headlight looks good now if you use <i>View-&gt;Shader-&gt;Enable For Everything</i>, the spot is calculated nicely.</p></li>

  <li><p>You have much more options for customizing the headlight, by new <a href="http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_headlight">KambiNavigationInfo.headlightNode</a> extension. You can put any X3D light (directional, spot, point) there, and it will work.</p>

    <p>Old <tt>KambiHeadLight</tt> node is deprecated and ignored now. It was exposing headlight in a way that was a little alien to other VRML/X3D lights, and is no longer comfortable to implement.</p></li>

  <li><p><tt>SpotLight.beamWidth</tt> is handled perfectly and precisely with shader pipeline. <a href="http://castle-engine.sourceforge.net/x3d_implementation_lighting.php#section_per_pixel_lighting">See per-pixel lighting notes</a></p></li>

  <li><p>Shadow volumes demos are moved to <tt>demo_models/shadow_volumes/</tt> (will be part of next release of <a href="http://castle-engine.sourceforge.net/demo_models.php">demo models</a>). Dragging with mouse moves/scales/rotates stuff (try dragging with Shift or Ctrl pressed), this uses standard VRML/X3D <tt>PlaneSensor</tt> and <tt>SphereSensor</tt> features. So you can actually see that shadow volumes work perfectly on dynamic scenes.</p>

    <p>Old and mostly useless demo <tt>examples/shadow_volume_test/</tt> is removed, all the fun can now be performed by opening the files from <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a> (or any other 3D browser from engine examples).</p></li>

  <li><p>Bartha Rolland (aka B42iso) has made a small game called <i>Hunter\'s Mark</i> using our engine :) The screenshots are visible on the side of this news post. An excerpt from the mail:</p>

    <div class="quote">
    I\'m really amazed at Kambi VRML. It\'s easy to use if you know the basics of Pascal programing language and Lazarus. [...] I was able to create a game using the map models from "The Castle".This game i named "Hunter\'s Mark" is just a project,but I\'m going to start the developing of a real game as soon as I feel like the time is right.
    </div>

    <p>Cool :)</p>
    </li>

  <li><p>Our <a href="http://castle-engine.sourceforge.net/blender.php">Blender X3D exporter</a> was updated to set correct <tt>creaseAngle</tt> in radians. This is also fixed in Blender (<a href="http://projects.blender.org/tracker/index.php?func=detail&amp;aid=27611&amp;group_id=9&amp;atid=127">my patch here, although they fixed it differently, by changing Python value to be already in radians</a>).</p></li>

  <li><p><i>Developers</i>: one large "internal" improvement (not directly visible to user) is that the light sources are now much more comfortable to dynamically assign. You can override <tt>TKamSceneManager.InitializeLights</tt> to add dynamic lights to the scene (feel free to make their existence depend e.g. on time, or player position etc.). You also have <tt>TVRMLGLScene.GlobalLights</tt>, and you can easily make all global lights shine on everything through <tt>TKamSceneManager.UseGlobalLights</tt>.

    <p>This works even for VRML 1.0 (<a href="http://castle-engine.sourceforge.net/x3d_extensions_vrml1.php#section_ext_global">because VRML 1.0 lights can also be global</a>, this is another extension of our engine).</p>

    <p>The old dirty idea of "light set" (load lights from special xxx_lights files into special <tt>TVRMLGLLightSet</tt> class) is removed. This was ugly and complicated. Now, just place your lights inside normal scene (like a level), and use <tt>UseGlobalLights</tt> to shine on everything. More elaborate scenarios are possible through InitializeLights.</p></li>
</ul>
'),

    array('title' => 'Development news: many shader pipeline improvements, many 3DS/Wavefront etc. converters improvements, more',
          'year' => 2011,
          'month' => 6,
          'day' => 1,
          'short_description' => '',
          'description' =>
castle_thumbs(array(
  array('filename' => 'light_street_lights_radius_no_shaders.png', 'titlealt' => 'Lights with radius without shader rendering'),
  array('filename' => 'light_street_lights_radius_shaders.png', 'titlealt' => 'Lights with radius with shader rendering'),
  array('filename' => 'horse_bump_from_3ds.png', 'titlealt' => 'Horse model from 3DS file with bump map'),
)) .
'<p>Work on next <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a> release continues. Some highlights from May:</p>

<ul>
  <li><p><a href="http://castle-engine.sourceforge.net/blender.php">Our Blender X3D exporter</a> updated to latest <a href="http://www.blender.org/">Blender 2.57</a>. Not much difference from upstream now, we just add a small fix and allow using our normalmap extensions.</p></li>

  <li><p>Lights improvements:</p>
    <ul>
      <li>Lights attenuation is correctly rendered in shader pipeline.</li>
      <li>X3D light source "global" field is correctly supported now. (Previously, we were following VRML 97 spec, where directional lights are never global and point/spot lights are always global.)</li>
      <li>Light radius is correctly and precisely (per-pixel) checked in shader pipeline. This allows to use light "radius" for much more dramatic effects, compare 2 screenshots from light_street_lights_radius demo (source model in <a href="http://castle-engine.sourceforge.net/demo_models.php">demo models</a> SVN).</li>
    </ul></li>

  <li><p>Other shader pipeline improvements:</p>
    <ul>
      <li>Shader programs are now correctly cached and shared between shapes. This results in <i>much</i> faster loading and less memory usage for shader pipeline.</li>
      <li>Workarounded ATI fglrx bugs (on newer versions, &gt;= Catalyst 10.10) for shadow maps and bump mapping (<a href="http://forums.amd.com/devforum/messageview.cfm?catid=392&amp;threadid=148827&amp;enterthread=y">this one</a>, and other). Again, fglrx wins the contest for the most buggy OpenGL driver.</li>
      <li>Variance Shadow Maps restored for new shader rendering. Work much better now! (But still should be treated as experimental. You can try <i>View-&gt;Shadow Maps-&gt;Variance Shadow Maps</i>.)</li>
    </ul></li>

  <li><p>Converters improvements:</p>
    <ul>
      <li>Conversion of 3DS, GEO, Wavefront OBJ and MD3 reimplemented to produce X3D (instead of old VRML 1.0).</li>
      <li>When loading 3DS and Wavefront OBJ, we try harder to find matching texture name: we search for texture filename ignoring case (useful if you\'re on case-sensitive file-system, like usually on Unix), we look inside textures/ subdirectory, we try to strip basename from filename in case it was absolute. If original texture filename was not found, but we found a suitable alternative, we use it (notifying you about this by a warning, so you know what going on &mdash; we used somewhat different texture filename).</li>
      <li>We read normalmap (aka bumpmap) information from 3DS or Wavefront OBJ models. This is naturally converted to <a href="http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_bump_mapping">our bump mapping extensions for VRML/X3D</a>.</li>
    </ul></li>

  <li><p><a href="http://castle-engine.sourceforge.net/x3d_implementation_interpolation.php#section_vector_interpolator"><tt>VectorInterpolator</tt> extension</a> implemented to animate MFFloat sets, for example to animate <tt>ElevationGrid.set_height</tt>. <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/animation_simple_interpolation_and_time/vector_interpolator.x3dv">Simple demo</a>.</p></li>
</ul>

<p>You may also be interested in <a href="http://castle-engine.sourceforge.net/compositing_shaders_sem_dokt_polish.pdf">slides, in Polish, about our "compositing shaders" idea</a>. Information in English <a href="http://castle-engine.sourceforge.net/compositing_shaders.php">is available here</a>.</p>

<p>As usual, you can try everything by <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/">our snapshots</a>.</p>
'),

    array('title' => 'Development news: Engine 2.4.4 fixes release, ColorSetInterpolator',
          'year' => 2011,
          'month' => 4,
          'day' => 21,
          'short_description' => '',
          'description' =>
castle_thumbs(array(
  array('filename' => 'vrml_browser_example.png', 'titlealt' => 'vrml_browser - our engine Lazarus example, simple 3D model browser in Lazarus.'),
  array('filename' => 'vrml_with_2d_controls_example.png', 'titlealt' => 'vrml_with_2d_controls - our engine Lazarus example, 3D scene with our 2D controls in OpenGL context.'),
  array('filename' => 'point_set_colors.png', 'titlealt' => 'PointSet, with coordinates and colors animated (the latter by our ColorSetInterpolator). From Jens van Schelve from www.ssbwindsystems.de.'),
)) .
'<p>In the last weeks, we have made a couple of small bugfix releases of <a href="http://castle-engine.sourceforge.net/kambi_vrml_game_engine.php">our engine for developers</a>. Only minimal bugfixes and small improvements:</p>

<ul>
  <li><i>Engine 2.4.2</i>: Fixes the <tt>kambi_glwindow</tt> package compilation with Lazarus 0.9.30 (correct include path). Default <tt>TWalkCamera.GravityUp</tt> is now set to +Y (previously was zero, causing errors with cameras that weren\'t initialized by <tt>TWalkCamera.Init</tt>).</li>
  <li><i>Engine 2.4.3</i>: Fixes the compilation of all examples with Lazarus 0.9.30. This will be checked more automatically for next releases, so Lazarus compilation problems should not occur again. Also fixes the key problems with Lazarus component (not always having initial focus, not always knowing the correct shift/alt/ctrl state).</li>
  <li><i>Engine 2.4.4</i>: TGLMenu improvements (Items, OnClick, some other useful stuff available and published). See <tt>examples/lazarus/vrml_with_2d_controls</tt> for demo.</li>
</ul>

<p>Of course, in the meantime work continues on finishing the engine 2.5.0 with pure shader rendering pipeline :) See <a href="http://castle-engine.sourceforge.net/news.php?id=2011-3-8-development_news__beautiful_shader_rendering__compositing_shaders_extensions__shadow_maps__blender_x3d_exporter_mods__and_more">news post from last month</a> for details.</p>

<p>Also, we added a <a href="http://castle-engine.sourceforge.net/x3d_implementation_interpolation.php#section_color_set_interpolator"><tt>ColorSetInterpolator</tt> extension</a> to animate MFColor (set of colors) fields.</p>
'),

    array('title' => 'Development news: Finishing shader rendering, steep parallax bump mapping reimplemented, engine 2.4.1 fixes release',
          'year' => 2011,
          'month' => 4,
          'day' => 9,
          'short_description' => '',
          'description' =>
castle_thumbs(array(
  array('filename' => 'castle_overburn.png', 'titlealt' => 'Castle &quot;overburn&quot; simple effect.'),
  array('filename' => 'rhan_shrine_solid_wireframe.png', 'titlealt' => 'Solid wireframe rendering.')
)) .
'<p>The work on finishing new shader renderer continues :) Bottom of this post contains more details.</p>

<p><i>Parallax bump mapping</i> (including <i>steep parallax bump mapping with optional self-shadowing</i>) has been reimplemented for the new shader pipeline. (See <a href="http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_bump_mapping">our bump mapping extensions docs</a>.) Specifying height maps for parallax bump mapping is different now: they should be passed as the alpha channel of a normal map texture. This is something that can be:</p>

<ol>
  <li>trivially easily created (in <a href="http://code.google.com/p/gimp-normalmap/">GIMP normal map plugin</a> just set <i>"Alpha Channel"</i> to <i>"Height"</i>),</li>
  <li>passed to OpenGL much faster (no need for passing a separate texture, and no need to combine images at loading).</li>
</ol>

<p>The old method for passing <tt>heightMap</tt> (by a separate <tt>Apperance.heightMap</tt> field) for now simply doesn\'t work &mdash; please report if you need it. I would advice to simply change your textures to the new method, that is put the height-map in the alpha channel of the normal-map. The new method feels easier for both texture creators and for the implementation.</p>

<p>In another news, we released <a href="http://castle-engine.sourceforge.net/kambi_vrml_game_engine.php">Kambi VRML game engine version 2.4.1</a>. This is a bugfix release, compared to 2.4.0 there are no new features, only most critical fixes (ported from SVN work):</p>

<ul>
  <li>fix FPC 2.2.4 and older compilation (like for Lazarus 0.9.28) (thanks to Stephen H. France for reporting this and other issues),</li>
  <li>fix to Lazarus components redraw issues,</li>
  <li>fixes for DRAFT.engine_tutorial (look in doc/ subdirectory),</li>
  <li>added <tt>examples/lazarus/load_model_and_camera_manually</tt>.</li>
</ul>

<p>Some details about changes in trunk:</p>

<ul>
  <li><tt>LineProperties</tt> implemented,</li>
  <li>transform animation optimizations,</li>
  <li>various prototype speed and memory optimizations,</li>
  <li>crude implementation of some BitManagement nodes (Layer2D, Layer3D, others),</li>
  <li>many shader renderer improvements and optimizations.</li>
</ul>
'),

    array('title' => 'Development news: Beautiful shader rendering, compositing shaders extensions, shadow maps, Blender X3D exporter mods, and more',
          'year' => 2011,
          'month' => 3,
          'day' => 8,
          'short_description' => '',
          'description' =>
castle_thumbs(array(
  array('filename' => 'metallic_shiny.png', 'titlealt' => 'Shiny dark metallic material under multiple lights, with per-pixel lighting.'),
  array('filename' => 'castle_light_2.png', 'titlealt' => 'Castle fountain level with shaders, fun with lighting 2'),
  array('filename' => 'castle_light_1.png', 'titlealt' => 'Castle fountain level with shaders, fun with lighting 1'),
  array('filename' => 'castle_light_3.png', 'titlealt' => 'Castle fountain level with shaders, fun with lighting 3'),
  array('filename' => 'volumetric_animated_fog_all.png', 'titlealt' => 'Volumetric fog'),
  array('filename' => 'volumetric_animated_fog_no_fog.png', 'titlealt' => 'Scene for the volumetric fog, here visible with fog turned off'),
  array('filename' => 'volumetric_animated_fog_no_light.png', 'titlealt' => 'Volumetric fog, with normal lighting turned off'),
  array('filename' => 'shadow_map_spot.png', 'titlealt' => 'Spot light casting shadows'),
  array('filename' => 'fancy_light_spot_shape.png', 'titlealt' => 'Textured spot light with shadow'),
  array('filename' => 'flowers.png', 'titlealt' => 'Flowers bending under the wind, transformed on GPU in object space'),
  array('filename' => 'fresnel_and_toon.png', 'titlealt' => 'Toon and Fresnel effects combined'),
  array('filename' => 'noise.png', 'titlealt' => '3D and 2D smooth noise on GPU, wrapped in ShaderTexture'),
), 2) .
'<p>We have some great news about the recent engine developments. As always, remember that you can try all the new features immediately by downloading a binary from our <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/">nightly builds</a>!</p>

<ol>
  <li><p>We have a new <b>shiny method of rendering everything through shaders</b> (OpenGL Shading Language, aka GLSL). All of the standard X3D rendering features, as well as our extensions, are available in this rendering mode. This results in much better look of many scenes.</p>

    <p>By default, the shader rendering is used only for shapes that require it &mdash; shapes that are shadow map receivers, use bump mapping, or have an explicit shader source code assigned (by <a href="http://castle-engine.sourceforge.net/x3d_implementation_shaders.php">the ComposedShader node</a>, or by the new <a href="http://castle-engine.sourceforge.net/compositing_shaders.php">Compositing Shaders extensions</a>). For testing purposes, you can switch to <i>"View -&gt; Shaders -&gt; Enable For Everything"</i> in <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a> menu. The results should be the same (or better) than current renderer.

    <ul>
      <li><p>All the <i>lighting is calculated per-pixel</i> in shader rendering (we use the <i>Phong shading</i>). This is targeted at high-end GPUs anyway, so for now I decided that there\'s no point in making alternative per-vertex lighting (<i>Gouraud shading</i>). This means you should expect much nicer specular and spot light highlights. Try to make some smooth and curvy metallic surfaces to appreciate it :)</p>

        <p>Note that shapes that don\'t need any shader effects are still by default rendered through the fixed-function pipeline. So the performance of simple scenes should not suffer on low-end GPUs.</p></li>

      <li><p>Our <a href="http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_bump_mapping">bump mapping</a> effect is very nicely unified within the new rendering process. Previously we used special bump mapping shaders, limited to common situations. Now bump mapping works under all lighting and texturing conditions, and takes all normal VRML/X3D lights into account.</p>

        <p>We also make bump mapping "enabled" by default.</p>

        <p>Our bump mapping works also with two-side lighting now.</p>
      </li>

      <li><p>The <a href="http://castle-engine.sourceforge.net/x3d_extensions_shadow_maps.php">shadow maps</a> implementation is also nicely unified with new rendering. This gives a huge improvement, as now we take into account the shadows in the correct place of the lighting equation, scaling down only the contribution of the obscured light. So the shadows maps work fully correctly with multiple lights and multiple shadow maps over the same shape.</p>

        <p>Shadow maps also work now with all the multi-texturing possibilities. And, in general, they work with every VRML/X3D lights/materials/textures settings.</p>
      </li>
    </ul>

    <p>New <i>"View -&gt; Shaders"</i> submenu allows to choose when shaders are used:</p>

    <ul>
      <li><i>Disable</i> means <i>never</i>. Our effects will not work, standard ComposedShader will not work, shadow maps will not work. Also, <a href="http://castle-engine.sourceforge.net/x3d_extensions_screen_effects.php">screen effects</a> (that always require shaders) are off.</li>
      <li><i>Enable When Required</i> means that we use fixed-function pipeline for the shapes that don\'t use any shader effects, and shader pipeline for shapes that do.</li>
      <li><i>Enable For Everything</i> means that we use shader pipeline for all the shapes. You will see beautiful per-pixel lighting on everything. On the other hand, rendering may be slower, unless you have really brand new GPU.</li>
    </ul>

    <p>Note that this all in development right now. There are some rough edges for now, that will be fixed before the next release, in particular:</p>

    <ul>
      <li>Shaders caching is not implemented, so scenes with many shapes may take a while to load.</li>
      <li><i>Parallax bump mapping</i> is not ported to the new renderer for now. Only standard bump mapping is available. Similarly, <i>Variance Shadow Maps</i> are not ported &mdash; only standard shadow maps work.</li>
      <li>Some less common multi-texturing options (add/subtract modes and such), and some volumetric fog options, are not ported yet to the new renderer.</li>
    </ul>
  </li>

  <li><p>Another big news is our <b>compositing shaders</b> extensions. The <a href="http://castle-engine.sourceforge.net/compositing_shaders.php">short introduction to our &quot;compositing shaders&quot; idea is here</a>, including the screenshots and information where to find the demo models.</p>

    <p>I wrote a paper about this that will be submitted for the <a href="http://www.web3d2011.org/">Web3D 2011 conference</a>, and will be available here publicly later (I don\'t think I should make it public before being accepted). Drop me a mail if you\'d like to get a peek at my paper PDF earlier.</p></li>

  <li><p><a href="http://castle-engine.sourceforge.net/demo_models.php">"Kambi VRML test suite" will be renamed to "VRML / X3D demo models"</a> upon the next release. The SVN already has many improvements:</p>

    <ul>
      <li>New directory layout, to emphasize foremost what features are tested (shadow_maps, shaders etc.)</li>
      <li>New <tt>compositing_shaders</tt> demos, testing our extensions for compositing shaders.</li>
      <li>Some new shadow_maps demos (sunny_street and others) are added. Previously they were in the <tt>papers/shadow_maps_x3d/</tt> subdir in SVN, which wasn\'t well known.</li>
      <li>Some basic demos from our vrml_engine_doc are added</li>
      <li>Many other fixes. Thanks in particular go to <i>circular</i> for a lot of reports on the <a href="http://www.lazarus.freepascal.org/index.php/topic,12059.0.html">Lazarus forum thread</a>.</li>
    </ul>
  </li>

  <li><p><a href="http://castle-engine.sourceforge.net/blender.php">Blender VRML/X3D exporters page</a> updated, I added there a modified version of Blender 2.56 X3D exporter, fixing small things, and adding exporting of normalMap (for our bump mapping extension).</p></li>

  <li><p>For developers using FPC 2.2.4 (or older): a bug slipped into the last engine sources, preventing compilation with FPC 2.2.4 or older. A fixed version of the sources is released, see <a href="http://castle-engine.sourceforge.net/kambi_vrml_game_engine.php#section_fpc_ver">FPC version notes</a> and <a href="http://castle-engine.sourceforge.net/kambi_vrml_game_engine.php#section_engine_src">engine sources</a>. Thanks to Stephen H. France for reporting this!</p></li>

  <li><p>view3dscene has new <i>File -&gt; Preferences</i> persistent settings for line width (controls all line visualization, like wireframe, bounding box, <tt>LineSet</tt> etc.), point size and default background color.</p></li>

  <li><p>VRML 1.0 <tt>PerspectiveCamera.heightAngle</tt> and <tt>OrthographicCamera.height</tt> support.</p></li>

  <li><p>Shadow maps <i>PCF 4 bilinear</i> fixes &mdash; it was too dark.</p></li>
</ol>
'),

    array('title' => 'view3dscene 3.9.0: new renderer, GLSL attributes, multiple viewports. Also: &quot;fundry&quot;, a way to donate to particular feature',
          'year' => 2011,
          'month' => 2,
          'day' => 6,
          'short_description' => '',
          'description' =>
castle_thumbs(array(
  array('filename' => 'glsl_flutter.png', 'titlealt' => 'GLSL demo &quot;flutter&quot; (from FreeWRL examples)'),
  array('filename' => 'upwind_turbine.png', 'titlealt' => 'Wind turbine simulations, from SSB Wind Systems, with 4 viewports'),
  array('filename' => 'atcs_viewports_frustum.png', 'titlealt' => 'Tremulous ATCS in VRML, with 2 viewports and frustum visualized in right viewport'),
), 1) .
'<p>We\'re proud to release a new version of <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene 3.9.0</a>, our VRML/X3D (and other 3D models) browser. As usual, the new release is accompanied by new <a href="http://castle-engine.sourceforge.net/kambi_vrml_game_engine.php">Kambi VRML game engine 2.4.0</a> (where all the magic actually happens) and new <a href="http://castle-engine.sourceforge.net/kambi_vrml_test_suite.php">Kambi VRML test suite 2.11.0</a> releases.</p>

<ol>
  <li><p>The main new feature of this release is a <b>new modern renderer</b>. It opens the door for pure shader rendering in the next release, which hopefully will blow your mind :) Features already implemented while improving the renderer:</p>

    <ul>
      <li>GLSL attributes from VRML/X3D nodes: support for <a href="http://www.web3d.org/x3d/specifications/ISO-IEC-19775-1.2-X3D-AbstractSpecification/Part01/components/shaders.html"><tt>FloatVertexAttribute</tt>, <tt>Matrix3VertexAttribute</tt>, <tt>Matrix4VertexAttribute</tt> nodes</a>.</li>
      <li><a href="http://web3d.org/x3d/specifications/ISO-IEC-19775-1.2-X3D-AbstractSpecification/Part01/components/enveffects.html#LocalFog">LocalFog</a> support. This allows you to limit (or turn off) fog for particular shapes. Our <a href="http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_fog_volumetric">volumetric fog</a> extensions are available for this type of fog, as well as normal <tt>Fog</tt>.</li>
      <li><a href="http://web3d.org/x3d/specifications/ISO-IEC-19775-1.2-X3D-AbstractSpecification/Part01/components/enveffects.html#FogCoordinate">FogCoordinate</a> node support (explicit per-vertex fog intensities). <a href="http://castle-engine.sourceforge.net/x3d_implementation_environmentaleffects.php">Support details are here</a>.</li>
      <li><a href="http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_bump_mapping">Bump mapping extensions</a> support for every shape (including X3D triangle/quad sets/strips/fans).</li>
      <li><tt>ElevationGrid.creaseAngle</tt> is now working correctly (previously only all smooth or all flat normals were possible for ElevationGrid).</li>
      <li>Loading GLSL shader source from data URI. For example, you can prefix inline shader source with line "<tt>data:text/plain,</tt>", which is a spec-conforming method of putting shader source inline (even though you can still omit it for our engine). See <a href="http://castle-engine.sourceforge.net/x3d_implementation_shaders.php">examples in our docs here</a>, also the "GLSL Vertex Shader" example in <a href="http://freewrl.sourceforge.net/examples.html">FreeWRL examples</a>.</li>
    </ul>

    <p>With the new renderer, you should enjoy better speed on many scenes &mdash; in some cases the improvement is large (although, admittedly, in some cases it\'s not really noticeable). If you\'re curious, some (not impressive, but also not bad) <a href="http://svn.code.sf.net/p/castle-engine/code/branches/view3dscene-old-renderer-for-comparison/STATS.txt">results are here</a>.</p>

    <p>For programmers, a description of how the new renderer works is available in our <a href="http://castle-engine.sourceforge.net/vrml_engine_doc/output/xsl/html/section.vrml_arrays.html">documentation (section "Geometry Arrays")</a>. You can grab it in PDF or other formats from <a href="http://castle-engine.sourceforge.net/vrml_engine_doc.php">here</a>.</p></li>

  <li><p>Another new feature are <b>multiple viewports</b>. This was <a href="http://castle-engine.sourceforge.net/vrml_engine_doc/output/xsl/html/section.custom_viewports.html">already implemented in our engine</a>, now it\'s used in view3dscene. Just open any scene, and try the new <i>Display -&gt; 1/2/4 viewports</i> menu items, and you will see what I mean. Hope you like this :) Remember that the main (upper-left) viewport is still the central one, for example it controls the headlight.</p>

    <p>Thanks to Jens van Schelve for suggesting this. A cool fact: the guys at <a href="http://www.ssbwindsystems.de/">SSB Wind Systems</a> are using our <a href="http://castle-engine.sourceforge.net/view3dscene.php">view3dscene</a> to visualize wind turbine simulations :) You can see a screenshot of their simulation output on the right.</p></li>

  <li><p>Other important new features / fixes:</p>
    <ul>
      <li>The <b>screenshot options work now more reliably</b> on modern GPUs (that have <i>Framebuffer Object</i>). This allows to hide the window during screenshot process on all the platforms, and capture larger image sizes reliably. See <a href="http://castle-engine.sourceforge.net/view3dscene.php#section_screenshot"><tt>--screenshot</tt> and <tt>--screenshot-range</tt> options documentation</a>.</li>
      <li><tt>TouchSensor.hitTexCoord_changed</tt> implemented, <tt>hitNormal_changed</tt> improved to generate smooth normals. See <a href="http://castle-engine.sourceforge.net/x3d_implementation_pointingdevicesensor.php">support details here</a>.</li>
      <li>For programmers, an improved TVRMLShape.LocalTriangulate callback is available. See <a href="https://sourceforge.net/apps/phpbb/vrmlengine/viewtopic.php?f=3&amp;t=25">this forum thread for more information</a>.</li>
    </ul>
  </li>

  <li><p>A new website feature is the possibility to <b>donate money specifically for implementing a particular feature</b>: <a href="https://fundry.com/project/91-kambi-vrml-game-engine">go to fundry page for our engine</a>. The fundry widget is available also on the <a href="http://castle-engine.sourceforge.net/donate.php">Donate page</a>.</p></li>

  <li><p>At the end: I decided to <b>deprecate some of our old extensions</b>. As far as I know noone used them, and they are rather useless in the light of new features:</p>

    <ul>
      <li>Fog.alternative &mdash; useless, because all decent (even old) GPUs support EXT_fog_coord. And for ancient GPUs automatic fallback to the non-volumetric fog works well enough.
      <li>Material.fogImmune &mdash; useless, as newly implemented LocalFog node allows you to locally disable fog too. LocalFog also allows much more (like locally <i>enable</i> fog), and it\'s part of the X3D specification. So our poor Material.fogImmune extension has no place anymore.
      <li>Also, menu item to switch "Smooth Shading" is removed from view3dscene menu. Forcing flat shading on the whole scene seemed rather useless debug feature. You can always set IndexedFaceSet.creaseAngle = 0 in your files (in fact it\'s the default) to achieve the same effect.
    </ul>
  </li>
</ol>'),

    array('title' => 'Development news: first milestone of new renderer reached, GLSL attributes and other new features',
          'year' => 2011,
          'month' => 1,
          'day' => 18,
          'short_description' => '',
          'description' =>
castle_thumbs(array(
  array('filename' => 'glsl_flutter.png', 'titlealt' => 'GLSL demo &quot;flutter&quot; (from FreeWRL examples)'),
  array('filename' => 'venus_spheremap.png', 'titlealt' => 'Venus model with environment sphere mapping (model referenced from FreeWRL examples)'),
), 1) .
'<p>I have committed to SVN a large rework of our renderer. Everything is now rendered through <i>locked interleaved vertex arrays</i>. And I mean <i>everything</i>, really every feature of VRML/X3D shapes &mdash; all colors, normals, texture coords etc. are loaded through vertex arrays. This opens wide the door for much more optimized, modern renderer using exclusively VBOs for nearest release. It will also eventually allow OpenGL ES version for modern mobile phones. (But shhhhh, this is all not ready yet.)</p>

<p>Improvements already done while improving our renderer:</p>

<ul>
  <li>GLSL attributes from VRML/X3D nodes: support for <tt>FloatVertexAttribute</tt>, <tt>Matrix3VertexAttribute</tt>, <tt>Matrix4VertexAttribute</tt> nodes.</li>
  <li><a href="http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_bump_mapping">Bump mapping extensions</a> support for every shape (including X3D triangle/quad sets/strips/fans).</li>
  <li><tt>ElevationGrid.creaseAngle</tt> is now working correctly (previously only all smooth or all flat normals were possible for ElevationGrid).</li>
  <li><tt>FogCoordinate</tt> node support (explicit per-vertex fog intensities).</li>
  <li>Loading GLSL shader source from data URI. For example, you can prefix inline shader source with line "<tt>data:text/plain,</tt>", which is a spec-conforming method of putting shader source inline (even though you can still omit it for our engine). For a demo, see the "GLSL Vertex Shader" example in <a href="http://freewrl.sourceforge.net/examples.html">FreeWRL examples</a>.</li>
</ul>

<p>As always, you can test the latest development version by downloading binary from our <a href="http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/">nightly builds</a>.</p>

<p>(Note that the <tt>Text</tt> nodes are an exception, they don\'t benefit from new renderer features. Parts of <tt>Text</tt> geometry are rendered through a different method, that is not integrated with vertex arrays. This will not be touched for next release, text nodes are not that much important.)</p>

<p>(Note that the fglrx (ATI Radeon proprietary drivers under Linux) sucks, as always. GLSL vertex attributes, and bump mapping, currently require that you change <i>Preferences -&gt; Rendering Optimizaton</i> to None. That\'s because glEnableVertexArrayARB seemingly doesn\'t work inside display lists. This problem doesn\'t occur with any other drivers (even with Radeon drivers for the same graphic card but on Mac OS X), so it\'s another clear fglrx fault. This will be fixed nicer before release, as VBO renderer without display lists will probably avoid these problems entirely.)</p>'),

    array('title' => 'view3dscene 3.8.0: 3D sound, skinned H-Anim, more',
          'year' => 2011,
          'month' => 1,
          'day' => 6,
          'short_description' => '',
          'description' =>
castle_thumbs(array(
  array('filename' => 'sound.png', 'titlealt' => 'Sound demo (from Kambi VRML test suite)'),
  array('filename' => 'lucy_test.png', 'titlealt' => 'Lucy (from Seamless3d test page)'),
  array('filename' => 'lucy_joints_visualization.png', 'titlealt' => 'Lucy with our joints visualization'),
), 1) .
'<p><b>3D sound in VRML/X3D</b> worlds is implemented. Grab the new ' . news_a_href_page('view3dscene 3.8.0', 'view3dscene') . ', and for some demo open files <tt>x3d/sound_final.x3dv</tt> and <tt>x3d/sound_location_animate.x3dv</tt> from the ' . news_a_href_page('kambi_vrml_test_suite', 'kambi_vrml_test_suite') . '. ' . news_a_href_page('Detailed documentation for Sound support is here', 'x3d_implementation_sound') . '.</p>

<p>Note that you have to install some additional libraries to hear sounds (OpenAL to hear anything, and VorbisFile to load OggVorbis format). For Windows, these are already included in the zip file, and you actually don\'t have to do anything. For Linux, you should install them using your package managar. For Mac OS X, ' . news_a_href_page('OpenAL is already preinstalled and you can get VorbisFile from fink', 'macosx_requirements') . '.</p>

<p>If you want to mute / unmute sound, you can use <i>File -&gt; Preferences -&gt; Sound</i> menu item of view3dscene. There\'s also <i>File -&gt; Preferences -&gt; Sound Device</i> choice. ' . news_a_href_page('Command-line options for controlling sound are documented here', 'openal') . '.</p>

<p>For developers, as usual we release a <b>new <a href="http://castle-engine.sourceforge.net/kambi_vrml_game_engine.php">Kambi VRML game engine 2.3.0</a></b>. Besides sound in VRML/X3D, you will notice a new shiny SoundEngine (instance of TALSoundEngine, in ALSoundEngine unit) that makes using OpenAL a breeze from ObjectPascal code. Sample usage:</p>

<pre class="sourcecode">
var Buffer: TALBuffer;
...
Buffer := SoundEngine.LoadBuffer(\'sample.wav\');
SoundEngine.PlaySound(Buffer, ...); // see TALSoundEngine.PlaySound parameters
</pre>

<p>See the <a href="http://castle-engine.sourceforge.net/reference.php">engine reference</a>, in particular <a href="http://castle-engine.sourceforge.net/apidoc/html/ALSoundEngine.TALSoundEngine.html">TALSoundEngine class reference</a>, for details. You can try adding this code spinnet to any example in engine sources, e.g. to the <tt>examples/vrml/scene_manager_demos.lpr</tt>.</p>

<p><b>Animating skinned H-Anim humanoids</b> is also implemented. You can use view3dscene to open e.g. <a href="http://www.seamless3d.com/browser_test/index.html">"Lucy" examples</a> from Seamless3D, also "The famous boxman" linked from the bottom of <a href="http://doc.instantreality.org/tutorial/humanoid-animation/">InstantReality H-Anim overview</a>. The <a href="http://castle-engine.sourceforge.net/x3d_implementation_hanim.php">details about H-Anim support are here</a>. The new view3dscene menu item <i>"Edit -&gt; Add Humanoids Joints Visualization"</i> may be useful too.</p>

<p>See also the video below. At first you see InstantReality results and then the view3dscene. Thanks to Peter "griff" Griffith for testing and creating this video!</p>

' . (!HTML_VALIDATION ? '<object width="425" height="344"><param name="movie" value="http://www.youtube.com/v/v20CFbKWAYU?fs=1&amp;hl=pl_PL"></param><param name="allowFullScreen" value="true"></param><param name="allowscriptaccess" value="always"></param><embed src="http://www.youtube.com/v/v20CFbKWAYU?fs=1&amp;hl=pl_PL" type="application/x-shockwave-flash" allowscriptaccess="always" allowfullscreen="true" width="425" height="344"></embed></object>' : '') . '

<p>Some <b>other notable features</b> implemented:</p>

<ul>
  <li><tt>MultiGeneratedTextureCoordinate</tt> node introduced, to better define the <a href="http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_tex_coord">Box/Cone/Cylinder/Sphere.texCoord</a>.</li>
  <li><a href="http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_tex_coord_bounds">Texture coord generation dependent on bounding box (TextureCoordinateGenerator.mode = BOUNDS*)</a>. This allowed fixing shadow maps implementation for the case when shape has a texture but no explicit texture coordinate node.</li>
  <li><a href="http://castle-engine.sourceforge.net/reference.php">Engine reference for developers</a> improved a lot.</li>
</ul>

<p>Also <b>' . news_a_href_page('castle 0.9.0', 'castle') . ' is released</b>. This doesn\'t bring any new user-visible features, however internally a lot of stuff was simplified and ported to our engine 2.x line.</p>')

);
