<?php

/* Next news:
- kambiShadows, kambiShadowsMain fields renamed to shadowVolumes, shadowVolumesMain ---- much more sensible names. But the old names will remain to be available, for compatibility (probably for a long time).
  TCastleAbstractViewport.ShadowVolumes is true by default now.
*/

array_push($news,
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
      <li>New methods to define creature/items (all resources) animations are available. You can load animation from <a href="http://castle-engine.sourceforge.net/kanim_format.php">kanim</a>, or from multiple VRML/X3D files (one VRML/X3D for each animation), or from a single VRML/X3D file (in which case the file will have to contain different TimeSensor to activate each animation). Description how it works is inside <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/castle_game_engine/doc/DRAFT.modeling_tutorial.txt">DRAFT.modeling_tutorial.txt</a> for now. Example of all 3 methods to define animations are inside <tt>castle_game_engine/examples/resource_animations/</tt> in sources.</li>
      <li>Many improvements to our <tt>RenderDebug3D</tt> and <tt>RenderDebugCaptions</tt> mechanisms (you can see them e.g. in fps_game demo): respect Orientation (work with games with either +Y or +Z up), sphere is displayed better,  caption is multi-line and contains "enemy distance" info.</li>
      <li><tt>TItem.Picked</tt> method can be overridden to configure what happens when you pick item. Example code in fps_game shows how to make an item that is consumed on pickup (aka "power-up").</li>
      <li><tt>TextureProperties</tt> mechanism was enhanced into <tt>MaterialProperties</tt>, that can describe much more things. In the future, we can add there more stuff about material/texture that cannot be comfortably expressed in Blender, but that is conceptually tied to material/texture. For now, the main addition is that you can define bump mapping by normal_map properties there. The hack in <a href="http://castle-engine.sourceforge.net/blender.php">our Blender exporter</a> (to autodetect xxx_normalmap textures) will possibly be removed at some point (as it has no future, it\'s dirty).</li>
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
      <li><tt>CastleImages</tt> supports resizing images with bilinear interpolation. <tt>TCastleImage.Resize</tt> and <tt>TCastleImage.MakeResized</tt> take <tt>Interpolation</tt> parameter, <tt>riNearest</tt> or <tt>riBilinear</tt>. Default is still <tt>riNearest</tt>, so fast (and ugly) like before. <tt>CastleWindowProgress</tt>, when it needs to scale progress background image, requests <tt>riBilinear</tt> interpolation. <a href="http://castle-engine.sourceforge.net/glviewimage.php">glViewImage</a> allows to test both resizing methods.</li>
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

  <li><p><b>The caustics demo</b> (you seen a movie of this <a href="http://castle-engine.sourceforge.net/news.php?id=2012-07-10">in earlier news</a>) is now committed to our <a href="http://castle-engine.sourceforge.net/demo_models.php">demo models</a>. Many thanks to Victor Amat! I hope to use these ideas to make such water available out-of-the-box for all games using our engine (right now you will have to adjust shaders for your specific case).</p></li>
</ol>')
);
