<?php
require_once 'castle_engine_functions.php';
castle_header('Planned features (roadmap)', array(
  'path' => array('documentation')
));
echo pretty_heading($page_title);
?>

<p>Don't see the feature you need? <a href="talk.php">Tell us on the forum or Discord</a>.

<p>If you would like to see some feature implemented sooner,
please <a href="<?php echo PATREON_URL; ?>">support the engine development on Patreon!</a>

<p>Incoming features in the next release (7.0) <a href="https://github.com/castle-engine/castle-engine/wiki/New-Features-in-Castle-Game-Engine-7.0">are documented here</a>.

<?php /*
<h2>Incoming in the next release (6.8)</h2>

<ul>

</ul>
*/ ?>

<h2>Future plans</h2>

<ul>
  <li><p><b>More visual editor</b>.

    <p>Extend our <a href="manual_editor.php">editor</a>.

    <ul>
      <li><p>Add gizmos to manipulate (translate / rotate / scale) 3D and 2D objects inside a viewport / scene manager.
      <li><p>Make the "files browser" at the bottom more useful (allow to drag assets onto a design).
      <li><p>Cover all use-cases mentioned in <a href="https://castle-engine.io/wp/2017/12/23/plans-6-4-release-asap-visual-editor-soon-2018-roadmap/">original post about the editor</a>:
        <ol>
          <li>I want to edit things within Lazarus and Delphi (like <i>GLScene</i> and <i>FireMonkey 3d</i>),
          <li>I want to have a standalone editor (like <i>Unity3d</i>; check!),
          <li>I want to edit at runtime too.
        </ol>
      <li><p>Implement <a href="https://github.com/castle-engine/castle-engine/blob/master/tools/castle-editor/README.md">everything planned for the editor</a>, and close all <a href="https://github.com/castle-engine/castle-engine/blob/master/tools/castle-editor/TODO.md">TODOs</a> :)
    </ul>

  <li><p><b>Delphi compatibility</b> (was planned for 6.6 release, postponed to 6.8).

    <p>Already started, <a href="supported_compilers.php">see here</a>.

    <?php /*
    <ul>
      <li><p>This will the free version of Delphi available as
        <a href="https://www.embarcadero.com/products/delphi/starter">Delphi Starter Edition</a>.
        I'll port (and maintain compatibility) with the latest
        <i>Delphi Starter Edition</i> available.
        Embarcadero seems willing to update the <i>Delphi Starter Edition</i>
        with each new version of Delphi, which is quite great.

        <!--
         &mdash; kudos to them for
        allowing us hobbyists to allow using Delphi,
        just as in the good old days!
        -->

      <li><p>Of course, bugreports and patches to fix support for any other Delphi version
        are also always welcome.

      <li><p>If someone wants to sponsor another Delphi version,
        then I can maintain a port to it too.

        <p>I'm afraid that any commercial edition of Delphi
        <a href="https://www.embarcadero.com/app-development-tools-store/delphi">costs a <i>lot</i></a>,
        so I cannot afford to buy it on my own.
        Especially since, personally, I usually work on Linux with FPC + Lazarus these days:)
    </ul>
    */ ?>

    <p>If you want to help in this effort by coding:

    <ul>
      <li><p>The suggested approach is to extend the existing "examples/delphi/base_tests/base_tests.dpr" to use more and more CGE units, fixing any Delphi compatibility that occurs (e.g. XML units, JSON units etc.).

      <li><p>The order of adding units can folow <a href="https://github.com/castle-engine/castle-engine/wiki/Units-Map">the map of dependencies</a>. So first we make sure all/most from "base" compiles, then "files", then "images", then "fonts"...

      <li><p>Once we get to CastleUIControls unit compiling, we can implement TCastleControl on VCL and/or FMX, and actually display something.
    </ul>

    <!--
    <p>Michalis is still an open-source fanatic,
    and most of the engine development happens on Linux, so don't worry
    &mdash; the FPC still gets, and will forever get, a first-class support.
    But I think that Delphi compatibility will open up the engine to many
    new developers, and it's also relatively easy to add.
    and I may improve some APIs BTW (like a vector3 API),
    and it will open us to more developers.
    -->
  </li>

  <li><p><b>Load glTF more efficiently to GPU.</b>

    <p>We should read meshes in a way that keeps binary layout of data in glTF, so that it is ultra-fast to load, and it is loaded almost straight from glTF to GPU.

    <p>Some modifications to CGE may be required here (CastleGeometryArrays will have to be made more flexible for this). New X3D nodes should be introduced, like <code>BufferGeometry</code> (same as X3DOM?).

    <p>We should also perform skinning fully on GPU, avoiding <code>CoordinateInterpolator</code> done on CPU.

  <li><p><b>More physics</b></p>
    <p>More integration with physics engine. The details are listed in the <a href="manual_physics.php">manual about physics</a>.
  </li>

  <li><p><b>Release mobile view3dscene (as Android and iOS application)</b>

    <p><a href="https://github.com/castle-engine/view3dscene-mobile">This is almost done, thanks to Jan Adamec.</a> We need to release it :)

    <p>Associated with X3D and other 3D / 2D formats that the view3dscene (and <i>Castle Game Engine</i>) handles. Available in the App Store / Google Play Store. For free or for 1 USD (not sure yet; but definitely without ads, I really dislike ads).</p>

  <li><p><b>Continuous integration (Jenkins) for free for all open-source projects.</b>

    <p>We have a <a href="https://github.com/castle-engine/castle-engine/wiki/Cloud-Builds-(Jenkins)">Jenkins + Docker infrastructure</a> to compile and test CGE, and CGE-based applications. I want to finish some stuff there and make it publicly accessible, and free for open-source projects.

  <li><p><b>Terrain designer</b>

    <p>Easily design a height map (X3D <?php api_link('ElevationGrid', 'X3DNodes.TElevationGridNode.html'); ?> node, with trees, rocks, grass). Saved and loaded as an X3D file.

    <p>Implementing this feature will most likely be split into a couple of small releases:</p>

    <ol>
      <li><p>Edit the heights.</li>
      <li><p>Edit the grass and trees and rocks.</li>
      <li><p>Efficiently render huge amounts of grass and trees and rocks.</li>
    </ol>

    <p>Implement nice shaders to show it, like
    <a href="https://www.getlazarus.org/videos/bareterrain/#learning_resources">this BareGame example</a>.

    <p><i>What we have now:</i> The engine includes a unit <code>CastleTerrain</code> to generate terrains in various ways (most notably, from smoothed random noise). We have <code>examples/terrain/</code> demo to play around with it. We have the <a href="https://github.com/castle-engine/wyrd-forest">"Wyrd Forest"</a> game that also uses <code>CastleTerrain</code>, and also includes a simple editor of terrain settings.

    <p><i>What we need:</i> Visual, interactive editor for the <code>ElevationGrid</code> (the thing you get from <code>TTerrain.Node</code> call). To make hills, valleys in a comfortable way. And comfortably plant there stuff (rocks, grass, trees...).

<?php /*
  <li><p><b>Blender X3D exporter improvements</b>

    <p><i>Rejected. Just export to glTF, the exporter is way better than X3D exporter ever was. glTF has more momentum as an interchange format than X3D.</i>

    <p>Current Blender X3D exporter doesn't support animations,
    configuring collisions (X3D <?php api_link('Collision', 'X3DNodes.TCollisionNode.html'); ?> node),
    3D sound sources and more.
    We would like to fix it!:) This will be useful for everyone using Blender and X3D,
    not only with our engine.

    <p>See also our page about <a href="creating_data_blender.php">creating data in Blender</a>
    and <a href="https://github.com/castle-engine/castle-engine/wiki/Blender">hints
    about exporting from Blender to X3D</a>.
*/ ?>

  <li><p><b>VR / AR</b>

    <p>Android Cardboard? And/or other VR devices &mdash; depending on demand, and our access to the test devices.

  <li><p><b>Ready components to replicate data over the Internet</b>

    <p>Allowing to trivially get multi-playter functionality in your games.

  <li><p><b>More renderers</b>

    <p>Vulkan renderer.

    <p>Maybe Metal renderer. Only <i>maybe</i>, as it's an API used by only one platform &mdash; iOS. So far, OpenGLES serves us good on iOS. In practice, this depends on the future, how much will Metal matter in a few years.

    <p>Maybe Direct3D renderer. Only <i>maybe</i>, as it's an API used only on Windows. So far, OpenGL serves us good. The rare platforms where OpenGL had problems on Windows are 1. really old right now (really old Intel GPUs), 2. we can consider using an OpenGLES->Direct3D bridge, like ANGLE, for them.

    <p>Help with this is most welcome. <i>We have a simple example code that shows how you can start a new renderer</i>: see <a href="https://github.com/castle-engine/castle-engine/blob/master/examples/research_special_rendering_methods/new_renderer_skeleton/new_renderer_skeleton.lpr">new_renderer_skeleton.lpr</a>. So <a href="https://github.com/castle-engine/castle-engine/">get the engine from GitHub</a>, and start changing the <code>new_renderer_skeleton.lpr</code>. Just fill the <code>TCastleSceneVulkan.PrepareResources</code> and <code>TCastleSceneVulkan.Render</code> inside.

  <li><p><b>Make TCastleScene, T3DTranform and friends to be special X3D nodes</b>

    <p>This would make the whole scene manager a single graph of X3D nodes,
    allowing for more sharing in code.
    The T3DTranform would be just like TTransformNode, but a little diferently
    optimized (but it would become toggable).

  <li><p><b>Distance field fonts</b>

    <p>See <a href="https://github.com/libgdx/libgdx/wiki/Distance-field-fonts">about Distance field fonts</a>.
    See code from <a href="https://github.com/neurolabusc/OpenGLCoreTutorials">Chris Rorden</a> showing
    how to do it in Lazarus.

  <li><p><b>WebGL (HTML5) support</b>

    <p>This is already possible &mdash; since pas2js supports WebGL, and has generics and <code>Generics.Collections</code> unit, and in general should be able to consume CGE code. It remains to actually do it, i.e. give our code to pas2js and go through all necessary changes. <a href="https://castle-engine.io/wp/2020/07/20/pas2js-with-generics-rocks-and-makes-castle-game-engine-for-webgl-possible/">See this news post.</a>

    <p>Another future possibility is to use FPC to recompile to WebAsembly (through LLVM or direct WebAsembly backend, both are in progress in FPC).
  <li>
    <p><b>Scripting in JavaScript</b></p>
    <p>Allow to use JavaScript (ECMAScript) directly inside VRML/X3D files (in Script nodes). This will follow VRML/X3D specification. Implementation will be through <a href="http://besen.sourceforge.net/">besen</a> (preferably, if it will work well enough), SpiderMonkey, or maybe some other JS library.</p>
  </li>

  <li>
    <p><b>Particle systems</b>

    <p>With a designer.

    <p>Maybe following the X3D "particle system" component, so it will be saved and loaded as an X3D file.

    <p>Note: My <b>highest</b> priority is to have particle system that has a great visual designer.
    Having something conforming to X3D "particle system" component would be great,
    but it is not highest priority.
    Having the visual designer available in CGE editor would also be great,
    but it is not the most critical priority.

    <p>My current probable approach is to develop own particle system,
    within CGE editor,
    based on X3D "particle system" and extending it freely as I need.
    But other approaches may be possible.

    <p>Example usage: blood, snow, rain, fire, smoke... 2D, 3D.

    <p>It would be nice to be able to export Blender particle engine to it, but possibly it's not really doable (different particle physics in Blender than X3D, we should go with own designer following more X3D).

    <!-- Use ARB_point_sprite? -->

    <p><i>(Note that we already have 2D particle system in CGE, see <a href="https://github.com/Kagamma/cge-2d-particle-emitter">cge-2d-particle-emitter by Kagamma</a>)</i></p>

  </li>

  <li>
    <p>Make a demo showing how to use Castle Game Engine together with <a href="https://github.com/BeRo1985/rnl">RNL - a real-time network library, in Pascal, very suitable for games, by Benjamin Rosseaux</a>.

  <li>
    <p><b>Make 100% rendering features available on OpenGLES too.</b>

    <p><a href="https://github.com/castle-engine/castle-engine/wiki/OpenGL-ES,-Android-and-iOS-TODOs">We have some small TODOs to make OpenGLES (mobile) renderer as capable as OpenGL (desktop).</a>

  <li>
    <p><b>Use Cocoa under macOS</b></p>

    <p>We already have a native look and feel, and easy installation,
    under macOS by
    and <a href="macosx_requirements.php">using LCL with Carbon or Cocoa on macOS</a>.
    Our programs do not have to use X11 and GTK under macOS.

    <p>However, the current solution is not optimal.
    Carbon is deprecated and only 32-bit (so memory is limited).
    Cocoa is not stable in LCL yet.
    Depending on LCL has it's own problems
    (mouse look is not smooth with LCL message loop).

    <p>The proposed task is to implement nice Cocoa backend
    in <code>CastleWindow</code> unit. You can of course look at LCL implementation
    of Cocoa widgetset, but I expect that we can implement it muuuch simpler
    (and more stable) as CastleWindow backend.
    <i>Contributions are welcome.
    This is an easy and rewarding task for a developer interested in macOS.</i>
  </li>

  <li><p><b>API improvement: Make <code>TCastleViewport.MainScene</code> not necessary</b>.

    <p>In the future, everything should be possible without assigning
    <code>TCastleViewport.MainScene</code>,
    and eventually <code>TCastleViewport.MainScene</code> property should be deprecated,
    later it should be ignored, and later it should be removed.

    <p>Currently, the engine requires you to assign scene to
    <code>TCastleViewport.MainScene</code>
    to achieve some features. For example:

    <ol>
      <li><p>To have some X3D lights shining on <i>all</i>
        scenes, these lights should be in <code>Viewport.Items.MainScene</code>.
      <li><p>To use a <code>Background</code> or <code>Fog</code> X3D node,
        this node needs to be inside <code>Viewport.Items.MainScene</code>.
      <li><p>The initial camera position may be derived from <code>Viewpoint</code>
        in the <code>Viewport.Items.MainScene</code>.
    </ol>

    <p>All these features should have a different solution,
    that doesn't require marking any scene as "main".
    This is not a trivial job, and it needs case-by-case solutions.
    For example (addressing above examples):

    <ol>
      <li><p>Every scene could have a Boolean property like
        <code>TCastleScene.LightsAffectAllScenes</code>.
      <li><p>The editor could allow to point a specific <code>Background</code> or <code>Fog</code>
        node to be currently used (which could come from any X3D scene).
      <li><p>The initial camera position can already be given in various
        alternative ways.
    </ol>

  <li><p><b>API improvement: CreatureCreatures / CastleResources / CastleLevels / CastleItems / CastlePlayer should be replaced with a better API.</b>

    <p>These 5 units (CreatureCreatures / CastleResources / CastleLevels / CastleItems / CastlePlayer) expose a high-level API, that sits on top on existing classes (like TCastleScene and TCastleTransform). But I am not 100% happy with their API. Reasons:

    <ol>
      <li><p>The API is too specialized at some points (3D games with creatures / items pickable),
      <li><p>It is confusing how it maps to API underneath (e.g. TPlayer somewhat controls the TCamera).
    </ol>

    <p>Gradually I will want to express their features in different ways, in ways that are more closely and obviously connected with TCastleScene / TCastleViewport / TCamera. Basically, I'm very happy with current API of TCastleScene and TCastleTransform, and I want to make it more obvious how it is related to creatures/placeholders and other concepts introduced in CastleLevels and CastleCreatures and CastkeResources. Currently they use TCastleScene and TCastleTransform inside, but the relationship is non-obvious.

    <p>It's not going to happen any time soon, but it will happen gradually, over the course of next 1 or more years. That is, some of the features of the 5 units mentioned above will become available in other ways too (more flexible, and more obviously connected to TCastleScene).

    <p>I know this sounds vague, because I have not yet clarified these plans in my head:) These 5 units *are* useful, they provide various features on top of TCastleScene. I'm not going to just "ditch" them or even deprecate them, before I made a better API that also has these features. For example:

    <ul>
      <li><p>New TCastleScene.Load should be able to take a callback (or some class instance) to easily perform the "placeholders" functionality of <?php api_link('TLevel.Load', 'CastleLevels.TLevel.html#Load'); ?> in a flexible manner (user can decide what to replace with what).

      <li><p>There will be TCastleSceneView that provides part of the functionality of T3DResource (multiple TCastleSceneView share a single TCastleScene but can show different animation frame of it), but without some often-unnecessary "baggage" from T3DResource (like refcounting of T3DResource, and it's special Prepare/Release methods).

      <li><p>It should be possible to add an A.I. (like TWalkAttackLogic) to any TCastleTransform instance.
    </ul>

    <p>I know I want to go in this direction. Based on the questions (including on Discord) I see that the API of these 5 units is not clear to people. It wraps TCastleScene / TCastleViewport / TCamera, but in ways that are not obvious, and that is something I want to improve.

    <p>Again, bear in mind that it will not happen any time soon :) You can safely and happily use these units, probably for a few years to come.

    <p>But it is something I think about for the future, and it may explain some of my decisions. E.g. that is why I don't plan special integration of TCastleCreature with castle-editor. Instead I want to enable you to add future TWalkAttackLogic to any TCastleTransform one day. And thus we will have "easy creatures" in CGE but with more flexible API.

  <li><p><b>Bridge TGLImage with TCastleScene, enable drawing UI into TCastleScene, to have UI rotated in 3D with perspective.</b>

    <p><a href="https://github.com/castle-engine/castle-engine/wiki/2D-Games">See also here.</a>
</ul>

<?php castle_footer(); ?>
