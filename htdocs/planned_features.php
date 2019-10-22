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

<p>Incoming features in the next release (6.6) <a href="https://github.com/castle-engine/castle-engine/wiki/New-Features-in-Castle-Game-Engine-6.6">are documented here</a>.

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
      <li><p>Make the "files browser" at the bottom actually useful (display asset icons, allow to drag them onto a design etc.).
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

  <li><p><b>More glTF: PBR, efficient meshes, animations</b>

    <p><i>Note that static glTF models already load OK in CGE.</i>

    <p>Things to do:</p>

    <ul>
      <li><p>Reading meshes in a way that keeps binary layout of data in glTF, so that it is ultra-fast to load, and it is loaded almost straight from glTF to GPU.

        <p>Some modifications to CGE may be required here (CastleGeometryArrays will have to be made more flexible for this). New X3D nodes should be introduced, like <code>BufferGeometry</code> (same as X3DOM?).

      <li><p>Rendering PBR materials. <a href="https://github.com/michaliskambi/x3d-tests/wiki/Include-PhysicalMaterial-and-PhysicalEnvironmentLight-in-the-official-X3D-specification">See here for documentation what is PBR (Physically-based Rendering) and do I plan to add it to X3D and Castle Game Engine, and thus seamlessly render glTF materials too</a>.

      <li><p>Reading animations. X3D should be capable of expressing glTF animations, using interpolators and H-Anim for skinned animation.
    </ul>

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

  <li><p><b>Blender X3D exporter improvements</b>

    <p>Current Blender X3D exporter doesn't support animations,
    configuring collisions (X3D <?php api_link('Collision', 'X3DNodes.TCollisionNode.html'); ?> node),
    3D sound sources and more.
    We would like to fix it!:) This will be useful for everyone using Blender and X3D,
    not only with our engine.

    <p>See also our page about <a href="creating_data_blender.php">creating data in Blender</a>
    and <a href="https://github.com/castle-engine/castle-engine/wiki/Blender">hints
    about exporting from Blender to X3D</a>.

    <p><i>Depending on the future, it is possible that this feature will be replaced with "better support for glTF animations".</i> Currently, the reality is that glTF has more momentum as an interchange format than X3D.

  <li><p><b>Android Cardboard (VR)</b>

    <p>Maybe also other VR devices &mdash; depending on demand, and our access to test devices.

  <li><p><b>Ready components to replicate data over the Internet</b>

    <p>Allowing to trivially get multi-playter functionality in your games.

  <li><p><b>More renderers</b>

    <p>Vulkan renderer.

    <p>Maybe Metal renderer. Only <i>maybe</i>, as it's an API used by only one platform &mdash; iOS. So far, OpenGLES serves us good on iOS. In practice, this depends on the future, how much will Metal matter in a few years.

    <p>Maybe Direct3D renderer. Only <i>maybe</i>, as it's an API used only on Windows. So far, OpenGL serves us good. The rare platforms where OpenGL had problems on Windows are 1. really old right now (really old Intel GPUs), 2. we can consider using an OpenGLES->Direct3D bridge, like ANGLE, for them.

    <p>Help with this is most welcome. <i>We have a simple example code that shows how you can start a new renderer</i>: see <a href="https://github.com/castle-engine/castle-engine/blob/master/examples/research_special_rendering_methods/new_renderer_skeleton/new_renderer_skeleton.lpr">new_renderer_skeleton.lpr</a>. So <a href="https://github.com/castle-engine/castle-engine/">get the engine from GitHub</a>, and start changing the <code>new_renderer_skeleton.lpr</code>. Just fill the <code>TCastleSceneVulkan.PrepareResources</code> and <code>TCastleSceneVulkan.Render</code> inside.

  <li><p><b>Larger scene processing and rendering improvements:</b>

    <ol>
      <li><p><b>More animations blending</b>

        <p>To smoothly fade in/out animation,
        with cross-fade between animations,
        for things played by
        <?php api_link('TCastleScene.PlayAnimation', 'CastleSceneCore.TCastleSceneCore.html#PlayAnimation'); ?>.

        <p>Animation cross-fade time for
        <a href="creating_data_resources.php">creatures from resource.xml files</a>
        could be configured using a CastleScript expression,
        so values like this could be possible:

<pre>
fade_duration="0.5"
fade_duration="animation_duration * 0.1"
fade_duration="min(animation_duration * 0.25, target_animation_duration * 0.25, 0.5)"
</pre>

        <p><i><a href="https://castle-engine.io/wp/2018/03/21/animation-blending/">Animation blending is already working, see here!</a> However, it still has two TODOs for now (it does not yet work for castle-anim-frames or resource.xml).</i>

      <li><p><b>Make TCastleScene, T3DTranform and friends to be special X3D nodes</b>

        <p>This would make the whole scene manager a single graph of X3D nodes,
        allowing for more sharing in code.
        The T3DTranform would be just like TTransformNode, but a little diferently
        optimized (but it would become toggable).

      <li><b>Distance field fonts</b>

        <p>See <a href="https://github.com/libgdx/libgdx/wiki/Distance-field-fonts">about Distance field fonts</a>.
        See code from <a href="https://github.com/neurolabusc/OpenGLCoreTutorials">Chris Rorden</a> showing
        how to do it in Lazarus.
    </ol>

  <li><p><b>WebGL (HTML5) support</b>

    <p>But this waits for the possibility from FPC to recompile to web (that is, JS or WebAsembly, probably through LLVM). Then our engine will jump on to the web platform. (Unlike the <a href="https://github.com/castle-engine/castle-engine/wiki/Web-Plugin">current web plugin</a>, which is soon deprecated by all browsers, unfortunately.)

  <li>
    <p><b>Scripting in JavaScript</b></p>
    <p>Allow to use JavaScript (ECMAScript) directly inside VRML/X3D files (in Script nodes). This will follow VRML/X3D specification. Implementation will be through <a href="http://besen.sourceforge.net/">besen</a> (preferably, if it will work well enough), SpiderMonkey, or maybe some other JS library.</p>
  </li>

  <li>
    <p><b>Particle systems</b>
    <p>With a designer, probably.

    <p>Probably following the X3D "particle system" component, so it will be saved and loaded as an X3D file.

    <p>Example usage: blood, snow, rain, fire, smoke... 2D, 3D.

    <p>It would be nice to be able to export Blender particle engine to it, but possibly it's not really doable (different particle physics in Blender than X3D, we should go with own designer following more X3D).

    <!-- Use ARB_point_sprite? -->

    <p><i>(Note that we already have 2D particle system in CGE, see <a href="https://github.com/Kagamma/cge-2d-particle-emitter">cge-2d-particle-emitter by Kagamma</a>)</i></p>

  </li>

  <li>
    <p>Make a demo showing how to use Castle Game Engine together with <a href="https://github.com/BeRo1985/rnl">RNL - a real-time network library, in Pascal, very suitable for games, by Benjamin Rosseaux</a>.

  <li>
    <p><b>More networking support</b></p>
    <p>Basic networking support is working already, see <a href="https://castle-engine.io/manual_network.php">the manual</a>.

    <p>Missing features / ideas:

    <ol>
      <li><p>Maybe integrate with
        <a href="http://lnet.wordpress.com/">LNet</a> or
        <a href="http://www.ararat.cz/synapse/">Synapse</a>, see also nice
        intro to Synapse on <a href="http://wiki.freepascal.org/Synapse">FPC wiki</a>.
        Maybe <a href="http://www.indyproject.org/index.en.aspx">Indy</a>.

        <p>Bear in mind that future engine version should work under both FPC and Delphi,
        so choosing one library that works under both FPC and Delphi is a plus.

      <li><p><b>Asynchronous downloading</b>.

        <p>So that you don't need to hang waiting for download.

        <p>The API design is already inside <code>castledownload.pas</code>,
        look for the line "<i>API for asynchronous downloader is below, not implemented yet</i>".

        <p>Using threading (<code>TThread</code>) to implement this is optional, as you can update the data
        during the <code>ApplicationProperties.OnUpdate</code> in the main thread
        (if only you use non-blocking API like LNet).
        Note that you need to use non-blocking API anyway (as we must be able to cancel
        the ongoing download, and you cannot instantly unconditionally terminate a running <code>TThread</code>).
        Using threads may still be reasonable for efficiency (no need to slow down
        the main thread), but then it should be 100% invisible to
        the user of <code>TDownload</code> class. From the point of view
        of engine user, the <code>TDownload</code> must be available in the main thread.

        <!--p>Using separate thread(s) for download seems like a good idea,
        the synchronization is not difficult as the thread needs only
        to report when it finished work.

        <p>The difficult part is reliably breaking the download.
        Using something like <code>TThread.Terminate</code> will not do anything
        good while the thread is hanging waiting for socket data
        (<code>TThread.Terminate</code> is a graceful way to close the thread,
        it only works as often as the thread explicitly checks
        <code>TThread.Terminated</code>). Hacks like <code>Windows.TerminateThread</code>
        are 1. OS-specific 2. very dirty,
        as <code>TThread.Execute</code> has no change to release allocated memory
        and such.
        The bottom line: <i>merely using TThread does <b>not</b> give
        you a reliable and clean way to break the thread execution at any time</i>.

        <p>This suggests that you <i>have</i> to use non-blocking
        API (so LNet or Sockets is the way to go,
        FpHttpClient and Synapse are useless for this)
        if you want to reliably break the download.
        Using it inside a separate thread may still be a good idea,
        to not hang the main event loop to process downloaded data.
        So the correct answer seems <i>use LNet/Sockets (not
        FpHttpClient/Synapse), with non-blocking API, within a TThread;
        thanks to non-blocking API you can guarantee checking
        <code>TThread.Terminated</code> at regular intervals</i>.

        <p>I'm no expert in threads and networking, so if anyone has
        any comments about this (including just comfirming my analysis)
        please let me (Michalis) know :)
        -->

        <!--
        http://wiki.freepascal.org/Example_of_multi-threaded_application:_array_of_threads
        http://www.freepascal.org/docs-html/rtl/classes/tthread.html
        http://stackoverflow.com/questions/4044855/how-to-kill-a-thread-in-delphi
        http://stackoverflow.com/questions/1089482/a-proper-way-of-destroying-a-tthread-object
        http://stackoverflow.com/questions/3788743/correct-thread-destroy
        -->


        <p>This will also enable <i>cancelling the ongoing download</i>.
        Maybe add a "cancel" button to <code>CastleWindowProgress</code> to cancel
        ongoing view3dscene downloads.
  </ol>

  <li>
    <p><b>Easy way to use 3rd-person camera movement in your games.</b>

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

  <li><p><b>API improvement: Make <code>SceneManager.MainScene</code> not necessary</b>.

    <p>In the future, everything should be possible without assigning
    <code>SceneManager.MainScene</code>,
    and eventually <code>SceneManager.MainScene</code> property should be deprecated,
    later it should be ignored, and later it should be removed.

    <p>Currently, the engine requires you to assign scene to
    <code>SceneManager.MainScene</code>
    to achieve some features. For example:

    <ol>
      <li><p>To have some X3D lights shining on <i>all</i>
        scenes, these lights should be in <code>SceneManager.MainScene</code>.
      <li><p>To use a <code>Background</code> or <code>Fog</code> X3D node,
        this node needs to be inside <code>SceneManager.MainScene</code>.
      <li><p>The initial camera position may be derived from <code>Viewpoint</code>
        in the <code>SceneManager.MainScene</code>.
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

    <p>Gradually I will want to express their features in different ways, in ways that are more closely and obviously connected with TCastleScene / TCastleSceneManager / TCamera. Basically, I'm very happy with current API of TCastleScene and TCastleTransform, and I want to make it more obvious how it is related to creatures/placeholders and other concepts introduced in CastleLevels and CastleCreatures and CastkeResources. Currently they use TCastleScene and TCastleTransform inside, but the relationship is non-obvious.

    <p>It's not going to happen any time soon, but it will happen gradually, over the course of next 1 or more years. That is, some of the features of the 5 units mentioned above will become available in other ways too (more flexible, and more obviously connected to TCastleScene).

    <p>I know this sounds vague, because I have not yet clarified these plans in my head:) These 5 units *are* useful, they provide various features on top of TCastleScene. I'm not going to just "ditch" them or even deprecate them, before I made a better API that also has these features. For example:

    <ul>
      <li><p>New TCastleScene.Load should be able to take a callback (or some class instance) to easily perform the "placeholders" functionality of <?php api_link('TGameSceneManager.Load', 'CastleLevels.TGameSceneManager.html#LoadLevel'); ?> in a flexible manner (user can decide what to replace with what).

      <li><p>There will be TCastleSceneView that provides part of the functionality of T3DResource (multiple TCastleSceneView share a single TCastleScene but can show different animation frame of it), but without some often-unnecessary "baggage" from T3DResource (like refcounting of T3DResource, and it's special Prepare/Release methods).

      <li><p>It should be possible to add an A.I. (like TWalkAttackLogic) to any TCastleTransform instance.
    </ul>

    <p>I know I want to go in this direction. Based on the questions (including on Discord) I see that the API of these 5 units is not clear to people. It wraps TCastleScene / TCastleSceneManager / TCamera, but in ways that are not obvious, and that is something I want to improve.

    <p>Again, bear in mind that it will not happen any time soon :) You can safely and happily use these units, probably for a few years to come.

    <p>But it is something I think about for the future, and it may explain some of my decisions. E.g. that is why I don't plan special integration of TCastleCreature with castle-editor. Instead I want to enable you to add future TWalkAttackLogic to any TCastleTransform one day. And thus we will have "easy creatures" in CGE but with more flexible API.

  <li><p><b>Bridge TGLImage with TCastleScene, enable drawing UI into TCastleScene, to have UI rotated in 3D with perspective.</b>

    <p><a href="https://github.com/castle-engine/castle-engine/wiki/2D-Games">See also here.</a>
</ul>

<?php castle_footer(); ?>
