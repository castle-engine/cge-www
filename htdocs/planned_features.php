<?php
require_once 'castle_engine_functions.php';
castle_header('Planned features (roadmap)', array(
  'path' => array('documentation')
));
echo pretty_heading($page_title);
?>

<p>Don't see the feature you need? <a href="<?php echo FORUM_URL; ?>">Tell us on the forum</a>:)

<p>If you would like to see some feature implemented sooner,
please <a href="<?php echo PATREON_URL; ?>">support the engine development on Patreon!</a>

<h2>Incoming in the next release (6.6)</h2>

<ul>
  <li><p><b>Prototype of a visual editor</b>.

    <p><a href="https://castle-engine.io/wp/2018/09/01/castle-game-engine-editor/">In progress, see here</a>. And <a href="https://castle-engine.io/wp/2017/12/23/plans-6-4-release-asap-visual-editor-soon-2018-roadmap/">see here for plans.</a>
</ul>

<h2>Future plans</h2>

<ul>
  <li><p><b>Delphi compatibility</b> (was planned for 6.6 release, postponed to 6.8)

    <p>As for the Delphi version: <a href="supported_compilers.php">see here</a>.

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

    <p><a href="https://castle-engine.io/wp/2017/08/14/delphi-base-compatibility-spine-improvements-other-stuff/">This is already in-progress, see here.</a>

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

  <li><p><b>Fully working visual editor</b>

    <p>I outlined my details in <a href="https://castle-engine.io/wp/2017/12/23/plans-6-4-release-asap-visual-editor-soon-2018-roadmap/">this post</a>.
    I want to edit things within Lazarus and Delphi (like <i>GLScene</i> and <i>FireMonkey 3d</i>),
    and I want to have a standalone editor (like <i>Unity3d</i>),
    and I want to edit at runtime too.
    And I think we can do it:)

    <!--
    <p>Implementing this feature will most likely be split into a couple of small releases:</p>

    <ol>
      <li><p>Visually design 2D controls within <code>TCastleControl</code>.
      <li><p>Visually design T3D hierarchy in <code>SceneManager.Items</code>.
        So you could look and move whole <code>TCastleScene</code> instances.
      <li><p>Make published, and editable through Lazarus, various properties
        currently only public. E.g. we need to figure out some way to edit
        vectors and matrices in Lazarus (they are records/old-style objects
        internally).
      <li><p>Design X3D nodes hierarchy (inside the TCastleScene) visually,
        which means you can edit shapes, materials, textures...
    </ol>
    -->

  <li><p><b>More physics</b></p>
    <p>More integration with physics engine. The details are listed in the <a href="manual_physics.php">manual about physics</a>.
  </li>

  <li><p><b>Mobile view3dscene (as Android and iOS application)</b>

    <p>Associated with X3D and other 3D / 2D formats that the view3dscene (and <i>Castle Game Engine</i>) handles. Available in the App Store / Google Play Store. For free or for 1 USD (not sure yet; but definitely without ads, I really dislike ads).</p>

    <p>I have not decided yet whether it would be based on our <a href="https://github.com/castle-engine/view3dscene">view3dscene source code</a>, as desktop "view3dscene" contains a ton of GUI features that would not work comfortably on Android / iOS. Instead, we can develop a simple application that allows to open files, switch navigation type, turn on/off collisions and make screenshots (features that are available through the current view3dscene toolbar).

    <p><a href="https://github.com/castle-engine/view3dscene-mobile">This is already started by Jan Adamec!</a>

  <li><p><b>glTF format support and PBR</b>

    <p>glTF is a cool format for 3D models by Khronos. See <a href="https://www.khronos.org/gltf">glTF overview</a>, <a href="https://github.com/KhronosGroup/glTF">glTF specification and extensions</a>, <a href="https://github.com/KhronosGroup/glTF-Sample-Models">glTF sample models</a>, <a href="https://github.com/KhronosGroup/glTF-Blender-Exporter">Blender glTF 2.0 exporter</a>.

    <p>It supports meshes, advanced materials, animations. The file format is a readable JSON, but additional binary files are used to transfer coordinates, so it's fast to load from disk straight to GPU. It's also a Khronos format, so it's developed by people who really know what they are doing (the same people develop OpenGL[ES] and WebGL, Vulkan, KTX, Collada ...).

    <p>Because of this, it may (or already is?) become a widely supported 3D format across a range of 3D software. So we want to have really good support for it &mdash; reading all the features (including animations), and preserving the efficiency of binary-encoded meshes (to do this, we will probably invent some new X3D nodes).

    <p><a href="https://castle-engine.io/wp/2017/12/23/plans-6-4-release-asap-visual-editor-soon-2018-roadmap/">See this post for my 2018 roadmap -- it includes glTF and PBR.</a>

    <p><a href="https://github.com/michaliskambi/x3d-tests/wiki/Include-PhysicalMaterial-and-PhysicalEnvironmentLight-in-the-official-X3D-specification">See here for documentation what is PBR (Physically-based Rendering) and do I plan to add it to X3D and Castle Game Engine, and thus seamlessly render glTF materials too</a>.

    <p>Strategy of implementation (if you'd like to help, <a href="talk.php">speak up!</a>):

    <ol>
      <li><p>(<i>This is already started in <a href="https://github.com/castle-engine/castle-engine/tree/pasgltf">branch pasgltf</a></i>:)

        <p>Create a unit <code>X3DLoadIternalGLTF</code>, consistent with other such units e.g. <code>src/x3d/x3dloadinternalstl.pas</code>.

        <p>It should define a function <code>LoadGLTF</code> that takes a URL and returns TX3DRootNode. See the examples in all other x3dloadinternal*.pas how to construct X3D graph, and read the documentation of our <a href="vrml_x3d.php">scene graph (X3D)</a> to know what is possible.

        <p>The function <code>LoadGLTF</code> should be "registered", which for now just means that glTF should be added as a new format to X3DLoad unit. Just follow what other formats are doing (like STL) and add appropriate few lines.

        <p>Inside <code>LoadGLTF</code>, read the file (using <code>Download(URL)</code>) and parse it.

        <p>One approach to parsing glTF is to use Bero's <a href="https://github.com/BeRo1985/pasgltf">PasGLTF</a>. This is what I advice to try first. You will need to figure our the API by reading the source code and examples, but it should be straightforward. You will need to add PasGLTF along with dependencies (like PasJSON) to CGE source code, just like Kraft is added now (see in src/compatibilty/).

        <p><b>The stuff mentioned above is already started in <a href="https://github.com/castle-engine/castle-engine/tree/pasgltf">branch pasgltf</a>. You can use it, and CGE will be able to open glTF 2.0 files using PasGLTF, and output basic information about them on the log.</b>

      <li><p><i>Only if PasGLTF will be unsuitable for some reason (but this is plan B, don't start with this!)</i>: We can also implement our own glTF reading. Just use FpJson (which is the JSON unit we already use for Spine loading and <code>CastleComponentSerialize</code>) for the JSON part.

      <li><p>The initial task is to read meshes with transformations. I expect you will have to use nodes like <code>Transform</code>, <code>Shape</code>, <code>IndexedFaceSet</code>, <code>Coordinate</code>.

      <li><p>I expect you to test it with <a href="https://github.com/KhronosGroup/glTF-Sample-Models">glTF sample models</a> and the output of <a href="https://github.com/KhronosGroup/glTF-Blender-Exporter">Blender glTF exporter</a>.

      <li><p>Once the above is working, we can look into more advanced glTF features:
        <ol>
          <li>Read textures and texture coords.
          <li>Reading meshes in a way that keeps binary layout of data in glTF, so that it is ultra-fast to load, and it is loaded almost straight from glTF to GPU. Some modifications to CGE may be required here (CastleGeometryArrays will have to be made more flexible for this). Maybe some new X3D nodes should be introduced.
          <li>Reading materials with PBR parameters. Modifications to CGE, including adding new X3D nodes will be needed for this, see my links above about PBR.
          <li>Reading animations. X3D should be capable of expressing glTF animations, using interpolators and H-Anim for skinned animation.
        </ol>
    </ol>

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
      <li><p><b>Animations blending</b>

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

      <li><p><b>Batching</b>

        <p>Batching of shapes that have equal appearance for start, to optimize the rendering.

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
    <p><b>Support Material.mirror field for OpenGL rendering</b></p>

    <p><i>(This feature is already in progress, but with somewhat different API: Instead of Material.mirror, we give you <a href="https://castle-engine.io/wp/2018/06/16/easy-mirrors-on-flat-surfaces-more-progress/">more straightforward way to use RenderedTexture for a mirror</a>. This approach is different, doesn't look so perfect, but it has many other advantages, it is also more straightforward to maintain. So, for practical purposes, it replaces the Material.mirror idea.)</i></p>

    <p>An easy way to make planar (on flat surfaces) mirrors. Just set Material.mirror field to something > 0 (setting it to 1.0 means it's a perfect mirror, setting it to 0.5 means that half of the visible color is coming from the mirrored image, and half from normal material color).</p>
    <p>Disadvantages: This will require an additional rendering pass for such shape (so expect some slowdown for really large scenes). Also your shape will have to be mostly planar (we will derive a single plane equation by looking at your vertexes).</p>
    <p>Advantages: The resulting mirror image looks perfect (there's no texture pixelation or anything), as the mirror is actually just a specially rendered view of a scene. The mirror always shows the current scene (there are no problems with dynamic scenes, as mirror is rendered each time).</p>
    <p>This will be some counterpart to current way of making mirrors by RenderedTexture (on flat surfaces) or <a href="x3d_implementation_cubemaptexturing.php">GeneratedCubeMap</a> (on curvy surfaces).</p>
  </li>

  <li>
    <p>Make a demo showing how to use Castle Game Engine together with <a href="https://github.com/BeRo1985/rnl">RNL - a real-time network library, in Pascal, very suitable for games, by Benjamin Rosseaux</a>.

  <li>
    <p><b>Advanced networking support</b></p>
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

      <li><p>(Low priority) <b>Support for <code>ftp</code></b>. By using LNet or Synapse, unless
        something ready in FPC appears in the meantime.
        Both LNet (through LFtp unit) and Synapse (FtpGetFile) support ftp.

      <li><p>(Low priority) <b>Support for HTTP basic authentication</b>. This can be done in our
        CastleDownload unit. Although it would be cleaner to implement it
        at FpHttpClient level, see
        <a href="http://bugs.freepascal.org/view.php?id=24335">this
        proposal</a>.
        Or maybe just use LNet or Synapse, I'm sure they have some support
        for it.

      <li><p>(Low priority) <b>Support X3D <code>LoadSensor</code> node</b>.

      <li><p>(Low priority) <b>Caching on disk of downloaded data</b>.
        Just like WWW browsers, we should be able to cache
        resources downloaded from the Internet.
        <ul>
          <li>Store each resource under a filename in cache directory.
          <li>Add a function like ApplicationCache, similar existing ApplicationData
            and ApplicationConfig, to detect cache directory.
            For starters, it can be ApplicationConfig (it cannot be
            ApplicationData, as ApplicationData may be read-only).
            Long-term, it should be something else (using the same
            directory as for config files may not be adviced,
            e.g. to allow users to backup config without backuping cache).
            See standards suitable for each OS (for Linux, and generally Unix
            (but not macOS) see <a href="http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html">basedir-spec</a>;
            specific Microsoft, Apple specs may be available
            for Windows and macOS).
          <li>Probably it's best to store a resource under a filename
            calculated by MD5 hash on the URL.
          <li>For starters, you can just make the max cache life
            a configurable variable for user.
            Long-term: Like WWW browsers, we should honor various HTTP headers that
            say when/how long cache can be considered valid,
            when we should at least check with server if something changed,
            when we should just reload the file every time.
          <li>Regardless of above, a way to reload file forcibly disregarding
            the cache should be available (like Ctrl+Shift+R in WWW browsers).
          <li>A setting to control max cache size on disk, with some reasonable
            default (look at WWW browsers default settings) should be available.
        </ul>

        <p>Note: don't worry about caching in memory, we have this already,
        for all URLs (local files, data URIs, network resources).
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
</ul>

<?php castle_footer(); ?>
