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

<h2>Incoming in the next release (6.4.0)</h2>

<ul>
  <li><p><b>Delphi compatibility</b>

    <p>As for the Delphi version:
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

    <p><a href="https://castle-engine.sourceforge.io/wp/2017/08/14/delphi-base-compatibility-spine-improvements-other-stuff/">This is already in-progress, see here.</a>

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

  <li><p><b>API improvements</b>. I plan to attempt
    some long-planned API upgrades. These include:

    <ul>
      <li><p>(DONE) Ultra-flexible <code>TCastleTransform</code> class
        instead of most current <code>T3D*</code> classes.
        The unit <code>Castle3D</code> will be replaced by a new unit
        <code>CastleTransform</code> with the <code>TCastleTransform</code> class,
        that will perform the tasks of current <code>T3D</code>,
        <code>T3DCustomTransform</code>,
        <code>T3DTransform</code>,
        <code>T3DOrient</code> classes.
        So it's more flexible, and has a name that does not (falsely) suggest
        it's only for 3D games.

        <p>Also <code>TCastleTransform</code> will be an ancestor
        of <code>TCastleScene</code>.
        So you will be able to change <code>Translation</code>,
        <code>Rotation</code> etc. of a
        <code>TCastleScene</code> directly, without the need to "wrap"
        it inside <code>TCastleTransform</code> container.

      <li><p>(DONE) Vector API improvements, using <code>TVector3</code>
        as an advanced record, instead of current <code>TVector3Single</code>.
        <a href="https://castle-engine.sourceforge.io/wp/2017/07/23/new-modern-api-for-vectors-and-matrices/">This is already done, see here.</a>
    </ul>
</ul>

<h2>Future plans</h2>

<ul>
  <li><p><b>Visual designing of the castle components</b>

    <p>Editing of engine 3D and 2D things within Lazarus and Delphi.
    For a "start":
    1. designing the scenes and transformations
    under <code>SceneManager.Items</code> and 2. designing the 2D controls under
    <code>TCastleControl.Controls</code>.

    <p>Inside a Lazarus form. Like <i>GLScene</i> and <i>FireMonkey 3d</i>.

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

  <li><p><b>Physics</b></p>
    <p>More integration with physics engine. The details are listed in the <a href="manual_physics.php">manual about physics</a>.
  </li>

  <li><p><b>Mobile view3dscene (as Android and iOS application)</b>

    <p>Associated with X3D and other 3D / 2D formats that the view3dscene (and <i>Castle Game Engine</i>) handles. Available in the App Store / Google Play Store. For free or for 1 USD (not sure yet; but definitely without ads, I really dislike ads).</p>

    <p>I have not decided yet whether it would be based on our <a href="https://github.com/castle-engine/view3dscene">view3dscene source code</a>, as desktop "view3dscene" contains a ton of GUI features that would not work comfortably on Android / iOS. Instead, we can develop a simple application that allows to open files, switch navigation type, turn on/off collisions and make screenshots (features that are available through the current view3dscene toolbar).

  <li><p><b>glTF format support</b>

    <p>glTF is a cool format for 3D models by Khronos. See <a href="https://www.khronos.org/gltf">glTF overview</a>, <a href="https://github.com/KhronosGroup/glTF">glTF specification and extensions</a>, <a href="https://github.com/KhronosGroup/glTF-Sample-Models">glTF sample models</a>, <a href="https://github.com/KhronosGroup/glTF-Blender-Exporter">Blender glTF 2.0 exporter</a>.

    <p>It supports meshes, advanced materials, animations. The file format is a readable JSON, but additional binary files are used to transfer coordinates, so it's fast to load from disk straight to GPU. It's also a Khronos format, so it's developed by people who really know what they are doing (the same people develop OpenGL[ES] and WebGL, Vulkan, KTX, Collada ...).

    <p>Because of this, it may (or already is?) become a widely supported 3D format across a range of 3D software. So we want to have really good support for it &mdash; reading all the features (including animations), and preserving the efficiency of binary-encoded meshes (to do this, we will probably invent some new X3D nodes).

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

      <li><p><b>Batching</b>

        <p>Batching of shapes that have equal appearance for start, to optimize the rendering.

      <li><p><b>Make TCastleScene, T3DTranform and friends to be special X3D nodes</b>

        <p>This would make the whole scene manager a single graph of X3D nodes,
        allowing for more sharing in code.
        The T3DTranform would be just like TTransformNode, but a little diferently
        optimized (but it would become toggable).

      <li><p><b>Make TCastleScene descend from T3DTranform?</b>

        <p>Also, allow <code>SceneManager.MainScene</code> to have some transformation
        (right now, it's not 100% correct).

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

  </li>

<?php /*
  <li>
    <p><b>Use Cocoa under Mac OS X</b></p>

    <p>We already have a native look and feel, and easy installation,
    under Mac OS X, see
    <a href="<?php echo CURRENT_URL; ?>old_news.php?id=devel-2013-04-19">relevant news</a>
    and <a href="<?php echo CURRENT_URL; ?>macosx_requirements.php">docs for Mac OS X</a>.
    Our programs no longer have to use X11 and GTK under Mac OS X.
    Still, current solution is not optimal:
    we use LCL with Carbon under the hood. Carbon is deprecated and only
    32-bit (Cocoa should be used instead), and depending on LCL has it's
    own problems (mouse look is not smooth with LCL message loop).

    <p>The proposed task is to implement nice Cocoa backend
    in <code>CastleWindow</code> unit. Contributions are welcome.
    This is an easy and rewarding task for a developer interested in Mac OS X.
  </li>

*/ ?>

  <li>
    <p><b>Support Material.mirror field for OpenGL rendering</b></p>
    <p>An easy way to make planar (on flat surfaces) mirrors. Just set Material.mirror field to something > 0 (setting it to 1.0 means it's a perfect mirror, setting it to 0.5 means that half of the visible color is coming from the mirrored image, and half from normal material color).</p>
    <p>Disadvantages: This will require an additional rendering pass for such shape (so expect some slowdown for really large scenes). Also your shape will have to be mostly planar (we will derive a single plane equation by looking at your vertexes).</p>
    <p>Advantages: The resulting mirror image looks perfect (there's no texture pixelation or anything), as the mirror is actually just a specially rendered view of a scene. The mirror always shows the current scene (there are no problems with dynamic scenes, as mirror is rendered each time).</p>
    <p>This will be some counterpart to current way of making mirrors by RenderedTexture (on flat surfaces) or <a href="x3d_implementation_cubemaptexturing.php">GeneratedCubeMap</a> (on curvy surfaces).</p>
  </li>

  <li>
    <p><b>Advanced networking support</b></p>
    <p>Basic networiking support is done already, we use <a href="http://wiki.freepascal.org/fphttpclient">FpHttpClient unit distributed with FPC</a>, see <a href="https://castle-engine.sourceforge.io/manual_network.php">the manual</a>. Things working: almost everything handles URLs, we support <code>file</code> and <code>data</code> and <code>http</code> URLs.

    <p>Things missing are listed below:

    <ol>
      <li><p><b>Support for <code>https</code></b>.

        <p>FpHttpClient should be able to handle https in new version (at least in FPC 3.1.1, but possibly in 3.0.x too). See the FPC mailing list and wiki for info. So this is mostly a matter of adding the <code>https</code> to the recognized protocol names in <code>CastleDownload</code>, and testing.

        <p>Or we can use LNet or Synapse (they both include https support).

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
            (but not Mac OS X) see <a href="http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html">basedir-spec</a>;
            specific Microsoft, Apple specs may be available
            for Windows and Mac OS X).
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
    <p><b>Easy way to use 3rd-person camera movement in your games</b>

  <li><p><b>iOS improvements:</b>

    <ul>
      <li><p>(IN PROGRESS) <i>Services on iOS</i>.
        For now, most of <a href="https://github.com/castle-engine/castle-engine/wiki/Android-Project-Services-Integrated-with-Castle-Game-Engine">our services</a> are only on Android.
        Most of them are possible also on iOS, e.g.
        <?php api_link('TInAppPurchases', 'CastleInAppPurchases.TInAppPurchases.html'); ?>,
        or <i>Apple Game Center</i> as an equivalent to <i>Google Play Games</i> from Android.

      <li><p>(DONE) <i>Package the vorbisfile library</i>. To be able to read OggVorbis sound files.

      <li><p><i>Enable texture compression</i>. Because of Apple's weird implementation of PVRTC format (only square textures are allowed), for now we disabled texture compression on iOS. This should be done better, at least to enable the texture compression of other formats.
    </ul>
</ul>

<?php castle_footer(); ?>
