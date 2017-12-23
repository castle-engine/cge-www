<?php
require_once 'castle_engine_functions.php';
manual_header('Optimization and profiling');

$toc = new TableOfContents(
  array(
    new TocItem('Watch <i>Frames Per Second</i>', 'fps'),
      new TocItem('The short version: what to watch', 'fpc_short', 1),
      new TocItem('How to show the FPS value', 'fpc_show', 1),
      new TocItem('How to interpret <i>Frames Per Second</i> values?', 'fpc_meaning', 1),
    new TocItem('Making your games run fast', 'models'),
      new TocItem('Basic rule: use small and static geometry, as much as possible', 'basic', 1),
      new TocItem('Compile in "release" mode for speed', 'release_mode', 1),
      new TocItem('Backface culling', 'culling', 1),
      new TocItem('Textures', 'textures', 1),
      new TocItem('Animations', 'animations', 1),
      new TocItem('Create complex shapes, not trivial ones', 'shapes', 1),
      new TocItem('Share TCastleScenes instances if possible', 'scenes', 1),
      new TocItem('Collisions', 'collisions', 1),
      new TocItem('Avoid loading (especially from disk!) during the game', 'loading', 1),
      new TocItem('Consider using occlusion query', 'occlusion_query', 1),
      new TocItem('Blending', 'blending', 1),
      new TocItem('Loading PNG using libpng', 'libpng', 1),
    new TocItem('Profile (measure speed and memory usage)', 'profiling'),
    new TocItem('Measure memory use and watch out for memory leaks', 'memory'),
      new TocItem('Detect memory leaks with HeapTrc (-gh)', 'heaptrc', 1),
      new TocItem('Other tools', 'memory_other', 1),
  )
);
?>

<p>Once you have a large game, with many large 3D models, you will
probably start to wonder about the speed and memory usage.</p>

<?php echo $toc->html_toc(); ?>
<?php echo $toc->html_section(); ?>

<p>The main tool to measure your game speed is the <i>Frames Per Second (FPS)</i>
number. Use the
 <?php api_link('TCastleControlCustom.Fps', 'CastleControl.TCastleControlCustom.html#Fps'); ?> or
 <?php api_link('TCastleWindowCustom.Fps', 'CastleWindow.TCastleWindowCustom.html#Fps'); ?>
 to get an instance of
 <?php api_link('TFramesPerSecond', 'CastleTimeUtils.TFramesPerSecond.html'); ?>.
 Look at these two numbers:
 <?php api_link('TFramesPerSecond.RealFps', 'CastleTimeUtils.TFramesPerSecond.html#RealFps'); ?> and
 <?php api_link('TFramesPerSecond.OnlyRenderFps', 'CastleTimeUtils.TFramesPerSecond.html#OnlyRenderFps'); ?>.

<?php echo $toc->html_section(); ?>

<p>In short, watch the <code>Window.Fps.ToString</code> value, by displaying it anywhere.
It intelligently combines some information, to show you how fast is your application.

<p>Eventually, display directly the <code>Window.Fps.RealFps</code> value.
In this case, it is easiest to have
 <?php api_link('TCastleControlCustom.AutoRedisplay', 'CastleControl.TCastleControlCustom.html#AutoRedisplay'); ?> or
 <?php api_link('TCastleWindowCustom.AutoRedisplay', 'CastleWindow.TCastleWindowCustom.html#AutoRedisplay'); ?> set
 to <code>true</code>, otherwise the meaning of <code>RealFps</code>
 may not actually indicate the potential speed of your application.
 It is <code>true</code> by default, so you're already set.
 <!-- (But note that <a href="view3dscene.php">view3dscene</a>
 has <code>AutoRedisplay</code> set to <code>false</code> by default.
 -->

<?php echo $toc->html_section(); ?>

<ul>
  <li><p>If you use <code>TCastleWindow</code>, you can trivially turn on <code>TCastleWindow.FpsShowOnCaption</code>.

  <li><p>You can display FPS using <code>TCastleLabel</code>. See the <a href="manual_2d_user_interface.php">manual page about using our user-interface classes</a>. Just update the <code>TCastleLabel.Caption</code> in every <code>OnUpdate</code> event to show the current FPS value. An an example, <a href="https://github.com/castle-engine/castle-engine/blob/master/examples/portable_game_skeleton/game.pas">see how examples/portable_game_skeleton/game.pas shows the FPS</a> (search for <code>Container.Fps.ToString</code> there).

    <p>Or you can display FPS using <code>TCastleFont.Print</code> in every <code>Render</code> event. See the <?php echo a_href_page('manual about custom drawing', 'manual_2d_ui_custom_drawn'); ?>. As an example, <a href="https://github.com/castle-engine/castle-engine/blob/master/examples/physics/physics_3d_demo/game.pas">see how examples/physics/physics_3d_demo/game.pas shows the FPS</a> (search for <code>Container.Fps.ToString</code> there).

    <p>Display the value like <code>Format('%f', [Window.Fps.RealFps])</code>. Or, even better (and simpler), use <code>Window.Fps.ToString</code>. The <code>Window.Fps.ToString</code> shows more information, nicely formatted.

  <li><p>You can show the FPS value on some Lazarus label or form caption.

    <p>Warning: do not change the Lazarus control too often (like every frame).
    <i>Updating normal Lazarus controls all
    the time may slow your OpenGL context drastically</i>.
    Also, do not write to the console (e.g. using <code>Writeln</code>)
    or anything else (e.g. using our <code>WritelnLog</code>) every frame
    &mdash; the very fact of doing this will slow down your application a lot.

    <p>If you need to change some Lazarus control,
    or write the FPS to some log, use a timer
    (like <?php api_link('TCastleTimer', 'CastleControls.TCastleTimer.html'); ?> or
    Lazarus <code>TTimer</code>) to write it e.g. only once per second.
    The <code>RealFps</code> and <code>OnlyRenderFps</code>
    are actually just an average from the last second,
    so there's really no need to show them more often.
</ul>

<?php echo $toc->html_section(); ?>

<p>There are two FPS numbers measured: "<i>real FPS</i>" and "<i>only render FPS</i>".
"<i>Only render FPS</i>" is usually slightly larger.
Larger is better, of course: it means that you have smoother animation.

<p><b>Use "<i>real FPS</i>" to measure your overall game speed. This is the actual
number of frames per second that we managed to display.</b>

<p>Caveats:

<ul>
  <li><p>Make sure to have an animation that constantly updates your
    screen, or use <code>AutoRedisplay</code> = <code>true</code>
    (it is the default since CGE 6.0, so you're probably already set).

    <p>Otherwise, we may not refresh the screen continously (no point to
    redraw, if both the scene and camera are completely static; this way we let
    other applications to work more smoothly, and we save your laptop battery).
    Then "<i>real FPS</i>" will drop to almost zero. This can be detected by looking at
    <code>Window.Fps.WasSleeping</code>. The output of <code>Window.Fps.ToString</code>
    also accounts for it, showing <i>"no frames rendered"</i>
    or <i>"no need to render all frames"</i>.

<!--
    <p>(However, <a href="view3dscene.php">view3dscene</a> has it set to <code>false</code>,
    since version 3.18.0.)

    <p>If you don't use <code>AutoRedisplay</code>,
    then check <code>Window.Fps.WasSleeping</code> value.
    When it's <code>true</code>, we were not rendering some frames because
    there was no need to (everything was static - scene and camera).
    In this case, the <code>RealFps</code> value does not reflect your program
    speed, and can be ignored (the <code>Window.Fps.ToString</code>
    will show <code>no need to redraw</code>).
-->

  <li><p>If you hope to see higher values than 100 (the default
    <code>LimitFPS</code> value) then turn off "<i>limit FPS</i>" feature.

    <ul>
      <li>In games using <code>TCastleWindow</code>
        (if you use a standard program template, or manually call
        <code>Window.ParseParameters</code>) you can do it just by passing
        <code>--no-limit-fps</code> command-line option.
      <li>Or use <?php echo a_href_page('view3dscene', 'view3dscene'); ?>
        <i>"Preferences -&gt; Frames Per Second"</i> menu item to set them to zero.
      <li>Or change
        <?php api_link('LimitFPS global variable', 'CastleControl.html#LimitFPS'); ?>
        (if you use
        <?php api_link('CastleControl', 'CastleControl.html'); ?>
        unit with Lazarus) or change
        <?php api_link('Application.LimitFPS', 'CastleWindow.TGLApplication.html#LimitFPS'); ?>
        (if you use <?php api_link('CastleWindow', 'CastleWindow.html'); ?> unit)
        to zero.
        Changing them to zero disables the "limit fps" feature.
    </ul>

    <p>You will also need to turn off "<i>vertical synchronization</i>"
    of the GPU to achieve arbitrarily high FPS.

  <li><p>Note that the monitor will actually drop some frames above it's
    frequency, like 60. (This is relevant only if "<i>vertical synchronization</i>" is off.)

    <p>This <i>may</i> cause you to observe that above some
    threshold, FPS are "easier to gain" by optimizations, which may lead you
    to a false judgement about which optimizations are more useful than
    others. <i>To make a good judgement about what is faster / slower,
    compare two versions of your program when only one thing changes.</i>
</ul>

<p><b><i>"Only render FPS"</i> measures how much frames we
would get, if we ignore the time spent outside <code>Render</code> events.</b>
It's useful to compare it with <i>"real FPS"</i>,
large difference <i>may</i> indicate that you can make some optimizations
in CPU code (e.g. collision detection or animations) to gain overall speed.
Caveats:

<ul>
  <li><p>Modern GPUs work in parallel to the CPU. So <i>"how much time CPU spent
    in Render"</i> doesn't necessarily relate to <i>"how much time GPU spent on
    performing your drawing commands"</i>.

    <p>For example: if you set <code>LimitFPS</code> to a small value (like 10),
    you may observe
    that <i>"only render FPS"</i> grows very high. Why? Because when the CPU is idle
    (which is often if <code>LimitFPS</code> is small), then GPU has a free time to
    finish rendering previous frame. So the GPU does the work for free,
    outside of <code>Render</code> time, when your CPU is busy waiting.
    OTOH when CPU works on producing new frames all the time, then you have to
    wait inside <code>Render</code> until previous frame finishes.

    <p>In other words, improvements to <i>"only render FPS"</i> must be taken with a
    grain of salt. We spend less or more time in <code>Render</code> event:
    this does not always mean that we render more efficiently.

    <p>Still, <i>"only render FPS"</i> is often a useful indicator.
</ul>

<dl>
  <dt>If you see a large <i>"only render FPS"</i> value,
    much larger than <i>"real FPS"</i>...</dt>

  <dd><p>It means that <code>Render</code> is quick.
    So we probably don't need to wait for the whole previous frame to finish
    when starting rendering a new frame. To some extent, that's good &mdash;
    you're probably doing useful work in the meantime on CPU, while GPU is working.
    Often it means that there is something to gain optimizing the CPU side,
    like collisions or animations.

    <p><i>No guarantees:
    It does not mean that you can actually achieve this number of FPS
    as "real FPS".</i> At some point,
    decreasing CPU work will just uncover that we have
    to wait for GPU to finish anyway. In which case, you will observe
    <i>"only render FPS"</i> to drop (which is nothing alarming,
    it doesn't necessarily mean that rendering is less efficient;
    it just means that GPU speed becomes a factor too).

  <dt>When <i>"only render FPS"</i> is almost equal to
    <i>"real FPS"</i>...</dt>

  <dd><p>Then we spend most time in <code>Render</code>.
    This is normal if neither rendering nor collisions are a bottleneck
    &mdash; then we probably just spend time in the <code>Render</code> waiting
    for vertical synchronization to happen, and you can't really achieve more
    than 60 real FPS in the typical case with "<i>vertical synchronization</i>"
    turned on.

    <p>However, if your <i>"real FPS"</i> is much lower than your refresh rate,
    and your <i>"only render FPS"</i> is equal to <i>"real FPS"</i>,
    then you probably can optimize the rendering. (Make smaller models,
    use less demanding shader effects etc.)
</dl>

<!--p>If you turn off <code>LimitFPS</code>, and compare <i>"only render FPS"</i> with
<i>"real FPS"</i>,
you can see how much time was spent outside <code>Render</code>. Usually, <i>"frame
time"</i> will be close to <i>"real FPS"</i>. If the gap is large, it may mean
that you have a bottleneck in non-rendering code (like collision
detection and creature AI).
-->

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

<p>First of all, watch the number of vertexes and faces of the models you load.
Use <?php echo a_href_page('view3dscene', 'view3dscene'); ?>
 menu item <i>Help -&gt; Scene Information</i> for this.

<p>Graphic effects dealing with <i>dynamic and detailed lighting</i>,
like <i>shadows</i> or <i>bump mapping</i>, have a cost.
So use them only if necessary. In case of static scenes,
try to "bake" such lighting effects to regular textures (use e.g. Blender
<i>Bake</i> functionality), instead of activating a costly runtime effect.

<?php echo $toc->html_section(); ?>

<p>Both Lazarus and our <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">build tool</a>
support the idea of "build modes".

<ul>
  <li><p>When you're in the middle of the development and you're testing the game for bugs,
    use the <code>debug</code> mode,
    that adds a lot of run-time checks to your code. This allows to get
    a clear and nice error when you e.g. access an invalid array index.
    If you use our
    <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">build tool</a>,
    just pass the <code>--mode=debug</code> command-line parameter to it.

    <p>Our vectors are also like arrays, so doing stuff like <code>MyVector[2] := 123.0;</code>
    is also checked (it's valid if <code>MyVector</code> is a 3D or 4D vector, invalid if it's a 2D vector).
    Actually, this simple case is checked at compile-time with the new vector API
    in Castle Game Engine 6.3,
    <!-- (since the index "2" is a constant and
    compiler knows that the range of indexes for each vector type).-->
    but more convoluted cases are still checked at run-time.

  <li><p>When you need the maximum speed (when you want to build a <i>"final"</i>
    version for the player, or when you check / compare / profile the speed),
    always use the <code>release</code> mode.

    <p>The code runs <b>much faster</b> in release mode.
    The speed difference may be really noticeable. As of Castle Game Engine 6.3,
    the ray-tracer is <i>1.9 times slower in development mode vs release mode</i>.
    The speed differences of a typical game are usually not that drastic
    (since you don't spend 100% of your time calculating math expressions,
    unlike a ray-tracer), but significant differences are still expected,
    especially if you measure the performance of a particular calculation
    (not just looking at game FPS).

    <p>So in most cases it's really important that you measure the speed only of the
    <b>release</b> build of your game, and this is the version that you want to provide
    to your players.
</ul>

<?php echo $toc->html_section(); ?>

<p>If the player can see the geometry faces only from one side,
then <i>backface culling</i> should be <b>on</b>.
This is the default case (X3D nodes like <code>IndexedFaceSet</code>
have their <code>solid</code> field equal <code>TRUE</code> by default).
It avoids useless drawing of the other side of the faces.

<?php echo $toc->html_section(); ?>

<p>Optimize textures to increase the speed and lower GPU memory usage:

<ul>
  <li>Use texture compression (makes GPU memory usage more efficient).
    You can do it very easily by <?php echo a_href_page('using <i>material properties</i> and auto-compressing the textures using our build tool', 'creating_data_material_properties'); ?>.
  <li>Scale down textures on low-end devices (desktops and mobiles).
    You can do it at loading, by <?php echo a_href_page('using <i>material properties</i> and auto-downscaling the textures using our build tool', 'creating_data_material_properties'); ?>,
    see <?php api_link('TextureLoadingScale', 'CastleMaterialProperties.html#TextureLoadingScale'); ?>.
    Or you can do it at runtime, by <?php api_link('GLTextureScale', 'CastleGLImages.html#GLTextureScale'); ?>.
    Both of these approaches have their strengths, and can be combined.
  <li>Use texture atlases
    (try to reuse the whole X3D <code>Appearance</code> across many X3D shapes, if possible).
    This avoids texture switching when rendering, so the scene renders faster.
    When exporting from <a href="https://github.com/castle-engine/castle-engine/wiki/Spine">Spine</a>,
    be sure to use atlases.
  <li>Use spite sheets (<code>TSprite</code> class) instead of separate images
    (like <code>TGLVideo2D</code> class). This again avoids
    texture switching when rendering, making the scene render faster.
    It also allows to easily use any texture size (not necessarily a power of two)
    for the frame size, and still compress the whole sprite,
    so it cooperates well with texture compression.
  <li>Don't set too high <code>TextureProperties.anisotropicDegree</code>
    if not needed. <code>anisotropicDegree</code> should only be set to
    values &gt; 1 when it makes a visual difference in your case.
</ul>

<?php echo $toc->html_section(); ?>

<p>There are some <code>TCastleScene</code> features that are usually turned on,
but in some special cases may be avoided:

<ul>
  <li>Do not enable <code>ProcessEvents</code> if the scene should remain static.
  <li>Do not add <code>ssDynamicCollisions</code> to <code>Scene.Spatial</code> if you don't need better collisions than versus scene bounding box.
  <li>Do not add <code>ssRendering</code> to <code>Scene.Spatial</code> if the scene is always small on the screen, and so it's usually either completely visible or invisible. <code>ssRendering</code> adds frustum culling per-shape.
</ul>

<p>Various techniques to optimize animations include:

<ul>
  <li><p>If your model has animations but is often not visible (outside
    of view frustum), then consider using <code>Scene.AnimateOnlyWhenVisible := true</code>
    (see <?php api_link('TCastleSceneCore.AnimateOnlyWhenVisible',
    'CastleSceneCore.TCastleSceneCore.html#AnimateOnlyWhenVisible'); ?>).

  <li><p>If the model is small, and not updating it's animations every frame will not be noticeable, then consider setting <code>Scene.AnimateSkipTicks</code>
    to something larger than 0 (try 1 or 2).
    (see <?php api_link('TCastleSceneCore.AnimateSkipTicks',
    'CastleSceneCore.TCastleSceneCore.html#AnimateSkipTicks'); ?>).

  <li><p>For some games, turning globally <code>OptimizeExtensiveTransformations := true</code> improves the speed. This works best when you animate multiple <code>Transform</code> nodes within every X3D scene, and some of these animated <code>Transform</code> nodes are children of other animated <code>Transform</code> nodes. A typical example is a skeleton animation, for example from <a href="https://github.com/castle-engine/castle-engine/wiki/Spine">Spine</a>, with non-trivial bone hierarchy, and with multiple bones changing position and rotation every frame.

  <!--
  <li><p>Consider using <code>TCastlePrecalculatedAnimation</code> to "bake" animation from events as a series of static scenes. This makes sense if your animation is from Spine or X3D exported from some software that understands X3D interpolation nodes.

    <p>Note that there's no point doing this if your animation is from castle-anim-frames or M3D, they are already "baked". Although this baking will become optional (not forced) in the future.

    <p>TODO: The API for "baking" should use TNodeInterpolator, not deprecated <code>TCastlePrecalculatedAnimation</code>.
  -->

  <li><p>Watch out what you're changing in the X3D nodes. Most changes, in particular the ones that can be achieved by sending X3D events (these changes are kind of "suggested by the X3D standard" to be optimized) are fast. But some changes are very slow, cause rebuilding of scene structures, e.g. reorganizing X3D node hierarchy. So avoid doing it during game. To detect this, set <code>LogSceneChanges := true</code> and watch log (see <a href="manual_log.php">manual chapter "Logging"</a>) for lines saying <i>"ChangedAll"</i> - these are costly rebuilds, avoid them during the game!
</ul>

<?php echo $toc->html_section(); ?>

<p>Modern GPUs can "consume" a huge number of vertexes very fast,
as long as they are provided to them in a single "batch" or "draw call".</p>

<p>In our engine, the "shape" is the unit of information we provide to
GPU. It is simply a VRML/X3D shape. In most cases, it also corresponds to the
3D object you design in your 3D modeler, e.g. Blender 3D object in
simple cases is exported to a single VRML/X3D shape (although it may
be split into a couple of shapes if you use different
materials/textures on it, as VRML/X3D is a little more limited (and
also more GPU friendly)).</p>

<p>The general advice is to compromise:</p>

<ol>
  <li><p>Do not make too many too trivial shapes. Do not make millions of
    shapes with only a few vertexes &mdash; each shape will be provided
    in a separate VBO to OpenGL, which isn't very efficient.

  <li><p>Do not make too few shapes. Each shape is passed as a whole
    to OpenGL (splitting shape on the fly would cause unacceptable
    slowdown), and shapes may be culled using frustum culling or
    occlusion queries. By using only a few very large shapes, you make
    this culling worthless.
</ol>

<p>A rule of thumb is to keep your number of shapes in a scene between
100 and 1000. But that's really just a rule of thumb, different level
designs will definitely have different considerations.

<p>You can also look at the number of triangles in your shape.
Only a few triangles for a shape is not optimal &mdash; we will waste
resources by creating a lot of VBOs, each with only a few triangles (the engine cannot
yet combine the shapes automatically). Instead, merge your shapes &mdash;
to have hundreds or thousands of triangles in a single shape.

<?php echo $toc->html_section(); ?>

<ul>
  <li><p>To reduce memory usage, you can use the same <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?> instance many times within <code>SceneManager.Items</code>, usually wrapped in a different <?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?>. The whole code is ready for such "<i>multiple uses</i>" of a single scene instance.

    <p>For an example of this approach, see <a href="https://github.com/castle-engine/frogger3d">frogger3d</a> game (in particular, it's main unit <a href="https://github.com/castle-engine/frogger3d/blob/master/code/game.pas">game.pas</a>). The game adds <i>hundreds</i> of 3D objects to <code>SceneManager.Items</code>, but there are only <i>three</i> <code>TCastleScene</code> instances (player, cylinder and level).

    <p>However, this optimization is suitable only if all the visible scenes (that are actually a single <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?> instance) are always in the same animation frame (or maybe they are not animated at all). If you want to play different animations, you have to create separate TCastleScene instances (you can create them efficiently using the <?php api_link('TCastleScene.Clone', 'CastleScene.TCastleScene.html#Clone'); ?> method).

  <li><p>In some cases, combining many <code>TCastleScene</code> instances into one helps. To do this, load your 3D models to <code>TX3DRootNode</code> using <code>Load3D</code>, and then create a new single <code>TX3DRootNode</code> instance that will have many other nodes as children. That is, create one new <code>TX3DRootNode</code> to keep them all, and for each scene add it's <code>TX3DRootNode</code> (wrapped in <code>TTransformNode</code>) to that single <code>TX3DRootNode</code>.

    <p>This allows you to load multiple 3D files into a single <code>TCastleScene</code>, which may make stuff faster &mdash; there will be only one octree (used for collision routines and frustum culling) for the whole scene. Right now, we have an octree inside each TCastleScene, so it's not optimal to have thousands of TCastleScene instances with collision detection.
</ul>

<?php echo $toc->html_section(); ?>

<p>If you include <code>ssStaticCollisions</code> or <code>ssDynamicCollisions</code>
inside <code>TCastleScene.Spatial</code>, then we build a spatial structure (octree)
that performs collisions with the actual triangles of your 3D model.
This results in very precise collisions, but it can eat an unnecessary
amount of memory (and, sometimes, take unnecessary amount of time)
if you have a high-poly mesh.
Often, many shapes don't need to have such precise collisions
(e.g. a complicate 3D tree may be approximated using a simple cylinder
representing tree trunk).

<p>Use X3D <code>Collision</code> node to mark some shapes as
non-collidable or to provide a simpler "proxy" shape to use for
collisions. Right now, using the <code>Collision</code> requires
writing X3D code manually, but it's really trivial. You can
still export your scenes from 3D software, like Blender &mdash;
you only need to manually write a "wrapper" X3D file around them.

<ul>
  <li>An example X3D file showing this technique: <a href="https://github.com/castle-engine/wyrd-forest/blob/master/data/tree/oaktree_with_good_collisions.x3dv">tree from "Wyrd Forest" game</a>.
  <li>More examples are in <code>vrml_2/collisions_final.wrl</code> demo inside <?php echo a_href_page('our demo VRML/X3D models', 'demo_models'); ?>.
</ul>

<!--
It's really trivial
in X3D, and we support it 100% &mdash; I just wish there was a way to
easily set it from 3D modelers like Blender. Hopefully we'll get
better X3D exporter one day. Until them, you can hack X3D source, it's
quite easy actually. And thanks to using X3D Inline node, you can keep
your auto-generated X3D content separated from hand-written X3D code
&mdash; that's the reason for xxx_final.x3dv and xxx.x3d pairs of
files around the demo models.
-->

<p>You can also build a <code>Collision</code> node by code.
We have a helper method for this: <?php api_link('TCollisionNode.CollideAsBox', 'X3DNodes.TCollisionNode.html#CollideAsBox'); ?>.

<p>Another possible octree optimization is to adjust the parameters how
the octree is created. You can
<a href="x3d_implementation_navigation_extensions.php#section_ext_octree_properties">set octree parameters in VRML/X3D file</a> or by ObjectPascal code.
Although in practice I usually find that the default values are really good.
<!--found that the default values are optimal
for a wide range of scenes.
-->

<?php echo $toc->html_section(); ?>

<p>Avoid any loading (from disk to normal memory, or from normal memory to GPU memory) once the game is running. Doing this during the game will inevitably cause a small stutter, which breaks the smoothness of the gameplay. Everything necessary should be loaded at the beginning, possibly while showing some "loading..." screen to the user. Use <code>TCastleSceneManager.PrepareResources</code> to load everything referenced by your scenes to GPU.

<p>Enable some (or all) of these flags to get extensive information in the log about all the loading that is happening:

<ul>
  <li><code>LogTextureLoading</code>
  <li><code>LogAllLoading</code>
  <li><code>TextureMemoryProfiler.Enabled</code>
  <li><code>LogRenderer</code> (from <code>CastleRenderer</code> unit)
</ul>

<p>Beware: This is usually <i>a lot</i> of information, so you probably don't want to see it always. Dumping this information to the log will often cause a <b>tremendous slowdown</b> during loading stage, so do not bother to measure your loading speed when any of these flags are turned on. Use these flags only to detect if something "fishy" is happening during the gameplay.

<?php echo $toc->html_section(); ?>

<p>The engine by default performs frustum culling, using per-shape
and per-scene bounding boxes and spheres. If you add <code>ssRendering</code>
flag to the <code>Scene.Spatial</code>, this will be even faster thanks
to using shapes octree.

<p>Using the <i>hardware occlusion query</i> is often a good idea
in large city or indoor levels,
where walls or large buildings can obscure a significant part of your geometry.
Activate it by simply turnnig on the flag
<?php api_link('UseOcclusionQuery', 'CastleScene.TSceneRenderingAttributes.html#UseOcclusionQuery'); ?>,
like <code>Scene.Attributes.UseOcclusionQuery := true</code>.
Note that our simple implementation may sometimes show a lag of 1 frame
when the object is not rendered, but it should be.

<p>You can also define custom culling methods.
See the <code>examples/3d_rendering_processing/fog_culling.lpr</code>.

<?php echo $toc->html_section(); ?>

<p>We use <i>alpha blending</i> to render partially transparent
shapes. Blending is used automatically if you have a texture with
a smooth alpha channel, or if your <code>Material.transparency</code>
is less than 1.

<p>Note: Just because your texture has <i>some</i> alpha channel,
it doesn't mean that we use blending. By default, the engine
analyses the alpha channel contents, to determine whether it indicates
alpha blending (<i>smooth</i> alpha channel), alpha testing (all alpha
values are either "0" or "1"), or maybe it's opaque (all alpha values equal "1").
<a href="x3d_implementation_texturing_extensions.php#section_ext_alpha_channel_detection">You
can always explicitly specify the texture alpha channel treatment using the
<code>alphaChannel</code> field in X3D</a>.

<p>Rendering blending is a little costly, in a general case.
The transparent shapes have to be sorted every frame.
Hints to make it faster:

<ul>
  <li><p>If possible, do not use many transparent shapes.
    This will keep the cost of sorting minimal.

  <li><p>If possible, turn off the sorting, using <code>Scene.Attributes.BlendingSort := bsNone</code>. See <?php api_link('TBlendingSort', 'CastleBoxes.html#TBlendingSort'); ?> for the explanation of possible <code>BlendingSort</code> values. Sorting is only necessary if you may see multiple partially-transparent shapes on the same screen pixel, otherwise sorting is a waste of time.

  <li><p>Sorting is also not necessary if you use some <i>blending modes</i> that make the order of rendering partially-transparent shapes irrelevant. For example, blending mode with <code>srcFactor = "src_alpha"</code> and <code>destFactor = "one"</code>. <a href="x3d_extensions.php#section_ext_blending">You can use a <code>blendMode</code> field in X3D to set a specific blending mode</a>. Of course, it will look differently, but maybe acceptably?

    <p>So, consider changing the blending mode <i>and</i> then turning off sorting.

  <li><p>Finally, consider do you really need transparency by <i>blending</i>. Maybe you can work with a transparency by <i>alpha testing</i>? <i>Alpha testing</i> means that every pixel is either opaque, or completely transparent, depending on the alpha value. It's much more efficient to use, as alpha tested shapes can be rendered along with the normal, completely opaque shapes, and only the GPU cares about the actual "testing". There's no need for sorting. Also, <i>alpha testing</i> cooperates nicely with <a href="x3d_extensions_shadow_maps.php">shadow maps</a>.

    <p>Whether the <i>alpha testing</i> looks good depends on your use-case, on your textures.

    <p>To use alpha-testing, you can:</p>

    <ol>
      <li>Either make the alpha channel of your texture non-smooth, that is: every pixel should have alpha value equal to 0 or 1, never something in between. For example, in GIMP, increase the contrast (to maximum) of the alpha channel mask.</li>

      <li>Or you can force using alpha testing by <a href="x3d_implementation_texturing_extensions.php#section_ext_alpha_channel_detection">using <code>alphaChannel "TEST"</code> in X3D</a></li>
    </ol>
  </li>
</ul>

<?php echo $toc->html_section(); ?>

<p>By default, our engine uses FpImage to load various image formats, including PNG. This is comfortable, as it does not require any external libraries, and thus it instantly works (and in the same way) on all platforms. So you don't need to worry about using <code>libpngXXX.dll</code> on Windows, or linking with <i>libpng</i> on Android or iOS.

<p>However, using external <i>libpng</i> is often much (even 4x) faster. That is because <i>libpng</i> allows to make various transformations during file reading (instead of processing the pixels later), and it doesn't force us to read using 16-bit-per-channel API (like FpImage does). So if you have a lot of PNG files, and want to speed up the loading process, consider switching to using external <i>libpng</i>.

<p>To use external <i>libpng</i> library, just define <code>-dCASTLE_PNG_DYNAMIC</code> when compiling the engine. E.g. define it inside <a href="https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples">CastleEngineManifest.xml as &lt;custom_options&gt;</a> and use our <a href="https://github.com/castle-engine/castle-engine/wiki/Build%20Tool">build tool</a> to compile your game.

<p>When testing or distributing the game, make sure that you have libpng and zlib available.

<ul>
  <li>On Linux, FreeBSD, Mac OS X and other desktop Unix systems it's usually installed system-wide, so you don't need to worry.
  <li>On Windows, the <a href="https://github.com/castle-engine/castle-engine/wiki/Android/Build%20Tool">build tool</a> will make sure to include the appropriate DLLs when you call <code>castle-engine package ...</code>. For testing, you can copy the appropriate DLLs to your game directory yourself, or copy them somewhere on $PATH. At the bottom of the <a href="documentation.php">getting started</a> page we documented from where you can take these DLLs.
  <li>On Android and iOS, we will still use internal FpImage for now. (Modify <code>castleconf.inc</code> if you want to change it.)
</ul>

<?php echo $toc->html_section(); ?>

<p>You can compile your
application with the <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">build tool</a>
using <code>--mode=valgrind</code> to get an executable ready to be tested
with the magnificent <a href="http://valgrind.org/">Valgrind</a> tool.

<p>You can use any FPC tool to profile your code, for memory and
speed. There's a small document about it in engine sources, see
<code>castle_game_engine/doc/miscellaneous_notes/profiling_howto.txt</code>
(TODO: it should be moved to our wiki at some point).
See also <a href="http://wiki.lazarus.freepascal.org/Profiling">FPC
wiki about profiling</a>.

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

<p>To detect memory leaks, we advice to regularly compile your code with
FPC options <code>-gl -gh</code>. <!--This automatically uses the special units
<code>HeapTrc</code> and <code>LineInfo</code> in your program.-->
There are many ways to do this, for example you can
add this to your <code>fpc.cfg</code> file (<?php echo FPC_CFG_DOCS; ?>):

<pre>
#IFDEF DEBUG
-gh
-gl
#ENDIF
</pre>

<p>Then all the programs compiled in debug mode (with
<code>castle-engine compile --mode=debug</code>,
or with an explicit FPC option <code>-dDEBUG</code>)
will automatically check for memory leaks.

<p>The end result is that at the program's exit, you will get a very useful
report about the allocated and not freed memory blocks,
with a stack trace to the allocation call.
This allows to easily detect and fix memory leaks.

<p>If everything is OK, the output looks like this:

<pre>
Heap dump by heaptrc unit
12161 memory blocks allocated : 2290438/2327696
12161 memory blocks freed     : 2290438/2327696
0 unfreed memory blocks : 0
True heap size : 1212416
True free heap : 1212416
</pre>

<p>But when you have a memory leak, it tells you about it,
and tells you where the relevant memory was allocated, like this:

<pre>
Heap dump by heaptrc unit
4150 memory blocks allocated : 1114698/1119344
4099 memory blocks freed     : 1105240/1109808
51 unfreed memory blocks : 9458
True heap size : 851968
True free heap : 834400
Should be : 835904
Call trace for block $00007F9B14E42980 size 44
  $0000000000402A83 line 162 of xxx.lpr
  ...
</pre>

<p>Note: when you exit with <code>Halt</code>,
you will always have some memory leaks, that's unavoidable for now.
You can ignore the "<i>Heap dump by heaptrc unit</i>" output in this case.
<!-- the OS cleans up the memory of a terminated program anyway. -->

<p>Note: In the future, we may add "-gh" automatically
to the options added by the build tool in the debug mode.
So programs compiled with <code>castle-engine compile --mode=debug</code>
will automatically show this output.

<?php echo $toc->html_section(); ?>

<p>We do not have any engine-specific tool to measure memory usage or
detect memory problems, as there are plenty of them available with
FPC+Lazarus already. To simply see the memory usage, just use process
monitor that comes with your OS. See also Lazarus units
like <code>LeakInfo</code>.

<p>You can use full-blown memory profilers like
valgrind's massif with FPC code (see section <i>"Profiling"</i> above
on this page about valgrind).

<?php
manual_footer();
?>
