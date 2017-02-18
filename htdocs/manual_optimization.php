<?php
require_once 'castle_engine_functions.php';
manual_header('Optimization and profiling');

$toc = new TableOfContents(
  array(
    new TocItem('Watch <i>Frames Per Second</i>', 'fps'),
      new TocItem('How to interpret <i>Frames Per Second</i> values?', 'fpc_meaning', 1),
    new TocItem('Making your games run fast', 'models'),
      new TocItem('Backface culling', 'culling', 1),
      new TocItem('Textures', 'textures', 1),
      new TocItem('Animations', 'animations', 1),
      new TocItem('Create complex shapes, not trivial ones', 'shapes', 1),
      new TocItem('Do not instantiate too many TCastleScenes', 'scenes', 1),
      new TocItem('Collisions', 'collisions', 1),
      new TocItem('Avoid loading (especially from disk!) during the game', 'loading', 1),
      new TocItem('Consider using occlusion query', 'occlusion_query', 1),
      new TocItem('Blending', 'blending', 1),
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

<p>The main thing that measures your game speed is the <i>Frames Per Second</i>.
Engine automatically keeps track of this for you.
Use the <code>TCastleControl.Fps</code> or <code>TCastleWindow.Fps</code>
to get an instance of
 <?php api_link('TFramesPerSecond', 'CastleTimeUtils.TFramesPerSecond.html'); ?>,
and inside you have two important numbers:
 <?php api_link('TFramesPerSecond.FrameTime', 'CastleTimeUtils.TFramesPerSecond.html#FrameTime'); ?> and
 <?php api_link('TFramesPerSecond.RealTime', 'CastleTimeUtils.TFramesPerSecond.html#RealTime'); ?>.
We will explain the
difference between <code>FrameTime</code> and <code>RealTime</code> shortly.</p>

<p>How to show them? However you like:</p>

<ul>
  <li>If you use <code>TCastleWindow</code>,
    you can trivially enable <code>TCastleWindow.FpsShowOnCaption</code>
    to show FPS on your window caption.

  <li>You can show them on Lazarus label or caption. Just be sure to
    not update them too often &mdash; <i>updating normal Lazarus controls all
    the time may slow your OpenGL context drastically</i>. Same warning goes
    about writing them to the console with <code>Writeln</code> &mdash; don't call it too
    often, or your rendering will be slower. It's simplest to use
    <?php api_link('TCastleTimer', 'CastleControls.TCastleTimer.html'); ?> or
    Lazarus <code>TTimer</code> to update it e.g. only once per second.
    Actually, these properties show you an average from last second, so
    there's not even a reason to redraw them more often.

  <li>You can also simply display them on an OpenGL context (see the
    <?php echo a_href_page('example about designing your own <code>TGame2DControls</code>
    in earlier chapter', 'manual_2d_ui_custom_drawn'); ?>).
</ul>

<?php echo $toc->html_section(); ?>

<p>There are two FPS values available: <i>frame time</i> and <i>real time</i>.
<i>Frame time</i> is usually the larger one.
Larger is better, of course: it means
that you have smoother animation.

<p><b>Use "<i>real time</i>" to measure your overall game speed. This is the actual
number of frames per second that we managed to render.</b> Caveats:

<ul>
  <li><p>Make sure to turn off "<i>limit FPS</i>" feature, to get maximum number
    available. Use <?php echo a_href_page('view3dscene', 'view3dscene'); ?>
    "Preferences -&gt; Frames Per Second" menu
    item, or (in your own programs) change
    <?php api_link('LimitFPS global variable', 'CastleControl.html#LimitFPS'); ?>
    (if you use
    <?php api_link('CastleControl', 'CastleControl.html'); ?>
    unit with Lazarus) or change
    <?php api_link('Application.LimitFPS', 'CastleWindow.TGLApplication.html#LimitFPS'); ?>
    (if you use <?php api_link('CastleWindow', 'CastleWindow.html'); ?> unit).
    Change them to zero to disable the "limit fps" feature.

  <li><p>Make sure to have an animation that constantly updates your
    screen.
    Otherwise, we will not refresh the screen (no point to
    redraw the same thing), and "<i>real time</i>" will drop to almost zero if
    you look at a static scene.
    Since the engine version 6.0.0,
    <?php api_link('TCastleWindow.AutoRedisplay', 'CastleWindow.TCastleWindowCustom.html#AutoRedisplay'); ?>
    is by default <code>true</code>, so this is already OK by default.
    <!-- this is probably already OK. -->
    <!--
    If not, then set <code>AutoRedisplay</code> to <code>true</code>,
    E.g. keep camera moving, or have something animated on the
    screen, or set
    .
    -->

  <li><p>Note that the monitor will actually drop some frames above it's
    frequency, like 80. This <i>may</i> cause you to observe that above some
    limit, FPS are easier to gain by optimizations, which may lead you
    to a false judgement about which optimizations are more useful than
    others. To make a valuable judgement about what is faster/slower,
    always compare two versions of your program when only the relevant
    thing changed &mdash; nothing else.
</ul>

<p><b><i>"Frame time"</i> measures how much frames we
would get, if we ignore the time spent outside <code>OnRender</code> events.</b>
It's often useful to compare it with <i>"real
time"</i> (with <code>LimitFPS</code> feature turned off),
as it may then tell you whether the
bottleneck is in rendering or outside of rendering (like collision
detection and creature AI). Caveats:

<ul>
  <li><p>Modern GPUs work in parallel to the CPU. So <i>"how much time CPU spent
    in OnRender"</i> doesn't necessarily relate to <i>"how much time GPU spent on
    performing your drawing commands"</i>.
</ul>

<p>So making your CPU busy with something else (like collisions, or
waiting) makes your <i>"frame time"</i> lower, while in fact rendering time
is the same &mdash; you're just not clogging you GPU. Which is a
good thing, actually, if your game can spend this time on something useful
like collisions. Just don't overestimate it &mdash; you didn't make
rendering faster, but you managed to do a useful work in the meantime.

<p>For example: if you set <code>LimitFPS</code> to a small value, you may observe
that <i>"frame time"</i> grows higher. Why? Because when the CPU is idle
(which is often if <code>LimitFPS</code> is small), then GPU has a free time to
finish rendering previous frame. So the GPU does the work for free,
outside of <code>OnRender</code> time, when your CPU is busy with something
else. OTOH when CPU works on producing new frames, then you have to
wait inside <code>OnRender</code> until previous frame finishes.

<p>In other words, improvements to <i>"frame time"</i> must be taken with a
grain of salt. We spend less time in <code>OnRender</code> event: this does not
necessarily mean that we really render faster.

<p>Still, often <i>"frame time"</i> does reflect the speed of GPU rendering.

<p>If you turn off <code>LimitFPS</code>, and compare <i>"frame time"</i> with
<i>"real time"</i>,
you can see how much time was spent outside <code>OnRender</code>. Usually, <i>"frame
time"</i> will be close to <i>"real time"</i>. If the gap is large, it may mean
that you have a bottleneck in non-rendering code (like collision
detection and creature AI).

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

  <li><p>Consider using <code>TCastlePrecalculatedAnimation</code> to "bake" animation from events as a series of static scenes. This makes sense if your animation is from Spine or X3D exported from some software that understands X3D interpolation nodes.

    <p>Note that there's no point doing this if your animation is from castle-anim-frames or M3D, they are already "baked". Although this baking will become optional (not forced) in the future.

    <p>TODO: The API for "baking" should use TNodeInterpolator, not deprecated <code>TCastlePrecalculatedAnimation</code>.

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

<p>You usually do not need to create too many <code>TCastleScene</code> instances.

<ul>
  <li><p>To reduce memory usage, you can place the same <code>TCastleScene</code> (or <code>TCastlePrecalculatedAnimation</code>) instance many times within <code>SceneManager.Items</code>, usually wrapped in a different <code>T3DTransform</code>. The whole code is ready for such "<i>multiple uses</i>" of a single scene instance.

    <p>For an example of this approach, see <a href="https://github.com/castle-engine/frogger3d">frogger3d</a> game (in particular, it's main unit <a href="https://github.com/castle-engine/frogger3d/blob/master/code/game.pas">game.pas</a>). The game adds <i>hundreds</i> of 3D objects to <code>SceneManager.Items</code>, but there are only <i>three</i> <code>TCastleScene</code> instances (player, cylinder and level).

  <li><p>To improve the speed, you can often combine many <code>TCastleScene</code> instances into one. To do this, load your 3D models to <code>TX3DRootNode</code> using <code>Load3D</code>, and then create a new single <code>TX3DRootNode</code> instance that will have many other nodes as children. That is, create one new <code>TX3DRootNode</code> to keep them all, and for each scene add it's <code>TX3DRootNode</code> (wrapped in <code>TTransformNode</code>) to that single <code>TX3DRootNode</code>. This allows you to load multiple 3D files into a single <code>TCastleScene</code>, which may make stuff faster &mdash; octrees (used for collision routines and frustum culling) will work Ok. Right now, we have an octree only inside each TCastleScene, so it's not optimal to have thousands of TCastleScene instances with collision detection.
</ul>

<?php echo $toc->html_section(); ?>

<p>We build an octree (looking at exact triangles in your 3D model)
for precise collision detection with a level.
For other objects, we use bounding volumes
like boxes and spheres. This means that the number of shapes doesn't
matter much for collision speed. However, number of triangles still
matters for level.

<p>Use X3D <code>Collision</code> node to easily mark unneeded shapes as
non-collidable or to provide a simpler "proxy" mesh to use for
collisions with complicated objects. See
<code>demo_models/vrml_2/collisions_final.wrl</code>
inside <?php echo a_href_page('our demo VRML/X3D models', 'demo_models'); ?>.
It's really trivial
in X3D, and we support it 100% &mdash; I just wish there was a way to
easily set it from 3D modelers like Blender. Hopefully we'll get
better X3D exporter one day. Until them, you can hack X3D source, it's
quite easy actually. And thanks to using X3D Inline node, you can keep
your auto-generated X3D content separated from hand-written X3D code
&mdash; that's the reason for xxx_final.x3dv and xxx.x3d pairs of
files around the demo models.

<p>To wrap something in simpler collisions in code,
you can build appropriate <code>Collision</code> node by code.
See helpers like
<?php api_link('TCollisionNode.CollideAsBox', 'X3DNodes.TCollisionNode.html#CollideAsBox'); ?>.

<p>You can adjust the parameters how the octree is created. You can
<a href="x3d_extensions.php#section_ext_octree_properties">set octree
parameters in VRML/X3D file</a> or by ObjectPascal code.
Although in practice I usually find that the default values are really good.
<!--found that the default values are optimal
for a wide range of scenes.
-->

<?php echo $toc->html_section(); ?>

<p>Avoid any loading (from disk to normal memory, or from normal memory to GPU memory) once the game is running. Doing this during the game will inevitably cause a small stutter, which breaks the smoothness of the gameplay. Everything necessary should be loaded at the beginning, possibly while showing some "loading..." screen to the user. Use <code>TCastleScene.PrepareResources</code> to load everything referenced by your scenes to GPU.

<p>Enable some (or all) of these flags to get extensive information in the log about all the loading that is happening:

<ul>
  <li><code>LogTextureLoading</code>
  <li><code>LogAllLoading</code>
  <li><code>TextureMemoryProfiler.Enabled</code>
  <li><code>LogRenderer</code> (from <code>CastleRenderer</code> unit)
</ul>

<p>Beware: This is usually <i>a lot</i> of information, so you probably don't want to see it always. Dumping this information to the log will often cause a <b>tremendous slowdown</b> during loading stage, so do not bother to measure your loading speed when any of these flags are turned on. Use these flags only to detect if something "fishy" is happening during the gameplay.

<?php echo $toc->html_section(); ?>

<p>The engine allows you to easily define custom culling methods
or use hardware occlusion query (see examples and docs). This may help
a lot in large scenes (city or indoors).

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

<p>You can compile your
application with the <a href="https://github.com/castle-engine/castle-engine/wiki/Build-Tool">build tool</a>
using <code>--mode=valgrind</code> to get an executable ready to be tested
with the magnificent <a href="http://valgrind.org/">Valgrind</a> tool.

<p>You can use any FPC tool to profile your code, for memory and
speed. There's a small document about it in engine sources, see
<code>castle_game_engine/doc/old_notes/profiling_howto.txt</code>
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
