<?php
require_once 'castle_engine_functions.php';
tutorial_header('Optimization and profiling');
?>

<p>One you have a large game, with many large 3D models, you will
probably start to wonder about the speed and memory usage.</p>

<h2>Watch <i>Frames Per Second</i></h2>

<p>You have your speed, as the number of <i>Frames Per Second</i>, stored
in the TCastleControl.Fps or TCastleWindow.Fps object. It has fields
FrameTime and RealTime that you can read. (See docs of
TFramesPerSecond class in CastleTimeUtils.) We will explain the
difference between FrameTime and RealTime in a second.</p>

<p>How to show them? However you like:</p>

<ul>
  <li>If you use TCastleWindow,
    you can trivially enable FpsShowOnCaption to show FPS on your window
    caption.

  <li>You can show them on Lazarus label or caption. Just be sure to
    not update them too often --- updating normal Lazarus controls all
    the time may slow your OpenGL context drastically. Same warning goes
    about writing them to the console with Writeln --- don't call it too
    often, or your rendering will be slower. It's simplest to use
    Lazarus TTimer to update it only once per second or such. Actually,
    these properties actually show you an average from last second, so
    there's not even a reason to redraw them more often.

  <li>You can also simply display them on an OpenGL context (see the
    example about TGame2DControls in previous section).
</ul>

<p>We do not have any engine-specific tool to measure memory usage or
detect memory problems, as there are plenty of them available with
FPC+Lazarus already. To simply see the memory usage, just use process
monitor that comes with your OS. To detect memory leaks, be sure to
use FPC HeapTrc.pas (compile with -gl -gh). See also Lazarus units
like LeakInfo. Finally, you can use full-blown memory profilers like
valgrind's massif with FPC code (see section "Profiling" lower in this
tutorial).

<h2>How to interpret <i>Frames Per Second</i> values?</h2>

<p>There are two FPS values available: frame time and real time. Frame
time is usually the larger one. Larger is better, of course: it means
that you have smoother animation.

<p>Use "real time" to measure your overall game speed. This is the actual
number of frames per second that we managed to render. Caveats:

<ul>
  <li><p>Make sure to turn off "limit FPS" feautre, to get maximum number
    available. Use view3dscene "Preferences -> Frames Per Second" menu
    item, or (if you're a developer) change LimitFPS global variable (if
    you use CastleControls unit with Lazarus) or change
    Application.LimitFPS (if you use CastleWindow unit). Change them to
    zero to disable the "limit fps" feature.

  <li><p>Make sure to have an animation that constantly updates your
    screen. E.g. keep camera moving, or have something animated on
    screen. Otherwise, we will not refresh the screen (no point to
    redraw the same thing), and "real time" will drop to almost zero if
    you look at a static scene.

  <li><p>Note that the monitor will actually drop some some frames above it's
    frequency, like 80. This *may* cause you to observe that above some
    limit, FPS are easier to gain by optimizations, which may lead you
    to a false judgement about which optimizations are more useful than
    others. To make a valuable judgement about what is faster/slower,
    always compare two versions of your program when only the relevant
    thing changed &mdash; nothing else.
</ul>

<p>Use "frame time"... with caution. It's useful to compare it with "real
time", with LimitFPS feature = off: it may then tell you whether the
bottleneck is in rendering or outside of rendering (like collision
detection and creature AI). "Frame time" measures how much frame we
would get, if we ignore the time spent outside OnDraw events. Caveats:

<ul>
  <li><p>Modern GPUs work in parallel to the CPU. So "how much time CPU spent
    in OnDraw" doesn't necessarily relate to "how much time GPU spent on
    performing your drawing commands".
</ul>

<p>So making your CPU busy with something else (like collisions, or
waiting) makes your "frame time" lower, while in fact rendering times
the same time &mdash; you're just not clogging you GPU. Which is a
good thing, actually, if your game spend this time on something useful
like collisions. Just don't overestimate it &mdash; you didn't make
rendering faster, but you managed to do a useful work in the meantime.

<p>For example: if you set LimitFPS to a small value, you may observe
that "frame time" grows higher. Why? Because when the CPU is idle
(which is often if LimitFPS is small), then GPU has a free time to
finish rendering previous frame. So the GPU does the work for free,
outside of OnDraw time, when your CPU is busy with something
else. OTOH when CPU works on producing new frames, then you have to
wait inside OnDraw until previous frame finishes.

<p>In other words, improvements to "frame time" must be taken with a
grain of salt. We spend less time in OnDraw event: this does not
necessarily mean that we really render faster.

<p>Still, often "frame time" does reflect the speed of GPU rendering.

<p>If you turn off LimitFPS, and compare "frame time" with "real time",
you can see how much time was spent outside OnDraw. Usually, "frame
time" will be close to "real time". If the gap is large, it may mean
that you have a bottleneck in non-rendering code (like collision
detection and creature AI).

<h2>Preparing your 3D models to render fast</h2>

<p>The less vertexes and faces you can have, the better.
Also, the simpler models (no shadows etc.), the better. That's fairly
obvious. Exactly what matters most depends on your GPU a lot &mdash;
modern GPUs can consume a huge number of vertexes very fast, as long
as they are provided to them in a proper way.</p>

<p>In our engine, the "shape" is the unit of information we provide to
GPU. It is a VRML/X3D shape. In most cases, it also corresponds to the
3D object you design in your 3D modeller, e.g. Blender 3D object in
simple cases is exported to a single VRML/X3D shape (although it may
be split into a couple of shapes if you use different
materials/textures on it, as VRML/X3D is a little more limited (and
also more GPU friendly)).</p>

<p>The general advice is to compromise:</p>

<ol>
  <li><p>Do not make too many too trivial shapes. Do not make milions of
    shapes with only a few vertexes &mdash; each shape will be provided
    in a separate VBO to OpenGL, which isn't very efficient.

  <li><p>Also, do make too few shapes. Each shape is be provided in it's
    entirely to OpenGL (splitting it on the fly would cause unacceptable
    slowdown), and shapes may be culled using frustum culling or
    occlusion queries. By using only a few very large shapes, you make
    this culling worthless.
</ol>

<p>A rule of thumb is to keep your number of shapes in a scene between
100 and 1000. But that's really just a rule of thumb, different level
designs will definitely have different considerations.

<h2>Optimizing collisions</h2>

<p>We use an octree based on 3D model triangles for a precise collisions
detection with a level. For other objects, we use bounding volumes
like boxes and spheres. This means that the number of shapes doesn't
matter much for collision speed. However, number of triangles still
matters for level.

<p>Use X3D Collision node to easily mark unneeded shapes as
non-collidable or to provide a simpler "proxy" mesh to use for
collisions with complicated objects. See
demo_models/vrml_2/collisions_final.wrl for demo. It's really trivial
in X3D, and we support in 100% &mdash; I just wish there was a way to
easily set it from 3D modellers like Blender. Hopefully we'll get
better X3D exporter one day. Until them, you can hack X3D source, it's
quite easy actually. And thanks to using X3D Inline node, you can keep
your auto-generated X3D content separated from hand-written X3D code
&mdash; that's the reason for xxx_final.x3dv and xxx.x3d pairs of
files around the demo models.

<p>You can ajust the parameters how the octree is created. You can do it
in VRML/X3D file or by ObjectPascal code, see
http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_octree_properties
. But in practice I usually find that the default values are optimal,
for a wide range of scenes.

<h2>Profiling</h2>

<p>You can use any FPC tools to profile your code, for memory and
speed. There's a small document about it in engine sources, see
castle_game_engine/doc/profiling_howto.txt . See also wiki
http://wiki.lazarus.freepascal.org/Profiling .

<?php
tutorial_footer();
?>
