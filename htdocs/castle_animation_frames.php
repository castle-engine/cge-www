<?php
require_once 'castle_engine_functions.php';
require_once 'x3d_implementation_common.php';

vrmlx3d_header("Castle Animation Frames (castle-anim-frames) file format");

$toc = new TableOfContents(
  array(
    new TocItem('What it is', 'what'),
    new TocItem('Exact format specification', 'specification'),
    new TocItem('Shortcomings of this format', 'shortcomings'),
  )
);

echo pretty_heading($page_title);
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>Files with extension <code>*.castle-anim-frames</code>
(older version: <code>*.kanim</code>)
represent <i>"Castle Game Engine's Animation Frames"</i>.
These are XML files that describe animation as a sequence of 3D states
(3D files).
Animation shows the transition from the first model to the last.
Where models are structurally equal, intermediate frames are
created by linear interpolation to show smooth changes.</p>

<p><b>When possible, it's better to use animation straight from an
X3D file, using X3D events and interpolators.</b>
But this format is useful if your favorite 3D modeler (like Blender)
cannot export X3D animation with interpolators.
It has also the additional advantage that is handles <i>any</i>
kind of animation you can produce (transform,
through armature or not, deform in any way,
fluids, physics, material animations...).

<div class="download jumbotron">
    <a class="btn btn-primary btn-lg" href="https://raw.githubusercontent.com/castle-engine/cge-blender/master/export_kanim.py"><span class="glyphicon glyphicon-download" aria-hidden="true"></span><br>Download castle-anim-frames exporter</a>
</div>

<p><a href="http://wdune.ourproject.org/">White Dune</a>
also can generate <code>*.kanim</code> files
from an VRML animation by interpolators.</p>

<!--p>If you work with 3D modeler that can export proper VRML animation
with interpolators, then you don't need to use Kanim format.
Our engine handles events and interpolators perfectly.
Internally, they may even be converted (after loading) to precalculated
animations.</p-->

<!--p>There's also a crude converter from kanim format to VRML/X3D
interpolators in our engine examples (see <code>examples/vrml/tools/kanim_to_interpolators</code>).
It's a little crude (works only when you animate only a single mesh),
but may be enough for simple uses. So this is one way to generate an animated
VRML/X3D file from Blender: export from Blender to kanim, then convert
kanim to VRML/X3D.</p-->

<!--p>For more technical insight, see
<a href="<?php echo CURRENT_URL; ?>vrml_engine_doc/output/xsl/html/chapter.animation.html">description
of animation handling in our VRML engine documentation</a>.</p-->

<hr /> <?php echo $toc->html_section(); ?>

<p>File format is simple:

<pre>

&lt;?xml version="1.0"?&gt;
&lt;animation              // Root node is always "animation".
                        // All it's attributes are optional
                        // (default values are shown below).
                        // Some of it's attributes should be treated like a
                        // "hint for the renderer". General programs like
                        // view3dscene may honor them,
                        // but more specialized programs (like "The Castle"
                        // game) may ignore them, since "they know better".

  scenes_per_time="30"  // If the animation will be "baked":
                        //
                        // Suggested number of scenes per time to be generated.
                        // This is a hint for the renderer &mdash; by default it's
                        // 30, but it may be ignored. Larger values make
                        // animation smoother, but also much more memory consuming.
                        // Special value 0 is allowed here, and means
                        // that animation will simply show each &lt;frame&gt;,
                        // suddenly changing into the next &lt;frame&gt;,
                        // without any smoothing of transitions with
                        // intermediate scenes.

  equality_epsilon="0.001"
                        // If the animation will be "baked":
                        //
                        // Epsilon to use when comparing animation frames
                        // and deciding which parts didn't move at all between
                        // two adjacent frames. You can set this even to literal
                        // "0.0", but this may cause a lot of memory to be wasted
                        // when loading large animation. It's better to set
                        // this to some very small value &mdash; so small that you're
                        // sure that user will not notice such small move
                        // anyway.

  loop="false"          // Should the animation loop? This is a hint for
                        // the renderer, and may be ignored. Allowed values
                        // are "false" and "true", not case-sensitive.
                        // When this animation is used for creature/item in game,
                        // this is <b>ignored</b>.

  backwards="false"     // Should the animation go backwards after going
                        // forward? Allowed values
                        // are "false" and "true", not case-sensitive.
                        // When this animation is used for creature/item in game,
                        // this is <b>not ignored</b>.
&gt;

  // A number of &lt;frame&gt; nodes should follow. At least one is required.
  // Note that exactly one &lt;frame&gt; node will actually define a still scene,
  // you need at least 2 frames if you want a real animation.

  &lt;frame

    url="file_1.x3d" // This specifies the URL from which to load this
                     // animation frame. Any 3D file format is allowed here:
                     // most of all, VRML/X3D, but also
                     // <?php echo a_href_page_hashlink(
                          'other formats understood by view3dscene and our engine',
                          'view3dscene',
                          'section_features'); ?>.
                     // There is also a deprecated attribute "file_name"
                     // that means the same as "url".
                     //
                     // This attribute may be omitted, in which case
                     // we expect the &lt;frame&gt; element to contain X3D
                     // scene graph encoded in XML (with top-level &lt;X3D&gt;
                     // element).

    time="0.0"       // This is a required attribute specifying a
                     // time of this frame. For now, all frames
                     // must be specified in the strictly increasing order
                     // of their "time".
                     // This is understood to be in seconds.
  /&gt;

  // For example, assume that the second &lt;frame&gt; node follows.
  // So this defines an animation that changes from
  // file_1.x3d and file_2.x3d in exactly 1 second.

  &lt;frame url="file_2.x3d" time="1.0" /&gt;

&lt;/animation&gt;
</pre>

<?php /*

<hr/> <?php echo $toc->html_section(); ?>

<p>As I mentioned above, kanim format is obsolete.
Some things that cannot be achieved using kanim (and probably never
will be, as we would advice everyone to use VRML/X3D interpolators
for all your needs):</p>

<ul>
  <li><p>Our collision detection uses the first (or both first and last)
    frame. Octrees are not updated between frames.
    So collision detection, mouse picking,
    ray-tracer look only at the 1st animation frame,
    because our octree represents only this frame.</p>

    <p>Use instead VRML/X3D interpolators, when octree is properly managed.</p>
  </li>

  <li><p>Background animations do not work (we use MainScene.Background always).</p>

    <p>Use instead VRML/X3D interpolators, when background is fast updated.
    Note that you can use our
    <?php echo a_href_page('ColorSetInterpolator (extension to the interpolation component)',
    'x3d_implementation_interpolation'); ?> to animate sets of colors like
    <code>skyColor</code>, <code>groundColor</code>.
    See our <?php echo a_href_page('VRML/X3D demo models',
    'demo_models'); ?>
    (look inside <code>background/background_animate*</code>) for demos.</p>
  </li>

  <li><p>Some view3dscene features, like saving to VRML/X3D and
    "Remove Selected Geometry/Face", only work on the 1st animation frame.</p></li>
</ul>

*/ ?>

<?php
  vrmlx3d_footer();
?>
