<?php
require_once 'castle_engine_functions.php';
require_once 'x3d_implementation_common.php';

vrmlx3d_header("Castle Animation Frames (castle-anim-frames) file format");

$toc = new TableOfContents(
  array(
    new TocItem('Introduction', 'intro'),
    new TocItem('Advantages and Disadvantages', 'good_and_bad'),
    new TocItem('Exact Specification', 'specification'),
  )
);

echo pretty_heading($page_title);
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>Files with extension <code>*.castle-anim-frames</code>
(older version: <code>*.kanim</code>)
represent <i>"Castle Game Engine's Animation Frames"</i>.</p>

<div class="download jumbotron">
    <a class="btn btn-primary btn-lg" href="https://raw.githubusercontent.com/castle-engine/cge-blender/master/export_castle_anim_frames.py"><span class="glyphicon glyphicon-download" aria-hidden="true"></span><br>Download castle-anim-frames Blender exporter</a>
</div>

<p>More information about <a href="creating_data_blender.php">exporting animations from Blender is available here</a>.</p>

<p><a href="http://wdune.ourproject.org/">White Dune</a>
also can generate <code>*.kanim</code> files
from an VRML animation by interpolators.</p>

<?php echo $toc->html_section(); ?>

<p>The format is a series of static 3D frames. They can be inlined in a large XML file (in which case they must follow X3D XML encoding), or they can be external files (in which case they can be any 3D format supported by the <i>Castle Game Engine</i>). Animation shows the transition from the first frame to the last. Where models are <i>structurally equal</i>, intermediate frames are created by a linear interpolation to show smooth changes.

<p>All the advantages and disadvantages of this format come from this simplicity. On one hand, it can be used to transfer <i>any</i> Blender animation to Castle Game Engine. On the other hand, it can be quite heavy: <i>loading large animations takes some time</i>, and they eat a significant amount of memory.</p>

<p>Some of these disadvantages may be mitigated in the future (as we will internally convert it to better X3D interpolator nodes). But it will always remain somewhat "heavy solution".</p>

<p>At the same time, it will always remain something that <i>can handle any Blender animation, right now</i>. As opposed to X3D exporter (that currently cannot export Blender animation at all, and in the future will support a limited subset of Blender possibilities).</p>

<p>A temporary disadvantage (TODO) is that right now we do not interpolate at runtime using nice X3D interpolators. Instead, we generate a series of frames at loading, merge the tree when nodes are equal, and move through them using X3D <code>Switch</code> and <code>IntegerSequencer</code>. Once we improve this, the runtime memory usage will be <i>somewhat</i> better, and animation will be always perfectly smooth at runtime, and the collision detection will account for dynamic changes OK.

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

<hr />

<?php echo $toc->html_section(); ?>

<p>File format is simple:

<pre>

&lt;?xml version="1.0"?&gt;
&lt;animations              // Root node should be "animations".
                        // It should contain any number (at least one)
                        // "animation" nodes inside.
                        // For backward compatibility,
                        // old (single-animation) files may also
                        // use "animation" as the root node.

  &lt;animation              // A single animation.
                          // All it's attributes are optional
                          // (default values are shown below).
                          // Some of it's attributes should be treated like a
                          // "hint for the renderer". General programs like
                          // view3dscene may honor them,
                          // but more specialized programs (like "The Castle"
                          // game) may ignore them, since "they know better".

    name="animation"      // Animation name.
                          // To be used e.g. with TCastleScene.PlayAnimation.

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

  // More animations follow, if you want:
  &lt;animation name="another_animation"&gt;
    &lt;frame url="another_animation_frame_0.x3d" time="0.0" /&gt;
    &lt;frame url="another_animation_frame_1.x3d" time="1.0" /&gt;
  &lt;/animation&gt;

  &lt;animation name="yet_another_animation"&gt;
    &lt;frame url="yet_another_animation_frame_0.x3d" time="0.0" /&gt;
    &lt;frame url="yet_another_animation_frame_1.x3d" time="1.0" /&gt;
  &lt;/animation&gt;

&lt;/animations&gt;
</pre>

<?php
  vrmlx3d_footer();
?>
