<?php
  require_once 'castle_engine_functions.php';
  require_once 'vrml_implementation_common.php';

  vrmlx3d_header("Kanim file format");

  $toc = new TableOfContents(
    array(
      new TocItem('What it is', 'what'),
      new TocItem('Exact format specification', 'specification'),
      new TocItem('Shortcomings of this format', 'shortcomings'),
    )
  );
?>

<?php
  echo pretty_heading("Kanim file format");
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>Files with extension *.kanim represent <i>"Castle Game Engine's animations"</i>.
These are XML files that describe precalculated animation as a sequence of files.
Animation shows the transition from the first model to the last.
Where models are structurally equal, intermediate frames are
created by linear interpolation to show smooth changes.</p>

<p><b>Since animation by VRML/X3D events and interpolators is implemented
in our engine now, Kanim format becomes obsolete.</b>
It's useful only if your favorite 3D modeler cannot export VRML/X3D
animation with interpolators, but it can export static VRML/X3D files.</p>

<p><?php echo a_href_page(
  "Blender exporter for this format is available", "blender"); ?>,
since Blender cannot export animations with interpolators to VRML.</p>

<p>On the positive side, there is at least one open-source
program that <i>can</i> create animations with interpolators:
<a href="http://vrml.cip.ica.uni-stuttgart.de/dune/">White Dune</a>.
<i>White dune</i> has even an exporter to Kanim format,
given a VRML animation by interpolators it generates a Kanim file
and corresponding VRML files for each frame.</p>

<p>If you work with 3D modeler that can export proper VRML animation
with interpolators, then you don't need to use Kanim format.
Our engine handles events and interpolators perfectly.
Internally, they may even be converted (after loading) to precalculated
animations.</p>

<p>There's also a crude converter from kanim format to VRML/X3D
interpolators in our engine examples (see <tt>examples/vrml/tools/kanim_to_interpolators</tt>).
It's a little crude (works only when you animate only a single mesh),
but may be enough for simple uses. So this is one way to generate an animated
VRML/X3D file from Blender: export from Blender to kanim, then convert
kanim to VRML/X3D.</p>

<p>For more technical insight, see
<a href="<?php echo CURRENT_URL; ?>vrml_engine_doc/output/xsl/html/chapter.animation.html">description
of animation handling in our VRML engine documentation</a>.</p>

<hr /> <?php echo $toc->html_section(); ?>

<p>File format is simple:

<pre>

&lt;?xml version="1.0"?&gt;
&lt;animation              // Root node is always "animation".
                        // All it's attributes are optional
                        // (default values are shown below).
                        // Some of it's attributes should be treated like a
                        // "hint for the renderer". General programs like
                        // view3dscene and demo_animation may honour them,
                        // but more specialized programs (like "The Castle"
                        // game) may ignore them, since "they know better".

  scenes_per_time="30"  // Suggested number of scenes per time to be generated.
                        // This is a hint for the renderer --- by default it's
                        // 30, but it may be ignored. Larger values make
                        // animation smoother, but also much more memory consuming.
                        // Special value 0 is allowed here, and means
                        // that animation will simply show each &lt;frame&gt;,
                        // suddenly changing into the next &lt;frame&gt;,
                        // without any smoothing of transitions with
                        // intermediate scenes.

  equality_epsilon="0.001"
                        // Epsilon to use when comparing animation frames
                        // and deciding which parts didn't move at all between
                        // two adjacent frames. You can set this even to literal
                        // "0.0", but this may cause a lot of memory to be wasted
                        // when loading large animation. It's better to set
                        // this to some very small value --- so small that you're
                        // sure that user will not notice such small move
                        // anyway.

  loop="false"          // Should the animation loop ? This is a hint for
                        // the renderer, and may be ignored. Allowed values
                        // are "false" and "true", not case-sensitive.

  backwards="false"     // Should the animation go backwards after going
                        // forward ? Allowed values
                        // are "false" and "true", not case-sensitive.
&gt;

  // A number of &lt;frame&gt; nodes should follow. At least one is required.
  // Note that exactly one &lt;frame&gt; node will actually define a still scene,
  // you need at least 2 frames if you want a real animation.

  &lt;frame

    file_name="file_1.wrl" // This is a required attribute, and specifies
                           // the filename from which to load this
                           // animation frame. Any 3D file format is allowed here:
                           // most of all, VRML/X3D, but also
                           // <?php echo a_href_page_hashlink(
                                'other formats understood by my engine',
                                'kambi_vrml_extensions',
                                'section_ext_inline_for_all'); ?>.

    time="0.0"             // This is a required attribute specyfying a
                           // time of this frame. For now, all frames
                           // must be specified in the strictly increasing order
                           // of their "time".
                           // This is understood to be in seconds.
  /&gt;

  // For example, assume that the second &lt;frame&gt; node follows.
  // So this defines an animation that changes from
  // file_1.wrl and file_2.wrl in exactly 1 second.

  &lt;frame file_name="file_2.wrl" time="1.0" /&gt;

&lt;/animation&gt;
</pre>

<hr/> <?php echo $toc->html_section(); ?>

<p>As I mentioned above, kanim format is obsolete.
Some things that cannot be achieved using kanim (and probably never
will be, as we would advice everyone to use VRML/X3D interpolators
for all your needs):</p>

<ul>
  <li><p>Our collision detection uses the first (or both first and last)
    frame. Octrees are not updated between frames.
    So collision detection, mouse picking,
    raytracer rendering are all done using octree for the 1st animation frame.</p>

    <p>Use instead VRML/X3D interpolators, when octree is properly managed.</p>
  </li>

  <li><p>Background animations do not work (we use MainScene.Background always).</p>

    <p>Use instead VRML/X3D interpolators, when background is fast updated.
    Note that you can use our
    <?php echo a_href_page('ColorSetInterpolator (extension to the interpolation component)',
    'vrml_implementation_interpolation'); ?> to animate sets of colors like
    <tt>skyColor</tt>, <tt>groundColor</tt>.
    See our <?php echo a_href_page('VRML/X3D demo models',
    'demo_models'); ?>
    (look inside <tt>background/background_animate*</tt>) for demos.</p>
  </li>

  <li><p>Some view3dscene features, like saving to VRML/X3D and
    "Remove Selected Geometry/Face", only work on the 1st animation frame.</p></li>
</ul>


<?php
  vrmlx3d_footer();
?>
