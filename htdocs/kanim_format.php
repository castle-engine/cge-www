<?php
  require_once 'vrmlengine_functions.php';

  common_header("Kanim file format", LANG_EN,
    "Specification of kanim file format - Kambi VRML engine's animations");
?>

<?php
  echo pretty_heading("kanim file format");
?>

<p>Files with extension *.kanim represent <i>"Kambi VRML engine's animations"</i>.
These are XML files that describe animation as a sequence of files,
all with the same structure. The animation is created by interpolating
among the files in specified order and with specified speed.</p>

<hr />

<p>Before you say that this is a lousy way of storing animations,
and storing everything inside one file is obviously more flexible
for complicated animations: I know. And VRML >= 2 even has nodes
(called interpolators) to express this animation inside a file:
I know that too. But there is no existing open-source professional
3D authoring tool that can generate such VRML 2 files.
(By which I mean: Blender can't export animations to VRML 2 files yet,
and Blender is *the best*.) AFAIK, of course.</p>

<p>For more: see <a href="vrml_engine_doc/output/xsl/html/ch06.html">description
of animation handling in Kambi VRML engine</a>.</p>

<p>So for now, this is a practical and working way of creating animations:
just export from Blender a couple of times, making a couple of VRMLs,
and then create *.kanim file to glue them all together and load it into
my engine.</p>

<hr />

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
                        // 30, but it may be ignored, and you don't have to really
                        // understand what it means... If you want, see
                        // TVRMLGLAnimation class documentation.

  optimization="separate-shape-states-no-transform"
                        // Suggested optimization method.
                        // This is again only a hint for the renderer, and may
                        // be ignored, see `view3dscene --help' to see the
                        // list of avail values.
                        // Ignore this if you don't know what it means.

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
                           // most of all, VRML, but also
                           // <?php echo a_href_page_hashlink(
                                'other formats understood by my engine',
                                'kambi_vrml_extensions',
                                'ext_inline_for_all'); ?>.

    time="0.0"             // This is a required attribute specyfying a
                           // time of this frame. For now, all frames
                           // must be specified in the strictly increasing order
                           // of their "time".
                           // This is understood to be in seconds.
  /&gt;

  // For example, assume that the second &lt;frame&gt; node follows.
  // So this defines an animation that interpolates between
  // file_1.wrl and file_2.wrl in exactly 1 second.

  &lt;frame file_name="file_2.wrl" time="1.0" /&gt;

&lt;/animation&gt;
</pre>

<?php
  if (!IS_GEN_LOCAL) {
    $counter = php_counter("kanim_format", true);
  };

  common_footer();
?>
