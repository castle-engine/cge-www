<?php
  require_once 'castle_engine_functions.php';

  castle_header("view3dscene",
    'view3dscene is a lightweight browser for X3D and VRML 3D models, and a viewer for various other 3D model formats (3DS, MD3, Wavefront OBJ, and Collada, Spine JSON). It supports viewing the scene in the &quot;Examine&quot; mode and walking in the virtual world with features such as collision detection and gravity. Rendering is done using OpenGL. Many advanced 3D graphic effects are available, like mirrors (flat and through cube environment mapping), shadows (shadow maps and shadow volumes), GLSL shaders and more. The program can also convert files from 3DS, MD3, Collada, Spine and other formats to X3D and pretty-print VRML and X3D files. ' .
    'Free software. For Linux, Mac OS X and Windows.');

  function section($make_hr = true)
  {
    if ($make_hr)
      echo "<hr class=\"ruler_between_sections\">";
    global $toc;
    echo $toc->html_section();
  }

  echo flattr_button();

  echo pretty_heading("view3dscene", VERSION_VIEW3DSCENE);
  echo castle_thumbs(array(
    array('filename' => 'castle_sunset.png', 'titlealt' => '&quot;view3dscene&quot; rendering tower with sunset sky'),
    array('filename' => 'atcs_viewports_frustum.png', 'titlealt' => 'Tremulous ATCS in VRML, with 2 viewports and frustum visualized in right viewport'),
    array('filename' => 'horse_bump_from_3ds.png', 'titlealt' => 'Horse model from 3DS file with bump map'),
//    array('filename' => 'view3dscene_2.0.0_screen_demo.png', 'titlealt' => '&quot;view3dscene&quot; rendering tower with mountains sky'),
    array('filename' => 'view3dscene_tooltip_and_smoke.png', 'titlealt' => 'Examine navigation tooltip, and some fog by texture layers'),
    array('filename' => 'view3dscene_screen_demo_1.png', 'titlealt' => '&quot;view3dscene&quot; rendering Tremulous creature from MD3'),
//    array('filename' => 'view3dscene_screen_demo_2.png', 'titlealt' => '&quot;view3dscene&quot; in the middle of ray-tracing'),
//    array('filename' => 'view3dscene_screen_demo_3.png', 'titlealt' => '&quot;view3dscene&quot; with ProximitySensor visualization'),
    array('filename' => 'lucy_joints_visualization.png', 'titlealt' => 'Lucy with our joints visualization'),
//    array('filename' => 'upwind_turbine.png', 'titlealt' => 'Wind turbine simulations, from SSB Wind Systems, with 4 viewports'),
  ));
?>

<p>view3dscene is a VRML / X3D browser, and a viewer for other 3D model
formats.</p>

<?php
  $toc = new TableOfContents(
    array(
      new TocItem('Downloading and installing', 'install'),
      new TocItem('Optionally install GNOME (and other freedesktops) integration', 'install_free_desktop', 1),
      new TocItem('Features', 'features'),
      new TocItem('Navigation with keys &amp; mouse', 'navigation'),
      new TocItem('Command-line options', 'command_line_options'),
        new TocItem('Capturing screenshots and movies of 3D scenes and animations', 'screenshot', 1),
        new TocItem('Converting to VRML/X3D', 'converting', 1),
        new TocItem('Other options', 'other_options', 1),
        new TocItem('Deprecated options', 'deprecated_options', 1),
      new TocItem(DEPENDS, 'depends'),
    )
  );
  echo $toc->html_toc();
?>

<?php section(false); ?>

<?php echo_standard_program_download('view3dscene', 'view3dscene',
  VERSION_VIEW3DSCENE, true); ?>

<p>No installation is required. Just download and unpack these archives wherever
you want, and run the <code>view3dscene</code> program inside.
Included is also the <code>tovrmlx3d</code> program,
useful for <a href="#section_converting">converting 3D models to VRML/X3D in batch (command-line) mode</a>.
The documentation (this web page) is also included inside, for offline viewing
(open the <code>documentation/view3dscene.html</code> file).</p>

<p><?php echo SOURCES_OF_THIS_PROG_ARE_AVAIL; ?></p>

<p><i>Demo scenes</i>: our
<?php echo a_href_page("VRML/X3D demo models", "demo_models"); ?>
 contains a lot of interesting models,
you can open them all with <code>view3dscene</code>.</p>

<p><i>For Linux:</i> Install also <a href="http://projects.gnome.org/gtkglext/">gtkglext</a>
on your system using your package manager.
If you want to hear 3D sound in some VRML/X3D worlds, install also
<?php echo a_href_page('OpenAL', 'openal'); ?> and
<a href="http://xiph.org/vorbis/">OggVorbis (VorbisFile and dependencies)</a>
libraries. <i>For Windows:</i> All useful libraries
are already included in the archive, so you don't have to do anything.
<?php echo a_href_page('Mac OS X requirements are listed here', 'macosx_requirements'); ?>.</p>

<?php section(false); ?>

<p>If you use GNOME (or other desktops following
<a href="http://freedesktop.org/">freedesktop.org</a> specifications),
you can optionally install also view3dscene menu item
(will be placed in the <i>Graphics</i> menu category), with a nice icon,
and associate it with appropriate 3D model types.</p>

<pre>
# Place view3dscene on $PATH, for example like this:
sudo ln -s /usr/local/bin/view3dscene view3dscene

# Install menu items, icons, mime types:
cd desktop/
./install.sh
</pre>

<p>You may need to logout and login again to your GNOME/desktop
session for all programs to catch up (alternatively, you can do
<code>killall gnome-panel &amp;&amp; killall nautilus</code>
but this is obviously somewhat brutal method).</p>

<p>If you use GNOME file manager Nautilus there's
one more cool thing you can do: use
view3dscene to <b>generate on-the-fly thumbnails of 3D models</b>
in the viewed directory. Assuming that view3dscene is on the $PATH
and you already did previous <code>./install.sh</code>, you can run:</p>

<pre>
./install_thumbnailer.sh
</pre>

<p><!--inside the <code>desktop</code> directory.-->
This will add the gconf keys to run thumbnailers on your 3D models.
Enter some directory with VRML / X3D / other 3D files,
and enjoy your thumbnails :)
<i>Beware that loading arbitrary 3D scene may take a lot of time,
so using the thumbnailer may consume a lot of memory and CPU power</i>.
But it seems that thumbnailer is nicely run with appropriate priority,
so it doesn't actually exhaust your cpu.
<!--
Although we try
to be fast, and some things are specially optimized for screenshot,
there are no guarantees. No engine can load arbitrary large
3D data without any noticeable resource use.
Nautilus should automatically terminate thumbnailer that
runs too long, so this is not critical problem. After all, reading large
movies or images has similar problems, but it works quite Ok, right?
(Actually, 3D data is much more difficult than reading just a few starting
seconds of a movie or a simple 2D image...) -->
And the author of this
text is using view3dscene thumbnailer all the time, and it works
flawlessly :) So give it a try!

<?php section(); ?>

<p>Supported file formats:
<ul>
  <li><p><b><?php echo a_href_page('VRML 1.0, 2.0 and X3D', 'vrml_x3d'); ?></b>.
    Usual extensions for VRML files are
    <code>.wrl</code>, <code>.wrz</code> and <code>.wrl.gz</code>.
    For X3D (we support fully both
    XML and classic encoding) extensions are <code>.x3d</code>,
    <code>.x3dz</code>, <code>.x3d.gz</code>
    and <code>.x3dv</code>, <code>.x3dvz</code>, <code>.x3dv.gz</code>.</p>

    <p>Almost complete VRML&nbsp;1.0 support is done.
    VRML&nbsp;2.0 (aka VRML&nbsp;97) and X3D support is also quite advanced,
    a lot of nodes and features are implemented (including
    advanced texturing and GLSL shaders,
    <code>PROTO</code> and <code>EXTERNPROTO</code> support,
    events mechanism with routes, sensors and interpolators).

    <!-- Among things that work are embedded textures,
    semi-transparent materials and semi-transparent textures,
    automatic normals smoothing (based on <code>creaseAngle</code>),
    triangulating non-convex faces, understanding camera nodes,
    WWWInline handling, text rendering and more. -->

    See <?php echo a_href_page('VRML/X3D implementation status',
      'x3d_implementation_status'); ?> for detailed list of supported
    features. See also <?php echo a_href_page('VRML/X3D extensions',
    'x3d_extensions'); ?>, <?php
      echo a_href_page('VRML/X3D demo models', 'demo_models'); ?>,
    and finally <a href="http://web3d.org/x3d/specifications/">
    the official X3D specifications</a>.

  <li><p><b><?php echo a_href_page(
    "Kanim (Castle Game Engine animations)", 'kanim_format'); ?></b> format
    is handled, animation is played.</p>

  <li><p><b><a href="http://www.khronos.org/collada/">Collada</a></b>
    (<code>.dae</code> extension).
    We support a lot of Collada features &mdash; geometry with materials,
    textures, cameras, lights. Tested on many Collada examples,
    like <a href="http://collada.org/owl/">Collada Test Model Bank</a>
    and Collada models exported from various <a href="http://www.blender.org/">Blender</a>
    versions. Animations in Collada files are <i>not</i> handled yet.
    All modern Collada versions (1.3, 1.4, 1.5) are handled.
    </p>

  <li><p>Also many
    <a href="http://oss.sgi.com/projects/inventor/"><b>OpenInventor's</b></a>
    1.0 ASCII files (<code>.iv</code> extension) are handled.
    Mainly it's because Inventor 1.0 and VRML 1.0 are very similar
    formats, but view3dscene handles also some additional
    Inventor-specific nodes.

  <li><p><b>3d Studio 3DS format</b>. Not every information in 3DS
    is handled by view3dscene but most important things, like
    materials, texture coordinates and texture filenames are supported.

  <li><p><b>MD3</b>. This is the format used for models
    in Quake 3 and derivatives (<a href="http://tremulous.net/">Tremulous</a>
    etc.). Almost everything useful is read from MD3 file:
    geometry with texture (coordinates, and texture filename from
    associated <code>xxx_default.skin</code> file), <i>animation is also read
    and played</i>.</p>

  <li><p><b>Wavefront OBJ files</b>. Most useful things are supported:
    geometry (with texture coords, normal vectors), materials
    (colors, opacity, texture filenames).</p>

  <li><p><b>Spine JSON animations</b>.
    <a href="http://esotericsoftware.com/">Spine</a> is a powerful program
    for 2D game animations.</p>

  <li><p><b><a href="http://local.wasp.uwa.edu.au/~pbourke/dataformats/geo/">Videoscape
    GEO</a></b> (<code>.geo</code> extension).
    Very basic support for this very old 3D format.
</ul>

<p>Among many features are:
<ul>
  <li>Various navigation modes are available:
    <code>Examine</code> (easily rotate and move the whole model),
    <code>Walk</code> (walk like in FPS games,
    with collision detection, gravity and related features available),
    <code>Fly</code> (similar to <code>Walk</code> but without gravity).
  <li>Conversion of Collada, 3DS, MD3, Wavefront OBJ and GEO files to X3D.
  <li>You can convert between X3D classic and XML encodings (in both directions),
    and you can convert from VRML 2 to X3D.
    You can also use view3dscene as a "pretty-printer",
    just open and save any VRML/X3D file without any version conversion.

    <p>Command-line options to convert in batch mode (<code>--write</code>)
    are available in view3dscene. Special minimized binary
    <code>tovrmlx3d</code> (useful to install on servers without GUI libraries
    available) is also included in view3dscene archive.
  <li>A wealth of Castle Game Engine's rendering features are available,
    like GLSL shaders, bump mapping and shadows.
  <li>Built-in ray-tracer
    (that is also available as a separate command-line program,
    <?php echo a_href_page("rayhunter", "rayhunter"); ?>)
    to generate nice views of the scene (with shadows, mirrors,
    and transmittance). Classic ray-tracer implements exactly VRML 97 / X3D
    lighting equations.

  <li><p>You can inspect your model (select triangles by clicking
    <i>right mouse button</i> in <i>Walk / Fly</i> mode,
    and use menu item <i>Help</i> -> <i>Selected object information</i>).

    <p>There are also very limited editing capabilities. They are
    intended to be used only as post-processing of some model.
    We intentionally do not try to implement a full 3D authoring program here.

  <li><p><i>Interactive animations</i> may be played from VRML / X3D files,
    using sensors, scripts, interpolators and all other VRML events features.

    <p>You can activate VRML pointing-device sensors by clicking with
    <i>left mouse button</i> (the cursor will change shape and you will
    get status information when your cursor is over some clickable sensor).
    Note that this works only when <i>Collision
    detection</i> is on (as it requires octree).</p></li>

  <li><p><i>Precalculated animations</i> are played from
    <?php echo a_href_page("Kanim", 'kanim_format'); ?> or MD3 files
    (and you can convert any interactive VRML/X3D animation to precalculated one).</p>

    <p>Note that for now, for precalculated animations
    some features (collision checking, mouse picking,
    ray-tracer &mdash; everything that requires some octree) always use the
    <i>first animation frame</i>,
    regardless of current animation frame displayed.</p>

  <li><p>There are menu items and <a href="#section_screenshot">command-line
    options to catch screenshots and movies
    of 3D scenes and animations</a>. GNOME users will be happy to hear
    that it can also be used as <a href="#section_install_free_desktop">Nautilus
    thumbnailer</a>, providing thumbnails when you view directory with
    VRML / X3D and other 3D models. We can also make a special
    "screenshot" of 3D environment as a cube map (to DDS, or six
    separate images).
</ul>

<?php section(); ?>

<h3>All navigation modes:</h3>

<ul>
  <li><p><i>Left mouse button</i> is used for interacting
    with the VRML/X3D world. When the cursor turns into a grabbing hand
    you know you can click or drag on this 3D object. This uses
    the <?php echo a_href_page('VRML/X3D pointing-device sensors',
    'x3d_implementation_pointingdevicesensor'); ?>, and is fully configurable
    by 3D world authors.</p>

  <li><p><i>Ctrl + Right mouse click</i> picks a point,
    selecting a triangle and it's containing shape.
    The selected point / triangle / shape is then used for some
    operations, like <i>"Help -&gt; Selected Object Information"</i>.
</ul>

<h3><code>Examine</code> navigation mode:</h3>

<table class="key_list">
  <tr><th colspan="2">Mouse:</th></tr>
  <tr><td>Rotate</td> <td>Left mouse dragging</td>                           </tr>
  <tr><td>Move</td>   <td>Middle mouse dragging (or Left mouse + Shift)</td> </tr>
  <tr><td>Zoom</td>   <td>Right mouse dragging (or Left mouse + Ctrl; or scroll wheel)</td>   </tr>

  <tr><th colspan="2">Keys:</th></tr>
  <tr><td>Rotate</td>        <td>Arrows</td>        </tr>
  <tr><td>Stop rotating</td> <td>Space</td>                             </tr>
  <tr><td>Move</td>          <td>Ctrl + Arrows</td> </tr>
  <tr><td>Scale</td>         <td>+ / -</td>                             </tr>
</table>

<h3><code>Walk</code> / <code>Fly</code> navigation modes:</h3>

<img src="<?php echo CURRENT_URL; ?>images/original_size/navigation_controls.png" alt="Walk/Fly navigation controls" />

<table class="key_list">

  <tr><th colspan="2">Mouse:</th></tr>
  <tr><td>Forward / backward</td>    <td>Drag up / down with left mouse button</td></tr>
  <tr><td>Rotate</td>                <td>Drag left / right with left mouse button</td></tr>
  <tr><td>Move (strafe) left / right</td>     <td>Drag left / right with right mouse button</td></tr>
  <tr><td>Fly up / down</td>         <td>Drag up / down with right mouse button</td></tr>
  <tr><td>Raise / bow your head</td> <td>Mouse wheel</td></tr>

  <tr><th colspan="2">Keys:</th></tr>
  <tr><td>Forward / backward</td>                <td>W / S or Up / Down</td> </tr>
  <tr><td>Rotate</td>                            <td>Left / Right</td>       </tr>
  <tr><td>Move (strafe) left / right</td>        <td>A / D</td>              </tr>
  <tr><td>Jump / Crouch (or fly up / down)</td>  <td>Space / C</td>          </tr>
  <tr><td>Run</td>                               <td>Shift</td>              </tr>

  <tr><td colspan="2" style="text-align: left"><b>Turn <i>Mouse Look</i> <i>On</i>
    (Ctrl+M) to look around by moving the mouse.</b><br>
    It is usually comfortable to combine it with movement using AWSD keys.</td></tr>

  <tr><th colspan="2">Additional controls:</th></tr>

  <tr><td>Increase / decrease moving speed <!-- (has effect on keys
        Up / Down, Insert / Delete, Comma / Period) --></td> <td>+ / -</td></tr>
  <tr><td>Increase / decrease avatar height (preferred camera height above the ground)</td>
      <td>Ctrl + Insert / Delete</td></tr>
  <tr><td>Rotate <i>slower</i> (useful to precisely set the camera)</td>
  <!-- when you want to set up camera
       very precisely, e.g. to use this camera setting to render a scene
       image using ray-tracer)-->
       <td>Ctrl + Left / Right</td> </tr>
</table>

<p>We also support <a href="http://www.3dconnexion.com/">3D mouse devices</a>,
see <a href="http://www.youtube.com/watch?v=7tV9Qmjgx1U">the demo video about 3D mouse inside
view3dscene</a>.</p>

<p>There are many more operations with key shortcuts, that work in all
navigation modes.
Just explore the <i>view3dscene</i> menu, and look at the key shortcuts.</p>

<?php section(); ?>

<p>All options described below may be given in any order.
They all are optional.

<?php section(false); ?>

<p>Command-line options:

<pre>
--screenshot  TIME  FILE-NAME
--screenshot-range  TIME-BEGIN  TIME-STEP  FRAMES-COUNT  FILE-NAME
</pre>

<p>These options allow you to capture a screenshot of the loaded scene.
They know about animations stored in 3D files, that's
why they take parameters describing the animation time to capture.
They are used to take screenshots in "batch mode".
(In interactive mode, you can use comfortable
menu items <i>Display -> Screenshot...</i>.)</p>

<p><i>For a still 3D scene</i>, you usually just want to use the simpler
<code>--screenshot</code> option with <code>TIME</code>
set to anything (like zero) and not worry about anything else.

<p><i>For animations</i>, more possibilities are available. You can capture
any frames of the animation by using many <code>--screenshot</code> options.
You can also capture a movie by <code>--screenshot-range</code>
(as a series of images or, if
<a href="http://ffmpeg.mplayerhq.hu/">ffmpeg</a> is installed and
available on $PATH, even directly to a single movie file).
The biggest advantage of recording movie this way
is that the movie is guaranteed to be captured with stable number
of frames per second. This is different than using
some independent programs to capture OpenGL output, like
the fine <a href="https://github.com/nullkey/glc/wiki/">GLC</a>
(<a href="http://www.dedoimedo.com/computers/glc.html">nice GLC overview here</a>),
as real-time capture usually means that the program
runs slower, and often you loose movie quality.

<p>You most definitely want to pass 3D model file to load
at command-line too, otherwise we'll just make a screenshot
of the default empty (black) scene. So to take a simple screenshot
of a scene, at it's default camera, just call</p>

<pre>
view3dscene my_model.wrl --screenshot 0 output.png
</pre>

<p><b>The detailed specification how screenshot options work</b>:

<ul>
  <li><p>First of all, after all the <code>--screenshot</code>
    and <code>--screenshot-range</code> options are processed,
    view3dscene exits. So they work in "batch mode".

  <li><p>The <code>--screenshot  TIME  FILE-NAME</code> simply saves
    the screenshot at time <code>TIME</code> to an image file <code>FILE-NAME</code>.

    <p>Image format is guessed from <code>FILE-NAME</code> extension,
    see <?php echo a_href_page("glViewImage", "glviewimage") ?>
    for detailed list of image formats that we can handle.
    In short, we handle many popular image formats, like PNG and JPG,
    and these are what usually you want to use.

  <li><p>The <code>--screenshot-range  TIME-BEGIN  TIME-STEP  FRAMES-COUNT  FILE-NAME</code>
    option takes <code>FRAMES-COUNT</code> screenshots. The first screenshot
    is at time <code>TIME-BEGIN</code>, second screenshot is at
    <code>TIME-BEGIN + TIME-STEP</code>, next one is at
    <code>TIME-BEGIN + 2 * TIME-STEP</code>... you get the idea.

    <p>The <code>FILE-NAME</code> is either
    <ol>
      <li><p>A movie filename. This must have a recognized movie MIME type
        (for local files, MIME type is just recognized from extension),
        this includes:

        <ul>
          <!-- Based on FfmpegVideoMimeType and URIMimeType implementations -->
          <li>video/x-msvideo (.avi)
          <li>video/mpeg (.mpeg, .mpg, .mpe)
          <li>video/ogg (.ogv)
          <li>video/quicktime (.mov)
          <li>video/x-flv (.flv)
          <li>application/x-shockwave-flash (.swf, .swfl)
          <li>video/mp4 (.mp4)
        </ul>

        <p>Please report if we miss some MIME type (and file extension)
        above, it's trivially easy to add all formats supported by ffmpeg.
        Availability of all these video formats may depend on installed ffmpeg
        codecs.

        <p><a href="http://ffmpeg.mplayerhq.hu/">ffmpeg</a>
        must be installed and available on $PATH for this to work.

      <li><p><code>FILE-NAME</code> may also be a pattern to generate
        names of images to save, like <code>@counter(4).png</code>.
        Details about using filename patterns are below (although you
        can probably already guess how it works :) ).
    </ol>

  <li><p>All filenames for both screenshot options may specify a pattern
    instead of an actual filename. A pattern is simply a filename
    with macro <code>@counter(&lt;padding&gt;)</code> inside, when capturing <code>@counter(4)</code>
    will be replaced by the current screenshot number (padded with zeros to 4 digits).
    For capturing a series of images
    by <code>--screenshot-range</code> it's even required to specify
    a pattern with <code>@counter(&lt;padding&gt;)</code> (since capturing
    a number of images to a single image file has no point...). But
    it's also allowed in all other cases, even a movie filename
    may also be a pattern with <code>@counter(&lt;padding&gt;)</code> sequence,
    in case you want to use multiple <code>--screenshot-range</code>
    options to get multiple auto-named movies.

    <p>The precise description how <code>@counter(&lt;padding&gt;)</code> works:
    All <code>--screenshot</code>
    and <code>--screenshot-range</code> options are processed in order.
    When a filename with pattern <code>@counter(&lt;padding&gt;)</code> is found, we replace all
    <code>@counter(&lt;padding&gt;)</code> occurrences in this filename with current counter value
    and increment the counter. For <code>--screenshot-range</code> with
    an image pattern, we do this for every single frame.
    The counter starts at 1.

    <p>The parameter of <code>@counter(&lt;padding&gt;)</code>
    is the padding.
    For example, <code>@counter(1)</code> results in names like
    1, 2, ..., 9, 10... while <code>@counter(4)</code>
    results in names like 0001, 0002, ..., 0009, 0010, ...

  <li><p>Add <code>--screenshot-transparent</code> option to have a transparent image,
    with transparent pixels in place where background color would be visible.
</ul>

<p><b>Examples</b>:

<ul>
  <li><p>Simply get a single screenshot at given time:

<pre>
view3dscene my_model.wrl --screenshot 0 output.png
</pre>
  </li>

  <li><p>Simply get a movie of 2 seconds of animation.
    To calculate the numbers, note that we generate a movie with
    25 frames per second:

<pre>
view3dscene my_model.kanim --screenshot-range 0 0.04 50 output.avi
</pre>

    <p>To get this as a sequence of images, just use <code>output@counter(4).png</code>
    instead of <code>output.avi</code>.
  </li>

  <li><p>Example of more complicated use:

<pre>
view3dscene my_model.kanim \
  --screenshot-range 0 0.04 50 output@counter(1).avi \
  --screenshot-range 10 0.04 50 output@counter(1).avi
</pre>

    <p>This generates two files: <code>output1.avi</code> with 2 second animation
    from 0.0 to 2.0 time, and <code>output2.avi</code> with 2 second animation
    from 10.0 to 12.0 time.
  </li>
</ul>

<p><b>Hints:</b></p>

<ul>
  <li><p>To control the look of your screenshot, you often want to
    use VRML/X3D nodes like <code>Viewpoint</code>, <code>NavigationInfo</code>,
    <code>Background</code>. For example, take a look at
    <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/rift/data/creatures/humanoid/screenshot_for_kambi_www/walk_1.wrl">this sample VRML file</a>.</p>

    <p>You can use <code>--viewpoint</code> command-line option (see below)
    to choose a different viewpoint for screenshot.</p>
    </li>

  <li><p>You can generate wanted <code>Viewpoint</code> node
    also by using view3dscene, just set your camera (in interactive mode)
    the way you like and use menu item
    <i>Console -> Print Current Camera Node</i>.</p>

  <li><p>To control the size of resulting screenshot, just use
    <code>--geometry</code> command-line parameter
    (documented at <?php echo a_href_page("standard options
    understood by our OpenGL programs", "opengl_options") ?>).
    For example, take a look at
    <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/rift/data/creatures/humanoid/screenshot_for_kambi_www/mk_screenshot_for_kambi_www.sh">mk_screenshot_for_kambi_www.sh</a>
    script.</p></li>

  <li><p>To make your screenshot look best, you may want to use anti-aliasing,
    see <code>--anti-alias</code> option below.</p>

    <p>Take a look at the example how to make
    a screenshot from animation in
    <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/rift/data/creatures/humanoid/screenshot_for_kambi_www/">screenshot_for_kambi_www/</a>
    directory.</p></li>

  <li><p>To <b>take a screenshot on a stripped-down Unix server</b>,
    please bear in mind that you need to install a graphic environment
    (that is, <i>X Windows</i> and <i>OpenGL</i>) on your server. Even in batch mode,
    we still use OpenGL to grab the screenshot images
    (because using off-screen Mesa or our toy ray-tracer doesn't result
    in a really nice output; we really want OpenGL for all those GLSL effects
    and such).<!-- Which means that all that GUI stuff must be installed
    and working on your server.--></p>

    <p>Normally, you also need <i>GTK</i> and <i>GtkGLExt</i> libraries installed.
    However, you can compile from sources a version of view3dscene that doesn't
    need these libraries, and directly accesses XWindows. It will miss
    a menu bar and some other nice GUI stuff, but that's not a problem if you only
    want to run it in batch mode for screenshots. To do this:

    <ol>
      <li><?php echo a_href_page('Download the sources of
        castle_game_engine and view3dscene', 'all_programs_sources'); ?>.
        Unpack them, such that <code>castle_game_engine/</code> and <code>view3dscene/</code>
        directories are siblings.
      <li>In the terminal, do <pre>
export CASTLE_FPC_OPTIONS=-dCASTLE_WINDOW_XLIB
cd view3dscene/
./compile.sh
# and copy resulting view3dscene (and tovrmlx3d) binaries wherever you want
</pre></li>
    </ol>

    <p>On a server, you probably want to initialize taking a screenshot
    from a script, and your script isn't necessarily running within the X server.
    There are basically two solutions to this:
    <!--
    Note that the instructions below are not really specific to view3dscene,
    the same concepts work with any X program.--></p>

    <ol>
      <li><p>You can keep the X server running continuously,
        and keep your user logged in to the X server,
        and instruct view3dscene to connect to your running X server.
        You do this by adding <code>--display=:0</code> option
        to the view3dscene command-line (where <code>:0</code> is a common example; for details,
        see X manuals). Or you can set and export the <code>DISPLAY</code>
        environment variable, like <code>export DISPLAY=:0</code>.

        <p>Unfortunately, this method sometimes doesn't work.
        On some systems, the view3dscene will get an OpenGL context without
        a FrameBuffer (long story short, it means that you cannot capture
        the screen without actually seeing the window) and the resulting screenshot
        will be pure black (or garbage).
        On other systems, there is a problem with <code>glXChooseVisual</code>
        that may hang until you actually switch the current terminal
        to the X server.

      <li><p>The other approach, more reliable in my experience
        (<?php echo a_href_page('please share your own experience
        on the forum', 'forum'); ?>) is to create new X server along with running
        view3dscene, by using <code>xinit</code>. See <code>man xinit</code> for full
        details, in short use something like <code>xinit /full/path/to/view3dscene my_model.x3d --screenshot 0 /tmp/output.png -- :1</code>. The important thing
        is to specify the full path of the view3dscene binary (otherwise xinit
        only adds the arguments to some useless default xterm command-line). Adding <code>-- :1</code>
        at the end is only necessary if the default display (<code>:0</code>)
        may be already taken. <code>xinit</code> will create an X server
        with new display name, run view3dscene, and exit immediately
        when view3dscene exits (which should be as soon as a screenshot is done).
    </ol>
  </li>
</ul>

<?php section(false); ?>

<p>In interactive mode, you can use view3dscene menu items
<i>File -&gt; Save As..</i> to save (converting if needed) all 3D model
formats to VRML/X3D.

<ul>
  <li><p>Formats <b>Collada, 3DS, MD3, Wavefront OBJ and GEO</b>
    are always converted <b>to X3D</b>.</p>

  <li><p>Formats <b>Inventor, VRML 1.0, VRML 2.0, X3D</b>
    can be saved back <b>to their original format</b>.
    In this case, view3dscene is simply a "pretty-printer",
    exactly preserving all the information inside the file,
    only reformatting your content and removing the comments.

  <li><p><b>VRML 2.0</b> can be also converted <b>to X3D</b>.

    <p>Conversion from VRML 2.0 to X3D is mostly trivial.
    There are some different keywords between VRML 2 and X3D,
    <!--(and only X3D has XML encoding),-->
    but generally X3D is simply a superset of everything that VRML 2 has.
    The only slightly more involved conversion is done for NURBS nodes
    (<code>NurbsCurve</code>, <code>NurbsSurface</code> in VRML 2,
    <code>NurbsPatchSurface</code> in X3D, <code>NurbsPositionInterpolator</code>)
    as <a href="http://castle-engine.sourceforge.net/x3d_implementation_nurbs.php#section_vrml2_support">these nodes are not compatible between VRML 2 and X3D</a>.
</ul>

<p>You can also change the X3D encoding (from classic to XML or the other way
around). Changing encoding is a lossless operation,
as the same nodes graph can be exactly expressed in both encodings.</p>

<p>All these conversions can be also performed in batch mode
by command-line options described below. You can either use view3dscene with <code>--write</code>
option, or you can use separate binary <code>tovrmlx3d</code>.
Separate <code>tovrmlx3d</code> may be sometimes more desirable,
as it's smaller, not linked with any GUI libraries (so it will work
even on a stripped-down system) and has simpler command-line options
(as it's purpose is only to convert).</p>

<p>Examples:</p>
<pre>
# Convert Collada to X3D
view3dscene input.dae --write &gt; output.x3dv
# Same as above, but by tovrmlx3d binary
tovrmlx3d   input.dae         &gt; output.x3dv

# Convert VRML 2.0 to X3D in classic encoding.
# You could add --encoding=classic, but it's not needed
# (it is the default anyway).
view3dscene input.wrl --write --write-force-x3d &gt; output.x3dv
# Same as above, but by tovrmlx3d binary
tovrmlx3d   input.wrl               --force-x3d &gt; output.x3dv

# Convert VRML 2.0 to X3D in XML encoding.
# You could add --[write-]force-x3d, but it's not needed
# (it is implied by XML encoding anyway).
view3dscene input.wrl --write --write-encoding=xml &gt; output.x3d
# Same as above, but by tovrmlx3d binary
tovrmlx3d   input.wrl               --encoding=xml &gt; output.x3d
</pre>

<p>Detailed docs of <code>view3dscene</code> command-line options for converting:</p>

<dl class="params_list">
  <dt>--write</dt>
  <dd><p>Do not open any window,
    only write the 3D model to the standard output as VRML/X3D and exit.
    Other <code>--write-xxx</code> options affect the generated output.
    Model will also be processed by <code>--scene-change-*</code> options,
    if specified (see their docs lower on this page).
  </dd>

  <dt>--write-encoding classic|xml</dt>
  <dd><p>Choose encoding of the output file. By default, we use classic encoding.</p>

    <p>This option is meaningful only when <code>--write</code> option is also used.</p>
  </dd>

  <dt>--write-force-x3d</dt>
  <dd><p>Force output to be an X3D file. This is really useful only
    when input model is VRML 2.0.

    <p>Conversion to X3D is also automatically forced (no need to specify it explicitly
    by this option) if the chosen encoding is XML
    (that is, you used <code>--write-encoding=xml</code>).
    That's because only X3D supports XML encoding.</p>

    <p>Summarizing, you only need
    to use this option when you want to convert VRML 2 to X3D in classic encoding.

    <p>When this is used on VRML 1.0 or Inventor models, we'll also
    convert parts of them to X3D. But the result is not really useful:
    you will get a file encoded using X3D keywords, but using
    VRML 1.0/Inventor node names. Real conversion from VRML 1.0/Inventor
    to X3D is not implemented (yet).

    <p>This has no effect when used on 3D models that are already X3D,
    or that can be only output as X3D (Collada, 3DS, etc.).

    <p>This option is meaningful only when <code>--write</code> option is also used.</p>
  </dd>
</dl>

<p><code>tovrmlx3d</code> has analogous options for converting,
but without the <code>write-</code> prefix (as <code>tovrmlx3d</code>
is only useful for converting). More precisely:

<ul>
  <li><code>tovrmlx3d</code> always reads input 3D model (from filename given on a command-line),
    and outputs it on standard output as VRML/X3D.
  <li><code>--encoding=classic|xml</code> instructs to use given encoding.
    See <code>--write-encoding=classic|xml</code> docs above.
  <li><code>--force-x3d</code> instructs to force X3D conversion.
    See <code>--write-force-x3d</code> docs above.
</ul>

<?php section(false); ?>

<dl class="params_list">
  <dt>--hide-menu
  <dd><p>Hide the top menubar. Useful for full-screen presentations.

  <dt>--anti-alias AMOUNT</dt>
  <dd><p>Use full-screen anti-aliasing. You can also configure it from
    the menu <i>File -&gt; Startup Preferences -&gt; Anti aliasing</i>.
    Using this command-line option is mainly useful together with
    <code>--screenshot</code> option.</p>

    <p>Argument <code>AMOUNT</code>
    is an integer &gt;= 0. Exact 0 means "no anti-aliasing", this is the default.
    Each successive integer generally makes method one step better.
    But also more demanding &mdash; program may run slower
    (if your graphic card cannot provide context with sufficient number of samples
    needed for multi-sampling). See <i>Anti aliasing</i> in interactive mode
    for the meaning of <code>AMOUNT</code> values.
    Currently, highest value is 4. So <code>AMOUNT</code> numbers above 4 are
    exactly the same as 4.</p>

    <p>There is no guarantee what specific values of <code>AMOUNT</code> exactly
    mean, as this depends on your graphic card capabilities. The graphic cards
    themselves don't provide methods to reliably set some specific FSAA method
    (only hints, like <code>glHint(GL_MULTISAMPLE_FILTER_HINT_NV, ...)</code>)
    since the general idea is that better GPU models may provide the same or even
    better results using different methods. From your (user) point of view,
    you can test each method and just decide which looks best and isn't too slow
    on your 3D model and graphic card.</p></dd>

  <dt>--viewpoint VIEWPOINT-NAME</dt>
  <dd>
  <p>Specifies the name or a number of the viewpoint that will be bound (used) when the scene is loaded.</p>

  <p>By default, when this option is not used, we follow VRML/X3D standard and use the first viewpoint found in the file (but not in the inlined files). Of course you can always add nodes to the scene to trigger binding other viewpoints at the beginning (for example, add <code>ProximitySensor</code> with very large size that sends the <code>enter</code> event to the <code>set_bind</code> of chosen viewpoint). Or you can just exchange the order of viewpoint nodes. But sometimes it's not comfortable to edit the scene. Especially if you want to use the <code>--screenshot</code> options to capture a scene, it's useful to be able to choose a viewpoint by this command-line option.</p>

  <p>If you use this option: when the given <code>VIEWPOINT-NAME</code> is a number, it is treated as the index of viewpoint to be used (0 means the first viewpoint, 1 means the 2nd viewpoint and so on). Otherwise, <code>VIEWPOINT-NAME</code> is treated as a node name (node name is given by <code>"DEF Xxx"</code> in VRML/X3D, and it cannot start with a digit, so this is unambigous).</p>

  <p>In interactive mode, remember that you don't need this option &mdash; instead you can use comfortable <i>Navigation -&gt; Viewpoints</i> menu.</p>
  </dd>
</dl>

<p>As usual all
<?php echo a_href_page("standard options understood by OpenGL programs", "opengl_options") ?>,
<?php echo a_href_page('standard options understood by OpenAL (3D sound) programs','openal'); ?>,
<?php echo a_href_page('standard options understood by all my programs','common_options'); ?>
 are also allowed. Run with command-line <code>--help</code> to get full list.

<?php section(false); ?>

<dl class="params_list">
  <dt>--scene-change-no-normals<br>
      --scene-change-no-solid-objects<br>
      --scene-change-no-convex-faces
  <dd><p>Using one of these options changes the scene before it
    is displayed (or saved to VRML/X3D, if you used <code>--write</code>
    option). These options are useful when you suspect that some
    of the informations in scene file are incorrect.

    <p>These options change only the scene which filename was specified
    at command-line. Later scenes (that you open using "Open"
    menu item) are not affected by these options.
    Instead, you can use "Edit" menu commands to perform any
    of these scene changes at any time.
    Really, these command-line options are usable mostly
    when you're using parameter <code>--write</code>.

    <p>Below is the detailed description of what each
    scene change does. This is also a documentation what
    corresponding command in "Edit" menu of view3dscene does.

    <ul>
      <li><p><code>--scene-change-no-normals</code> :
        <p><b>Scene change:</b> Clear <code>normal</code> and <code>normalIndex</code> fields,
          remove <code>Normal</code> and VRML 1.0 <code>NormalBinding</code> nodes.
        <p><b>Effect:</b> view3dscene will always calculate by itself
          normal vectors. Useful when you suspect that normals recorded
          in scene file are incorrect (incorrectly oriented, incorrectly
          smoothed etc.)

      <li><p><code>--scene-change-no-solid-objects</code> :
        <p><b>Scene change:</b> For VRML 1.0, in all <code>ShapeHints</code> nodes
          we will set <code>shapeType</code> to <code>UNKNOWN_SHAPE_TYPE</code>.
          <code>UNKNOWN_SHAPE_TYPE</code> is the default value of this field,
          so the purpose of this modification is to cancel <code>SOLID</code>
          values for this field.
          For VRML &gt;= 2.0, all <code>solid</code> fields are set to <code>FALSE</code>
          (on all geometric nodes, like <code>IndexedFaceSet</code>,
          actually all <code>X3DComposedGeometryNode</code>, <code>Extrusion</code>, etc.).
        <p><b>Effect:</b> program will not use <i>back-face culling</i>
          optimization. This optimization often saves us time because we don't
          have to render faces that would be seen from "inside" if these
          faces are part of some solid object. Unfortunately, many VRML
          models have objects incorrectly marked as solid. There are also
          some scenes that were prepared for some special viewing (e.g. as game
          levels) and so some amount of "cheating" to optimize these scenes
          was allowed, e.g. to mark some non-solid objects as solid.

          <p>To view such
          models properly you have to tell view3dscene (using this command-line
          option) that such objects are not really solid.

      <li><p><code>--scene-change-no-convex-faces</code> :
        <p><b>Scene change:</b> For VRML 1.0, in all <code>ShapeHints</code> nodes
          we will set  <code>faceType</code> to <code>UNKNOWN_FACE_TYPE</code>.
          Moreover we will wrap whole scene in <code>Group</code> node and we
          will add at the beginning node
          <pre>ShapeHints { faceType UNKNOWN_FACE_TYPE }</pre>
          For VRML &gt;= 2.0, all <code>convex</code> fields are set to <code>FALSE</code>.
        <p><b>Effect:</b> All <code>IndexedFaceSet</code>
          and <code>Extrusion</code> faces will be treated
          as potentially non-convex. This means that we will load the scene
          a little longer but all faces will be correctly interpreted
          and displayed. It's useful when you suspect that some scene faces
          are non-convex but it's not correctly marked in the scene
          by VRML author.
    </ul>

    <p>Example: I have here some model <code>helicopter.wrl</code> that looks
    incorrectly when it is displayed because all parts of model are marked
    as SOLID while they are not solid. So to view this model correctly
    I can use command<br>
    <code>&nbsp;&nbsp;view3dscene --scene-change-no-solid-objects helicopter.wrl</code><br>
    I can also correct this model once using command<br>
    <code>&nbsp;&nbsp;view3dscene --scene-change-no-solid-objects helicopter.wrl
      --write &gt; helicopter-corrected.wrl</code>.

    <p><i>Deprecated:</i> I don't think the <code>--scene-change-*</code>
    options are useful.
    Doing this operation interactively is sometimes useful (to check bad models),
    doing it from command-line probably not (you better fix your exporter).
    Please report if you have a good reason to keep this working.

  <dt>--camera-radius &lt;float&gt;
  <dd><p>When you move in the scene with collision
    detection, the "user" is treated as a colliding sphere with given radius.
    Default radius of this sphere is the average size of scene bounding box
    divided by 100.
    Using this command-line option, you can set the radius of this sphere
    to any value (greater than 0). This can be very useful, but be careful:
    too large radius will make moving (with collision detection turned on)
    impossible (because every possible move will produce a collision).
    Too little radius may produce precision-errors in depth-buffer
    (this can lead to some strange display artifacts).

    <p><i>Deprecated:</i> instead of using this option,
    consider adding/editing a <code>NavigationInfo</code> node in your scene
    (camera radius is taken from the first float on <code>NavigationInfo.avatarSize</code>).
    Editing your scene (or creating a VRML/X3D file that includes the original
    scene, by <code>Inline</code> node, and only adds some customization)
    is much more flexible.

  <dt>--navigation EXAMINE|WALK|FLY|NONE...
  <dd><p>Set initial navigation type. Default is <code>EXAMINE</code>.
    This can be overridden in particular VRML/X3D scene by using the
    <a href="http://www.web3d.org/x3d/specifications/ISO-IEC-19775-1.2-X3D-AbstractSpecification/Part01/components/navigation.html#NavigationInfo">NavigationInfo</a>
    node. Valid values for this option are the navigation type names
    for VRML/X3D <code>NavigationInfo.type</code>, see link above.</p>

    <p>You can always change navigation mode later, while the program is running:
    use the menu <i>Navigation</i>.</p>

    <p><i>Deprecated:</i> instead of using this option,
    consider adding/editing a <code>NavigationInfo</code> node in your scene.
    Editing your scene (or creating a VRML/X3D file that includes the original
    scene, by <code>Inline</code> node, and only adds some customization)
    is much more flexible.

  <dt>--detail-quadric-slices &lt;integer&gt;<br>
      --detail-quadric-stacks &lt;integer&gt;<br>
      --detail-rect-divisions &lt;integer&gt;
  <dd><p>
    These options control triangulating. Two <code>--detail-quadric-xxx</code>
    options control triangulating of spheres, cones and cylinders:
    how many <i>slices</i> (like slices of a pizza)
    and how many <i>stacks</i> (like stacks of a tower) to create.
    The 3rd option, <code>--detail-rect-divisions</code>, says how
    we triangulate faces of cubes. It's best to test how your models
    look in <i>wireframe</i> mode to see how these options work.

    <p>Note that my programs do two different variants of triangulation,
    and they automatically decide which variant to use in each case:

    <ol>
      <li>Normal triangulation, that is intended to improve the
        approximation of quadrics as triangle meshes.
        This is used for collision detection and for ray-tracer.

      <li>The so-called <i>over-triangulation</i> (it's my term,
        used in various places in my code and documentation and
        printed messages), that is intended to improve the effect
        of Gouraud shading. This is used when rendering models with OpenGL.

        <p>In this variant we do some more triangulation than in
        "normal" triangulation. E.g. in normal triangulation
        we don't divide cones and cylinders into stacks,
        and we don't divide cube faces (because this doesn't give
        any better approximation of an object). But when
        <i>over-triangulating</i>, we do such dividing, because
        it improves how objects look with OpenGL shading.
    </ol>

    <p><i>Deprecated:</i> instead of using this option,
    consider adding/editing <?php echo a_href_page_hashlink(
    'a <code>KambiTriangulation</code> node in your scene',
    'x3d_extensions', 'section_ext_kambi_triangulation'); ?>.
</dl>

<?php section(); ?>

<?php echo depends_ul(array(
  DEPENDS_OPENGL,
  DEPENDS_LIBPNG_AND_ZLIB,
  DEPENDS_UNIX_CASTLE_WINDOW_GTK_2,
  SUGGESTS_OPENAL_VORBISFILE,
  DEPENDS_MACOSX)); ?>

<p>To play movies (in VRML/X3D <code>MovieTexture</code> nodes) and
to record movies (by <code>--screenshot-range</code> option)
you have to install <a href="http://ffmpeg.mplayerhq.hu/">ffmpeg</a>
and make sure it's available on $PATH.
<ul>
  <li><i>Linux and FreeBSD</i> users should find <code>ffmpeg</code> package
    suitable for their distribution,
  <li><i>Mac OS X</i> users can install <code>ffmpeg</code> using
    <a href="http://www.finkproject.org/">fink</a> (<a href="http://pdb.finkproject.org/pdb/package.php/ffmpeg">in stable currently, although source-only</a>),
  <li>finally <i>Windows</i> users may try these
    <a href="http://ffmpeg.zeranoe.com/builds/">FFmpeg Windows Builds</a>
    (<a href="http://ffmpeg.zeranoe.com/">see here for more information about
    ffmpeg on Windows</a>). Remember to add bin/ directory to your $PATH after unpacking.
</ul>

<p>Also <code>convert</code> program from
<a href="http://www.imagemagick.org/">ImageMagick</a>
package must be available on $PATH for some miscellaneous image formats to work.
See <?php echo a_href_page("glViewImage documentation", "glviewimage"); ?>
 for a list of image formats handled.

<?php
  castle_footer();
?>
