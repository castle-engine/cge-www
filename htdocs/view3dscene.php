<?php
  require_once 'vrmlengine_functions.php';

  vrmlengine_header("view3dscene",
    'view3dscene is a VRML / X3D browser, also a viewer for other 3D models. ' .
    'Supported formats are X3D, VRML (1.0 and 2.0 (aka VRML 97)), 3DS, MD3, Wavefront OBJ and Collada scenes. ' .
    'Can do collision detection. ' .
    'Can be used as command-line converter from Collada, 3DS, OBJ, MD3 to VRML. ' .
    'Has built-in ray-tracer. Rendering uses OpenGL. ' .
    'Free software. For Linux, FreeBSD, Mac OS X and Windows.');

  function section($make_hr = true)
  {
    if ($make_hr)
      echo "<hr class=\"ruler_between_sections\">";
    global $toc;
    echo $toc->html_section();
  }
?>

<div style="float: right; margin: 1em;">
<a class="FlattrButton" style="display:none;"
href="http://vrmlengine.sourceforge.net/"></a>
</div>

<?php
  echo pretty_heading("view3dscene", VERSION_VIEW3DSCENE);
  echo vrmlengine_thumbs(array(
    array('filename' => 'view3dscene_2.0.0_screen_demo.png', 'titlealt' => 'Screenshot from &quot;view3dscene&quot;'),
    array('filename' => 'view3dscene_screen_demo_1.png', 'titlealt' => 'Screenshot from &quot;view3dscene&quot;'),
    array('filename' => 'view3dscene_screen_demo_2.png', 'titlealt' => 'Screenshot from &quot;view3dscene&quot;'),
    array('filename' => 'view3dscene_screen_demo_3.png', 'titlealt' => 'Screenshot from &quot;view3dscene&quot;'),
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
      new TocItem('Controlling program with keys &amp; mouse', 'keys'),
      new TocItem('Command-line options', 'command_line_options'),
      new TocItem('Capturing screenshots and movies of 3D scenes and animations', 'screenshot', 1),
      new TocItem('Converting to VRML/X3D', 'converting', 1),
      new TocItem('Other options', 'other_options', 1),
      new TocItem(DEPENDS, 'depends'),
      new TocItem('Freshmeat entry', 'freshmeat'),
    )
  );
  $toc->echo_numbers = true;
  echo $toc->html_toc();
?>

<?php section(false); ?>

<?php echo_standard_program_download('view3dscene', 'view3dscene',
  VERSION_VIEW3DSCENE, $std_releases_post_1_8_0); ?>

<p>No installation is required. Just download and unpack these archives wherever
you want, and run the <tt>view3dscene</tt> program inside.
Included is also the <tt>tovrmlx3d</tt> program,
useful for <a href="#section_converting">batch converting 3D models to VRML/X3D</a>.
The documentation (this web page) is also included inside, for offline viewing
(open the <tt>documentation/view3dscene.html</tt> file).</p>

<p><?php echo SOURCES_OF_THIS_PROG_ARE_AVAIL; ?></p>

<p><i>Demo scenes</i>: our
<?php echo a_href_page("VRML/X3D demo models", "demo_models"); ?>
 contains a lot of interesting models,
you can open them all with <tt>view3dscene</tt>.</p>

<p>If you want to hear 3D sound, you should also install
<?php echo a_href_page('OpenAL', 'openal'); ?> and
<a href="http://xiph.org/vorbis/">OggVorbis (VorbisFile and dependencies)</a>
libraries.
For Windows, they are already included in the zip file, so you don't
have to do anything. For Linux, you should install them using your
package managar. For Mac OS X, <?php echo a_href_page('OpenAL is already
preinstalled and you can get VorbisFile from fink', 'macosx_requirements'); ?>.</p>

<?php section(false); ?>

<p>If you use GNOME (or other desktops following
<a href="http://freedesktop.org/">freedesktop.org</a> specifications),
you can optionally install also view3dscene menu item
(will be placed in the <i>Graphics</i> menu category), with a nice icon,
and associate it with appropriate 3D model types.</p>

<pre class="bordered_code">
# Place view3dscene on $PATH, for example like this:
sudo ln -s /usr/local/bin/view3dscene view3dscene

# Install menu items, icons, mime types:
cd desktop/
./install.sh
</pre>

<p>You may need to logout and login again to your GNOME/desktop
session for all programs to catch up (alternatively, you can do
<tt>killall gnome-panel &amp;&amp; killall nautilus</tt>
but this is obviously somewhat brutal method).</p>

<p>If you use GNOME file manager Nautilus there's
one more cool thing you can do: use
view3dscene to <b>generate on-the-fly thumbnails of 3D models</b>
in the viewed directory. Assuming that view3dscene is on the $PATH
and you already did previous <tt>./install.sh</tt>, you can run:</p>

<pre class="bordered_code">
./install_thumbnailer.sh
</pre>

<p><!--inside the <tt>desktop</tt> directory.-->
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
    <tt>.wrl</tt>, <tt>.wrz</tt> and <tt>.wrl.gz</tt>.
    For X3D (we support fully both
    XML and classic encoding) extensions are <tt>.x3d</tt>,
    <tt>.x3dz</tt>, <tt>.x3d.gz</tt>
    and <tt>.x3dv</tt>, <tt>.x3dvz</tt>, <tt>.x3dv.gz</tt>.</p>

    <p>Almost complete VRML&nbsp;1.0 support is done.
    VRML&nbsp;2.0 (aka VRML&nbsp;97) and X3D support is also quite advanced,
    a lot of nodes and features are implemented (including
    advanced texturing and GLSL shaders,
    <tt>PROTO</tt> and <tt>EXTERNPROTO</tt> support,
    events mechanism with routes, sensors and interpolators).

    <!-- Among things that work are embedded textures,
    semi-transparent materials and semi-transparent textures,
    automatic normals smoothing (based on <tt>creaseAngle</tt>),
    triangulating non-convex faces, understanding camera nodes,
    WWWInline handling, text rendering and more. -->

    See <?php echo a_href_page('VRML implementation status',
      'vrml_implementation_status'); ?> for detailed list of supported
    features. See also <?php echo a_href_page('Kambi extensions to VRML',
    'kambi_vrml_extensions'); ?>, our <?php
      echo a_href_page('VRML/X3D demo models', 'demo_models'); ?>,
    and finally <a href="http://web3d.org/x3d/specifications/">
    the official X3D specifications</a>.

  <li><p><b><?php echo a_href_page(
    "Kanim (Kambi VRML engine animations)", 'kanim_format'); ?></b> format
    is handled, animation is played.</p>

  <li><p><b><a href="http://www.khronos.org/collada/">Collada</a></b>
    (<tt>.dae</tt> extension). All existing Collada versions (in particular,
    we support both 1.3.x and 1.4.x versions) are handled.</p>

    <p>As far as features go, we currently support only geometry with materials.
    It was tested on various Collada files, in particular on Collada files
    generated by Blender (both 1.3.1 and 1.4 exporters, some sample Collada
    models used are in our <?php
      echo a_href_page('demo models', 'demo_models'); ?>
    in <tt>collada/</tt> subdir) and on <i>Basic Collada samples</i> from
    <a href="http://www.collada.org/">collada.org</a>.</p>

  <li><p>Also many
    <a href="http://oss.sgi.com/projects/inventor/"><b>OpenInventor's</b></a>
    1.0 ASCII files (<tt>.iv</tt> extension) are handled.
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
    associated <tt>xxx_default.skin</tt> file), <i>animation is also read
    and played</i>.</p>

  <li><p><b>Wavefront OBJ files</b>. Most useful things are supported:
    geometry (with texture coords, normal vectors), materials
    (colors, opacity, texture filenames).</p>

  <li><p><b><a href="http://local.wasp.uwa.edu.au/~pbourke/dataformats/geo/">Videoscape
    GEO</a></b> (<tt>.geo</tt> extension).
    Very basic support for this very old 3D format.
</ul>

<p>Among many features are:
<ul>
  <li>Various navigation modes are available:
    <tt>Examine</tt> (easily rotate and move the whole model),
    <tt>Walk</tt> (walk like in FPS games,
    with collision detection, gravity and related features available),
    <tt>Fly</tt> (similar to <tt>Walk</tt> but without gravity).
  <li>Conversion of 3DS, MD3, Wavefront OBJ, Collada and GEO files to X3D.
  <li>You can convert between X3D classic and XML encodings (in both directions),
    and you can convert from VRML 2 to X3D.
    You can also use view3dscene as a "pretty-printer",
    just open and save any VRML/X3D file without any version conversion.

    <p>Command-line options to convert in batch mode (<tt>--write</tt>)
    are available in view3dscene. Special minimized binary
    <tt>tovrmlx3d</tt> (useful to install on servers without GUI libraries
    available) is also included in view3dscene archive.
  <li>A wealth of Kambi engine's rendering features are available,
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

<p><b>Controls in <tt>Examine</tt> navigation mode :</b>
<table border="1" class="key_list">
  <tr><th colspan="2">Mouse:</th></tr>
  <tr><td>Rotate</td> <td>Left mouse dragging</td>                           </tr>
  <tr><td>Move</td>   <td>Middle mouse dragging (or Left mouse + Shift)</td> </tr>
  <tr><td>Zoom</td>   <td>Right mouse dragging (or Left mouse + Ctrl)</td>   </tr>

  <tr><th colspan="2">Keys:</th></tr>
  <tr><td>Rotate</td>        <td>Arrows / PageUp / PageDown</td>        </tr>
  <tr><td>Stop rotating</td> <td>Space</td>                             </tr>
  <tr><td>Move</td>          <td>Ctrl + Arrows / PageUp / PageDown</td> </tr>
  <tr><td>Scale</td>         <td>+ / -</td>                             </tr>
  <tr><td>Restore default transformation</td>           <td>Home</td>   </tr>
</table>

<p><b>Controls in <tt>Walk / Fly</tt> navigation mode :</b><br>
<table border="1" class="key_list">

  <tr><th colspan="2">Basic:</th></tr>

  <tr><td>Forward / backward</td>    <td>Up / Down</td>            </tr>
  <tr><td>Rotate</td>                <td>Left / Right</td>         </tr>
  <tr><td>Raise / bow your head</td> <td>PageUp / PageDown</td>    </tr>
  <tr><td>Restore head raise to initial position (neutralize any effect of
          PageUp / PageDown)</td>    <td>Home</td>                 </tr>
  <tr><td>Fly up / down</td>         <td>Insert / Delete</td>      </tr>
  <tr><td>Move left / right</td>     <td>Comma / Period</td>       </tr>
  <tr><td>Jump / crouch (only when <i>Gravity</i> works, in <tt style="background-color: transparent;">Walk</tt> mode)</td>
      <td>A / Z</td> </tr>

  <tr><td colspan="2" style="text-align: left"><b>Turn <i>Mouse Look</i> <i>On</i>
    (Ctrl+M) to comfortably look around
    by moving the mouse.</b> In the "Mouse Look" mode, the keys for strafe and rotations swap their
    meaning:
    <ul style="margin: 0;">
      <li>Left / Right keys move left / right</li>
      <li>Comma / Period rotate</li>
    </ul></td></tr>

  <tr><th colspan="2">Additional controls:</th></tr>

  <tr><td>Increase / decrease moving speed (has effect on keys
        Up / Down, Insert / Delete, Comma / Period)</td> <td>+ / -</td></tr>
  <tr><td>Increase / decrease avatar height (preferred camera height above the ground)</td>
      <td>Ctrl + Insert/Delete</td></tr>
  <tr><td>Rotate <i>slower</i> (useful when you want to set up camera
         very precisely, e.g. to use this camera setting to render a scene
         image using ray-tracer)</td>
       <td>Ctrl + Left / Right</td> </tr>
  <tr><td>Raise / bow your head <i>slower</i></td>
      <td>Ctrl + PageUp / PageDown</td></tr>
  <tr><td>Pick a point, selecting triangle and object</td>
      <td>Right mouse click</td>  </tr>
</table>

<p><i>Left mouse button</i> is also universally used for interacting
with the VRML/X3D world. When the cursor turns into a grabbing hand
you know you can click or drag on this object. This is realized
by the <?php echo a_href_page('VRML/X3D pointing-device sensors',
'vrml_implementation_pointingdevicesensor'); ?>.</p>

<p>There are a lot of other keys that work independent of current navigation
mode. You can see them all by exploring menus, and looking at
the key shortcuts.</p>

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
<tt>--screenshot</tt> option with <tt>TIME</tt>
set to anything (like zero) and not worry about anything else.

<p><i>For animations</i>, more possibilities are available. You can capture
any frames of the animation by using many <tt>--screenshot</tt> options.
You can also capture a movie by <tt>--screenshot-range</tt>
(as a series of images or, if
<a href="http://ffmpeg.mplayerhq.hu/">ffmpeg</a> is installed and
available on $PATH, even directly to a single movie file).
The biggest advantage of recording movie this way
is that the movie is guaranteed to be captured with stable number
of frames per second. This is different than using
some independent programs to capture OpenGL output, like
the fine <a href="http://nullkey.ath.cx/projects/glc">GLC</a>
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
  <li><p>First of all, after all the <tt>--screenshot</tt>
    and <tt>--screenshot-range</tt> options are processed,
    view3dscene exits. So they work in "batch mode".

  <li><p>The <tt>--screenshot  TIME  FILE-NAME</tt> simply saves
    the screenshot at time <tt>TIME</tt> to an image file <tt>FILE-NAME</tt>.

    <p>Image format is guessed from <tt>FILE-NAME</tt> extension,
    see <?php echo a_href_page("glViewImage", "glviewimage") ?>
    for detailed list of image formats that we can handle.
    In short, we handle many popular image formats, like PNG and JPG,
    and these are what usually you want to use.

  <li><p>The <tt>--screenshot-range  TIME-BEGIN  TIME-STEP  FRAMES-COUNT  FILE-NAME</tt>
    option takes <tt>FRAMES-COUNT</tt> screenshots. The first screenshot
    is at time <tt>TIME-BEGIN</tt>, second screenshot is at
    <tt>TIME-BEGIN + TIME-STEP</tt>, next one is at
    <tt>TIME-BEGIN + 2 * TIME-STEP</tt>... you get the idea.

    <p>The <tt>FILE-NAME</tt> is either
    <ol>
      <li><p>A movie filename. This must have
        a recognized movie extension, currently this means

<pre>
  .avi
  .mpg
  .dvd
  .ogg
  .mov
  .swf
</pre>

        <p>Availability of all these video formats may depend on installed ffmpeg
        codecs. If in doubt, avi seems to be most reliable and plays everywhere.
        If I missed some possible movie file extension, please report.
        <a href="http://ffmpeg.mplayerhq.hu/">ffmpeg</a>
        must be installed and available on $PATH for this to work.

      <li><p><tt>FILE-NAME</tt> may also be a pattern to generate
        names of images to save, like <tt>image%d.png</tt>.
        Details about using filename patterns are below (although you
        can probably already guess how it works :) ).
    </ol>

  <li><p>All filenames for both screenshot options may specify a pattern
    instead of an actual filename. A pattern is simply a filename
    with sequence <tt>%d</tt> inside, when capturing <tt>%d</tt>
    will be replaced by current screenshot number.
    For capturing a series of images
    by <tt>--screenshot-range</tt> it's even required to specify
    a pattern (since capturing
    a number of images to a single image file has no point...). But
    it's also allowed in all other cases, even a movie filename
    may also be a pattern with <tt>%d</tt> sequence,
    in case you want to use multiple <tt>--screenshot-range</tt>
    options to get multiple auto-named movies.

    <p>The precise description how <tt>%d</tt> works:
    All <tt>--screenshot</tt>
    and <tt>--screenshot-range</tt> options are processed in order.
    When a filename with pattern <tt>%d</tt> is found, we replace all
    <tt>%d</tt> occurrences in this filename with current counter value
    and increment the counter. For <tt>--screenshot-range</tt> with
    an image pattern, we do this for every single frame.
    The counter starts at 1.

    <p>You can specify a number between <tt>%</tt> and <tt>d</tt>,
    like <tt>%4d</tt>, to pad counter with zeros. For example, normal
    <tt>%d</tt> results in names like 1, 2, ..., 9, 10... But <tt>%4d</tt>
    results in names like 0001, 0002, ..., 0009, 0010, ...

    <p>To allow you do specify literal <tt>%</tt> character in filename
    reliably, you can write it twice: <tt>%%</tt>.
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

    <p>To get this as a sequence of images, just use <tt>output%d.png</tt>
    instead of <tt>output.avi</tt>.
  </li>

  <li><p>Example of more complicated use:

<pre>
  view3dscene my_model.kanim \
    --screenshot-range 0 0.04 50 output%d.avi \
    --screenshot-range 10 0.04 50 output%d.avi
</pre>

    <p>This generates two files: <tt>output1.avi</tt> with 2 second animation
    from 0.0 to 2.0 time, and <tt>output2.avi</tt> with 2 second animation
    from 10.0 to 12.0 time.
  </li>
</ul>

<p><b>Hints:</b></p>

<ul>
  <li><p>To control the look of your screenshot, you often want to
    use VRML nodes like <tt>Viewpoint</tt>, <tt>NavigationInfo</tt>,
    <tt>Background</tt>. For example, take a look at
    <a href="https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/rift/data/creatures/humanoid/screenshot_for_kambi_www/walk_1.wrl">this sample VRML file</a>.</p></li>

  <li><p>You can generate wanted <tt>Viewpoint</tt> node
    also by using view3dscene, just set your camera (in interactive mode)
    the way you like and use menu item
    <i>Console -> Print Current Camera Node</i>.</p>

  <li><p>To control the size of resulting screenshot, just use
    <tt>--geometry</tt> command-line parameter
    (documented at <?php echo a_href_page("standard options
    understood by our OpenGL programs", "opengl_options") ?>).
    For example, take a look at
    <a href="https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/rift/data/creatures/humanoid/screenshot_for_kambi_www/mk_screenshot_for_kambi_www.sh">mk_screenshot_for_kambi_www.sh</a>
    script.</p></li>

  <li><p>To make your screenshot look best, you may want to use anti-aliasing,
    see <tt>--anti-alias</tt> option below.</p></li>
</ul>

<p>Generally, you can take a look at (complex) example how to make
a screenshot from animation in
<a href="https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/rift/data/creatures/humanoid/screenshot_for_kambi_www/">screenshot_for_kambi_www/</a>
directory.</p>

<?php section(false); ?>

<p>In interactive mode, you can use view3dscene menu items
<i>File -&gt; Save As..</i> to save (converting if needed) all 3D model
formats to VRML/X3D.

<ul>
  <li><p>Formats <b>3DS, MD3, Wavefront OBJ, Collada and GEO</b>
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
    (<tt>NurbsCurve</tt>, <tt>NurbsSurface</tt> in VRML 2,
    <tt>NurbsPatchSurface</tt> in X3D, <tt>NurbsPositionInterpolator</tt>)
    as <a href="http://vrmlengine.sourceforge.net/vrml_implementation_nurbs.php#section_vrml2_support">these nodes are not compatible between VRML 2 and X3D</a>.
</ul>

<p>You can also change the X3D encoding (from classic to XML or the other way
around). Changing encoding is a lossless operation,
as the same nodes graph can be exactly expressed in both encodings.</p>

<p>All these convertions can be also performed in batch mode
by command-line options described below. You can either use view3dscene with <tt>--write</tt>
option, or you can use separate binary <tt>tovrmlx3d</tt>.
Separate <tt>tovrmlx3d</tt> may be sometimes more desirable,
as it's smaller, not linked with any GUI libraries (so it will work
even on a stripped-down system) and has simpler command-line options
(as it's purpose is only to convert).</p>

<p>Examples:</p>
<pre class="bordered_code">
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

<p>Detailed docs of <tt>view3dscene</tt> command-line options for converting:</p>

<dl class="params_list">
  <dt>--write</dt>
  <dd><p>Do not open any window,
    only write the 3D model to the standard output as VRML/X3D and exit.
    Other <tt>--write-xxx</tt> options affect the generated output.
    Model will also be processed by <tt>--scene-change-*</tt> options,
    if specified (see their docs lower on this page).
  </dd>

  <dt>--write-encoding classic|xml</dt>
  <dd><p>Choose encoding of the output file. By default, we use classic encoding.</p>

    <p>This option is meaningful only when <tt>--write</tt> option is also used.</p>
  </dd>

  <dt>--write-force-x3d</dt>
  <dd><p>Force output to be an X3D file. This is really useful only
    when input model is VRML 2.0.

    <p>Conversion to X3D is also automatically forced (no need to specify it explicitly
    by this option) if the chosen encoding is XML
    (that is, you used <tt>--write-encoding=xml</tt>).
    That's because only X3D supports XML encoding.</p>

    <p>Summarizing, you only need
    to use this option when you want to convert VRML 2 to X3D in classic encoding.

    <p>When this is used on VRML 1.0 or Inventor models, we'll also
    convert parts of them to X3D. But the result is not really useful:
    you will get a file encoded using X3D keywords, but using
    VRML 1.0/Inventor node names. Real convertion from VRML 1.0/Inventor
    to X3D is not implemented (yet).

    <p>This has no effect when used on 3D models that are already X3D,
    or that can be only output as X3D (3DS, MD3, Wavefront OBJ etc.).

    <p>This option is meaningful only when <tt>--write</tt> option is also used.</p>
  </dd>
</dl>

<p><tt>tovrmlx3d</tt> has analogous options for converting,
but without the <tt>write-</tt> prefix (as <tt>tovrmlx3d</tt>
is only useful for converting). More precisely:

<ul>
  <li><tt>tovrmlx3d</tt> always reads input 3D model (from filename given on a command-line),
    and outputs it on standard output as VRML/X3D.
  <li><tt>--encoding=classic|xml</tt> instructs to use given encoding.
    See <tt>--write-encoding=classic|xml</tt> docs above.
  <li><tt>--force-x3d</tt> instructs to force X3D convertion.
    See <tt>--write-force-x3d</tt> docs above.
</ul>

<?php section(false); ?>

<dl class="params_list">
  <dt>--anti-alias AMOUNT</dt>
  <dd><p>Use full-screen anti-aliasing. You can also configure it from
    the menu <i>File -&gt; Startup Preferences -&gt; Anti aliasing</i>.
    Using this command-line option is mainly useful together with
    <tt>--screenshot</tt> option.</p>

    <p>Argument <tt>AMOUNT</tt>
    is an integer &gt;= 0. Exact 0 means "no anti-aliasing", this is the default.
    Each successive integer generally makes method one step better.
    But also more demanding &mdash; program may run slower
    (if your graphic card cannot provide context with sufficient number of samples
    needed for multi-sampling). See <i>Anti aliasing</i> in interactive mode
    for the meaning of <tt>AMOUNT</tt> values.
    Currently, highest value is 4. So <tt>AMOUNT</tt> numbers above 4 are
    exactly the same as 4.</p>

    <p>There is no guarantee what specific values of <tt>AMOUNT</tt> exactly
    mean, as this depends on your graphic card capabilities. The graphic cards
    themselves don't provide methods to reliably set some specific FSAA method
    (only hints, like <tt>glHint(GL_MULTISAMPLE_FILTER_HINT_NV, ...)</tt>)
    since the general idea is that better GPU models may provide the same or even
    better results using different methods. From your (user) point of view,
    you can test each method and just decide which looks best and isn't too slow
    on your 3D model and graphic card.</p></dd>

  <dt>--scene-change-no-normals<br>
      --scene-change-no-solid-objects<br>
      --scene-change-no-convex-faces
  <dd><p>Using one of these options changes the scene before it
    is displayed (or saved to VRML/X3D, if you used <tt>--write</tt>
    option). These options are useful when you suspect that some
    of the informations in scene file are incorrect.

    <p>These options change only the scene which filename was specified
    at command-line. Later scenes (that you open using "Open"
    menu item) are not affected by these options.
    Instead, you can use "Edit" menu commands to perform any
    of these scene changes at any time.
    Really, these command-line options are usable mostly
    when you're using parameter <tt>--write</tt>.

    <p>Below is the detailed description of what each
    scene change does. This is also a documentation what
    corresponding command in "Edit" menu of view3dscene does.

    <ul>
      <li><p><tt>--scene-change-no-normals</tt> :
        <p><b>Scene change:</b> For VRML 1.0,
          all <tt>Normal</tt> and <tt>NormalBinding</tt>
          nodes are deleted. Values of <tt>normalIndex</tt> field
          in <tt>IndexedFaceSet</tt> and <tt>IndexedTriangleMesh</tt> nodes
          are deleted.
          For VRML &gt;= 2.0, all <tt>normal</tt> fields are set to <tt>NULL</tt>.
        <p><b>Effect:</b> view3dscene will always calculate by itself
          normal vectors. Useful when you suspect that normals recorded
          in scene file are incorrect (incorrectly oriented, incorrectly
          smoothed etc.)

      <li><p><tt>--scene-change-no-solid-objects</tt> :
        <p><b>Scene change:</b> For VRML 1.0, in all <tt>ShapeHints</tt> nodes
          we will set <tt>shapeType</tt> to <tt>UNKNOWN_SHAPE_TYPE</tt>.
          <tt>UNKNOWN_SHAPE_TYPE</tt> is the default value of this field,
          so the purpose of this modification is to cancel <tt>SOLID</tt>
          values for this field.
          For VRML &gt;= 2.0, all <tt>solid</tt> fields are set to <tt>FALSE</tt>
          (on all geometric nodes, like <tt>IndexedFaceSet</tt>,
          actually all <tt>X3DComposedGeometryNode</tt>, <tt>Extrusion</tt>, etc.).
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

      <li><p><tt>--scene-change-no-convex-faces</tt> :
        <p><b>Scene change:</b> For VRML 1.0, in all <tt>ShapeHints</tt> nodes
          we will set  <tt>faceType</tt> to <tt>UNKNOWN_FACE_TYPE</tt>.
          Moreover we will wrap whole scene in <tt>Group</tt> node and we
          will add at the beginning node
          <pre>ShapeHints { faceType UNKNOWN_FACE_TYPE }</pre>
          For VRML &gt;= 2.0, all <tt>convex</tt> fields are set to <tt>FALSE</tt>.
        <p><b>Effect:</b> All <tt>IndexedFaceSet</tt>
          and <tt>Extrusion</tt> faces will be treated
          as potentially non-convex. This means that we will load the scene
          a little longer but all faces will be correctly interpreted
          and displayed. It's useful when you suspect that some scene faces
          are non-convex but it's not correctly marked in the scene
          by VRML author.
    </ul>

    <p>Example: I have here some model <tt>helicopter.wrl</tt> that looks
    incorrectly when it is displayed because all parts of model are marked
    as SOLID while they are not solid. So to view this model correctly
    I can use command<br>
    <tt>&nbsp;&nbsp;view3dscene --scene-change-no-solid-objects helicopter.wrl</tt><br>
    I can also correct this model once using command<br>
    <tt>&nbsp;&nbsp;view3dscene --scene-change-no-solid-objects helicopter.wrl
      --write &gt; helicopter-corrected.wrl</tt>.

    <p><i>Deprecated:</i> I don't think the <tt>--scene-change-*</tt>
    options are useful.
    Doing this operation interactively is sometimes useful (to check bad models),
    doing it from command-line probably not (you better fix your exporter).
    Please report if you have a good reason to keep this working.

  <dt>--navigation EXAMINE|WALK|FLY|NONE...
  <dd><p>Set initial navigation type. Default is <tt>EXAMINE</tt>.
    This can be overridden in particular VRML/X3D scene by using the
    <a href="http://www.web3d.org/x3d/specifications/ISO-IEC-19775-1.2-X3D-AbstractSpecification/Part01/components/navigation.html#NavigationInfo">NavigationInfo</a>
    node. Valid values for this option are the navigation type names
    for VRML/X3D <tt>NavigationInfo.type</tt>, see link above.</p>

    <p>You can always change navigation mode later, while the program is running:
    use the menu <i>Navigation</i>.</p>

    <p><i>Deprecated:</i> instead of using this option,
    consider adding/editing a <tt>NavigationInfo</tt> node in your scene.
    Editing your scene (or creating a VRML/X3D file that includes the original
    scene, by <tt>Inline</tt> node, and only adds some customization)
    is much more flexible.

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
    consider adding/editing a <tt>NavigationInfo</tt> node in your scene
    (camera radius is taken from the first float on <tt>NavigationInfo.avatarSize</tt>).
    Editing your scene (or creating a VRML/X3D file that includes the original
    scene, by <tt>Inline</tt> node, and only adds some customization)
    is much more flexible.

  <dt><a name="command_line_options_detail"></a>
      --detail-quadric-slices &lt;integer&gt;<br>
      --detail-quadric-stacks &lt;integer&gt;<br>
      --detail-rect-divisions &lt;integer&gt;
  <dd><p>
    These options control triangulating. Two <tt>--detail-quadric-xxx</tt>
    options control triangulating of spheres, cones and cylinders:
    how many <i>slices</i> (like slices of a pizza)
    and how many <i>stacks</i> (like stacks of a tower) to create.
    The 3rd option, <tt>--detail-rect-divisions</tt>, says how
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
    'a <tt>KambiTriangulation</tt> node in your scene',
    'kambi_vrml_extensions', 'section_ext_kambi_triangulation'); ?>.
</dl>

<p>As usual all
<?php echo a_href_page("standard options understood by OpenGL programs", "opengl_options") ?>,
<?php echo a_href_page('standard options understood by OpenAL (3D sound) programs','openal'); ?>,
<?php echo a_href_page('standard options understood by all my programs','common_options'); ?>
 are allowed. Run with command-line <tt>--help</tt> to get full list.

<?php section(); ?>

<?php echo depends_ul(array(
  DEPENDS_OPENGL,
  DEPENDS_LIBPNG_AND_ZLIB,
  DEPENDS_UNIX_GLWINDOW_GTK_2,
  SUGGESTS_OPENAL_VORBISFILE,
  DEPENDS_MACOSX)); ?>

<p>To play movies (in VRML/X3D <tt>MovieTexture</tt> nodes) and
to record movies (by <tt>--screenshot-range</tt> option)
you have to install <a href="http://ffmpeg.mplayerhq.hu/">ffmpeg</a>
and make sure it's available on $PATH.
<ul>
  <li><i>Linux and FreeBSD</i> users should find <tt>ffmpeg</tt> package
    suitable for their distribution,
  <li><i>Mac OS X</i> users can install <tt>ffmpeg</tt> using
    <a href="http://www.finkproject.org/">fink</a> (<a href="http://pdb.finkproject.org/pdb/package.php/ffmpeg">in stable currently, although source-only</a>),
  <li>finally <i>Windows</i> users may try these
    <a href="http://ffmpeg.arrozcru.org/builds/">unofficial FFmpeg Win32 Builds</a>
    (<a href="http://ffmpeg.arrozcru.org/">see here for more information about
    ffmpeg on Windows</a>). Remember to add bin/ directory to your $PATH after unpacking.
</ul>

<p>Also <tt>convert</tt> program from
<a href="http://www.imagemagick.org/">ImageMagick</a>
package must be available on $PATH for some image formats to work.
See <?php echo a_href_page("glViewImage documentation", "glviewimage"); ?>
 for a list of image formats handled.

<?php section(); ?>

<p>Here's a link to
<a href="http://freshmeat.net/projects/view3dscene/">view3dscene
entry on freshmeat</a>. You can use this e.g. to subscribe to new
releases, so that you will be automatically notified about new
releases of view3dscene.</p>

<?php
  vrmlengine_footer();
?>
