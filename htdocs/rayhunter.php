<?php
  require_once 'vrmlengine_functions.php';
  require_once 'raytr_gallery_funcs.php';

  common_header("rayhunter", LANG_EN,
    "rayhunter - simple raytracer. Classic ray tracer " .
    "and basic Monte Carlo path tracer implemented. Open-source, portable " .
    "(for Linux, FreeBSD, Mac OS X and Windows).");
  require_once "octree_consts.php";

  $toc = new TableOfContents(
    array(
      new TocItem('Download', 'download'),
      new TocItem('Required command-line options', 'required_options'),
      new TocItem('Optional command-line options', 'optional_options'),
      new TocItem('Some notes about 3d models handling by this ray-tracer', 'vrml_interpretation'),
      new TocItem('Advantages over using ray-tracer built in view3dscene', 'adv_over_view3dscene')
    )
  );
?>

<?php echo pretty_heading("rayhunter", VERSION_RAYHUNTER); ?>

<table align="right" class="table_with_thumbs_and_text" style="width: 110px">
  <tr><td><?php echo image_tag('graz-wlight-1-classic-filt'); ?></td></tr>
  <tr><td><?php echo image_tag('sibenik-wlight-1-classic-filt'); ?></td></tr>
  <tr><td><?php echo image_tag('forest'); ?></td></tr>
  <tr><td><?php echo image_tag('alien_two_mirrors_2'); ?></td></tr>
  <tr><td><?php echo image_tag('zupa-wlight-path'); ?></td></tr>
  <tr><td><?php echo image_tag('box-path-samp10x5-depth2-rroul0.5'); ?></td></tr>
  <tr><td><?php echo image_tag('graz-wlight-1-path-filt'); ?></td></tr>
  <tr><td>More sample images in
    <?php echo a_href_page("rayhunter gallery.","raytr_gallery"); ?></td></tr>
</table>

<p><tt>rayhunter</tt> is a command-line program that takes
a 3d model (given as VRML or 3DS file) and renders an image that would be visible
from given camera looking at given scene. Two ray-tracing algorithms
may be used: deterministic (classic Whitted-style ray-tracer)
and Monte Carlo path tracing.

<p>You can take a look at the
<?php echo a_href_page("gallery of images rendered using rayhunter",
"raytr_gallery"); ?> as a demonstration of rayhunter's capabilities.

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p><?php echo S_HERE_ARE_BINARIES; ?>
<?php echo_standard_program_download('rayhunter', 'rayhunter',
  VERSION_RAYHUNTER, $std_releases_post_1_8_0); ?>

<p><?php echo SOURCES_OF_THIS_PROG_ARE_AVAIL; ?>

<?php /* Too small:
  echo depends_par(array(
    DEPENDS_LIBPNG_AND_ZLIB,
    DEPENDS_MACOSX)); */ ?>

<?php echo $toc->html_section(); ?>

<p>Basic syntax to call <tt>rayhunter</tt> is

<p style="font-family: monospace">
  rayhunter classic &lt;recursion-depth&gt;
    &lt;image-width&gt; &lt;image-height&gt;
    &lt;input-filename&gt; &lt;output-filename&gt;

<p>or

<p style="font-family: monospace">
  rayhunter path &lt;recursion-depth&gt;
    &lt;non-primary-samples-count&gt; &lt;image-width&gt; &lt;image-height&gt;
    &lt;input-filename&gt; &lt;output-filename&gt;

<p>
<ul>
  <li><p>First option, <tt>classic</tt> or <tt>path</tt>,
    says which ray-tracing algorithm to use.

    <dl>
      <dt><tt>classic</tt></dt>

      <dd><p>This is normal Whitted-style deterministic ray-tracer.
        All normal VRML lights are handled (point, spot, directional,
        including the headlight).
        No area lights, so no soft shadows.
        Algorithm sends one primary ray for each pixel.
        Ray-tracing is recursive, where the ray casts on some
        surface we check rays to light sources and eventually
        we recursively check refracted ray (when <tt>Material</tt>
        has <tt>transparency</tt> &gt; 0) and reflected ray
        (when <tt>Material</tt> has <tt>mirror</tt> &gt; 0).
        Wikipedia has <a href="http://en.wikipedia.org/wiki/Ray-tracing">
        a nice article about ray-tracing</a> that describes
        Whitted-style ray-tracing.</dd>

      <dt><tt>path</tt></dt>

      <dd><p>This is path tracer. Every surface with non-zero
        <tt>emissiveColor</tt> is a light emitter. For each
        pixel many random paths are checked and final pixel color
        is the average color from all paths.

        <p>Actually <tt>rayhunter</tt> for every pixel
        checks <tt>&lt;primary-samples-count&gt;</tt> of primary
        rays, and then each primary ray that hits something splits into
        <tt>&lt;non-primary-samples-count&gt;</tt>.
        So in total we check <tt>&lt;primary-samples-count&gt;</tt> *
        <tt>&lt;non-primary-samples-count&gt;</tt> paths.
        This is a sensible optimization, because usually there
        is no need to take many <tt>&lt;primary-samples-count&gt;</tt>,
        since all primary rays hit more-or-less the same thing,
        since they have very similar direction.

        <p><tt>&lt;non-primary-samples-count&gt;</tt> is
        set using the 3rd required command-line option,
        and <tt>&lt;primary-samples-count&gt;</tt> is set
        using optional option <tt>--primary-samples-count</tt>
        (by default it's 1).
      </dd>
    </dl>

  <li><p><tt>&lt;recursion-depth&gt;</tt>

    <p>This is interpreted differently by different ray-tracing algorithms:
    <ul>
      <li>For <b>classic</b> ray-tracer this is <i>maximum</i> allowed
        recursion depth. If you will give here too small value,
        then some optical effects will not be visible.
        For normal scenes (without too complex mirror and transparent
        surfaces' setting) values 2 or 3 are appropriate.

	<p>0 means that each object will have only his own color.
        1 means that light rays can light the surface,
        and ray may be once reflected or refracted.
        Greater values allow rays to be reflected and/or refracted
        more times along the way.

      <li>For <b>path tracer</b> this is <i>minimal</i> path length.
        I.e. Russian-roulette will <i>not</i> be used to decide
        about the termination until the path will reach this length.
        (Of course, some paths may still be shorter than this
        minimal length, because the ray will not hit anything in the scene.)
        E.g. if you will set this to 3, and Russian-roulette parameter
        (see <tt>--r-roul-continue</tt> option) will be 0.5
        (the default value), then 1/2 of all paths will have length 3,
        1/4 of all paths will have length 4,
        1/8 of all paths will have length 5 etc.

	<p>Russian-roulette makes sure that the result
        is <i>unbiased</i>, i.e. the expected value is the correct result
        (i.e. the perfect beautiful realistic image).
        However, Russian-roulette introduces also a large variance,
        visible as a <i>noise</i> on the image.

	<p>That's where forcing some minimal path length helps.
        Sensible values for this are 1 or 2. Of course, the greater
        the better, but it will also slow down the rendering.
        0 means that Russian-roulette will always be used to decide
        about path termination (expect a lot of noise on the image!).
    </ul>

  <li><p><tt>&lt;non-primary-samples-count&gt;</tt>

    <p>Only for <i>path tracer</i>. Together with
    <tt>&lt;primary-samples-count&gt;</tt>, this specifies
    how many paths will be checked at each pixel.
    Various scenes may require different numbers here to look
    nice &mdash; you can start with 10, then 100, then even 1000.
    But beware &mdash; this value directly affects rendering speed.

  <li><p><tt>&lt;image-width&gt; &lt;image-height&gt;</tt>

    <p>Width and height of resulting image.

  <li><p><tt>&lt;input-filename&gt;</tt>

    <p>3d model filename.

    <p>Anything that my code can read (see
    <?php echo a_href_page("view3dscene docs", "view3dscene"); ?>)
    is accepted here, but actually rendering
    OBJ (Wavefront) or GEO models has not much sense (because you
    can't record light information in these formats).
    VRML 1.0 or 2.0 are the best formats.

    <p><tt>-</tt> (single dash) as a filename means stdin.

    <p><b>Notes about rendering 3DS models:</b>
    There is no good specification how lights in 3DS files should
    be read and interpreted, and I don't own the proprietary program
    to check how 3DS are supposed to be rendered.
    My test renderings with 3DS models look sensible, but I can't
    guarantee anything. If you can provide or point me to some exact
    spec about how the lights and materials in 3DS are supposed
    to be interpreted then post to
    <?php echo MAILING_LIST_LINK; ?>.

  <li><p><tt>&lt;output-filename&gt;</tt>

    <p>Filename where to write final image. Image format will
    be automatically derived from filename extension,
    known image formats and extensions are
    <ul>
      <li><tt>rgbe</tt> (or <tt>pic</tt>)
      <li><tt>png</tt>
      <li><tt>jpg</tt> (or <tt>jpeg</tt>)
      <li><tt>ppm</tt>
      <li><tt>bmp</tt>
    </ul>

    <p>If you will use <tt>--write-partial-image</tt> features
    (see below), then partial images will be also written to this file.
    Moreover, if you will use <tt>--first-row</tt> features (see below),
    then initial image contents will be read from this file.

    <p><b>Notes about RGBE format:</b>
    This format was developed by Greg Ward, and is used e.g. by
    <a href="http://floyd.lbl.gov/radiance/">Radiance</a>.
    Colors in RGBE images are written with a very good precision,
    while not wasting a lot of disk space. Good precision means
    that you may be able to expose in the image some details that
    were not initially visible for the human eye, e.g. by brightening
    some areas. Also color components are not clamped to [0; 1] range &mdash;
    each component can be any large number. This means that even if resulting
    image is too bright, and some areas look just like white stains,
    you can always correct the image by darkening it or applying gamma
    correction etc.

    <p>You can process RGBE images using
    <a href="http://floyd.lbl.gov/radiance/">Radiance</a> programs.
    Also my <?php echo a_href_page("glViewImage", "glviewimage"); ?>
    can be used to view RGBE images.
</ul>

<?php echo $toc->html_section(); ?>

<p>Options below may be placed anywhere at command-line
(before, between or after required options listed above).

<dl class="command_line_options_list_custom">
  <dt><span class="command_line_option">--camera-pos &lt;float&gt; &lt;float&gt; &lt;float&gt;</span>
      (short form <span class="command_line_option">-p</span>)<br>
      <span class="command_line_option">--camera-dir &lt;float&gt; &lt;float&gt; &lt;float&gt;</span>
      (short form <span class="command_line_option">-d</span>)<br>
      <span class="command_line_option">--camera-up &lt;float&gt; &lt;float&gt; &lt;float&gt;</span>
      (short form <span class="command_line_option">-u</span>)
  </dt>

  <dd><p>These options set camera position, looking direction and up vector.
    <tt>--camera-up-z</tt> is shortcut for <tt>--camera-up 0 0 1</tt>.

    <p>Initial camera settings are determined by

    <ol>
      <li>These command-line parameters.

      <li>First <tt>*Camera</tt> or <tt>*Viewpoint</tt> node in VRML file.
        Remember that placement of camera node in VRML file is important --
        camera's properties are modified by current transformation
        of camera node.

      <li>Default values (default VRML 1.0 perspective camera) :
        <ul>
          <li><tt>camera-pos = 0 0 1</tt>,
          <li><tt>camera-dir = 0 0 -1</tt>,
          <li><tt>camera-up = 0 1 0</tt>,
        </ul>
    </ol>

    <p>Things above are given in descending priority.
    E.g. if you will use <tt>--camera-pos 1 2 3</tt> parameter then
    starting camera position will be always <tt>1 2 3</tt>.
    If you don't use <tt>--camera-pos</tt> parameter then program will try to find
    camera node in VRML file. If such node is present &mdash; we will use camera
    position specified there. If not &mdash; we will use default camera position
    (i.e. <tt>0 0 1</tt>).

    <p>Notes:
    <ul>
      <li>If <tt>camera-dir</tt> and <tt>camera-up</tt>
        will not be orthogonal
        <!--
        (bez wzglêdu na to jak± metod± zosta³y uzyskane - z parametru,
        z warto¶ci zapisanej w VRMLu czy z warto¶ci domy¶lnej)
        -->
        vector <tt>up</tt> will be corrected.
        <tt>camera-dir</tt> and <tt>camera-up</tt> vectors must not be
        parallel. In particular, they must not be zero-vectors.
        Lengths of <tt>camera-dir</tt> and <tt>camera-up</tt> are not important,
        <tt>dir</tt> vector is always scaled based on <tt>camera-radius</tt> when
        program starts.
      <li>What about formats other than VRML (3DS, OBJ, GEO) ?
        In 3DS we can get camera definition from 3DS file (it's always
        perspective camera, never orthographic).
        In OBJ and GEO we simply can't get camera definition from file.
    </ul>

    <p>Note that you can use <?php echo a_href_page("view3dscene", "view3dscene"); ?>
    to comfortably determine good values for these options &mdash;
    see <i>Console -> Print rayhunter command-line to render this view</i>
    menu item in view3dscene.</dd>

  <dt><span class="command_line_option">--view-angle-x &lt;float&gt;</span></dt>

  <dd><p>Use perspective projection,
    with given field of view angle in horizontal direction. In degrees.
    Default is 60 degrees. Note: vertical angle will be automatically
    determined based on horizontal view angle and window width/height
    (unless you specify <tt>force-view-angle-y</tt>, see below).

  <dt><span class="command_line_option">--force-view-angle-y &lt;float&gt;</span></dt>

  <dd><p>Use perspective projection,
    with given field of view angle in vertical direction.
    Normally camera vertical angle will be automatically
    derived from <tt>--view-angle-x</tt> and requested
    output <tt>&lt;image-width&gt;</tt> and <tt>&lt;image-height&gt;</tt>.
    You can use this parameter to force some other non-proportional
    camera vertical angle.</dd>

  <dt><span class="command_line_option">--ortho &lt;float&gt; &lt;float&gt; &lt;float&gt; &lt;float&gt;</span></dt>

  <dd><p>Use orthographic projection, with given left, bottom, right, top
    dimensions. This makes previous <tt>--view-angle-x</tt> and
    <tt>--force-view-angle-y</tt> ignored (they are not sensible in orthographic
    projection). Note that order of arguments follows the
    X3D OrthoViewpoint specificication (and differs from typical OpenGL,
    like used by glOrtho).</p></dd>

  <dt><span class="command_line_option">--scene-bg-color &lt;red&gt; &lt;green&gt; &lt;blue&gt;</span></dt>

  <dd><p>Sets scene background color. Each RGB color component is a float
    in the range [0; 1]. By default background is black.</dd>

  <dt><span class="command_line_option">--write-partial-rows &lt;rows&gt; &lt;log-rows-file&gt;</span></dt>

  <dd><p>Always after generating <tt>&lt;rows&gt;</tt> rows,
    rayhunter will write the (partially finished) output image
    (to <tt>&lt;output-filename&gt;</tt>, i.e. to the same file where
    final image is supposed to be written). Additionally, each time
    when writing such partial image, rayhunter will write a file
    <tt>&lt;log-rows-file&gt;</tt> that will contain a single integer &mdash;
    the number of already finished rows.

    <p>This option has a couple of uses.
    <ol>
      <li>First of all, obviously, it lets you view unfinished
        results of <tt>rayhunter</tt>, if you're not patient.

      <li>This allows you to interrupt <tt>rayhunter</tt> process
        at any time and you will remain with a partially completed image.
        Then you can run <tt>rayhunter</tt> once again
        with <tt>--first-row</tt> option
        to finish rendering the image. You can look at
        <tt>&lt;log-rows-file&gt;</tt> to know how many rows are already finished.

        <p>E.g. you can use such script:
<pre>
  rayhunter ... --first-row `cat rows.log` --write-partial-rows 10 rows.log
</pre>
        This way after doing every 10 rows, <tt>rayhunter</tt>
        will save it's partially
        finished work. And if you will interrupt, kill etc.
        <tt>rayhunter</tt> process,
        you will be able to just run this script once again and
        <tt>rayhunter</tt> will start rendering roughly at the point where it finished.

      <li>If you want to render the image on more than one computer
        at the same time, you can easily run <tt>rayhunter</tt> on every computer
        with different <tt>--first-row</tt> argument and with appropriately
        small <tt>&lt;rows&gt;</tt> value for <tt>--write-partial-rows</tt>.
        When all processes will finish their work, you will only have
        to combine resulting images into one.
    </ol>

    <p>Note that very small <tt>&lt;rows&gt;</tt> value can slow down the
    rendering process. But usually it's not a significant slowdown,
    after all usually ray-tracing takes a lot of time and wasting
    a couple of additional seconds is not a problem.

    <p>Give value of 0 for <tt>&lt;rows&gt;</tt> to disable writing
    partial image and <tt>&lt;log-rows-file&gt;</tt> file.
    This is the default behavior.

    <p><i>Note about writing partial image:</i> when writing
    partial image, any errors during write are <i>reported as warnings
    and then ignored</i>. This is handy is you will view the image
    with some other program while rayhunter process is running:
    there's always a small chance that rayhunter will write the partial
    image exactly when another program is trying to read it.
    Depending on your OS, in such case writing of the partial image file
    may fail. That's why errors when writing partial image are ignored
    and rayhunter will just proceed with rendering.</dd>

  <dt><span class="command_line_option">--r-roul-continue &lt;float&gt;</span></dt>

  <dd><p>This option is meaningful only when you use <i>path tracer</i>.
    Argument is a float number in range [0; 1].
    At each point on the path, if the specified <tt>&lt;recursion-depth&gt;</tt>
    is exceeded, Russian-roulette is used to determine whether the
    path should terminate here. If random number from [0; 1] range
    is larger than argument given to this option, the path will terminate.
    In other words, argument for this option is the "continuation probability"
    &mdash; larger argument means that average path will be longer.

    <p>Usually too small values will produce a lot of noise,
    and too large values will make long rendering time.

    <p>However, note that there are some sensible scenarios
    when argument 1 for this option is sensible:
    if your scene is very very simple and you want all paths
    to terminate in a "natural" way (when ray will not hit anything
    in the scene). This will give you very accurate result
    (although <i>some</i> noise on the image will still be present,
    since the ray directions are still random), although
    rendering time will be <i>really long</i> even for simplest scenes.

    <p>When argument for this option is exactly 0,
    Russian-roulette will always lose. In other words,
    Russian-roulette will not be used at all. In this case
    <tt>&lt;recursion-depth&gt;</tt> value will work like
    for classic ray-tracer. But the result will always be biased,
    i.e. (slightly) incorrect, usually darker than it should be.

    <p>Default value is 0.5.</dd>

  <dt><span class="command_line_option">--primary-samples-count &lt;count&gt;</span></dt>

  <dd><p>This option is meaningful only when you use <i>path tracer</i>.
    By default this is 1. Generally there is no sense in incrementing
    this too much. You can use here 10 or something like that to
    have anti-aliased image. If you want to just increment
    number of paths, usually it's wiser to increment
    <tt>&lt;non-primary-samples-count&gt;</tt>, this way you will
    slow down rendering by a little lesser factor.</dd>

  <dt><span class="command_line_option">--direct-illum-samples-count &lt;count&gt;</span></dt>

  <dd><p>This option is meaningful only when you use <i>path tracer</i>.
    At each path point, the specified number of random rays directed
    towards light sources are checked. By default this is 1. You can
    increase this,
    which can bring good results if your scene has a complex light
    setting (many light sources, in different places etc.)
    Remember that you can also just increase number of paths
    (i.e. <tt>&lt;non-primary-samples-count&gt;</tt>),
    as this will also produce more checks for rays to light sources.

    <p>For educational purposed you can also set this to 0
    &mdash; then you will get a naive path tracer
    that just hopes to hit some light sources with random rays.
    <!-- tzn. bezpo¶rednie o¶wietlenie bêdzie liczone tak samo jak po¶rednie. -->
    You will then need many many paths, even for simplest models,
    to get nice renderings.

  <dt><span class="command_line_option">--first-row &lt;first-row-num&gt;</span></dt>

  <dd><p>If you will use this option with non-zero argument then
    <tt>&lt;output-filename&gt;</tt> must already exist.
    <tt>rayhunter</tt> will load the initial image from this file.
    Then the image will be resized, if needed,
    to <tt>&lt;image-width&gt;</tt> and <tt>&lt;image-height&gt;</tt>.
    Then <tt>rayhunter</tt> will start rendering,
    but not from the beginning: from the given <tt>&lt;first-row-num&gt;</tt>
    row number. Rendering results will be written on the image.

    <p>This way you can tell <tt>rayhunter</tt> to resume rendering
    that is already partially done. This is particularly handy
    in connection with <tt>--write-partial-rows</tt> option, see there for
    description.

    <p>Argument 0 (the default) for this options means that
    rendering should proceed normally, from the beginning of the image.

  <dt><span class="command_line_option">--detail-quadric-slices &lt;integer&gt;</span><br>
      <span class="command_line_option">--detail-quadric-stacks &lt;integer&gt;</span><br>
      <span class="command_line_option">--detail-rect-divisions &lt;integer&gt;</span></dt>

  <dd><p>These options control the triangulation.
    view3dscene has exactly the same options, and they are documented
    in detail in <?php echo a_href_page_hashlink(
    "view3dscene <tt>--detail-*</tt> options documentation",
    "view3dscene", "command_line_options_detail"); ?>.

    <p>Notes specific to <tt>rayhunter</tt>: note that <tt>rayhunter</tt>
    never does so-called <i>over-triangulating</i>.
    This also means that option <tt>--detail-rect-divisions</tt>
    is useless. It was added here only for completeness.
  </dd>

  <dt><span class="command_line_option">--octree-max-depth &lt;integer&gt;</span><br>
      <span class="command_line_option">--octree-leaf-capacity &lt;integer&gt;</span></dt>

  <dd><p>These parameters control octree generation.
    By default
    <tt>--octree-max-depth</tt>=<?php echo RAYHUNTER_DEF_OCTREE_MAX_DEPTH; ?> and
    <tt>--octree-leaf-capacity</tt>=<?php echo RAYHUNTER_DEF_OCTREE_LEAF_CAPACITY; ?>.
    You can also specify octree propeties inside VRML/X3D file:
    use <tt>KambiNavigationInfo.octreeVisibleTriangles</tt>
    (see <?php echo a_href_page_hashlink('octree properties extension',
    'kambi_vrml_extensions', 'section_ext_octree_properties'); ?>).

    <?php /* Niniejsze drzewo ósemkowe nie zawsze bêdzie absolutnie optymalne,
    ale zazwyczaj bêdzie naprawdê dobre i nie bêdzie zajmowa³o zbyt du¿o
    miejsca w pamiêci (oko³o 10 MB na scenach do 100 000 trójk±tów -
    nie jest tak ¼le). Ale je¶li chcesz zmusiæ */ ?></dd>
</dl>

<p>See also <?php echo a_href_page(
"notes about command-line options for all my programs", "common_options") ?>.

<?php echo $toc->html_section(); ?>

<ul>
  <li><p><i>Only for <tt>classic</tt> ray-tracer :</i><br>
    Implemented light model is as close as possible to the
    <a href="http://www.web3d.org/x3d/specifications/vrml/ISO-IEC-14772-VRML97/part1/concepts.html#4.14">
    light model outlined in VRML 97 specification</a>.
    Some modifications were needed because I have recursive
    ray-tracing (while VRML 97 specifies only local
    light model). Also VRML 1.0 models require different treating
    in some cases (e.g. <tt>SpotLight</tt> focus is specified using a different
    way and <tt>ambientIntensity</tt> is not available in standard VRML 1.0).

    <p>We handle all VRML light nodes &mdash; <tt>DirectionalLight</tt>,
    <tt>SpotLight</tt> and <tt>PointLight</tt>. Also the headlight
    is used, configurable by <tt>NavigationInfo.headlight</tt>
    and <?php echo a_href_page_hashlink('KambiHeadLight node',
    'kambi_vrml_extensions', 'section_ext_headlight'); ?>.

    <p>For <tt>&lt;recursion-depth&gt;</tt> equal zero we use only
    <tt>diffuse</tt> material color. According to VRML 97 light model,
    <tt>emission</tt> color would be more correct but in 99% of real
    cases <tt>emission</tt> color is just black so the whole rendered
    image would be black. That's why I decided to use <tt>diffuse</tt>
    color instead of <tt>emission</tt>. Everyone understands that setting
    <tt>&lt;recursion-depth&gt;</tt> to zero is only for testing purposes
    anyway.

  <li><p><i>Mostly for <tt>classic</tt> ray-tracer :</i><br>
    <?php echo a_href_page_hashlink(
      "Use <tt>mirror</tt> field of <tt>Material</tt> node to create mirrors",
      "kambi_vrml_extensions", "ext_material_mirror"); ?>.

  <li><p><i>Only for <tt>path</tt> tracer :</i><br>
    We don't use point and directional lights,
    so VRML <tt>DirectionalLight</tt>, <tt>SpotLight</tt> and
    <tt>PointLight</tt> nodes are completely ignored.
    Only the surface lights are used. Every object with
    a non-black <tt>emissiveColor</tt> is a light source.

    <p>Implemented BRDF is Phong's BRDF.
    See <?php echo a_href_page_hashlink(
     "fields describing physical properties (Phong's BRDF) for " .
     "<tt>Material</tt>node", "kambi_vrml_extensions",
     "ext_material_phong_brdf_fields"); ?>.

  <li><p>Some things not handled (yet): textures, interpolating
    normal vectors (i.e. we're of course calculating pixel color
    at every surface point separately, but our surfaces are flat
    with regards to normal vectors).

  <li><p>This concerns actually most 3d engines, including
    ray-tracer algorithms inside <tt>rayhunter</tt>
    and real-time OpenGL rendering inside
    <?php echo a_href_page("view3dscene", "view3dscene"); ?>:
    3d model's geometry must be correct. Which means that:
    <ul>
      <li><p>No T-Intersections allowed. Otherwise you may see cracks.

      <li><p>If two faces cross each other, the intersection may
        be a line, not a plane. I.e. they
        shouldn't be coplanar in such case. Otherwise
        they will fight which one is nearer than the other
        and which one casts the shadow over the other.

        <p>Actually <tt>rayhunter</tt> should be able to handle such
        bad case of coplanar surfaces (i.e. result will look OK
        as long as surfaces have the same material), but there
        is never 100% warranty for such things (because it involves
        floating-point errors).
        <!-- ale nie ma to 100% gwarancji i nie mo¿e mieæ
        (bo nigdy dok³adnie nie wiadomo - czy rzeczywi¶cie na tym wielok±cie
        te dwa trójk±ty nie zas³aniaj± siê wzajemnie czy te¿ raczej twórcy sceny
        chodzi³o o to ¿e jeden wielok±t jest bardzo minimalnie nad drugim ?)
        -->
    </ul>
</ul>

<?php echo $toc->html_section(); ?>

<p>The same ray-tracer code as is used by <tt>rayhunter</tt>
is also used inside
<?php echo a_href_page("view3dscene", "view3dscene"); ?>.
While using ray-tracer from
<?php echo a_href_page("view3dscene", "view3dscene"); ?>
 is more comfortable, using command-line <tt>rayhunter</tt> has also
some advantages:

<ul>
  <li><p>The obvious advantage is that you can use <tt>rayhunter</tt>
    in batch mode,from scripts etc.

  <li><p><tt>rayhunter</tt> can produce images in RGBE format.

  <li>You can use options <tt>--first-row</tt> and
    <tt>--write-partial-rows</tt> to be able to kill rayhunter process
    at any time and then later resume rendering from last point.
    You can also use the same options to render various parts of
    the image simultaneously on multiple systems.
    These advantages may be crucial if you want to do some serious
    rendering using path tracer, since they may take a lot of time.

  <li><p>Options <tt>--r-roul-continue</tt>,
    <tt>--direct-illum-samples-count</tt>,
    <tt>--primary-samples-count</tt> that allow you to better control
    path tracing are not available in <tt>view3dscene</tt>.

  <li><tt>rayhunter</tt> may work a little faster since it doesn't
    display the image while rendering. Although using
    <tt>--write-partial-rows</tt> you can force <tt>rayhunter</tt>
    to write partial result from time to time.
</ul>

<?php common_footer() ?>
