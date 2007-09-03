<?php
  require_once 'vrmlengine_functions.php';

  common_header("view3dscene", LANG_EN,
    'view3dscene is a viewer for VRML 1.0, VRML 2.0 (aka VRML 97), ' .
    '3DS, OBJ (Wavefront) and MD3 scenes. ' .
    'Can do collision detection. ' .
    'Can be used as command-line converter from 3DS, OBJ, MD3 to VRML 1.0. ' .
    'Has built-in raytracer. Uses OpenGL. ' .
    'Free software. For Linux, FreeBSD, Mac OS X and Windows.',
    NULL,
    '<style type="text/css"><!--
      HR.ruler_between_sections { width: 100%; }
      DL.params_list DT  {
        font-weight: normal; font-family: monospace;
        margin-top: 1em;
      }
    --></style>
    ');
  require "octree_consts.php";

  function section($section_title, $section_anchor, $make_hr = true)
  {
    if ($make_hr)
      echo "<hr class=\"ruler_between_sections\">";
    echo "<h3 class=\"h_section\"><a name=\"section_$section_anchor\">$section_title</a></h3>";
  }
?>

<?php
  echo pretty_heading("view3dscene", VERSION_VIEW3DSCENE);
  echo '<table align="right">' .
    '<tr><td>' . medium_image_progs_demo("view3dscene_2.0.0_screen_demo.png", "view3dscene", false) .
    '<tr><td>' . medium_image_progs_demo("view3dscene_screen_demo_1.png", "view3dscene", false) .
    '<tr><td>' . medium_image_progs_demo("view3dscene_screen_demo_2.png", "view3dscene", false) .
    '<tr><td>' . medium_image_progs_demo("view3dscene_screen_demo_3.png", "view3dscene", false) .
    '</table>';
?>

<p>view3dscene is a viewer for 3D scene files.

<!-- Removed: too long useless text:

<p>Below is full documentation of a program. Basic things that you
want to read are in first two sections (<a href="#section_install">Downloading
and installing</a> and <a href="#section_run">Running</a>)
and it's good to take a look at the most important keys
in section <a href="#section_keys">Controlling with keys &amp; mouse</a>.
-->

<ol>
  <li><a href="#section_features">Features</a>
  <li><a href="#section_install">Downloading and installing</a>
  <li><a href="#section_run">Running</a>
  <li><a href="#section_keys">Controlling program with keys &amp; mouse</a>
  <li><a href="#section_command_line_options">Command-line options</a>
  <li><a href="#section_sthg_about_shading">A few words about flat/smooth shading</a>
  <li><a href="#section_raytracer">Notes about ray-tracer</a>
  <li><a href="#section_depends">Requirements</a>
  <li><a href="#section_freshmeat">Freshmeat entry</a>
</ol>

<?php section('Features', 'features', false); ?>

<p>Supported file formats:
<ul>
  <li><p><b>VRML 1.0 and 2.0</b>.
    Almost complete VRML 1.0 support is ready.
    VRML 2.0 (aka VRML 97)
    support is also done, although some advanced features
    are missing right now (like prototypes, scripting, interpolators, sensors).
    VRML files usually have <tt>WRL</tt> extension.

    <!-- Among things that work are embedded textures,
    semi-transparent materials and semi-transparent textures,
    automatic normals smoothing (based on <tt>creaseAngle</tt>),
    triangulating non-convex faces, understanding camera nodes,
    WWWInline handling, text rendering and more. -->

    <p>See <?php echo a_href_page('VRML implementation status',
      'vrml_implementation_status'); ?> for detailed list of supported
    features. See also <?php echo a_href_page('my extensions to VRML',
    'kambi_vrml_extensions'); ?>, <?php
      echo a_href_page('my VRML test suite', 'kambi_vrml_test_suite'); ?>,
    and finally <a href="http://www.web3d.org/x3d/specifications/vrml/">
    the official VRML specifications</a>.

  <li><p><b><?php echo a_href_page(
    "Kanim (Kambi VRML engine animations)", 'kanim_format'); ?></b> format
    is handled, animation is played.</p>

  <li><p>Also many
    <a href="http://oss.sgi.com/projects/inventor/"><b>OpenInventor's</b></a>
    1.0 ASCII files (extension <tt>IV</tt>) are handled.
    Mainly it's because Inventor 1.0 and VRML 1.0 are very similar
    formats, but view3dscene handles also some additional
    Inventor-specific nodes.

  <li><p><b>3d Studio 3DS format</b>. Not every information in 3DS
    is handled by view3dscene but most important things, like
    materials, texture coordinates and texture filenames are supported.

  <li><p><b>MD3</b>. This is the format used for models
    in Quake 3 and derivatives (<a href="http://tremulous.net/">Tremulous</a>
    etc.). Everything usefull (almost ?) is read from MD3 file:
    geometry with texture (coordinates, and texture filename from
    associated <tt>xxx_default.skin</tt> file), <i>animation is also read
    and played</i>.</p>

  <li><p><b>Wavefront OBJ files</b>. Only very basic support :
    geometry and texture coords. Texture named <tt>default_obj_texture.png</tt>
    in the current directory will be used if texture coords are specified
    in the file.

  <li><p><b>GEO</b>. Some simple format; I don't even know what program
    writes files in this format, it just happened that I had a few files
    in this format. No, it's not Videoscape GEO and it's not Houdini GEO.
</ul>

<p>Among many features are:
<ul>
  <li>Two navigation modes are available: <tt>Walk</tt>
    (walking like in FPS (Doom, Quake) games,
    with collision detection, gravity and related features available) and
    <tt>Examine</tt> (this allows you to easily rotate and scale the whole
    model). <!-- trochê tak jakbysmy trzymali w rêkach
    pude³eczko ze scen± w ¶rodku -->
  <li>Convertion of 3DS, OBJ, MD3 and GEO files to VRML 1.0
  <li>You can also simply open and save any VRML 1.0 or 2.0 file
    and in effect view3dscene will work as a "pretty-printer" for VRML files.
  <li>Built-in ray-tracer
    (that is also available as a separate command-line program,
    <?php echo a_href_page("rayhunter", "rayhunter"); ?>)
    to generate nice views of the scene (with shadows, mirrors,
    and transmittance). Classic ray-tracer implements exactly VRML 97
    lighting equations.
    <!-- promienie za³amane. a w przypadku
    path tracera Monte Carlo uwzglêdniamy fizyczne w³a¶ciwo¶ci materia³u
    i powierzchniowe ¼ród³a ¶wiat³a których wynikiem s± dodatkowe efekty
    w rodzaju pó³cieni i krwawienia kolorów). -->
  <li>You can inspect your model (select triangles with left mouse click
    and use menu item <i>Help</i> -> <i>Selected object information</i>).

    <p>There are also very limited editing capabilities. They are
    intended to be used only as a post-processing of some model.
    We intentionally do not try to implement a full 3D authoring program here.
  <li>Animations may be played (currently, they may be loaded
    from <?php echo a_href_page("Kanim", 'kanim_format'); ?> or MD3 files;
    animations recorded as VRML 2.0 interpolators are not played yet).

    <p>There's one small caveat with animations right now:
    some features (collision checking, mouse picking,
    ray-tracer &mdash; everything that requires some octree) always use the
    <i>first animation frame</i>, regardless of current animation frame
    displayed.
</ul>

<!-- Removed becase usual user is not interested in this.

<p>Badanie kolizji i raytracing s± przyspieszane przy pomocy drzewa ósemkowego -
je¶li wiesz co to jest to mo¿e Ciê zainteresowaæ ¿e program
pozwala odrobinkê kontrolowaæ generowanie drzewa (patrz ni¿ej -
parametry programu <tt>- -triangle-octree-...</tt>) i potem ogl±daæ wygenerowane
drzewo (patrz ni¿ej - klawisze <b>O</b>, <b>Ctrl+U</b>, <b>Ctrl+D</b>).

-->

<?php section('Downloading and installing', 'install'); ?>

<p>Here are archived binaries of the program. No special installation
is required, just unpack these archives and run the program.
This documentation is also included in the archives
(look in <tt>documentation</tt> subdirectory) for offline viewing.
<?php echo_standard_program_download('view3dscene', 'view3dscene',
  VERSION_VIEW3DSCENE, true); ?>

<p><?php echo SOURCES_OF_THIS_PROG_ARE_AVAIL; ?>

<p><i>Demo scenes</i>:
In <?php echo a_href_page("my VRML test suite",
"kambi_vrml_test_suite"); ?> you can find many simple VRML models
that demonstrate what can be expressed in VRML 1.0 and 2.0
and what view3dscene can handle and display. These are not any big or
beautiful scenes, you can find some (slightly) more impressive VRMLs
in data files of my games
 <?php echo a_href_page("The Castle", "castle"); ?>,
 <?php echo a_href_page("lets_take_a_walk", "lets_take_a_walk"); ?>,
 <?php echo a_href_page("malfunction", "malfunction"); ?>.

<?php section('Running', 'run'); ?>

<p>Simply run without any command-line parameters.
<!-- In this case some default "welcome scene" will be loaded
(for now it's just empty scene, but I'm working on something more impressive). -->
Load your model file using "Open" menu item.</p>

<p>You can provide on the command-line file name to load.
As usual, dash (<tt>-</tt>) means that standard input will be read
(in this case the input must be in VRML (or Inventor) format).</p>

<p>Also read <a href="#section_command_line_options">about
view3dscene additional command-line options</a>.</p>

<?php section('Controlling program with keys &amp; mouse', 'keys'); ?>

<p><b>Keys in <tt>Examine</tt> navigation mode :</b>
<table border="1" class="key_list">
  <tr><td>Arrows / PageUp / PageDown   <td>rotate scene
  <tr><td>Space                    <td>stop rotating scene
  <tr><td>Ctrl + Arrows / PageUp / PageDown <td>move scene
  <tr><td>+ / -                      <td>scale scene
  <tr><td>Home  <td>restore default rotation, translation and scale
</table>

<!--
<p>What is the difference between <tt>Walk</tt> and <tt>FreeWalk</tt> navigation
modes ? In <tt>Walk</tt> mode your moves and rotations are
more closely related to your <i>initial</i> camera up vector.
In <tt>FreeWalk</tt> mode your moves and rotations tend to be rather relative
to the <i>current</i> camera up vector. <tt>Walk</tt> mode usually tends
to "feel better", but only when your camera's up vector was initialized to a
useful value. You can set initial camera up vector using <tt>- -camera-up</tt>
command-line option (look below) or using menu item "Change camera up vector".
-->

<p><b>Keys in <tt>Walk</tt> navigation mode :</b><br>
<table border="1" class="key_list">
  <tr><th colspan="2">Basic:
  <tr><td>Up / Down            <td>move
  <tr><td>Left / Right         <td>rotate
  <tr><td>PageUp / PageDown    <td>raise/bow your head
  <tr><td>Home
      <td>restore head raise to initial position (neutralize any effect of
          PageUp / PageDown)
  <tr><td>Insert / Delete     <td>fly up / down
  <tr><td>Comma / Period      <td>strafe moving
  <tr><td>A / Z
    <td>jump / crouch (only when <i>Gravity</i> works in <tt>Walk</tt> mode)
  <tr><td colspan="2">Note: when <i>mouse look</i> in turned <i>on</i> then
    <ul style="margin: 0;">
      <li>Left / Right keys are responsible for strafe moving</li>
      <li>Comma / Period keys are for rotations</li>
    </ul></td></tr>
  <tr><th colspan="2">Additional keys:
  <tr><td>+ / -
      <td>Increase / Decrease moving speed (has effect on keys
        Up / Down, Insert / Delete, Comma / Period)
  <tr><td>Ctrl + Left / Right
       <td>rotate <i>slower</i> (useful when you want to set up camera
         very precisely, e.g. to use this camera setting to render a scene
         image using ray-tracer)
  <tr><td>Ctrl + PageUp / PageDown
      <td>raise / bow your head <i>slower</i>
</table>

<p>Now, there's a lot of keys that work independent of current navigation
mode. But I will not list them all here. They all can be seen
by looking at available menu items. Probably most useful keys
(and menu items) are <tt>v</tt> (switch to next navigation mode),
<tt>F1</tt> (toggle status text visibility)
and <tt>Escape</tt> (exit).

<!--
  Some Polish docs that is not translated and probably will not be ever
  translated, it's not needed. Keys listed below are already visible
  as menu item shortcuts, what they do should be either obvious
  or not important for a user.

<p><b>Klawisze które dzia³aj± niezale¿nie od stylu nawigacji :</b><br>
<table border="1" class="key_list">
  <tr><th colspan="2">Dodatkowe klawisze:
  <tr><td>f
      <td>prze³±cz renderowanie mg³y zapisanej w modelu VRMLa
	(mg³a jest odczytywana z wêz³a <tt>Fog</tt> VRMLa,
	<?php echo a_href_page_hashlink('patrz tutaj po wiêcej informacji',
        'kambi_vrml_extensions', 'ext_fog'); ?>)
  <tr><td>Ctrl+B
      <td>prze³±cza u¿ywanie blending (domy¶lnie jest w³±czone).
	Przy wy³±czonym blending warto¶æ <tt>transparency</tt> materia³ów bêdzie
	bez znaczenia dla OpenGLa - wszystkie obiekty bed± renderowane jakby
	by³y zupe³nie nieprzezroczyste.
  <tr><td>Ctrl+U, Ctrl+D
      <td>zmieñ poziom drzewa ósemkowego który ma byæ
	wy¶wietlany; poziom -1 oznacza ¿e ¿aden poziom nie jest wy¶wietlany.
	Wy¶wietlanie poziomu polega na rysowaniu prostopad³o¶cianów otaczaj±cych
	dla wszystkich niepustych elementów drzewa (czyli wez³ów wewnêtrznych lub
	niepustych li¶ci) na danej g³êboko¶ci drzewa. ("u"/"d" jak "up"/"down")
  <tr><td>Ctrl+R
      <td>wypisz na konsoli stosown± komendê do uruchomienia
      <?php echo a_href_page('rayhuntera', 'rayhunter'); ?>
      z aktualnym u³o¿eniem kamery itp.
  <tr><td>Ctrl+C
      <td>wypisz na konsoli odpowiedni node <tt>PerspectiveCamera { ... }</tt>
	lub <tt>OrthographicCamera { ... }</tt> z aktualnymi ustawieniami kamery.
	Mo¿esz np. wstawiæ taki node na pocz±tek ogl±danego w³a¶nie pliku
	VRMLa aby nastêpnym razem gdy za³adujesz tego VRMLa uaktywniæ
	takie w³a¶nie po³o¿enie kamery.
</table>
-->

<?php section('Command-line options', 'command_line_options'); ?>

<!-- Since we have menu item "Save as VRML ..." I moved description of
- -write-to-vrml parameter to "additional params" section -->

<p>All options described below may be given in any order.
They all are optional.

<dl class="params_list">

  <dt>--write-to-vrml
  <dd><p>Option <tt>--write-to-vrml</tt> means "don't open any window,
    just convert the input model to VRML 1.0,
    write the result to the standard output and exit".
    This way you can use view3dscene to convert
    3DS, OBJ, MD3 and GEO files to VRML 1.0, like<br>
    <tt>&nbsp;&nbsp;view3dscene scene.3ds --write-to-vrml > scene.wrl</tt> , <br>
    you can also use this to do some VRML file processing using
    options <tt>--scene-change-*</tt> described below.

  <dt>--scene-change-no-normals<br>
      --scene-change-no-solid-objects<br>
      --scene-change-no-convex-faces
  <dd><p>Using one of these options changes the scene before it
    is displayed (or saved to VRML, if you used <tt>--write-to-vrml</tt>
    option). These options are useful when you suspect that some
    of the informations in scene file are incorrect.

    <p>These options change only the scene which filename was specified
    at command-line. Later scenes (that you open using "Open"
    menu item) are not affected by these options.
    Instead, you can use "Edit" menu commands to perform any
    of these scene changes at any time.
    Really, these command-line options are usable mostly
    when you're using parameter <tt>--write-to-vrml</tt>.

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
          For VRML 2.0, all <tt>normal</tt> fields are set to <tt>NULL</tt>.
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
          For VRML 2.0, all <tt>solid</tt> fields are set to <tt>FALSE</tt>.
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
          For VRML 2.0, all <tt>convex</tt> fields are set to <tt>FALSE</tt>.
        <p><b>Effect:</b> All <tt>IndexedFaceSet</tt> faces will be treated
          as potentially non-convex. This means that we will load the scene
          a little longer but all faces will be correctly interpreted
          and displayed. It's useful when you suspect that some scene faces
          are non-convex and it's not marked in the scene (by setting
          <tt>faceType</tt> to <tt>UNKNOWN_FACE_TYPE</tt>).
    </ul>

    <p>Example: I have here some model <tt>helicopter.wrl</tt> that looks
    incorrectly when it is displayed because all parts of model are marked
    as SOLID while they are not solid. So to view this model correctly
    I can use command<br>
    <tt>&nbsp;&nbsp;view3dscene --scene-change-no-solid-objects helicopter.wrl</tt><br>
    I can also correct this model once using command<br>
    <tt>&nbsp;&nbsp;view3dscene --scene-change-no-solid-objects helicopter.wrl
      --write-to-vrml > helicopter-corrected.wrl</tt>.

  <dt>--navigation Examine|Walk
  <dd><p>Set initial navigation mode. Default is <tt>Examine</tt>.
    You can change navigation mode while the program is running
    using key <tt>v</tt> or appropriate menu item.

  <dt>--camera-radius &lt;float&gt;
  <dd><p>When you are walking in the scene with
    <tt>Walk</tt> navigation model with collision
    detection on, for the sake of collision detection "user" is treated
    as a sphere with non-zero radius.
    Default radius of this sphere is the average size of scene bounding box
    divided by 100.
    Using this command-line option, you can set the radius of this sphere
    to any value (greater than 0). This can be very useful, but be careful:
    too large radius will make moving (with collision detection turned on)
    impossible (because every possible move will produce a collision).
    Too little radius may produce precision-errors in depth-buffer
    (this can lead to some strange display artifacts).

    <!--
    Badanie kolizji jest robione na zasadzie : ¿eby móc przej¶æ z POZYCJA1 do POZYCJA2
    nie mo¿e byæ ¿adnych kolizji sceny z odcinkiem POZYCJA1-POZYCJA2 i
    ze sfer± o promieniu d³ugo¶ci camera-radius i ¶rodku w punkcie POZYCJA2.
    -->

  <dt>--light-calculate on|off
  <dd><p>Sets initial state of "Lighting calculate" option.
    If this is "on", lighting calculations are performed.
    Every vertex may have different color, depending on surface material,
    lights set in the scene, camera position etc.
    If this is "off" scene is rendered without lighting:
    every triangle has always the same, single color.

    <p>Default is true, which is usually much more reasonable.
    There is also a menu item "Lighting calculate" that you can use to change
    this setting while the program is running.

    <!--
    (uwaga - tylko formaty VRML i 3DS pozwalaj± na
    definiowanie ¶wiate³ w pliku, w przypadku innych formatów ca³a scena
    bêdzie wy¶wietlana jako czarna - bo nic nie bêdzie o¶wietlone).

    Give somewhere some description of what is "headlight" and how it is
    initialized (to SceneLightCount = 0) ?
    -->

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

  <dt>--renderer-optimization none|scene-as-a-whole|separate-shape-states|separate-shape-states-no-transform
  <dd><p>Set rendering optimization.
    It's difficult to describe in short what each value means,
    you can just try them all. They all produce identical results, but some
    are slower and some are faster.
    <ul>
      <li><tt>"none"</tt> will always be the slowest one (use only for testing
        purposes)
      <li><tt>"scene-as-a-whole"</tt> is the best when you're always
        looking at the scene as a whole object (e.g. usually in
        Examine navigation mode)
      <li><tt>"separate-shape-states"</tt> may have great efficiency
        in Walk navigation mode, when you often can't see whole scene at once.
        This is the default optimization kind, because it can be superior
        over all others, and, in some worst cases, is not much slower
        than <tt>"scene-as-a-whole"</tt>.
      <li><tt>"separate-shape-states-no-transform"</tt> is like
        <tt>"separate-shape-states"</tt> but may additionally conserve
        memory used for OpenGL display lists. Don't use this when
        you use volumetric fog on the scene.
    </ul>

    <p>For more technical details see
    <a href="apidoc/html/VRMLFlatSceneGL.html#TGLRendererOptimization">documentation
    of TGLRendererOptimization type in VRMLFlatSceneGL unit</a>.

  <dt>--triangle-octree-max-depth &lt;integer&gt;<br>
      --triangle-octree-max-leaf-items-count &lt;integer&gt;<br>
      --ss-octree-max-depth &lt;integer&gt;<br>
      --ss-octree-max-leaf-items-count &lt;integer&gt;
  <dd><p>These options specify parameters for constructing octrees.
    These will be used when loading every 3d file
    (given on command-line or loaded using "Open ..." menu item).

    <!--
    Nie bêdê tu
    wyja¶nia³ dok³adnego znaczenia tych parametrów; mam nadziejê ¿e je¶li
    kto¶ wie co to jest drzewo o¶emkowe to szybko zorientuje siê jak
    te parametry kontroluj± konstrukcjê drzewa.
    -->

    <p>E.g. you can pass <tt>--triangle-octree-max-depth 0</tt> parameter
    to say that whole triangle octree must always contain only one node
    (so this only node must be a leaf). This way you de facto
    turn off using octree. This way you can see that using octree
    can really speed up collision detection, not to mention raytracing.

    <p>Default parameters are <?php echo VIEW3DSCENE_DEF_OCTREE_MAX_DEPTH; ?>
    for <tt>triangle-octree-max-depth</tt> and
    <?php echo VIEW3DSCENE_DEF_OCTREE_MAX_LEAF_ITEMS_COUNT; ?> for
    <tt>triangle-octree-max-leaf-items-count</tt>.
</dl>

<p>As usual all
<?php echo a_href_page("standard options understood by my OpenGL programs",
"opengl_options") ?> are also allowed.
See also <?php echo a_href_page(
"notes about command-line options understood by my programs", "common_options") ?>.

<?php section('A few words about flat/smooth shading', 'sthg_about_shading'); ?>

<p>Using key <tt>s</tt> and menu item "Switch flat/smooth shading" you
can switch between using flat and smooth shading. Default is to use
smooth shading.

<p>Flat shading means that each triangle has only one normal vector
and only one solid color. Smooth shading means that adjacent triangles
with a <i>similar</i> plane can share the same normal vectors at their
common edges. In effect shapes that are approximating some perfectly smooth
surfaces (e.g. spheres) may be rendered better with smooth shading.
Moreover smooth shading allows triangle to have different material
properties at each vertex (e.g. one vertex is yellow, the other one is blue),
you can see example of this in
<?php echo a_href_page('my VRML test suite',
'kambi_vrml_test_suite'); ?>
 in file <tt>vrml_1/materials.wrl</tt>.

<p>Group of planes are <i>similar</i> if angle between each pair
of planes is smaller than <b>creaseAngle</b> value of last seen
<b>ShapeHints</b> VRML node. For 3DS, OBJ, MD3 and GEO models we use
default creaseAngle = 0.5 radians (a little less than 30 degrees).

<p>Note: if VRML file already had some normal vectors recorded
(in <tt>Normal</tt> nodes) then program will use them, in both flat
and smooth shading.
Usually it's not important but to be sure that proper normals are
used you can use menu item "Edit->Remove normals info from scene".
<!-- or command-line parameter <tt>- -scene-change-no-normals</tt>. -->

<!-- Final finding: usually you will
Podsumowuj±c bêdziesz zapewne zawsze chcia³ u¿ywaæ cieniowania smooth
a na cieniowanie flat czasem zerkn±æ jako na ciekawostkê.
-->

<?php section('Notes about ray-tracer', 'raytracer'); ?>

<p>After pressing key <tt>r</tt> (or choosing "Raytrace !" menu item)
and answering some questions program will render image using
ray-tracing. I implemented two ray-tracing versions: classic
(Whitted-style) and path tracing. view3dscene will ask you which
algorithm to use, and with what parameters.

<p>Rendered image will be successively displayed.
You can press <tt>Escape</tt> to stop the process if it takes too long.
<!-- i bêdzie
stopniowo zakrywa³ oryginalny obraz wyrenderowany przy pomocy OpenGLa. -->
After generating image program will wait for pressing <tt>Escape</tt>,
you can also save generated image to file.

<p>What to do to make this process less time-consuming ?
First of all, the simplest thing to do is to shrink the window.
Second, the quality of octree has great influence on rendering time
(and you should note that by default view3dscene uses rather poor-quality
octree, <tt>max-depth =<?php echo VIEW3DSCENE_DEF_OCTREE_MAX_DEPTH; ?></tt>,
<tt>max-leaf-items-count =<?php echo VIEW3DSCENE_DEF_OCTREE_MAX_LEAF_ITEMS_COUNT; ?></tt>).
If you want to play with ray-tracer from within view3dscene I suggest
you prepare a script like<br>
<tt>&nbsp;&nbsp;view3dscene
  --triangle-octree-max-depth <?php echo RAYHUNTER_DEF_OCTREE_MAX_DEPTH; ?>
  --triangle-octree-max-leaf-items-count <?php echo RAYHUNTER_DEF_OCTREE_MAX_LEAF_ITEMS_COUNT; ?>
  --geometry 300x300 "$@"</tt><br>
and use it to run view3dscene in "ray-tracer-optimal" mode.

<p>More detailed description of how ray-tracer works is given in
<?php echo a_href_page('documentation of rayhunter', 'rayhunter'); ?>.

<?php section(DEPENDS, 'depends'); ?>

<?php echo depends_ul(array(
  DEPENDS_OPENGL,
  DEPENDS_LIBPNG_AND_ZLIB,
  DEPENDS_UNIX_GLWINDOW_GTK_2,
  DEPENDS_MACOSX)); ?>

<?php section('Freshmeat entry', 'freshmeat'); ?>

<p>Here's a link to
<a href="http://freshmeat.net/projects/view3dscene/">view3dscene
entry on freshmeat</a>. You can use this e.g. to subscribe to new
releases, so that you will be automatically notified about new
releases of view3dscene.</p>

<?php
  if (!IS_GEN_LOCAL) {
    $counter = php_counter("view3dscene", TRUE);
  };

  common_footer();
?>
