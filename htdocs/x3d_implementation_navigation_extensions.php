<?php
  require_once 'x3d_implementation_common.php';
  require_once 'x3d_extensions_functions.php';
  x3d_extensions_header('Navigation', 'navigation', 'navigation',
    'Extensions introduced in <a href="' . CURRENT_URL . '">Castle Game Engine</a> related to navigation.');

  $toc = new TableOfContents(
    array(
      new TocItem('Output events to generate camera matrix (<tt>Viewpoint.camera*Matrix</tt> events)', 'ext_viewpoint_camera_matrix'),
      new TocItem('Control head bobbing (<tt>KambiNavigationInfo.headBobbing*</tt> fields)', 'ext_head_bobbing'),
      new TocItem('Customize headlight (<tt>KambiNavigationInfo.headlightNode</tt>)', 'ext_headlight'),
      new TocItem('Specify blending sort (<tt>NavigationInfo.blendingSort</tt>)', 'ext_headlight'),
      new TocItem('Specify octree properties (node <tt>KambiOctreeProperties</tt>, various fields <tt>octreeXxx</tt>)', 'ext_octree_properties'),
      new TocItem('DEPRECATED: Force VRML time origin to be 0.0 at load time (<tt>KambiNavigationInfo.timeOriginAtLoad</tt>)', 'ext_time_origin_at_load'),
      new TocItem('DEPRECATED: Fields <tt>direction</tt> and <tt>up</tt> and <tt>gravityUp</tt> for <tt>PerspectiveCamera</tt>, <tt>OrthographicCamera</tt> and <tt>Viewpoint</tt> nodes', 'ext_cameras_alt_orient'),
    ));
  $toc->echo_numbers = true;
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>To every viewpoint node (this applies to all viewpoints usable
in our engine, including all <tt>X3DViewpointNode</tt> descendants,
like <tt>Viewpoint</tt> and <tt>OrthoViewpoint</tt>, and even to
VRML 1.0 <tt>PerspectiveCamera</tt> and <tt>OrthographicCamera</tt>)
we add output events that provide you with current camera matrix.
One use for such matrices is to route them to your GLSL shaders (as
uniform variables), and use inside the shaders to transform between
world and camera space.</p>

<?php
  echo node_begin('*Viewpoint');
  echo
  node_dots('all normal *Viewpoint fields') .
  node_field('SFMatrix4f', '[out]', 'cameraMatrix', '') .
  node_field('SFMatrix4f', '[out]', 'cameraInverseMatrix', '') .
  node_field('SFMatrix3f', '[out]', 'cameraRotationMatrix', '') .
  node_field('SFMatrix3f', '[out]', 'cameraRotationInverseMatrix', '') .
  node_field('SFBool', '[in,out]', 'cameraMatrixSendAlsoOnOffscreenRendering', 'FALSE') .
  node_end();
?>

<p><tt>"cameraMatrix"</tt> transforms from world-space (global 3D space
that we most often think within) to camera-space (aka eye-space;
when thinking within this space, you know then that the camera
position is at (0, 0, 0), looking along -Z, with up in +Y).
It takes care of both the camera position and orientation,
so it's 4x4 matrix.
<tt>"cameraInverseMatrix"</tt> is simply the inverse of this matrix,
so it transforms from camera-space back to world-space.</p>

<p><tt>"cameraRotationMatrix"</tt> again
transforms from world-space to camera-space, but now it only takes
care of camera rotations, disregarding camera position. As such,
it fits within a 3x3 matrix (9 floats), so it's smaller than full
<tt>cameraMatrix</tt> (4x4, 16 floats).
<tt>"cameraRotationInverseMatrix"</tt> is simply it's inverse.
Ideal to transform directions
between world- and camera-space in shaders.</p>

<p><tt>"cameraMatrixSendAlsoOnOffscreenRendering"</tt> controls
when the four output events above are generated.
The default (<tt>FALSE</tt>) behavior is that they are generated only
for camera that corresponds to the actual viewpoint, that is: for the
camera settings used when rendering scene to the screen.
The value <tt>TRUE</tt> causes the output matrix events to be generated
also for temporary camera settings used for off-screen rendering
(used when generating textures for <tt>GeneratedCubeMapTexture</tt>,
<tt>GeneratedShadowMap</tt>, <tt>RenderedTexture</tt>). This is a little
dirty, as cameras used for off-screen rendering do not (usually) have
any relation to actual viewpoint (for example, for
<tt>GeneratedCubeMapTexture</tt>, camera is positioned in the middle
of the shape using the cube map). But this can be useful: when you route
these events straight to the shaders, you usually need in shaders "actual
camera" (which is not necessarily current viewpoint camera) matrices.</p>

<p>These events are usually generated only by the currently bound viewpoint node.
The only exception is when you use <tt>RenderedTexture</tt>
and set something in <tt>RenderedTexture.viewpoint</tt>:
in this case, <tt>RenderedTexture.viewpoint</tt> will generate appropriate
events (as long as you set <tt>cameraMatrixSendAlsoOnOffscreenRendering</tt>
to <tt>TRUE</tt>). Conceptually, <tt>RenderedTexture.viewpoint</tt>
is temporarily bound (although it doesn't send isBound/bindTime events).

<?php echo $toc->html_section(); ?>

<p><i>"Head bobbing"</i> is the effect of camera moving slightly up
and down when you walk on the ground (when gravity works).
This simulates our normal human vision &mdash; we can't usually keep
our head at the exact same height above the ground when walking
or running :)
By default our engine does head bobbing (remember, only when gravity
works; that is when the navigation mode is <tt>WALK</tt>).
This is common in FPS games.</p>

<p>Using the extensions below you can tune (or even turn off)
the head bobbing behavior. For this we add new fields to the
<tt>KambiNavigationInfo</tt> node (introduced in the previous section,
can be simply used instead of the standard <tt>NavigationInfo</tt>).</p>

<?php
  echo node_begin('KambiNavigationInfo : NavigationInfo');
  $node_format_fd_name_pad = 22;
  echo
  node_dots('all normal NavigationInfo fields, and KambiNavigationInfo fields documented previously') .
  node_field('SFFloat', '[in,out]', 'headBobbing', '0.02') .
  node_field('SFFloat', '[in,out]', 'headBobbingTime', '0.5') .
  node_end();
?>

<p>Intuitively, <tt>headBobbing</tt> is the intensity of the whole effect
(0 = no head bobbing) and <tt>headBobbingTime</tt> determines
the time of a one step of a walking human.</p>

<p>The field <tt>headBobbing</tt> multiplied by the avatar height specifies how far
the camera can move up and down. The avatar height is taken from
the standard <tt>NavigationInfo.avatarSize</tt> (2nd array element).
Set this to exact 0 to disable head bobbing.
This must always be &lt; 1. For sensible effects, this should
be something rather close to 0.

<small>(<?php api_link('Developers: see also TWalkCamera.HeadBobbing property.', 'CastleCameras.TWalkCamera.html#HeadBobbing'); ?>)</small></p>

<p>The field <tt>headBobbingTime</tt> determines how much time passes
to make full head bobbing sequence (camera swing up and then down back to original height).

<small>(<?php api_link('Developers: see also TWalkCamera.HeadBobbingTime property.', 'CastleCameras.TWalkCamera.html#HeadBobbingTime'); ?>)</small></p>

<?php echo $toc->html_section(); ?>

<?php
echo castle_thumbs(array(
  array('filename' => "headlight_per_pixel.png", 'titlealt' => 'Spot headlight with per-pixel lighting'),
  array('filename' => 'castle_headlight_1.png', 'titlealt' => 'Castle level with sharp spot headlight'),
  array('filename' => 'castle_headlight_2.png', 'titlealt' => 'Castle level with smooth spot headlight'),
  array('filename' => 'castle_headlight_3.png', 'titlealt' => 'Castle level with smooth spot headlight'),
));
?>

<p>You can configure the appearance of headlight by the <tt>headlightNode</tt>
field of <tt>KambiNavigationInfo</tt> node.
<tt>KambiNavigationInfo</tt> is just a replacement of standard
<tt>NavigationInfo</tt>, adding some extensions specific to our engine.

<?php echo node_begin("KambiNavigationInfo : NavigationInfo");
  $node_format_fd_type_pad = 5;
  $node_format_fd_name_pad = 25;
  $node_format_fd_def_pad = 6;

  echo
  node_dots('all KambiNavigationInfo fields so far') .
  node_field('SFNode', '[in,out]', "headlightNode", "NULL", "[X3DLightNode]") .
  node_end();
?>

<p><tt>headlightNode</tt> defines the type and properties of the
light following the avatar ("head light"). You can put any
valid X3D light node here. If you don't give anything here (but still
request the headlight by <tt>NavigationInfo.headlight = TRUE</tt>,
which is the default) then the default <tt>DirectionalLight</tt>
will be used for headlight.

<ul>
  <li><p>Almost everything (with the exceptions listed below)
    works as usual for all the light sources.
    Changing colors and intensity obviously work.
    Changing the light type, including making it a spot light
    or a point light, also works.

    <p>Note that for nice spot headlights, you will usually want to
    <?php echo a_href_page_hashlink('enable per-pixel lighting
    on everything by View-&gt;Shaders-&gt;Enable For Everything',
    'x3d_implementation_lighting', 'section_per_pixel_lighting'); ?>.
    Otherwise the ugliness of default fixed-function Gouraud shading
    will be visible in case of spot lights (you will see how
    the spot shape "crawls" on the triangles,
    instead of staying in a nice circle).
    So to see the spot light cone perfectly, and also to see
    <tt>SpotLight.beamWidth</tt> perfectly,
    enable per-pixel shader lighting.

    <p>Note that instead of setting headlight to spot, you may also
    consider cheating: you can create a screen effect that simulates
    the headlight. See view3dscene <i>"Screen Effects -&gt; Headlight"</i>
    for demo, and <?php echo a_href_page('screen effects documentation',
    'x3d_extensions_screen_effects'); ?> for ways to create
    this yourself. This is an entirely different beast, more cheating
    but also potentially more efficient (for starters, you don't have
    to use per-pixel lighting on everything to make it nicely round).

  <li><p>Your specified <tt>"location"</tt> of the light (if you put here <tt>PointLight</tt>
    or <tt>SpotLight</tt>) will be ignored.
    Instead we will synchronize light location in each frame
    to the player's location
    (in world coordinates)<!--, TODO: modified by the
    headlightMove (default zero, see below)-->.

    <p>You can ROUTE your light's location to something, to see it changing.
    <!-- TODO: test. This is useful to visualize headlight location,
    together with headlightMove. -->

  <li><p>Similarly, your specified <tt>"direction"</tt> of the light
    (if this is <tt>DirectionalLight</tt> or <tt>SpotLight</tt>)
    will be ignored. Instead we will keep it synchronized
    with the player's normalized direction
    (in world coordinates). You can ROUTE this direction to see it changing.

 <li><p>The <tt>"global"</tt> field doesn't matter.
   Headlight always shines on everything, ignoring normal VRML/X3D
   light scope rules.

 <!--
   TODO: test
   Even the <a>"shadows" and related fields</a> on the light will work,
   so the headlight *can* cast a shadow.
   Yes, this makes sense and is even nicely noticeable in some settings.
   Try orthographic camera with a shadow-casting spot light,
   with this you will usually see the shadow.

   TODO: You'll usually want to set headlightTranslation to
   something small but non-zero, to add some translation
   (with respect to the camera position) to the headlight.
   This allows you to move the light source slightly with respect
   to the camera, which is a nice way of making the shadows from
   headlight visible. It is also useful to simulate e.g.
   a torch &mdash; a torch is probably held by a single hand
   (so it's more to the left or right side) and slightly above the head.
   Non-zero headlightTranslation allows to trivially simulate this &mdash;
   a light source that is close, but not precisely at, the player position.

   TODO: also headlightOrientation?
 -->
</ul>

<p><i>History</i>: We used to configure headlight by different,
specialized node. This is still parsed but ignored in new versions:

<?php
  echo node_begin('KambiHeadLight : X3DChildNode');
  $node_format_fd_name_pad = 20;
  echo
  node_field('SFFloat', '[in,out]', 'ambientIntensity', '0', '[0.0, 1.0]') .
  node_field('SFVec3f', '[in,out]', 'attenuation'     , '1 0 0', '[0, infinity)') .
  node_field('SFColor', '[in,out]', 'color'           , '1 1 1', '[0, 1]') .
  node_field('SFFloat', '[in,out]', 'intensity'       , '1', '[0, 1]') .
  node_field('SFBool', '[in,out]', 'spot'            , 'FALSE') .
  node_field('SFFloat', '[in,out]', 'spotDropOffRate' , 0) .
  node_field('SFFloat', '[in,out]', 'spotCutOffAngle' , '&pi;/4') .
  node_end();
?>

<?php echo $toc->html_section(); ?>

<?php
  echo node_begin('NavigationInfo') .
  node_dots() .
  node_field('SFString', '[in,out]', 'blendingSort', 'DEFAULT', '["DEFAULT", "NONE", "2D", "3D"]') .
  node_end();
?>

<p>Values other than "DEFAULT" force specific blending sort
treatment when rendering, which is useful since some scenes
sometimes have specific requirements to be rendered sensibly.
See <?php api_link('TBlendingSort', 'CastleScene.html#TBlendingSort'); ?>.

<?php echo $toc->html_section(); ?>

<?php
echo castle_thumbs(array(
  array('filename' => "octree_hello_world_shadow.png", 'titlealt' => 'Octree visualization'),
));
?>

<p>Like most 3D engines, <i>Castle Game Engine</i> uses a smart
tree structure to handle collision detection in arbitrary 3D worlds.
The structure used in our engine is the <i>octree</i>, with a couple
of special twists to handle dynamic scenes. See
<a href="<?php echo CURRENT_URL; ?>vrml_engine_doc/output/xsl/html/chapter.octree.html">documentation
chapter "octrees" for more explanation</a>.</p>

<p>There are some limits that determine how fast the octree is constructed,
how much memory does it use,
and how fast can it answer collision queries. While our programs have
sensible and tested defaults hard-coded, it may be useful (or just
interesting for programmers) to test other limits &mdash; this is
what this extension is for.</p>

<p><i>In all honesty, I (Michalis) do not
expect this extension to be commonly used... It allows you to
tweak an important, but internal, part of the engine. For most normal people,
this extension will probably look like an uncomprehensible black magic.
And that's Ok, as the internal defaults used in our engine really
suit (almost?) all practical uses.</i></p>

<p>If the above paragraph didn't scare you, and you want to know more
about octrees in our engine: besides
<a href="<?php echo CURRENT_URL; ?>vrml_engine_doc/output/xsl/html/chapter.octree.html">documentation
chapter "octrees"</a> you can
also take a look at the (source code and docs) of the
<?php api_link('TCastleSceneCore.Spatial', 'CastleSceneCore.TCastleSceneCore.html#Spatial'); ?> property.

<p>A new node:

<?php echo node_begin("KambiOctreeProperties : X3DNode");
  $node_format_fd_type_pad = 5;
  $node_format_fd_name_pad = 20;
  $node_format_fd_def_pad = 6;

  echo
  node_field('SFInt32', '[]', "maxDepth", "-1", "must be &gt;= -1") .
  node_field('SFInt32', '[]', "leafCapacity", "-1", "must be &gt;= -1") .
  node_end();
?>

<p>Limit <tt>-1</tt> means to use the default value hard-coded in the program.
Other values force the generation of octree with given limit.
For educational purposes, you can make an experiment and try
maxDepth = 0: this forces a one-leaf tree, effectively
making octree searching work like a normal linear searching.
You should see a dramatic loss of game speed on non-trivial models then.</p>

<p>To affect the scene octrees you can place <tt>KambiOctreeProperties</tt>
node inside <tt>KambiNavigationInfo</tt> node. For per-shape
octrees, we add new fields to <tt>Shape</tt> node:</p>

<?php echo node_begin("KambiNavigationInfo : NavigationInfo");
  $node_format_fd_type_pad = 5;
  $node_format_fd_name_pad = 25;
  $node_format_fd_def_pad = 6;

  echo
  node_dots('all KambiNavigationInfo fields so far') .
  node_field('SFNode', '[]', "octreeRendering", "NULL", "only KambiOctreeProperties node") .
  node_field('SFNode', '[]', "octreeDynamicCollisions", "NULL", "only KambiOctreeProperties node") .
  node_field('SFNode', '[]', "octreeVisibleTriangles", "NULL", "only KambiOctreeProperties node") .
  node_field('SFNode', '[]', "octreeCollidableTriangles", "NULL", "only KambiOctreeProperties node") .
  node_end();
?>

<?php echo node_begin("X3DShapeNode (e.g. Shape)");
  $node_format_fd_type_pad = 5;
  $node_format_fd_name_pad = 25;
  $node_format_fd_def_pad = 6;

  echo
  node_dots('all normal X3DShapeNode fields') .
  node_field('SFNode', '[]', "octreeTriangles", "NULL", "only KambiOctreeProperties node") .
  node_end();
?>

<p>See the API documentation for classes <tt>TCastleSceneCore</tt> and <tt>TShape</tt>
for precise description about what each octree is.
In normal simulation of dynamic 3D scenes,
we use only <tt>octreeRendering</tt>, <tt>octreeDynamicCollisions</tt> and
<tt>Shape.octreeTriangles</tt> octrees. Ray-tracers usually use
<tt>octreeVisibleTriangles</tt>.</p>

<p>We will use scene octree properties from the first bound
<tt>NavigationInfo</tt> node (see VRML/X3D specifications
about the rules for bindable nodes). If this node is not
<tt>KambiNavigationInfo</tt>, or appropriate <tt>octreeXxx</tt> field
is <tt>NULL</tt>, or appropriate field within <tt>KambiOctreeProperties</tt>
is <tt>-1</tt>, then the default hard-coded limit will be used.

<p>Currently, it's not perfectly specified what happens to octree limits
when you bind other <tt>[Kambi]NavigationInfo</tt> nodes during the game.
With current implementation, this <i>will</i> cause the limits to change,
but they will be actually applied only when the octree will be rebuild
&mdash; which may happen never, or only at some radical rebuild of
VRML graph by other events. So if you have multiple
<tt>[Kambi]NavigationInfo</tt> nodes in your world, I advice to
specify in all of them exactly the same <tt>octreeXxx</tt> fields values.

<?php echo $toc->html_section(); ?>

<p>By default, VRML/X3D time origin is at <i>00:00:00 GMT January 1, 1970</i>
and <tt>SFTime</tt> reflects real-world time (taken from your OS).
<?php echo a_href_page('This is somewhat broken idea in my opinion',
'x3d_time_origin_considered_uncomfortable'); ?>, unsuitable
for normal single-user games. So you can change this by using
<tt>KambiNavigationInfo</tt> node:

<?php
  echo node_begin('KambiNavigationInfo : NavigationInfo');
  $node_format_fd_name_pad = 18;
  echo
  node_dots('all normal NavigationInfo fields') .
  node_field('SFBool', '[]', 'timeOriginAtLoad', 'FALSE') .
  node_end();
?>

<p>The default value, <tt>FALSE</tt>, means the standard VRML behavior.
When <tt>TRUE</tt> the time origin for this VRML scene is considered
to be 0.0 when browser loads the file. For example this means that you can
easily specify desired <tt>startTime</tt> values for time-dependent nodes
(like <tt>MovieTexture</tt> or <tt>TimeSensor</tt>)
to start playing at load time, or a determined number of seconds
after loading of the scene.

<?php echo $toc->html_section(); ?>

<p>Standard VRML way of specifying camera orientation
(look direction and up vector) is to use <tt>orientation</tt> field
that says how to rotate standard look direction vector (&lt;0,0,-1&gt;)
and standard up vector (&lt;0,1,0&gt;). While I agree that this
way of specifying camera orientation has some advantages
(e.g. we don't have the problem with the uncertainty
"<i>Is look direction vector length meaningful ?</i>")
I think that this is very uncomfortable for humans.

<p>Reasoning:
<ol>
  <li>It's very difficult to write such <tt>orientation</tt> field
    by human, without some calculator. When you set up
    your camera, you're thinking about "<i>In what direction it looks ?</i>"
    and "<i>Where is my head ?</i>", i.e. you're thinking
    about look and up vectors.

  <li>Converting between <tt>orientation</tt> and look and up
    vectors is trivial for computers but quite hard for humans
    without a calculator (especially if real-world values are
    involved, that usually don't look like "nice numbers").
    Which means that when I look at source code of your VRML
    camera node and I see your <tt>orientation</tt> field
    &mdash; well, I still have no idea how your camera is oriented.
    I have to fire up some calculating program, or one
    of programs that view VRML (like view3dscene).
    This is not some terrible disadvantage, but still it matters
    for me.

  <li><tt>orientation</tt> is written with respect to standard
    look (&lt;0,0,-1&gt;) and up (&lt;0,1,0&gt;) vectors.
    So if I want to imagine camera orientation in my head &mdash;
    I have to remember these standard vectors.

  <li>4th component of orientation is in radians, that
    are not nice for humans (when specified as floating point
    constants, like in VRMLs, as opposed to multiplies of &pi;,
    like usually in mathematics). E.g. what's more obvious for you:
    "<i>1.5707963268 radians</i>" or "<i>90 degrees</i>" ? Again, these are equal
    for computer, but not readily equal for human
    (actually, "<i>1.5707963268 radians</i>" is not precisely equal to
    "<i>90 degrees</i>").
</ol>

<p>Also, VRML 2.0 spec says that the gravity upward vector should
be taken as +Y vector transformed by whatever transformation is applied
to <tt>Viewpoint</tt> node. This also causes similar problems,
since e.g. to have gravity upward vector in +Z you have to apply
rotation to your <tt>Viewpoint</tt> node.

<p>So I decided to create new fields for <tt>PerspectiveCamera</tt>,
<tt>OrthographicCamera</tt> and <tt>Viewpoint</tt>
nodes to allow alternative way to specify
an orientation:
<?php echo node_begin("PerspectiveCamera / OrthographicCamera / Viewpoint");
  echo
  node_dots('all normal *Viewpoint fields') .
  node_field('MFVec3f', '[in,out]', "direction",  "[]") .
  node_field('MFVec3f', '[in,out]', "up", "[]") .
  node_field('SFVec3f', '[in,out]', "gravityUp", "0 1 0") .
  node_end();
?>

<p>If at least one vector in <tt>direction</tt> field
is specified, then this is taken as camera look vector.
Analogous, if at least one vector in <tt>up</tt> field
is specified, then this is taken as camera up vector.
This means that if you specify some vectors for
<tt>direction</tt> and <tt>up</tt> then the value of
<tt>orientation</tt> field is ignored.
<tt>direction</tt> and <tt>up</tt> fields should have
either none or exactly one element.

<p>As usual, <tt>direction</tt> and <tt>up</tt> vectors
can't be parallel and can't be zero.
They don't have to be orthogonal &mdash; <tt>up</tt> vector will be
always silently corrected to be orthogonal to <tt>direction</tt>.
Lengths of these vectors are always ignored.
<!--
(m.in. dlatego że w standardowym VRMLu nie można przy
pomocy <tt>orientation</tt> ustalać długości tych wektorów, ale także dlatego
że tak jest wygodniej, zazwyczaj byłoby to raczej uciążliwe niż
funkcjonalne gdyby w jakiś sposób robić coś inaczej w zależnosci od
długości tych wektorow; także dlatego że jest w VRMLowej kamerze
pole <tt>focalDistance</tt> slużące własnie do robienia rzeczy które
móglbyś chcieć zrobić na podstawie dlugości wektora <tt>direction</tt>).
-->

<p>As for gravity: VRML 2.0 spec says to take standard +Y vector
and transform it by whatever transformation was applied to
<tt>Viewpoint</tt> node. So we modify this to say
<i>take <tt>gravityUp</tt> vector
and transform it by whatever transformation was applied to
<tt>Viewpoint</tt> node</i>. Since the default value for
<tt>gravityUp</tt> vector is just +Y, so things work 100% conforming
to VRML spec if you don't specify <tt>gravityUp</tt> field.

<p>In <?php echo a_href_page("view3dscene", "view3dscene") ?>
 "<i>Print current camera node</i>" command (key shortcut Ctrl+C)
writes camera node in both versions &mdash; one that uses
<tt>orientation</tt> field and transformations to get gravity upward vector,
and one that uses <tt>direction</tt> and <tt>up</tt> and <tt>gravityUp</tt>
fields.

 <!-- funkcje X3DFields.CamDirUp2Orient i CastleVectors.RotatePointAroundAxis -->

<?php
  x3d_status_footer();
?>