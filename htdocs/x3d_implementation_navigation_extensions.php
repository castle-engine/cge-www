<?php
  require_once 'x3d_implementation_common.php';
  require_once 'x3d_extensions_functions.php';
  x3d_extensions_header('Navigation', 'navigation', 'navigation',
    'Extensions introduced in <a href="' . CURRENT_URL . '">Castle Game Engine</a> related to navigation.');

  $toc = new TableOfContents(
    array(
      new TocItem('Output events to generate camera matrix (<code>Viewpoint.camera*Matrix</code> events)', 'ext_viewpoint_camera_matrix'),
      new TocItem('Control head bobbing (<code>KambiNavigationInfo.headBobbing*</code> fields)', 'ext_head_bobbing'),
      new TocItem('Customize headlight (<code>KambiNavigationInfo.headlightNode</code>)', 'ext_headlight'),
      new TocItem('Specify blending sort (<code>NavigationInfo.blendingSort</code>)', 'ext_blending_sort'),
      new TocItem('Specify octree properties (node <code>KambiOctreeProperties</code>, various fields <code>octreeXxx</code>)', 'ext_octree_properties'),
      new TocItem('DEPRECATED: Force VRML time origin to be 0.0 at load time (<code>KambiNavigationInfo.timeOriginAtLoad</code>)', 'ext_time_origin_at_load'),
      new TocItem('DEPRECATED: Fields <code>direction</code> and <code>up</code> and <code>gravityUp</code> for <code>PerspectiveCamera</code>, <code>OrthographicCamera</code> and <code>Viewpoint</code> nodes', 'ext_cameras_alt_orient'),
    ));
  $toc->echo_numbers = true;
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>To every viewpoint node (this applies to all viewpoints usable
in our engine, including all <code>X3DViewpointNode</code> descendants,
like <code>Viewpoint</code> and <code>OrthoViewpoint</code>, and even to
VRML 1.0 <code>PerspectiveCamera</code> and <code>OrthographicCamera</code>)
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

<p><code>"cameraMatrix"</code> transforms from world-space (global 3D space
that we most often think within) to camera-space (aka eye-space;
when thinking within this space, you know then that the camera
position is at (0, 0, 0), looking along -Z, with up in +Y).
It takes care of both the camera position and orientation,
so it's 4x4 matrix.
<code>"cameraInverseMatrix"</code> is simply the inverse of this matrix,
so it transforms from camera-space back to world-space.</p>

<p><code>"cameraRotationMatrix"</code> again
transforms from world-space to camera-space, but now it only takes
care of camera rotations, disregarding camera position. As such,
it fits within a 3x3 matrix (9 floats), so it's smaller than full
<code>cameraMatrix</code> (4x4, 16 floats).
<code>"cameraRotationInverseMatrix"</code> is simply it's inverse.
Ideal to transform directions
between world- and camera-space in shaders.</p>

<p><code>"cameraMatrixSendAlsoOnOffscreenRendering"</code> controls
when the four output events above are generated.
The default (<code>FALSE</code>) behavior is that they are generated only
for camera that corresponds to the actual viewpoint, that is: for the
camera settings used when rendering scene to the screen.
The value <code>TRUE</code> causes the output matrix events to be generated
also for temporary camera settings used for off-screen rendering
(used when generating textures for <code>GeneratedCubeMapTexture</code>,
<code>GeneratedShadowMap</code>, <code>RenderedTexture</code>). This is a little
dirty, as cameras used for off-screen rendering do not (usually) have
any relation to actual viewpoint (for example, for
<code>GeneratedCubeMapTexture</code>, camera is positioned in the middle
of the shape using the cube map). But this can be useful: when you route
these events straight to the shaders, you usually need in shaders "actual
camera" (which is not necessarily current viewpoint camera) matrices.</p>

<p>These events are usually generated only by the currently bound viewpoint node.
The only exception is when you use <code>RenderedTexture</code>
and set something in <code>RenderedTexture.viewpoint</code>:
in this case, <code>RenderedTexture.viewpoint</code> will generate appropriate
events (as long as you set <code>cameraMatrixSendAlsoOnOffscreenRendering</code>
to <code>TRUE</code>). Conceptually, <code>RenderedTexture.viewpoint</code>
is temporarily bound (although it doesn't send isBound/bindTime events).

<?php echo $toc->html_section(); ?>

<p><i>"Head bobbing"</i> is the effect of camera moving slightly up
and down when you walk on the ground (when gravity works).
This simulates our normal human vision &mdash; we can't usually keep
our head at the exact same height above the ground when walking
or running :)
By default our engine does head bobbing (remember, only when gravity
works; that is when the navigation mode is <code>WALK</code>).
This is common in FPS games.</p>

<p>Using the extensions below you can tune (or even turn off)
the head bobbing behavior. For this we add new fields to the
<code>KambiNavigationInfo</code> node (introduced in the previous section,
can be simply used instead of the standard <code>NavigationInfo</code>).</p>

<?php
  echo node_begin('KambiNavigationInfo : NavigationInfo');
  $node_format_fd_name_pad = 22;
  echo
  node_dots('all normal NavigationInfo fields, and KambiNavigationInfo fields documented previously') .
  node_field('SFFloat', '[in,out]', 'headBobbing', '0.02') .
  node_field('SFFloat', '[in,out]', 'headBobbingTime', '0.5') .
  node_end();
?>

<p>Intuitively, <code>headBobbing</code> is the intensity of the whole effect
(0 = no head bobbing) and <code>headBobbingTime</code> determines
the time of a one step of a walking human.</p>

<p>The field <code>headBobbing</code> multiplied by the avatar height specifies how far
the camera can move up and down. The avatar height is taken from
the standard <code>NavigationInfo.avatarSize</code> (2nd array element).
Set this to exact 0 to disable head bobbing.
This must always be &lt; 1. For sensible effects, this should
be something rather close to 0.

<small>(<?php api_link('Developers: see also TWalkCamera.HeadBobbing property.', 'CastleCameras.TWalkCamera.html#HeadBobbing'); ?>)</small></p>

<p>The field <code>headBobbingTime</code> determines how much time passes
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

<p>You can configure the appearance of headlight by the <code>headlightNode</code>
field of <code>KambiNavigationInfo</code> node.
<code>KambiNavigationInfo</code> is just a replacement of standard
<code>NavigationInfo</code>, adding some extensions specific to our engine.

<?php echo node_begin("KambiNavigationInfo : NavigationInfo");
  $node_format_fd_type_pad = 5;
  $node_format_fd_name_pad = 25;
  $node_format_fd_def_pad = 6;

  echo
  node_dots('all KambiNavigationInfo fields so far') .
  node_field('SFNode', '[in,out]', "headlightNode", "NULL", "[X3DLightNode]") .
  node_end();
?>

<p><code>headlightNode</code> defines the type and properties of the
light following the avatar ("head light"). You can put any
valid X3D light node here. If you don't give anything here (but still
request the headlight by <code>NavigationInfo.headlight = TRUE</code>,
which is the default) then the default <code>DirectionalLight</code>
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
    <code>SpotLight.beamWidth</code> perfectly,
    enable per-pixel shader lighting.

    <p>Note that instead of setting headlight to spot, you may also
    consider cheating: you can create a screen effect that simulates
    the headlight. See view3dscene <i>"Screen Effects -&gt; Headlight"</i>
    for demo, and <?php echo a_href_page('screen effects documentation',
    'x3d_extensions_screen_effects'); ?> for ways to create
    this yourself. This is an entirely different beast, more cheating
    but also potentially more efficient (for starters, you don't have
    to use per-pixel lighting on everything to make it nicely round).

  <li><p>Your specified <code>"location"</code> of the light (if you put here <code>PointLight</code>
    or <code>SpotLight</code>) will be ignored.
    Instead we will synchronize light location in each frame
    to the player's location
    (in world coordinates)<!--, TODO: modified by the
    headlightMove (default zero, see below)-->.

    <p>You can ROUTE your light's location to something, to see it changing.
    <!-- TODO: test. This is useful to visualize headlight location,
    together with headlightMove. -->

  <li><p>Similarly, your specified <code>"direction"</code> of the light
    (if this is <code>DirectionalLight</code> or <code>SpotLight</code>)
    will be ignored. Instead we will keep it synchronized
    with the player's normalized direction
    (in world coordinates). You can ROUTE this direction to see it changing.

 <li><p>The <code>"global"</code> field doesn't matter.
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

<p>Limit <code>-1</code> means to use the default value hard-coded in the program.
Other values force the generation of octree with given limit.
For educational purposes, you can make an experiment and try
maxDepth = 0: this forces a one-leaf tree, effectively
making octree searching work like a normal linear searching.
You should see a dramatic loss of game speed on non-trivial models then.</p>

<p>To affect the scene octrees you can place <code>KambiOctreeProperties</code>
node inside <code>KambiNavigationInfo</code> node. For per-shape
octrees, we add new fields to <code>Shape</code> node:</p>

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

<p>See the API documentation for classes <code>TCastleSceneCore</code> and <code>TShape</code>
for precise description about what each octree is.
In normal simulation of dynamic 3D scenes,
we use only <code>octreeRendering</code>, <code>octreeDynamicCollisions</code> and
<code>Shape.octreeTriangles</code> octrees. Ray-tracers usually use
<code>octreeVisibleTriangles</code>.</p>

<p>We will use scene octree properties from the first bound
<code>NavigationInfo</code> node (see VRML/X3D specifications
about the rules for bindable nodes). If this node is not
<code>KambiNavigationInfo</code>, or appropriate <code>octreeXxx</code> field
is <code>NULL</code>, or appropriate field within <code>KambiOctreeProperties</code>
is <code>-1</code>, then the default hard-coded limit will be used.

<p>Currently, it's not perfectly specified what happens to octree limits
when you bind other <code>[Kambi]NavigationInfo</code> nodes during the game.
With current implementation, this <i>will</i> cause the limits to change,
but they will be actually applied only when the octree will be rebuild
&mdash; which may happen never, or only at some radical rebuild of
VRML graph by other events. So if you have multiple
<code>[Kambi]NavigationInfo</code> nodes in your world, I advice to
specify in all of them exactly the same <code>octreeXxx</code> fields values.

<?php echo $toc->html_section(); ?>

<p>By default, VRML/X3D time origin is at <i>00:00:00 GMT January 1, 1970</i>
and <code>SFTime</code> reflects real-world time (taken from your OS).
<?php echo a_href_page('This is somewhat broken idea in my opinion',
'x3d_time_origin_considered_uncomfortable'); ?>, unsuitable
for normal single-user games. So you can change this by using
<code>KambiNavigationInfo</code> node:

<?php
  echo node_begin('KambiNavigationInfo : NavigationInfo');
  $node_format_fd_name_pad = 18;
  echo
  node_dots('all normal NavigationInfo fields') .
  node_field('SFBool', '[]', 'timeOriginAtLoad', 'FALSE') .
  node_end();
?>

<p>The default value, <code>FALSE</code>, means the standard VRML behavior.
When <code>TRUE</code> the time origin for this VRML scene is considered
to be 0.0 when browser loads the file. For example this means that you can
easily specify desired <code>startTime</code> values for time-dependent nodes
(like <code>MovieTexture</code> or <code>TimeSensor</code>)
to start playing at load time, or a determined number of seconds
after loading of the scene.

<?php echo $toc->html_section(); ?>

<p>Standard VRML way of specifying camera orientation
(look direction and up vector) is to use <code>orientation</code> field
that says how to rotate standard look direction vector (&lt;0,0,-1&gt;)
and standard up vector (&lt;0,1,0&gt;). While I agree that this
way of specifying camera orientation has some advantages
(e.g. we don't have the problem with the uncertainty
"<i>Is look direction vector length meaningful ?</i>")
I think that this is very uncomfortable for humans.

<p>Reasoning:
<ol>
  <li>It's very difficult to write such <code>orientation</code> field
    by human, without some calculator. When you set up
    your camera, you're thinking about "<i>In what direction it looks ?</i>"
    and "<i>Where is my head ?</i>", i.e. you're thinking
    about look and up vectors.

  <li>Converting between <code>orientation</code> and look and up
    vectors is trivial for computers but quite hard for humans
    without a calculator (especially if real-world values are
    involved, that usually don't look like "nice numbers").
    Which means that when I look at source code of your VRML
    camera node and I see your <code>orientation</code> field
    &mdash; well, I still have no idea how your camera is oriented.
    I have to fire up some calculating program, or one
    of programs that view VRML (like view3dscene).
    This is not some terrible disadvantage, but still it matters
    for me.

  <li><code>orientation</code> is written with respect to standard
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
to <code>Viewpoint</code> node. This also causes similar problems,
since e.g. to have gravity upward vector in +Z you have to apply
rotation to your <code>Viewpoint</code> node.

<p>So I decided to create new fields for <code>PerspectiveCamera</code>,
<code>OrthographicCamera</code> and <code>Viewpoint</code>
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

<p>If at least one vector in <code>direction</code> field
is specified, then this is taken as camera look vector.
Analogous, if at least one vector in <code>up</code> field
is specified, then this is taken as camera up vector.
This means that if you specify some vectors for
<code>direction</code> and <code>up</code> then the value of
<code>orientation</code> field is ignored.
<code>direction</code> and <code>up</code> fields should have
either none or exactly one element.

<p>As usual, <code>direction</code> and <code>up</code> vectors
can't be parallel and can't be zero.
They don't have to be orthogonal &mdash; <code>up</code> vector will be
always silently corrected to be orthogonal to <code>direction</code>.
Lengths of these vectors are always ignored.
<!--
(m.in. dlatego że w standardowym VRMLu nie można przy
pomocy <code>orientation</code> ustalać długości tych wektorów, ale także dlatego
że tak jest wygodniej, zazwyczaj byłoby to raczej uciążliwe niż
funkcjonalne gdyby w jakiś sposób robić coś inaczej w zależnosci od
długości tych wektorow; także dlatego że jest w VRMLowej kamerze
pole <code>focalDistance</code> slużące własnie do robienia rzeczy które
móglbyś chcieć zrobić na podstawie dlugości wektora <code>direction</code>).
-->

<p>As for gravity: VRML 2.0 spec says to take standard +Y vector
and transform it by whatever transformation was applied to
<code>Viewpoint</code> node. So we modify this to say
<i>take <code>gravityUp</code> vector
and transform it by whatever transformation was applied to
<code>Viewpoint</code> node</i>. Since the default value for
<code>gravityUp</code> vector is just +Y, so things work 100% conforming
to VRML spec if you don't specify <code>gravityUp</code> field.

<p>In <?php echo a_href_page("view3dscene", "view3dscene") ?>
 "<i>Print current camera node</i>" command (key shortcut Ctrl+C)
writes camera node in both versions &mdash; one that uses
<code>orientation</code> field and transformations to get gravity upward vector,
and one that uses <code>direction</code> and <code>up</code> and <code>gravityUp</code>
fields.

 <!-- funkcje X3DFields.CamDirUp2Orient i CastleVectors.RotatePointAroundAxis -->

<?php
  x3d_status_footer();
?>