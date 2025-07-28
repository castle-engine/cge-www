<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Navigation', 'navigation',
    'This component defines nodes to control the camera,
     and some effects closely related to the camera.
     <code>NavigationInfo</code> controls the camera behavior.
     <code>Viewpoint</code> and <code>OrthoViewpoint</code> define
     the initial camera position and rotation, and may be used to animate
     the camera. <code>LOD</code> allows to implement level-of-detail,
     where different versions of some object are displayed depending
     on the camera distance. <code>Collision</code> allows to use a simpler
     geometry for collision purposes, or even to turn the collisions off
     (like a fake wall hiding a secret room in games).
     <code>Billboard</code> may be used to create sprites, as it aligns
     geometry (flat or not) with respect to the camera.
     ');

  $toc = new TableOfContents(
    array(
      new TocItem('Demos', 'demos'),
      new TocItem('Supported nodes', 'support'),
    ));
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<p>See also <?php echo a_href_page('Castle Game Engine (and Castle Model Viewer) extensions related to navigation','x3d_implementation_navigation_extensions'); ?>.

<?php echo $toc->html_section(); ?>

<p>For demos and tests of these features,
see the <code>navigation</code> subdirectory inside <?php
echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>.</p>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p><?php echo x3d_node_link('Viewpoint'); ?>,<br>
    <?php echo x3d_node_link('OrthoViewpoint'); ?>.

    <p>These nodes define the initial camera position, rotation, and field of view.
    The <code>Viewpoint</code> makes a perspective projection (objects further away are smaller),
    <code>OrthoViewpoint</code> makes an orthographic projection.

    <p>You can even animate their transformation and properties, to animate the camera.

    <p>Note: In <i>Castle Game Engine</i> you can also control the camera by Pascal code,
    using the <code>Viewport.Camera</code> property that keeps a
    <?php echo cgeRef('TCastleCamera'); ?>
    instance.
    See <a href="camera">manual about controlling the camera</a>.
    Still, this X3D node is useful to set initial camera through e.g. <a href="blender">Blender</a>
    (you can export to glTF or X3D and importing them will set initial camera properly).

    <p><a href="castle-model-viewer">Castle Model Viewer</a>
    displays a nice menu allowing you to jump
    to any defined viewpoint, showing viewpoints' descriptions.
    Animating viewpoint's position and orientation
    (directly or by animating it's parent transformation) also works perfectly.</p>

    <p>Note that even merely adding a <code>Viewpoint</code> (or <code>OrthoViewpoint</code>) node to the X3D model, with everything at default, still changes how <a href="castle-model-viewer">Castle Model Viewer</a> and CGE interpret the model.

    <ul>
      <li>
        <p>Without the <code>Viewpoint</code> (or <code>OrthoViewpoint</code>), we determine center of rotation (important for proper <i>"Examine"</i> work) based on scene contents (bounding box middle).
      <li>
        <p>With <code>Viewpoint</code> (or <code>OrthoViewpoint</code>), we assume you have set proper <code>centerOfRotation</code>. The default <code>centerOfRotation</code> is just (0,0,0). This follows X3D standard.

        <p>You can set <code>Viewport.autoCenterOfRotation</code> (or <code>OrthoViewport.autoCenterOfRotation</code>) to <code>true</code> to explicitly request to automatically determine (based on bounding box middle) this center of rotation.
        This field is a CGE extension.
    </ul>

    <p>TODO: We support most, but not all, X3D fields.
    Fields not implemented yet: <code>jump</code>, <code>retainUserOffsets</code>.

    <ul>
      <li><p>As for animating viewpoints (and also possible <code>jump</code>, <code>retainUserOffsets</code> implementation):

        <p>We do not support right now the notion of preserving user offsets
        from navigation. Tracking them in the past required to have a special
        treatment of camera transformation (<i>initial</i> vs <i>current</i>),
        but it was too complicated too keep (as we wanted to make
        <code>TCastleCamera</code> descend from <code>TCastleTransform</code>)
        and it didn't seem useful for authors in reality. (Of course please report
        if it is useful for you!)

        <p>When you animate the viewpoint (either by animating viewpoint position/orientation
        or by animating viewpoint transformation) right now we just change
        the current camera to match the viewpoint. User offsets are not retained.
    </ul>

  <li><p><?php echo x3d_node_link('NavigationInfo'); ?></p>

    <p>Controls the navigation behavior (mode of movement, gravity, etc.) and the headlight.

    <p>Note: In <i>Castle Game Engine</i> you can also control the navigation by Pascal code.
    See <a href="navigation">manual about navigation</a>.
    And you can control the headlight by just adding lights as camera children.
    Still, this node is useful to define defaults.

    <p>Details about supported fields:

    <ul>
      <li><p><code>avatarSize</code> is honoured fully:
        <ol>
          <li><p>First <code>avatarSize</code> item is the camera radius.
            If you use <a href="castle-model-viewer">Castle Model Viewer</a>,
            note that <code>--camera-radius</code> command-line option overrides
            this value.

          <li><p>2nd <code>avatarSize</code> item is the preferred height
            above the ground.

          <li><p>3rd <code>avatarSize</code> item
            is the tallest object over which you can climb.

            <p>If this is missing (or it has value &lt;= 0) then there's no such
            limit, and you can climb as long as you can move forward.
            So you can climb the stairs with steps
            almost as high as your own height minus the camera radius.
            Simplifying (ignoring other effects, like head bobbing),
            you can say that avatarSize[2] is by default like
            avatarSize[1] - avatarSize[0]).

            <p>See TCastleWalkNavigation.ClimbHeight API docs for more details about this.
        </ol>

      <li><p><code>speed</code> field is supported, it sets
        the speed in meters/second. Speed = 0.0 is also correctly
        honored (user will not be able to move in Walk/Fly modes,
        only to rotate).

      <li><p><code>type</code> field is supported.

        <p>Navigation types supported are:

        <ul>
          <li><p><code>EXAMINE</code></li>

          <li><p><code>WALK</code></li>

          <li><p><code>FLY</code></li>

          <li><p><code>NONE</code>. This navigation method means that user <i>cannot</i> change
            the camera using navigation controls (keys, mouse, game controller, 3D mouse...).
            You can still change the camera from code (like Pascal or CastleScript)
            or by animating transformations (that are camera parents)
            or camera parameters.
          </li>

          <li><p><code>2D</code>. A navigation constrained to 2D (you cannot make free 3D rotation).

            <p>This is an extension in <i>Castle Game Engine</i>,

            <p> This is also activated by X_ITE-compatible names <code>PLANE</code>, <code>PLANE_create3000.github.io</code>, <code>PLANE_create3000.de</code>.</li>

          <li><p><code>TURNTABLE</code>. Similar to the Examine mode, with controls comfortable
            for viewing models that have a sense of floor/ground in the XZ plane,
            and vertical axis in +Y.

            <p>This is an extension in <i>Castle Game Engine</i>.
            Implementation is not finished yet.

          <li><p><code>ANY</code>. Supported by the engine, but simply ignored by the <a href="castle-model-viewer">Castle Model Viewer</a>. Why?

            <p>The specification says that users should be able to choose
            any navigation type they want only if the <code>ANY</code> name is present in the navigation types list.
            For <a href="castle-model-viewer">Castle Model Viewer</a>,
            we decided to allow users to <i>always</i> change the navigation
            method, regardless of whether <code>ANY</code> name is used.
            This makes more sense for us as a general 3D model viewer
            that enables users to freely inspect 3D models.

            <p>You can develop your own model viewers using <i>Castle Game Engine</i> that honor this more strictly, i.e. hide the UI to change navigation when <code>ANY</code> is not used.
          </li>
        </ul>

        <p>Inside the engine, the navigation paradigm is actually a little
        more flexible. You can fine-tune the rotations and gravity
        behavior by <a href="castle-model-viewer">Castle Model Viewer</a>
        menu "<i>Navigation -> Walk and Fly Settings</i>".
        <!--
        For example, the difference between <code>Walk</code> and <code>Fly</code>
        is not only that the gravity if off when flying.
        It's also that in fly mode, you rotate around <i>current</i> up vector,
        not around the <i>gravity</i> up vector,
        -->

      <li><p>Nice transitions between viewpoints are supported,
        honouring <code>transitionType</code>, <code>transitionTime</code> fields and <code>transitionComplete</code> event.
        See <a href="https://github.com/castle-engine/demo-models/blob/master/navigation/transition_multiple_viewpoints.x3dv">demo model navigation/transition_multiple_viewpoints.x3dv showing how to use it to make an animated transition between a couple of viewpoints</a>.
        </p>
    </ul>

    <p>Binding different <code>NavigationInfo</code> nodes,
    and changing their exposed fields by events, of course works.</p>

    <p>When no <code>NavigationInfo</code> node is present in the scene:
    <ul>
      <li><code>avatarSize</code> and <code>speed</code>
        are calculated based on scene's bounding box sizes, to a values that
        will hopefully "feel right".
        We try to calculate them intelligently, because simply using
        <code>NavigationInfo</code> defaults results in bad experience
        in many scenes.</li>

      <li><code>headlight</code> behaves like true,
        <code>type</code> behaves like <code>[EXAMINE, ANY]</code>,
        this follows <code>NavigationInfo</code> defaults.</li>
    </ul>

    <p><i>TODO</i>: <code>visibilityLimit</code> may be ignored if shadow
    volumes are allowed (We use frustum with z-far in infinity then.)</p>

  <li><p><?php echo x3d_node_link('LOD'); ?>

    <p>Allows to define various versions of the same object,
    with varying level-of-detail.
    An appropriate child is automatically displayed based on the current
    distance to the camera.

    <p><i>Note:</i> Right now, the only thing that decides
    <i>"which level of detail should be used"</i>
    is the distance to the camera.
    Which means that only the supplied <code>LOD.range</code> controls which level is displayed.
    The <code>forceTransitions</code> value is simply ignored,
    and when <code>range</code> is empty, we simply always use the first
    (highest-detail) version.

    <p>Example: The simplest example is part of our <a href="demo_models.php">demo models</a>.
    View the X3D code here:
    <a href="https://github.com/castle-engine/demo-models/blob/master/navigation/lod_test.x3dv">navigation/lod_test.x3dv</a>.

  <li><p><?php echo x3d_node_link('Billboard'); ?></p>

    <p>The children of this node are automatically aligned with the camera.
    Two modes are possible: where the objects are rotated only around a specified axis,
    or where objects are rotated freely to match the camera.</p>

    <p>Example: The simplest example is part of our <a href="demo_models.php">demo models</a>.
    View the X3D code here:
    <a href="https://github.com/castle-engine/demo-models/blob/master/navigation/billboard_simple.x3dv">navigation/billboard_simple.x3dv</a>.

  <li><p><?php echo x3d_node_link('Collision'); ?></p>

    <p>Allows to define a simpler
    geometry for collision purposes, or even to turn the collisions off
    (like a fake wall hiding a secret room in games).

    <p>Most things work: grouping (<code>children</code> property, in particular),
    allows to control collision detection by honoring
    <code>enabled</code> (named <code>collide</code> in VRML 97) and <code>proxy</code>
    fields.

    <p><code>bboxCenter/Size</code> is currently simply ignored, our engine
    always calculates and updates the bounding boxes where needed.

    <p>TODO: collideTime and isActive out events are not implemented yet.

  <li><p><?php echo x3d_node_link('ViewpointGroup'); ?></p>

    <p>You can use this to create submenus in <i>"Navigation -> Jump To Viewpoint"</i> menu in
    <a href="castle-model-viewer">Castle Model Viewer</a>.
    Fields <code>description</code> and <code>children</code> are taken into account.
    Also, you can use this node to hide some viewpoints from the menu:
    the <code>displayed</code> field also works.</p>

    <p>TODO: size/center is not honored yet. Group is displayed
    regardless of camera position. A possible workaround could be
    to use a <code>ProximitySensor</code> node,
    routing <code>ProximitySensor.isActive</code> to the <code>displayed</code> field...
    Except this workaround will not work too, because changing
    of the <code>displayed</code> field after the scene loading
    doesn't change the menu for now.</p>

    <p>TODO: retainUserOffsets is ignored.</p>
  </li>
</ul>

<?php
  castle_footer();
?>
