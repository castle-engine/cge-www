<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Navigation', 'navigation',
    'This component defines nodes to control the camera,
     and some effects closely related to the camera.
     <code>NavigationInfo</code> controls the camera behavior.
     <code>Viewpoint</code> and <code>OrthoViewpoint</code> define
     the initial camera position and rotation, and may be used to animate
     the camera. <code>LOD</code> allows to implement level-of-detail,
     where different versions of 3D scene are displayed depending
     on the camera distance. <code>Collision</code> allows to use a simpler
     geometry for collision purposes, or even to turn the collisions off
     (like a fake walls hiding secret rooms in games).
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

<p>See also <?php echo a_href_page('Castle Game Engine (and view3dscene) extensions related to navigation','x3d_implementation_navigation_extensions'); ?>.

<?php echo $toc->html_section(); ?>

<p>For demos and tests of these features,
see the <code>navigation</code> subdirectory inside <?php
echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>.</p>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p><?php echo x3d_node_link('Viewpoint'); ?>,
    <?php echo x3d_node_link('OrthoViewpoint'); ?>.

    <p>Supported.

    <p><?php echo a_href_page("view3dscene", "view3dscene") ?>
    displays a nice menu allowing you to jump
    to any defined viewpoint, showing viewpoints' descriptions.
    Animating viewpoint's position and orientation
    (directly or by animating it's parent transformation) also works perfectly.</p>

    <p>TODO: fields not implemented yet: <code>jump</code>, <code>retainUserOffsets</code>, <code>centerOfRotation</code>.

    <ul>
      <li><p>Support for <code>centerOfRotation</code> already exists in the engine
        (<code>TExamineCamera.CenterOfRotation</code>),
        so it should be easy to finish it (copy X3D <code>Viewpoint.centerOfRotation</code>
        to <code>TExamineCamera.CenterOfRotation</code>).
        But we may have to treat centerOfRotation = 0,0,0
        as special value meaning "middle of the box", otherwise many models will
        have weird navigation because they lack correct centerOfRotation value
        (and the default is just zero).

        <p>Suggestions and reports how does this work in other VRML/X3D browsers
        are welcome.

      <li><p>Support for <code>jump</code> and <code>retainUserOffsets</code> is probably easy, but someone
        has to actually understand the X3D specification about them,
        and explain how they are related to each other (and to
        <code>NavigationInfo.transitionType</code>).
        Suggestions and testcases and reports how do they work in other
        VRML/X3D browsers are welcome.
    </ul>

  <li><p><?php echo x3d_node_link('NavigationInfo'); ?></p>

    <p>Supported.

    <ul>
      <li><p><code>avatarSize</code> is honoured fully:
        <ol>
          <li><p>First <code>avatarSize</code> item is the camera radius.
            If you use <?php echo a_href_page('view3dscene','view3dscene'); ?>,
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

            <p>See TWalkCamera.ClimbHeight API docs for more details about this.
        </ol>

      <li><p><code>speed</code> field is supported, it sets
        the speed in meters/second. Speed = 0.0 is also correctly
        honored (user will not be able to move in Walk/Fly modes,
        only to rotate).

      <li><code>type</code> field is supported.
        Navigation types fully supported are: <code>EXAMINE</code>, <code>WALK</code>,
        <code>FLY</code>, <code>NONE</code>.

        Inside the engine, the navigation paradigm is actually a little
        more flexible. You can fine-tune the rotations and gravity
        behavior by view3dscene <i>Navigation -> Walk and Fly Settings</i>
        menu.
        <!--
        For example, the difference between <code>Walk</code> and <code>Fly</code>
        is not only that the gravity if off when flying.
        It's also that in fly mode, you rotate around <i>current</i> up vector,
        not around the <i>gravity</i> up vector,
        -->

        <p>As an extension, we also support new navigation mode <code>TURNTABLE</code>.
        This is similar to the Examine mode, with controls comfortable
        for viewing models that have a sense of floor/ground in the XZ plane,
        and vertical axis in +Y. Implementation is not finished yet.

        <p>The presence of navigation type
        <code>ANY</code> is ignored by
        <?php echo a_href_page('view3dscene','view3dscene'); ?>.
        We always show controls to change navigation settings, hiding them
        feels harmful to user.

      <li><p>Nice transitions between viewpoints are supported,
        honouring <code>transitionType</code> and <code>transitionTime</code> fields,
        and (since view3dscene 3.13.0) making <code>transitionComplete</code> event.
        See <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/navigation/transition_multiple_viewpoints.x3dv">demo model navigation/transition_multiple_viewpoints.x3dv showing how to use it to make an animated transition between a couple of viewpoints</a>.
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

    <p><i>Note:</i> We do not have any automatic LOD calculation implemented now,
    which means that your supplied <code>range</code>, and only
    your supplied <code>range</code>, controls which LOD is chosen.
    This means that <code>forceTransitions</code> value is simply ignored,
    and when <code>range</code> is empty, we simply always use the first
    (highest-detail) version. This is Ok, spec allows this.

  <li><p><?php echo x3d_node_link('Billboard'); ?></p>

    <p>Works fully.</p>

  <li><p><?php echo x3d_node_link('Collision'); ?></p>

    <p>Most things work: grouping (<code>children</code> property, in particular),
    allows to control collision detection by honoring
    <code>enabled</code> (named <code>collide</code> in VRML 97) and <code>proxy</code>
    fields.

    <p><code>bboxCenter/Size</code> is currently simply ignored, our engine
    always calculates and updates the bounding boxes where needed.

    <p>TODO: collideTime and isActive out events are not implemented yet.

  <li><p><?php echo x3d_node_link('ViewpointGroup'); ?></p>

    <p>You can use them to create submenus in "Viewpoints" menu in
    <?php echo a_href_page('view3dscene','view3dscene'); ?>:
    <code>description</code> and <code>children</code> work.
    Also, you can use this to hide some viewpoints from the menu:
    <code>displayed</code> field works.</p>

    <p>TODO: size/center is not honored yet. Group is displayed
    regardless of camera position. A possible workarond could be
    to use a <code>ProximitySensor</code> node,
    routing <code>ProximitySensor.isActive</code> to the <code>displayed</code> field...
    Except this workaround will not work too, because changing
    of the <code>displayed</code> field after the scene loading
    doesn't change the menu for now.</p>

    <p>TODO: retainUserOffsets is ignored.</p>
  </li>
</ul>

<?php
  x3d_status_footer();
?>
