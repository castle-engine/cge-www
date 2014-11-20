<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Navigation', 'navigation',
    'This component defines nodes to control the camera,
     and some effects closely related to the camera.
     <tt>NavigationInfo</tt> controls the camera behavior.
     <tt>Viewpoint</tt> and <tt>OrthoViewpoint</tt> define
     the initial camera position and rotation, and may be used to animate
     the camera. <tt>LOD</tt> allows to implement level-of-detail,
     where different versions of 3D scene are displayed depending
     on the camera distance. <tt>Collision</tt> allows to use a simpler
     geometry for collision purposes, or even to turn the collisions off
     (like a fake walls hiding secret rooms in games).
     <tt>Billboard</tt> may be used to create sprites, as it aligns
     geometry (flat or not) with respect to the camera.
     ');

  $toc = new TableOfContents(
    array(
      new TocItem('Demos', 'demos'),
      new TocItem('Supported nodes', 'support'),
    ));
  $toc->echo_numbers = true;
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<p>See also <?php echo a_href_page('Castle Game Engine (and view3dscene) extensions related to navigation','x3d_implementation_navigation_extensions'); ?>.

<?php echo $toc->html_section(); ?>

<p>For demos and tests of these features,
see the <tt>navigation</tt> subdirectory inside <?php
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

    <p>TODO: fields not implemented yet: <tt>jump</tt>, <tt>retainUserOffsets</tt>, <tt>centerOfRotation</tt>.

    <ul>
      <li><p>Support for <tt>centerOfRotation</tt> already exists in the engine
        (<tt>TExamineCamera.CenterOfRotation</tt>),
        so it should be easy to finish it (copy X3D <tt>Viewpoint.centerOfRotation</tt>
        to <tt>TExamineCamera.CenterOfRotation</tt>).
        But we may have to treat centerOfRotation = 0,0,0
        as special value meaning "middle of the box", otherwise many models will
        have weird navigation because they lack correct centerOfRotation value
        (and the default is just zero).

        <p>Suggestions and reports how does this work in other VRML/X3D browsers
        are welcome.

      <li><p>Support for <tt>jump</tt> and <tt>retainUserOffsets</tt> is probably easy, but someone
        has to actually understand the X3D specification about them,
        and explain how they are related to each other (and to
        <tt>NavigationInfo.transitionType</tt>).
        Suggestions and testcases and reports how do they work in other
        VRML/X3D browsers are welcome.
    </ul>

  <li><p><?php echo x3d_node_link('NavigationInfo'); ?></p>

    <p>Supported.

    <ul>
      <li><p><tt>avatarSize</tt> is honoured fully:
        <ol>
          <li><p>First <tt>avatarSize</tt> item is the camera radius.
            If you use <?php echo a_href_page('view3dscene','view3dscene'); ?>,
            note that <tt>--camera-radius</tt> command-line option overrides
            this value.

          <li><p>2nd <tt>avatarSize</tt> item is the preferred height
            above the ground.

          <li><p>3rd <tt>avatarSize</tt> item
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

      <li><p><tt>speed</tt> field is supported, it sets
        the speed in meters/second. Speed = 0.0 is also correctly
        honored (user will not be able to move in Walk/Fly modes,
        only to rotate).

      <li><tt>type</tt> field is supported.
        Navigation types fully supported are: <tt>EXAMINE</tt>, <tt>WALK</tt>,
        <tt>FLY</tt>, <tt>NONE</tt>.

        Inside the engine, the navigation paradigm is actually a little
        more flexible. You can fine-tune the rotations and gravity
        behavior by view3dscene <i>Navigation -> Walk and Fly Settings</i>
        menu.
        <!--
        For example, the difference between <tt>Walk</tt> and <tt>Fly</tt>
        is not only that the gravity if off when flying.
        It's also that in fly mode, you rotate around <i>current</i> up vector,
        not around the <i>gravity</i> up vector,
        -->

        <p>As an extension, we also support new navigation mode <tt>TURNTABLE</tt>.
        This is similar to the Examine mode, with controls comfortable
        for viewing models that have a sense of floor/ground in the XZ plane,
        and vertical axis in +Y. Implementation is not finished yet.

        <p>The presence of navigation type
        <tt>ANY</tt> is ignored by
        <?php echo a_href_page('view3dscene','view3dscene'); ?>.
        We always show controls to change navigation settings, hiding them
        feels harmful to user.

      <li><p>Nice transitions between viewpoints are supported,
        honouring <tt>transitionType</tt> and <tt>transitionTime</tt> fields,
        and (since view3dscene 3.13.0) making <tt>transitionComplete</tt> event.
        See <a href="http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/navigation/transition_multiple_viewpoints.x3dv">demo model navigation/transition_multiple_viewpoints.x3dv showing how to use it to make an animated transition between a couple of viewpoints</a>.
        </p>
    </ul>

    <p>Binding different <tt>NavigationInfo</tt> nodes,
    and changing their exposed fields by events, of course works.</p>

    <p>When no <tt>NavigationInfo</tt> node is present in the scene:
    <ul>
      <li><tt>avatarSize</tt> and <tt>speed</tt>
        are calculated based on scene's bounding box sizes, to a values that
        will hopefully "feel right".
        We try to calculate them intelligently, because simply using
        <tt>NavigationInfo</tt> defaults results in bad experience
        in many scenes.</li>

      <li><tt>headlight</tt> behaves like true,
        <tt>type</tt> behaves like <tt>[EXAMINE, ANY]</tt>,
        this follows <tt>NavigationInfo</tt> defaults.</li>
    </ul>

    <p><i>TODO</i>: <tt>visibilityLimit</tt> may be ignored if shadow
    volumes are allowed (We use frustum with z-far in infinity then.)</p>

  <li><p><?php echo x3d_node_link('LOD'); ?>

    <p><i>Note:</i> We do not have any automatic LOD calculation implemented now,
    which means that your supplied <tt>range</tt>, and only
    your supplied <tt>range</tt>, controls which LOD is chosen.
    This means that <tt>forceTransitions</tt> value is simply ignored,
    and when <tt>range</tt> is empty, we simply always use the first
    (highest-detail) version. This is Ok, spec allows this.

  <li><p><?php echo x3d_node_link('Billboard'); ?></p>

    <p>Works fully.</p>

  <li><p><?php echo x3d_node_link('Collision'); ?></p>

    <p>Most things work: grouping (<tt>children</tt> property, in particular),
    allows to control collision detection by honoring
    <tt>enabled</tt> (named <tt>collide</tt> in VRML 97) and <tt>proxy</tt>
    fields.

    <p><tt>bboxCenter/Size</tt> is currently simply ignored, our engine
    always calculates and updates the bounding boxes where needed.

    <p>TODO: collideTime and isActive out events are not implemented yet.

  <li><p><?php echo x3d_node_link('ViewpointGroup'); ?></p>

    <p>You can use them to create submenus in "Viewpoints" menu in
    <?php echo a_href_page('view3dscene','view3dscene'); ?>:
    <tt>description</tt> and <tt>children</tt> work.
    Also, you can use this to hide some viewpoints from the menu:
    <tt>displayed</tt> field works.</p>

    <p>TODO: size/center is not honored yet. Group is displayed
    regardless of camera position. A possible workarond could be
    to use a <tt>ProximitySensor</tt> node,
    routing <tt>ProximitySensor.isActive</tt> to the <tt>displayed</tt> field...
    Except this workaround will not work too, because changing
    of the <tt>displayed</tt> field after the scene loading
    doesn't change the menu for now.</p>

    <p>TODO: retainUserOffsets is ignored.</p>
  </li>
</ul>

<?php
  x3d_status_footer();
?>
