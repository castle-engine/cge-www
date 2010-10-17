<?php
  require_once 'vrml_implementation_common.php';
  x3d_status_header('Navigation', 'navigation');
?>

<p>Supported nodes:</p>

<ul>
  <li><p><tt>Viewpoint</tt></p>

    <p><i>Note</i>: view3dscene displays also nice menu allowing you to jump
    to any defined viewpoint, displaying viewpoints descriptions.
    Extensive tests of various viewpoint properties, including fieldOfView,
    are inside <?php
      echo a_href_page('my VRML test suite', 'kambi_vrml_test_suite'); ?>
    in <tt>vrml_2/viewpoint_*.wrl</tt> files.</p>

    <p>Animating viewpoint's position and orientation
    (directly or by it's transformations) works perfectly.</p>

  <li><p><tt>NavigationInfo</tt></p>

    <p>Nice transitions between viewpoints are supported,
    honouring <tt>transitionType</tt> and <tt>transitionTime</tt> fields.
    TODO: <tt>transitionComplete</tt> event is not generated for now.</p>

    <p>Binding different <tt>NavigationInfo</tt> nodes,
    and changing their exposed fields by events,
    (just like for <tt>X3DViewpointNode</tt>s) of course works.</p>

    <p>Various details about how we handle NavigationInfo node in
    <?php echo a_href_page('view3dscene','view3dscene'); ?>:
    <ul>
      <li>Note that <tt>--camera-radius</tt> command-line option overrides
        whatever was specified by <tt>avatarSize[0]</tt>.

      <li><tt>avatarSize[2]</tt> (tallest object over which you can move)
        is ignored for now. Camera radius decides what you can climb.

      <li><tt>speed</tt> is honored as appropriate, it sets
        the speed in meters/second. Speed = 0.0 is also correctly
        honored (user will not be able to move in Walk/Fly modes,
        only to rotate).

      <li><tt>type</tt> of navigation: <tt>EXAMINE</tt>, <tt>WALK</tt>,
        <tt>FLY</tt>, <tt>NONE</tt> are fully supported. They map to appropriate
        view3dscene internal navigation settings:
        <ul>
          <li><tt>EXAMINE</tt> in VRML &mdash; internal <tt>Examine</tt> style,
          <li><tt>WALK</tt> in VRML &mdash; internal <tt>Walk</tt> style
            with gravity and moving versus <i>gravity</i> up vector,
          <li><tt>FLY</tt> in VRML &mdash; internal <tt>Walk</tt> style
            without gravity, and moving versus <i>current</i> up vector,
          <li><tt>NONE</tt> in VRML &mdash; internal <tt>Walk</tt> style
            without gravity, and with "disable normal navigation".
        </ul>

      <li>The presence of navigation type
        <tt>ANY</tt> is not important (view3dscene always
        shows controls to change navigation settings).
    </ul>

    <p>When no <tt>NavigationInfo</tt> node is present in the scene,
    we try to intelligently guess related properties.
    (We try to guess "intelligently" because simply assuming that
    "no NavigationInfo node" is equivalent to "presence of
    default NavigationInfo" is <i>not good</i> for most scenes).
    <ul>
      <li><tt>avatarSize[0]</tt> and <tt>avatarSize[1]</tt>
        are guessed based on scene's bounding box sizes.

      <li><tt>headlight</tt> is set to true.

      <li><tt>type</tt> is set to <tt>"EXAMINE"</tt> (this follows the spec,
        as <tt>[EXAMINE, ANY]</tt> is the default <tt>NavigationInfo.type</tt> value).

      <li><tt>speed</tt> is calculated to something that should "feel sensible"
        based on scene's bounding box sizes.
    </ul>

    <p><i>TODO</i>: <tt>visibilityLimit</tt> may be ignored if shadow
    volumes are allowed (We use frustum with z-far in infinity then.)</p>

  <li><p><tt>LOD</tt>

    <p><i>Note:</i> We do not have any automatic LOD calculation implemented now,
    which means that your supplied <tt>range</tt>, and only
    your supplied <tt>range</tt>, controls which LOD is chosen.
    This means that <tt>forceTransitions</tt> value is simply ignored,
    and when <tt>range</tt> is empty, we simply always use the first
    (highest-detail) version. This is Ok, spec allows this.

  <li><p><tt>Billboard</tt></p>

    <p><i>TODO</i>: Not really handled:
    it just works like a <tt>Group</tt>. Often that's enough
    for it to look sensible, but it's hardly a real support...</p>

  <li><p><tt>Collision</tt></p>

    <p>Most things work: grouping (<tt>children</tt> property, in particular),
    allows to control collision detection by honoring
    <tt>enabled</tt> (named <tt>collide</tt> in VRML 97) and <tt>proxy</tt>
    fields.

    <p><tt>bboxCenter/Size</tt> is currently simply ignored, our engine
    always calculates and updates the bounding boxes where needed.

    <p>TODO: collideTime and isActive out events are not implemented yet.

  <li><p><tt>OrthoViewpoint</tt>

    <p>TODO: Although it's handled, some fields are ignored for now:
    jump, retainUserOffsets, centerOfRotation.
</ul>

<p><i>TODO</i>: ViewpointGroup missing.</p>

<?php
  x3d_status_footer();
?>
