<?php
  require_once 'x3d_implementation_common.php';
  require_once 'x3d_extensions_functions.php';
  x3d_extensions_header('Time', 'time', 'time',
    'Extensions introduced in <a href="' . page_url('index') . '">Castle Game Engine</a> related to the time.');

$toc = new TableOfContents(
  array(
    new TocItem('TimeSensor.fractionIncreasing field', 'fraction_increasing'),
    new TocItem('TimeSensor.detectAffectedFields field', 'detect_affected_fields'),
  ));
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<p>These feature are available since <i>Castle Game Engine</i> 6.5 and
<i>view3dscene</i> 3.19.0.

<?php echo $toc->html_section(); ?>

<p>As an extension, we add a <code>fractionIncreasing</code> field to
the <code>TimeSensor</code> node:

<?php echo
  node_begin('TimeSensor') .
  node_dots() .
  node_field('SFBool', '[in,out]', 'fractionIncreasing', 'TRUE') .
  node_end();
?>

<p>When <code>FALSE</code>, the animation runs backwards.
It's very simple: after calculating the <code>fraction</code> value,
following the X3D specification, we do <code>fraction := 1 - fraction</code>.
And then we send <code>fraction</code>
through the <code>TimeSensor.fraction_changed</code> event.

<p>If your interpolators react nicely to the <code>TimeSensor.fraction_changed</code>
then the animation will run backwards.
See <a href="x3d_implementation_interpolation.php">a description how
TimeSensor and interpolators are typically connected to implement animation</a>.

<p>Note that everything else works as usual, regardless of
the <code>fractionIncreasing</code> value. In particular,
<code>TimeSensor.elapsedTime</code> and <code>TimeSensor.time</code>
output events are always generated with increasing values
(trying to force them to go backward would make weird results).

<?php echo $toc->html_section(); ?>

<?php echo
  node_begin('TimeSensor') .
  node_dots() .
  node_field('SFBool', '[]', 'detectAffectedFields', 'TRUE') .
  node_end();
?>

<p><i>In short:</i> This field should remain <code>TRUE</code> (the default value) for time sensor nodes that represent animations that <i>Castle Game Engine</i> can run at any moment (e.g. using <a href="manual_scene.php">TCastleSceneCore.PlayAnimation</a>).

<p>This allows the CGE <code>TCastleSceneCore.ResetAnimationState</code> to work correctly. In turn, this means that <code>TCastleSceneCore.PlayAnimation</code> works always correctly, with and without animation blending (cross-fading).

<p><i>Details:</i>

<p>When this is <code>TRUE</code>, the engine will detect the fields affected by the animation defined by this <code>TimeSensor</code> node. This "detection" looks what interpolators (<code>X3DInterpolatorNode</code> or <code>X3DSequencerNode</code>) are affected by this TimeSensor. To be precise, TimeSensor <code>fraction_changed</code> must be routed to interpolator <code>set_fraction</code>, and then the interpolator <code>value_changed</code> must be routed to the field we call "affected". The values assigned to detected fields will be recorded, for the entire lifetime of this scene.

<p>The detected "affected fields" will be used:

<ul>

  <li><p>The <code>TCastleSceneCore.ResetAnimatedFields</code> sets all the fields affected by <i>any</i> animation to their initial (detected at loading) state.

  <li><p>The <code>TCastleSceneCore.PlayAnimation</code> sets all the fields affected by <i>any</i> animation (except the new animation) to their initial (detected at loading) state.

    <p>This is important e.g. if your animation "walk" only moves the model's legs (and doesn't modify model's hands), but some other animation moves hands. Then when doing "walk" we also make sure to put the hands in the original (not animated) position. This is useful, because previous animation could be e.g. "wave_hand" and you don't want the hand to be permanently raised up during "walk" animation cycle.

    <p>Note that if you want to play multiple animations simulteneously, you can use <code>TTimeSensorNode.Start</code> instead of <code>TCastleSceneCore.PlayAnimation</code>. See <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/animations/simultaneous_animations_one_scene">simultaneous_animations_one_scene example</a> . In this case, the "affected fields" do not matter, starting an animation using <code>TTimeSensorNode.Start</code> doesn't reset any fields.

</ul>

<p>Set the <code>detectAffectedFields</code> field to <code>FALSE</code> when this detection would be useless and time-consuming. This applies to time sensors which are not supposed to be used with <code>TCastleSceneCore.PlayAnimation</code>. Saving the state of "affected" fields may take time (if they are e.g. MFVec3f fields with lots of data) and may change memory management (if they are e.g. SFNode or MFNode fields).

<?php
  x3d_status_footer();
?>
