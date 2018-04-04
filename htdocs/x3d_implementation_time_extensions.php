<?php
  require_once 'x3d_implementation_common.php';
  require_once 'x3d_extensions_functions.php';
  x3d_extensions_header('Time', 'time', 'time',
    'Extensions introduced in <a href="' . CURRENT_URL . '">Castle Game Engine</a> related to the time.');

$toc = new TableOfContents(
  array(
    new TocItem('TimeSensor.fractionIncreasing field', 'fraction_increasing'),
  ));
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p><b>This feature is available since <i>Castle Game Engine</i> 6.5 and
<i>view3dscene</i> 3.19.0.
It's not yet available in stable CGE or view3dscene.</b>

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

<?php
  x3d_status_footer();
?>
