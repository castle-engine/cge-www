<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Key device sensor', 'keyboard',
    'This component defines nodes to interact with a keyboard.
     <code>KeySensor</code> processes simple key presses / releases.
     <code>StringSensor</code> provides a simple way for user to type and edit
     a string.');

  $toc = new TableOfContents(
    array(
      new TocItem('Demos', 'demos'),
      new TocItem('Supported nodes', 'support'),
    ));
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>This components allows to read and process keys using X3D nodes.

<p>Note: If you write a game using <i>Castle Game Engine</i>,

<ul>
  <li><p>These nodes are not the most comfortable way to process keyboard.
    Do not use them.

  <li><p>Instead, override <?php echo cgeRef('TCastleView.Press'); ?> method to listen to keys
    (see any <i>"New Project"</i> template created by the editor for an example).
    You can also react to
    <?php echo cgeRef('TCastleWindow.OnPress'); ?> or
    <?php echo cgeRef('TCastleControl.OnPress'); ?> or override
    <?php echo cgeRef('TCastleUserInterface.Press'); ?> method.
    See <a href="view_events">the manual describing handling events</a>.

  <li><p>If you definitely want to use these nodes with CGE, remember to set
    <?php echo cgeRef('TCastleTransform.ListenPressRelease'); ?>
    to <code>true</code>.
</ul>

<p>For demos and tests of these nodes,
see the <code>sensors_key</code> subdirectory inside <?php
echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>.</p>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p><?php echo x3d_node_link('KeySensor'); ?>

    <p>Listen to key press and release.

  <li><p><?php echo x3d_node_link('StringSensor'); ?>

    <p>Input and edit a string using keyboard.
</ul>

<p><i>Note</i>: <code>keyPress</code> and <code>actionKeyPress</code> events follow
the "key repeat" feature of your operating system/window manager.
This means that when the user holds down a key, we generate many key press events.
Sometimes it's useful (for simulating normal text input, for example).
Report if you would like to make this feature optional (and maybe
off by default?), a field like <code>KeySensor.repeatKey</code> may be added.</p>

<p><i>Note</i>: <code>isActive := TRUE</code> is also generated at each key press,
and <code>isActive := FALSE</code> at each key release. So they do not match in pairs.
And it's not that simple, even if we would turn off the "key repeat" feature
mentioned above: imagine that you press one key, then you press a 2nd key,
and release the 1st key. Turns out that we have to track all the keys pressed,
and deactivate only when all are released, right? Things get more hairy
when you consider the first key to be the letter "a", and the 2nd key to be "Shift".
Conceptually, "a" is pressed, then "A" is pressed, then "A" is released...
but when was lower-case "a" released?
Actually, we do have a smart code inside an engine to track it correctly,
but it's not passed to the <code>KeySensor</code> yet.</p>

<p><i>Note</i>: Many <code>X3DKeyDeviceSensorNodes</code> (both <code>KeySensor</code>
and <code>StringSensor</code>) may be active at given time. The specification
doesn't precisely say what should happen, with wording suggesting
that making a sensor enabled makes it also active (which contradicts the <code>KeySensor</code>
spec), and without saying which sensor is chosen if many are present (the first?
the last in the graph?).</p>

<?php
  x3d_status_footer();
?>
