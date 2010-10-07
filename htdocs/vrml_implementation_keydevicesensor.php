<?php
  require_once 'vrml_implementation_common.php';
  x3d_status_header('Key device sensor', 'keyboard');
?>

<p>Supported nodes: both <tt>KeySensor</tt> and <tt>StringSensor</tt>.

<p><i>TODO</i>: for now, only 8-bit ASCII characters are passed
(to the <tt>KeySensor.keyPress/keyRelease</tt> events,
and to the <tt>StringSensor.enteredText/finalText</tt>)..</p>

<p><i>Note</i>: <tt>keyPress</tt> and <tt>actionKeyPress</tt> events follow
the "key repeat" feature of your operating system/window manager.
This means that when the user holds down a key, we generate many key press events.
Sometimes it's useful (for simulating normal text input, for example).
Report if you would like to make this feature optional (and maybe
off by default?), a field like <tt>KeySensor.repeatKey</tt> may be added.</p>

<p><i>Note</i>: <tt>isActive := TRUE</tt> is also generated at each key press,
and <tt>isActive := FALSE</tt> at each key release. So they do not match in pairs.
And it's not that simple, even if we would turn off the "key repeat" feature
mentioned above: imagine that you press one key, then you press a 2nd key,
and release the 1st key. Turns out that we have to track all the keys pressed,
and deactivate only when all are released, right? Things get more hairy
when you consider the first key to be the letter "a", and the 2nd key to be "Shift".
Conceptually, "a" is pressed, then "A" is pressed, then "A" is released...
but when was lower-case "a" released?
Actually, we do have a smart code inside an engine to track it correctly,
but it's not passed to the <tt>KeySensor</tt> yet.</p>

<p><i>Note</i>: Many <tt>X3DKeyDeviceSensorNodes</tt> (both <tt>KeySensor</tt>
and <tt>StringSensor</tt>) may be active at given time. The specification
doesn't precisely say what should happen, with wording suggesting
that making a sensor enabled makes it also active (which contradicts the <tt>KeySensor</tt>
spec), and without saying which sensor is chosen if many are present (the first?
the last in the graph?).</p>

<p><i>History</i>: <tt>KeySensor</tt> is in fact the first sensor node implemented long time ago :)</p>

<?php
  x3d_status_footer();
?>
