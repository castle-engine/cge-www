<?php
  require_once 'x3d_implementation_common.php';
  require_once 'x3d_extensions_functions.php';
  x3d_extensions_header('Event utilities', 'eventutilities', 'utils',
    'Extensions introduced in <a href="' . CURRENT_URL . '">Castle Game Engine</a> related to X3D event utilities.');

  $toc = new TableOfContents(
    array(
      new TocItem('Boolean value toggler (<tt>Toggler</tt> node)', 'ext_toggler'),
      new TocItem('Force sequencer continous output (<tt>X3DSequencerNode.forceContinousValue_Changed</tt>)', 'forceContinousValue_Changed'),
    ));
  $toc->echo_numbers = true;
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>A simple event utility for setting/observing a boolean value in various
ways. Something like a standard X3D <tt>BooleanToggle</tt> on steroids.</p>

<?php echo node_begin("Toggler  : X3DChildNode");
  echo
  node_field('SFNode', '[in,out]', 'metadata', 'NULL', '[X3DMetadataObject]') .
  node_field('SFBool', '[in,out]', 'status',  'FALSE', '') .
  node_field('SFBool', '[in,out]', 'notStatus',  'TRUE', '') .
  node_field('XFAny' , '[in]'    , 'toggle', '', 'the type/value send here is ignored') .
  node_field('XFAny' , '[in]'    , 'set', '', 'the type/value send here is ignored') .
  node_field('XFAny' , '[in]'    , 'reset', '', 'the type/value send here is ignored') .
  node_field('SFBool', '[out]'   , 'changed', '', 'always sends TRUE') .
  node_field('SFBool', '[out]'   , 'on', '', 'always sends TRUE') .
  node_field('SFBool', '[out]'   , 'off', '', 'always sends TRUE') .
  node_field('SFBool', '[in,out]', 'enabled',  'TRUE') .
  node_end();
?>

<p><tt>"status"</tt> is the boolean value stored.
<tt>"notStatus"</tt> is always the negated value of <tt>"status"</tt>.
You can set either of them (by sending
<tt>"set_status"</tt> or <tt>"set_notStatus"</tt>). Changing any of them
causes both output events to be send. That is, <tt>"status_changed"</tt>
receives the new boolean value stored, <tt>"notStatus_changed"</tt>
received the negation of the new value stored.</p>

<p>The input events <tt>"toggle"</tt>, <tt>"set"</tt> and <tt>"reset"</tt>
provide altenative ways to change the stored boolean value.
They accept any VRML/X3D type/value as input
(this is called <tt>XFAny</tt> by Avalon), and the value send
is actually completely ignored.
<tt>"toggle"</tt> always toggles (negates) the stored value,
<tt>"set"</tt> changes the stored value to <tt>TRUE</tt>,
<tt>"reset"</tt> changes the stored value to <tt>FALSE</tt>.</p>

<p>The output events <tt>"changed"</tt>, <tt>"on"</tt>, <tt>"off"</tt> provide
altenative ways to observe the stored boolean value.
They always generate a boolean <tt>TRUE</tt> value when specific
thing happens. <tt>"changed"</tt> event is generated when the value
changes, <tt>"on"</tt> event is generated when the value changes to <tt>TRUE</tt>,
<tt>"off"</tt> event is generated when the value changes to <tt>FALSE</tt>.</p>

<p><tt>"enabled"</tt> allows to disable input/output events handling.
When <tt>enabled = FALSE</tt> then
sending input events to above fields has no effect (stored boolean value
doesn't change), and no output events are generated.</p>

<p>Compatible with <a href="http://www.instantreality.org/documentation/nodetype/Toggler/">InstantReality Toggler node</a>.

<?php echo $toc->html_section(); ?>

<?php
  echo node_begin('X3DSequencerNode') .
  node_dots() .
  node_field('SFBool', '[in,out]', 'forceContinousValue_Changed', 'FALSE') .
  node_end();
?>

<p>By default (when this field is <tt>FALSE</tt>) the behaviour of sequencer nodes follows the X3D spec:
<i>The sequencer node sends only one value_changed output event per key[i] interval</i>.

<p>When <tt>forceContinousValue_Changed</tt> is <tt>TRUE</tt> then
on <b>every</b> action posibly changing the continous key, we output <tt>value_changed</tt>.
In particular, this means that <b>every set_fraction will cause appropriate "value_changed"</b>,
even if previous set_fraction already generated the same "value_changed".
This is consistent with float interpolator nodes, and it is very useful sometimes: when multiple
<tt>IntegerSequencer</tt> nodes may affect the same <tt>Switch.whichChoice</tt>
(but only one <tt>IntegerSequencer</tt> is active
at a time, i.e. only one TimeSensor actually uses some <tt>IntegerSequencer</tt>),
you want to be sure to send <tt>IntegerSequencer.value_changed</tt> continously.

<?php
  x3d_status_footer();
?>