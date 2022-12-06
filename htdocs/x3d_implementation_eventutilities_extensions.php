<?php
  require_once 'x3d_implementation_common.php';
  require_once 'x3d_extensions_functions.php';
  x3d_extensions_header('Event utilities', 'eventutilities', 'utils',
    'Extensions introduced in <a href="' . page_url('index') . '">Castle Game Engine</a> related to X3D event utilities.');

  $toc = new TableOfContents(
    array(
      new TocItem('Boolean value toggler (<code>Toggler</code> node)', 'ext_toggler'),
      new TocItem('Force sequencer continuous output (<code>X3DSequencerNode.forceContinuousValue_Changed</code>)', 'ext_forceContinuousValue_Changed'),
      new TocItem('Trigger multiple outputs of any type when some input is received (<code>ValueTrigger</code>)', 'ext_value_trigger'),
    ));
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p><code>Toggler</code> <?php echo x3d_node_cgeRef('Toggler'); ?>
 is simple event utility for setting/observing a boolean value in various
ways. Something like a standard X3D <code>BooleanToggle</code> on steroids.</p>

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

<p><code>"status"</code> is the boolean value stored.
<code>"notStatus"</code> is always the negated value of <code>"status"</code>.
You can set either of them (by sending
<code>"set_status"</code> or <code>"set_notStatus"</code>). Changing any of them
causes both output events to be send. That is, <code>"status_changed"</code>
receives the new boolean value stored, <code>"notStatus_changed"</code>
received the negation of the new value stored.</p>

<p>The input events <code>"toggle"</code>, <code>"set"</code> and <code>"reset"</code>
provide altenative ways to change the stored boolean value.
They accept any VRML/X3D type/value as input
(this is called <code>XFAny</code> by InstantPlayer), and the value send
is actually completely ignored.
<code>"toggle"</code> always toggles (negates) the stored value,
<code>"set"</code> changes the stored value to <code>TRUE</code>,
<code>"reset"</code> changes the stored value to <code>FALSE</code>.</p>

<p>The output events <code>"changed"</code>, <code>"on"</code>, <code>"off"</code> provide
altenative ways to observe the stored boolean value.
They always generate a boolean <code>TRUE</code> value when specific
thing happens. <code>"changed"</code> event is generated when the value
changes, <code>"on"</code> event is generated when the value changes to <code>TRUE</code>,
<code>"off"</code> event is generated when the value changes to <code>FALSE</code>.</p>

<p><code>"enabled"</code> allows to disable input/output events handling.
When <code>enabled = FALSE</code> then
sending input events to above fields has no effect (stored boolean value
doesn't change), and no output events are generated.</p>

<p>Compatible with <a href="http://www.instantreality.org/documentation/nodetype/Toggler/">InstantReality Toggler node</a>.

<?php echo $toc->html_section(); ?>

<?php
  echo node_begin('X3DSequencerNode') .
  node_dots() .
  node_field('SFBool', '[in,out]', 'forceContinuousValue_Changed', 'FALSE') .
  node_end();
?>

<p>By default (when this field is <code>FALSE</code>) the behavior of sequencer nodes follows the X3D spec:
<i>The sequencer node sends only one value_changed output event per key[i] interval</i>.

<p>When <code>forceContinuousValue_Changed</code> is <code>TRUE</code> then
on <b>every</b> action possibly changing the continuous key, we output <code>value_changed</code>.
In particular, this means that <b>every set_fraction will cause appropriate "value_changed"</b>,
even if previous set_fraction already generated the same "value_changed".
This is consistent with float interpolator nodes, and it is very useful sometimes: when multiple
<code>IntegerSequencer</code> nodes may affect the same <code>Switch.whichChoice</code>
(but only one <code>IntegerSequencer</code> is active
at a time, i.e. only one TimeSensor actually uses some <code>IntegerSequencer</code>),
you want to be sure to send <code>IntegerSequencer.value_changed</code> continuously.

<?php echo $toc->html_section(); ?>

<?php
  echo node_begin('ValueTrigger : X3DTriggerNode') .
  node_dots() .
  node_field('SFBool', '[in,out]', 'enabled', 'TRUE') .
  node_field('SFBool', '[in]',     'trigger', '') .
  node_dots('additional custom fields') .
  node_end();
?>

<p>The way to specify <i>additional custom fields</i> is the same as for standard <code>Script</code>
or <code>ComposedShader</code> nodes.

<p>When the input <code>trigger</code> receives a value <code>TRUE</code>,
and <code>enabled</code> is <code>TRUE</code>, this node generates output using all
custom fields.

<p>An example usage (in X3D classic encoding):

<?php echo vrmlx3d_highlight(
'DEF MyValueTrigger ValueTrigger {
  inputOutput SFVec3f myBboxCenter 1  2  3
  inputOutput SFVec3f myBboxSize   10 20 30
}

# When a MyTimeSensor becomes active (when the animation starts)....
ROUTE MyTimeSensor.isActive TO MyValueTrigger.trigger

# ...set the MyShape bounding box to predefined values
ROUTE MyValueTrigger.myBboxCenter TO MyShape.bboxCenter
ROUTE MyValueTrigger.myBboxSize   TO MyShape.bboxSize'); ?>

<p><a href="https://doc.instantreality.org/documentation/nodetype/ValueTrigger/">This extension is mostly compatible with InstantReality node of the same name</a>, but we allow only <code>SFBool</code> for <code>trigger</code>.

<?php
  x3d_status_footer();
?>
