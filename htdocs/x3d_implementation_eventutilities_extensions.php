<?php
  require_once 'x3d_implementation_common.php';
  require_once 'x3d_extensions_functions.php';
  x3d_extensions_header('Event utilities', 'eventutilities', 'utils',
    'Extensions introduced in <a href="' . CURRENT_URL . '">Castle Game Engine</a> related to X3D event utilities.');

  $toc = new TableOfContents(
    array(
      new TocItem('Boolean value toggler (<code>Toggler</code> node)', 'ext_toggler'),
      new TocItem('Force sequencer continous output (<code>X3DSequencerNode.forceContinousValue_Changed</code>)', 'forceContinousValue_Changed'),
    ));
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p><code>Toggler</code> <?php echo x3d_node_api_link('Toggler'); ?>
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
  node_field('SFBool', '[in,out]', 'forceContinousValue_Changed', 'FALSE') .
  node_end();
?>

<p>By default (when this field is <code>FALSE</code>) the behaviour of sequencer nodes follows the X3D spec:
<i>The sequencer node sends only one value_changed output event per key[i] interval</i>.

<p>When <code>forceContinousValue_Changed</code> is <code>TRUE</code> then
on <b>every</b> action posibly changing the continous key, we output <code>value_changed</code>.
In particular, this means that <b>every set_fraction will cause appropriate "value_changed"</b>,
even if previous set_fraction already generated the same "value_changed".
This is consistent with float interpolator nodes, and it is very useful sometimes: when multiple
<code>IntegerSequencer</code> nodes may affect the same <code>Switch.whichChoice</code>
(but only one <code>IntegerSequencer</code> is active
at a time, i.e. only one TimeSensor actually uses some <code>IntegerSequencer</code>),
you want to be sure to send <code>IntegerSequencer.value_changed</code> continously.

<?php
  x3d_status_footer();
?>