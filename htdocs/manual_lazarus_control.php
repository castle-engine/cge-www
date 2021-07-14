<?php
require_once 'castle_engine_functions.php';
manual_header('Engine control to put on Lazarus form');

echo castle_thumbs(array(
  array('filename' => 'lazarus_control.png', 'titlealt' => 'TCastleControlBase designed in Lazarus'),
  array('filename' => 'lazarus_control_run.png', 'titlealt' => '3D model viewer using TCastleControlBase'),
));
?>

<p>You need a window on the screen to display the engine content.
We support two approaches to this:

<ol>
  <li>
    <p>Create a window using <?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?>.

  <li>
    <p>Use <?php api_link('TCastleControlBase', 'CastleControl.TCastleControlBase.html'); ?>
    inside a standard Lazarus LCL form.
</ol>

<p>Most of this manual describes the process of using the engine with
the <?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?>.
It is used by all new editor templates.
We generally advise this approach, as:

<ul>
  <li>
    <p><?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?>
      avoids some problems with LCL application loop (for example, to make mouse look perfectly smooth).

  <li>
    <p>Our <?php api_link('TCastleWindowBase', 'CastleWindow.TCastleWindowBase.html'); ?>
    works on all CGE platforms (desktop, mobile - Android and iOS,
    consoles - <a href="https://github.com/castle-engine/castle-engine/wiki/Nintendo-Switch">Nintendo Switch</a>).
</ul>

<p>On the other hand, using the <?php api_link('TCastleControlBase', 'CastleControl.TCastleControlBase.html'); ?>
 has one big benefit: you place the control inside a Lazarus form, and you can surround it with
all the standard LCL GUI controls. So you can use numerous LCL GUI controls,
with native look on all desktop systems,
together with <i>Castle Game Engine</i>. To use this:

<ul>
  <li>Create a normal new project (using Lazarus <i>"New Project"</i> menu item).
    Choose <i>"Application"</i>.
  <li>Pick <code>TCastleControlBase</code> from the component palette (tab
    <i>"Castle"</i>) and drop it on a regular Lazarus form.
  <li>Press "Run" :)
</ul>

<p>See the engine examples in <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/lazarus">examples/lazarus/</a> subdirectory for a demo of this approach.

<?php
manual_footer();
?>
