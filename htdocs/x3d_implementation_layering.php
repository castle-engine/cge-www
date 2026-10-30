<?php
require_once 'x3d_implementation_common.php';
x3d_status_header('Layering', 'layering',
  'Layers allow to explicitly define the rendering order of items (independently of what is closer, what is further from the camera).');

$toc = new TableOfContents(
  array(
    new TocItem('Supported nodes', 'support'),
    new TocItem('TODO', 'todo'),
  ));
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>For now, we have a really basic support for the nodes in this X3D component.

<ul>
  <li>
    <p>Our <code>LayerSet</code> just enumerates all the layers mentioned in <code>LayerSet.order</code>.

  <li>
    <p>Our <code>Layer</code>, if it is visible (<code>Layer.visible</code>), just enumerates all its children.
  </li>
</ul>

<p>This means that contents of your layers will render &mdash; but mostly
as if they were part of the normal scene graph. The layering order specified by
<code>LayerSet.order</code> is not yet enforced during rendering.

<p><b>Support us to improve this!</b>
<a href="https://castle-engine.io/helping">Various ways to
help are welcome</a>, in particular <a href="https://castle-engine.io/donate">donations
so that we can spend time on this feature</a>.

<?php echo $toc->html_section(); ?>

<p>Important features are missing to call it "real support":

<ul>
  <li>
    <p>We don't yet actually render things following the layers order.
    Everyting is rendered with the Z-buffer "on" and undefined order,
    so things are visible in the order "what is closer to the camera".

    <p>Note: Castle Game Engine has ways to do this, even 2. But they are "above"
    X3D rendering logic.

    <ol>
      <li><?php echo cgeRef('TCastleTransform.RenderLayer'); ?>
        (limited but trivial to use)

      <li><p>Multiple <?php echo cgeRef('TCastleViewport'); ?> with the same size,
        but different items
        to render, and transparent background (<?php echo cgeRef('TCastleViewport.Transparent'); ?>).
    </ol>

  <li>
    <p>Our treatment of <code>Layer.visible</code> is really simplistic: we completely
    exclude invisible layers from enumeration, so they will not even collide.

  <li>
    <p>Toggling the fields, like <code>Layer.visible</code>, at run-time is expensive.
    This is in contrast to our super-optimized
    <ul>
      <li><code>Switch.whichChoice</code>
      <li>and <code>X3DGroupingNode.visible</code>
    </ul>
  </li>
</ul>

<?php
  castle_footer();
?>
