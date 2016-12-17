<?php
  require_once 'x3d_implementation_common.php';
  x3d_status_header('Text', 'text',
    'This component defines nodes for rendering 3D text.');

  $toc = new TableOfContents(
    array(
      new TocItem('Demos', 'demos'),
      new TocItem('Supported nodes', 'support'),
    ));
?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>For complete demos and tests of these features,
see the <code>text</code> subdirectory inside <?php
echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>.</p>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p><?php echo x3d_node_link('Text'); ?>,<br>
    <?php echo x3d_node_link('FontStyle'); ?>

    <p>Most important properties
    (size, spacing, justify (including 2nd justify for vertical),
    family, style) are handled fully.

    <p><i>TODO</i>: But some properties are ignored for now:
    <ul>
      <li>FontStyle properties: From section
        <i>6.22.3 Direction and justification</i>
        horizontal, leftToRight, topToBottom fields are ignored
        (and things are always handled like they had default values
        TRUE, TRUE, TRUE). From section <i>6.22.4 Language</i>
        language field is ignored.
      <li><code>Text</code>: length, maxExtent are
        ignored (and handled like they had
        default values, which means that the text is not stretched).
    </ul>

    <p><code>Text</code> is "clickable" within
    <code>Anchor</code> and <code>TouchSensor</code> nodes.
    Although I didn't find any mention in the specifications that I should
    do this, many VRML models seem to assume this.
    We make an ultra-simple triangulation of the text
    (just taking 2 triangles to cover whole text, you don't want
    to produce real triangles for text node, as text node would have
    a lot of triangles!).<br/>
    <i>TODO</i>: unfortunately, for now these triangles also participate
    in collision detection, while spec says that text shouldn't collide.
</ul>

<?php
  x3d_status_footer();
?>
