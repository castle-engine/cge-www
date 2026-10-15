<?php
require_once 'castle_engine_functions.php';
castle_header('Additional Components', array(
  'path' => array('gallery'),
));

echo pretty_heading($page_title);

$additional_components_json = file_get_contents(__DIR__ . '/additional_components.json');
if ($additional_components_json === false) {
  throw new RuntimeException('Could not read additional_components.json');
}
$additional_components = json_decode($additional_components_json, true, 512, JSON_THROW_ON_ERROR);
?>

<p>Additional components listed here are built on top of <i>Castle Game Engine</i>.
You can use them in your own projects, to enhance what you can do with CGE.

<p><i>Want your project listed here?
<a href="talk.php">Tell us about it!</a>
We love to see how you use CGE.</i>

<div class="row">
  <?php
  foreach ($additional_components['components'] as $component) {
    $description = str_replace(
      '{{cgeRef:TCastleImage.AlphaBleed}}',
      cgeRef('TCastleImage.AlphaBleed'),
      $component['description']
    );
    gallery_link(
      $component['title'],
      $description,
      $component['image'],
      $component['page']
    );
  }
  ?>
</div>

<p>More:

<ul>
  <?php
  foreach ($additional_components['more'] as $component) {
    gallery_link_noimage(
      $component['title'],
      $component['description'],
      $component['page']
    );
  }
  ?>
</ul>

<?php castle_footer(); ?>
