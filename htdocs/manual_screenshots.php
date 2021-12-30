<?php
require_once 'castle_engine_functions.php';
castle_header('Screenshots');
?>

<p>Making a screenshot is very easy:

<?php echo pascal_highlight(
'procedure TakeScreenshot;
var
  Image: TRGBImage;
begin
  Image := Control.SaveScreen;
  try
    SaveImage(Image, \'screenshot_name.png\');
  finally FreeAndNil(Image) end;
end;

// Call TakeScreenshot
// from wherever you want in your game.
// For example, from the OnPress event.'); ?>

<p><?php api_link('TRGBImage', 'CastleImages.TRGBImage.html'); ?>, and it's ancestor
<?php api_link('TCastleImage', 'CastleImages.TCastleImage.html'); ?> and many other similar
classes, are defined in the unit <?php api_link('CastleImages', 'CastleImages.html'); ?>.
This unit provides various image
loading, saving and processing utilities to our engine.

<p>Note: we have our own image handling unit,
to store images in the format best
suitable for 3D graphics libraries, like OpenGL.
This gives us some capabilities not found in common image libraries,
for example we seamlessly handle S3TC compressed images (class TS3TCImage),
3D textures and float textures.
Still, we use the excellent FPC FpImage library to load/save some
image formats underneath.

<?php
castle_footer();
?>
