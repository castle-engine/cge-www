<?php
  require_once 'tutorial_common.php';
  tutorial_header('Screenshots');
?>

<p>Making a screenshot is very easy:

<?php echo pascal_highlight(
'var
  Image: TRGBImage;
begin
  Image := Control.SaveScreen;
  try
    SaveImage(Image, \'screenshot_name.png\');
  finally FreeAndNil(Image) end;
end;'); ?>

<p>TRGBImage, and it's ancestor TCastleImage and many other similar
classes, is defined in the unit CastleImages. This unit provides various image
loading, saving and processing utilities to our engine.

<p>Note: we have our own image handling unit,
to store images in the format best
suitable for 3D graphics libraries, like OpenGL.
This gives us some capabilities not found in common image libraries,
for example we handle S3TC compressed images (class TS3TCImage),
3D textures, float textures.
Still, we use the excellent FPC FpImage library to load/save some
image formats underneath.

<?php
  tutorial_footer();
?>
