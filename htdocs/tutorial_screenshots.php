<?php
  require_once 'tutorial_common.php';
  tutorial_header('Screenshots');
?>

Making a screenshot is very easy:

<?php echo pascal_highlight(
'var
  Image: TRGBImage;
begin
  Image := Control.SaveScreen;
  try
    SaveImage(Image, \'screenshot_name.png\');
  finally FreeAndNil(Image) end;
end;'); ?>

TRGBImage, and it's ancestor TCastleImage and many other similar classes, is defined in the unit Images. This provides various image loading, saving and processing utilities to our engine. By having our own image handling unit, we can store images in the format best suitable for 3D graphics libraries, like OpenGL (for example, our class TS3TCImage to handle S3TC compressed images is tightly integrated there; we also handle there 3D textures).

Still, we use the excellent FPC FpImage library to load/save some image formats underneath. So we do not implement everything from stratch here, we *are* using FpImage loading/saving features.

<?php
  tutorial_footer();
?>
