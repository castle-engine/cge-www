<?php
require_once 'castle_engine_functions.php';
manual_header('Recording movies');
?>

<p>Our engine does not include a ready API
to record movies during interactive applications / games.
This is not trivial to implement, and also you
can use existing external programs to do this. Under Linux I like
<a href="https://github.com/nullkey/glc/wiki/">glc</a>,
under Windows there is <code>fraps</code> and many others.

<p>You can of course <?php echo a_href_page('grab and save a screenshot',
'manual_screenshots'); ?> as often as you want.
But doing this every frame will make your FPS very, very low &mdash;
so it's not a good method to record a movie from a gameplay.

<p>That said, you can use the screenshot method (<code>SaveScreen</code>)
to record a <i>predefined animation</i>. That is, if you can programmatically
animate your world and/or move the camera and do other necessary stuff,
you can convert it into a perfect-quality movie.
So we support recording a predefined animation in VRML/X3D
(meaning: it plays without any user interaction) to a series of
images. Such animation can then be converted into a movie e.g. using
<code>ffmpeg</code> (our <?php echo a_href_page('view3dscene', 'view3dscene'); ?>
 does this automatically under the hood).

<p>Although this is
limited to predefined animations, in exchange this allows to record
the animation with perfect quality and with constant number of frames
per second.

<p>Do it like this:

<?php echo pascal_highlight(
'for .... do // in a loop, grab screenshot each frame
begin
  HandleInput := false;
  Viewport.Update(FrameTime, HandleInput);

  // capture and save screen
  Image := Window.Container.SaveScreen;
  try
    { Add your image to the movie sequence.
      Various options possible, e.g. you can save a sequence
      of images to a temporary location and later processes
      them by ffmpeg to produce a full movie. }
    SaveImage(Image, Format(\'screenshot_name_%4d.png\', [FrameNumber]));
  finally
    FreeAndNil(Image);
  end;
end;'); ?>

<p>You manually force the current time of the viewport (scenes, cameras) to
increase as much as you want before each screenshot. This makes it
work 100% reliably, without any worries how fast you can process
captured images.

<p><code>FrameTime</code> is a constant saying how many seconds passed between frames.
This is a float, in seconds. E.g. <code>FrameTime := 1/25</code> will
result in 25 frames per second.

<?php
manual_footer();
?>
