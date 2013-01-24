<?php
require_once 'castle_engine_functions.php';
tutorial_header('Recording movies');
?>

<p>Note that our engine doesn't support (yet) recording movies during
interactive operations, like during a game. One reason for this is
that it's non-trivial to implement, and the other reason is that you
can use existing external programs to do this. Under Linux I like
<a href="https://github.com/nullkey/glc/wiki/">glc</a>,
under Windows there is <tt>fraps</tt> and many others.

<p>However, we support recording a predefined animation in VRML/X3D
(meaning: it plays without any user interaction) to a series of
images. Such animation can then be converted into a movie e.g. using
<tt>ffmpeg</tt> (our <?php echo a_href_page('view3dscene', 'view3dscene'); ?>
 does this automatically under the hood). Although this is
limited to predefined animations, in exchange this allows to record
the animation with perfect quality and with constant number of frames
per second.

<p>Do it like this:

<?php echo pascal_highlight(
'// in a loop:
begin
  SceneManager.MainScene.IncreaseTime(FrameTime);
  SceneManager.Camera.Idle(FrameTime, true, DummyBooleanInitializedToTrue);

  { capture and save screen as before, like: }
  Image := Control.SaveScreen;
  try
    { add your image to the movie sequence.
      Various options possible, view3dscene for now just saves a sequence
      of images to temporary location and later processes them by ffmpeg
      to produce a full movie. }
    SaveImage(Image, Format(\'screenshot_name_%4d.png\', [FrameNumber]));
  finally FreeAndNil(Image) end;
end;'); ?>

<p>You manually force the current time of the scene/camera to
increase as much as you want before each screenshot. This makes it
work 100% reliably, without any worries how fast you can process
captured images.

<p>The time of the scene and of the camera is actually independent. You
can remove the <tt>"MainScene.IncreaseTime"</tt> call, if you know that your
scene stays static (or you want to force it to look like static), and
want to animate only camera. Or the other way around, if your camera
is static (or you want to force it to be static) but you have an
animation in VRML/X3D, you can remove the <tt>"Camera.Idle"</tt> line.

<p><tt>FrameTime</tt> is a constant saying how many seconds passed between frames.
Both <tt>TCastleSceneCore.IncreseTime</tt> and
<tt>TCamera.Idle</tt> take it's time argument in seconds. Don't worry, this
is a float, so actually it honors fractions of seconds as well.
So something like <tt>FrameTime := 1/25</tt> to get 25 frames per second will work
perfectly Ok.

<?php
tutorial_footer();
?>
