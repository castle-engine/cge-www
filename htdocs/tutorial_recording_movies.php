13. Recoding movies

Note that our engine doesn't support yet recording movies during interactive operations, like during a game. One reason for this is that it's non-trivial to implement, and the other reason is that you can use existing external programs to do this. Under Linux I like <a>glc</a>, under Windows there is <a>fraps</a> and many others.

However, we support recording a predefined animation in VRML/X3D (meaning: it plays without any user interaction) to a series of images. Such animation can then be converted into a movie e.g. using ffmpeg (our view3dscene does this automatically). Although this is limited to predefined animations, in exchange this allows to record the animation with perfect quality and with constant number of frames per second.

Do it like this:

[[
in a loop:
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
    SaveImage(Image, Format('screenshot_name_%4d.png', [FrameNumber]));
  finally FreeAndNil(Image) end;
end;
]]

IOW, you manually force the current time of the scene/camera to increase as much as you want before each screenshot. This makes it work 100% reliably, without any worries how fast you can process captured images.

The time of the scene and of the camera is actually independent. You can remove the "MainScene.IncreaseTime" call, if you know that your scene stays static (or you want to force it to look like static), and want to animate only camera. Or the other way around, if your camera is static (or you want to force it to be static) but you have an animation in VRML/X3D, you can remove the "Camera.Idle" line.

FrameTime is in seconds, as both TCastleSceneCore.IncreseTime and TCamera.Idle take it's time argument in seconds. But don't worry, this is a float, so actually it honors fractions of seconds as well. So something like 1/25 to get 25 frames per second is Ok.
