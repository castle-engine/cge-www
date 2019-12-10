uses CastleWindow, CastleSceneCore, CastleScene, CastleVectors, CastleFilesUtils;
var
  Window: TCastleWindow;
  Scene: TCastleScene;
begin
  Window := TCastleWindow.Create(Application);
  Window.Open;

  Scene := TCastleScene.Create(Application);
  Scene.Load('castle-data:/monkey_z_up.x3d');
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;
  // rotate by -90 degrees around X axis
  Scene.Rotation := Vector4(1, 0, 0, -Pi/2);

  Window.SceneManager.Items.Add(Scene);
  Window.SceneManager.MainScene := Scene;

  Application.Run;
end.
