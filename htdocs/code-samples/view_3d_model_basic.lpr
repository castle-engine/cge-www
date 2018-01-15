uses CastleWindow, CastleSceneCore, CastleScene;
var
  Window: TCastleWindow;
  Scene: TCastleScene;
begin
  Window := TCastleWindow.Create(Application);
  Window.Open;

  Scene := TCastleScene.Create(Application);
  Scene.Load('car.x3d');
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;

  Window.SceneManager.Items.Add(Scene);
  Window.SceneManager.MainScene := Scene;

  Application.Run;
end.
