uses SysUtils, CastleVectors,
  CastleFilesUtils, CastleWindow, CastleSceneCore, CastleScene;

var
  Window: TCastleWindow;
  CarScene, RoadScene: TCastleScene;
begin
  Window := TCastleWindow.Create(Application);
  Window.Open;

  CarScene := TCastleScene.Create(Application);
  CarScene.Load('castle-data:/car.x3d');
  CarScene.Spatial := [ssRendering, ssDynamicCollisions];
  CarScene.ProcessEvents := true;

  RoadScene := TCastleScene.Create(Application);
  RoadScene.Load('castle-data:/road.x3d');
  RoadScene.Spatial := [ssRendering, ssDynamicCollisions];
  RoadScene.ProcessEvents := true;

  Window.SceneManager.Items.Add(CarScene);
  Window.SceneManager.Items.Add(RoadScene);
  Window.SceneManager.MainScene := RoadScene;

  // nice camera to see the road
  Window.SceneManager.RequiredCamera.SetView(
    Vector3(-43.30, 27.23, -80.74),
    Vector3(  0.60, -0.36,   0.70),
    Vector3(  0.18,  0.92,   0.32)
  );

  Application.Run;
end.
