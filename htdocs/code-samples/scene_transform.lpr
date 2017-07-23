uses SysUtils, CastleVectors, Castle3D,
  CastleFilesUtils, CastleWindow, CastleSceneCore, CastleScene;

var
  Window: TCastleWindow;
  CarScene, RoadScene: TCastleScene;
  CarTransform: T3DTransform;
begin
  Window := TCastleWindow.Create(Application);

  CarScene := TCastleScene.Create(Application);
  CarScene.Load(ApplicationData('car.x3d'));
  CarScene.Spatial := [ssRendering, ssDynamicCollisions];
  CarScene.ProcessEvents := true;

  CarTransform := T3DTransform.Create(Application);
  CarTransform.Add(CarScene);

  RoadScene := TCastleScene.Create(Application);
  RoadScene.Load(ApplicationData('road.x3d'));
  RoadScene.Spatial := [ssRendering, ssDynamicCollisions];
  RoadScene.ProcessEvents := true;

  Window.SceneManager.Items.Add(CarTransform);
  Window.SceneManager.Items.Add(RoadScene);
  Window.SceneManager.MainScene := RoadScene;

  // nice camera to see the road
  Window.SceneManager.RequiredCamera.SetView(
    Vector3Single(-43.30, 27.23, -80.74),
    Vector3Single(  0.60, -0.36,   0.70),
    Vector3Single(  0.18,  0.92,   0.32)
  );

  Window.Open;
  Application.Run;
end.
