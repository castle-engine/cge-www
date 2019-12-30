uses SysUtils, CastleVectors, CastleViewport,
  CastleFilesUtils, CastleWindow, CastleSceneCore, CastleScene;

var
  Window: TCastleWindowBase;
  Viewport: TCastleViewport;
  CarScene, RoadScene: TCastleScene;
begin
  Window := TCastleWindowBase.Create(Application);
  Window.Open;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.AutoNavigation := true;
  Window.Controls.InsertFront(Viewport);

  CarScene := TCastleScene.Create(Application);
  CarScene.Load('castle-data:/car.x3d');
  CarScene.Spatial := [ssRendering, ssDynamicCollisions];
  CarScene.ProcessEvents := true;

  RoadScene := TCastleScene.Create(Application);
  RoadScene.Load('castle-data:/road.x3d');
  RoadScene.Spatial := [ssRendering, ssDynamicCollisions];
  RoadScene.ProcessEvents := true;

  Viewport.Items.Add(CarScene);
  Viewport.Items.Add(RoadScene);
  Viewport.Items.MainScene := RoadScene;

  // nice camera to see the road
  Viewport.Camera.SetView(
    Vector3(-43.30, 27.23, -80.74),
    Vector3(  0.60, -0.36,   0.70),
    Vector3(  0.18,  0.92,   0.32)
  );

  Application.Run;
end.
