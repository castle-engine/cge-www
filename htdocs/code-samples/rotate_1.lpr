uses CastleWindow, CastleSceneCore, CastleScene, CastleVectors, CastleFilesUtils,
  CastleViewport;
var
  Window: TCastleWindow;
  Viewport: TCastleViewport;
  Scene: TCastleScene;
begin
  Window := TCastleWindow.Create(Application);
  Window.Open;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.AutoCamera := true;
  Viewport.AutoNavigation := true;
  Window.Controls.InsertFront(Viewport);

  Scene := TCastleScene.Create(Application);
  Scene.Load('castle-data:/monkey_z_up.x3d');
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  // rotate by -90 degrees around X axis
  Scene.Rotation := Vector4(1, 0, 0, -Pi/2);

  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;

  Application.Run;
end.
