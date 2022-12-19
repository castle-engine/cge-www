uses CastleWindow, CastleSceneCore, CastleScene, CastleVectors, CastleFilesUtils,
  CastleViewport, CastleCameras;
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
  Window.Controls.InsertFront(Viewport);

  Viewport.InsertFront(TCastleExamineNavigation.Create(Application));

  Scene := TCastleScene.Create(Application);
  Scene.Load('castle-data:/monkey_z_up.x3d');
  Scene.PreciseCollisions := true;
  // rotate by -90 degrees around X axis
  Scene.Rotation := Vector4(1, 0, 0, -Pi/2);

  Viewport.Items.Add(Scene);

  Application.Run;
end.
