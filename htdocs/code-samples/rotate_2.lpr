uses CastleWindow, CastleSceneCore, CastleScene, CastleVectors, CastleFilesUtils,
  CastleViewport, CastleTransform;
var
  Window: TCastleWindowBase;
  Viewport: TCastleViewport;
  Scene1, Scene2: TCastleScene;
  Transform: TCastleTransform;
begin
  Window := TCastleWindowBase.Create(Application);
  Window.Open;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.AutoCamera := true;
  Viewport.AutoNavigation := true;
  Window.Controls.InsertFront(Viewport);

  Transform := TCastleTransform.Create(Application);
  // rotate by 90 degrees around X axis
  Transform.Rotation := Vector4(1, 0, 0, -Pi/2);

  Scene1 := TCastleScene.Create(Application);
  Scene1.Load('castle-data:/monkey_z_up.x3d');
  Scene1.Spatial := [ssRendering, ssDynamicCollisions];
  Scene1.Translation := Vector3(1, 1, 0);

  Scene2 := TCastleScene.Create(Application);
  Scene2.Load('castle-data:/monkey_z_up.x3d');
  Scene2.Spatial := [ssRendering, ssDynamicCollisions];
  Scene2.Translation := Vector3(-1, -1, 0);

  Transform.Add(Scene1);
  Transform.Add(Scene2);
  Viewport.Items.Add(Transform);

  Application.Run;
end.
