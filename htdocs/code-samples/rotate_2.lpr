uses CastleWindow, CastleSceneCore, CastleScene, CastleVectors, CastleFilesUtils,
  CastleTransform;
var
  Window: TCastleWindow;
  Scene1, Scene2: TCastleScene;
  Transform: TCastleTransform;
begin
  Window := TCastleWindow.Create(Application);
  Window.Open;

  Transform := TCastleTransform.Create(Application);
  // rotate by 90 degrees around X axis
  Transform.Rotation := Vector4(1, 0, 0, -Pi/2);

  Scene1 := TCastleScene.Create(Application);
  Scene1.Load('castle-data:/monkey_z_up.x3d');
  Scene1.Spatial := [ssRendering, ssDynamicCollisions];
  Scene1.ProcessEvents := true;
  Scene1.Translation := Vector3(1, 1, 0);

  Scene2 := TCastleScene.Create(Application);
  Scene2.Load('castle-data:/monkey_z_up.x3d');
  Scene2.Spatial := [ssRendering, ssDynamicCollisions];
  Scene2.ProcessEvents := true;
  Scene2.Translation := Vector3(-1, -1, 0);

  Transform.Add(Scene1);
  Transform.Add(Scene2);
  Window.SceneManager.Items.Add(Transform);

  Application.Run;
end.
