uses CastleWindow, CastleSceneCore, CastleScene, CastleViewport;
var
  Window: TCastleWindowBase;
  Viewport: TCastleViewport;
  Scene: TCastleScene;
begin
  Window := TCastleWindowBase.Create(Application);
  Window.Open;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.AutoCamera := true; // instead of this, you could do "Viewport.Camera.SetView(...)"
  Viewport.AutoNavigation := true; // instead of this, you could do "Viewport.Navigation := ..."
  Window.Controls.InsertFront(Viewport);

  Scene := TCastleScene.Create(Application);
  Scene.Load('castle-data:/car.x3d');
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;

  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;

  Application.Run;
end.
