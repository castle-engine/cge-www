uses SysUtils, CastleColors, CastleSceneCore, CastleScene, CastleFilesUtils,
  CastleWindow, CastleSceneManager, CastleControls, CastleUIControls,
  CastleCameras, CastleVectors;
var
  Window: TCastleWindowCustom;
  SceneManager: TCastleSceneManager;
  Scene: TCastleScene;
  ViewportRect: TCastleRectangleControl;
  Viewport: TCastleViewport;
begin
  Window := TCastleWindow.Create(Application);
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;

  Scene := TCastleScene.Create(Application);
  Scene.Load(ApplicationData('level1.x3d'));
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;

  SceneManager := TCastleSceneManager.Create(Application);
  SceneManager.FullSize := false;
  SceneManager.Left := 10;
  SceneManager.Bottom := 10;
  SceneManager.Width := 800;
  SceneManager.Height := 748;
  SceneManager.Items.Add(Scene);
  SceneManager.MainScene := Scene;
  (SceneManager.RequiredCamera as TUniversalCamera).NavigationType := ntWalk;
  (SceneManager.RequiredCamera as TUniversalCamera).Walk.MoveSpeed := 10;
  Window.Controls.InsertFront(SceneManager);

  { otherwise, inputs are only passed
    when mouse cursor is over the SceneManager. }
  Window.Container.ForceCaptureInput := SceneManager;

  ViewportRect := TCastleRectangleControl.Create(Application);
  ViewportRect.FullSize := false;
  ViewportRect.Left := 820;
  ViewportRect.Bottom := 10;
  ViewportRect.Width := 256;
  ViewportRect.Height := 256;
  ViewportRect.Color := Silver;
  Window.Controls.InsertFront(ViewportRect);

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := false;
  Viewport.Left := 10;
  Viewport.Bottom := 10;
  Viewport.Width := 236;
  Viewport.Height := 236;
  Viewport.SceneManager := SceneManager;
  Viewport.Transparent := true;
  (Viewport.RequiredCamera as TUniversalCamera).NavigationType := ntNone;
  (Viewport.RequiredCamera as TUniversalCamera).SetView(
    Vector3Single(5, 92.00, 0.99),
    Vector3Single(0, -1, 0),
    Vector3Single(0, 0, 1));
  ViewportRect.InsertFront(Viewport);

  Window.OpenAndRun;
end.