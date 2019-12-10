uses SysUtils, CastleVectors, CastleCameras,
  CastleColors, CastleSceneCore, CastleScene, CastleFilesUtils,
  CastleUIControls, CastleWindow, Castle2DSceneManager, CastleControls;
var
  Window: TCastleWindow;
  Button: TCastleButton;
  MyLabel: TCastleLabel;
  SceneManager: T2DSceneManager;
  Scene: T2DScene;
begin
  Window := TCastleWindow.Create(Application);
  Window.Open;

  Button := TCastleButton.Create(Application);
  Button.Anchor(hpMiddle);
  Button.Anchor(vpMiddle);
  Button.AutoSize := false;
  Button.Width := 400;
  Button.Height := 400;
  Window.Controls.InsertFront(Button);

  MyLabel := TCastleLabel.Create(Application);
  MyLabel.Caption := 'Click here for more dragons!';
  MyLabel.Anchor(hpMiddle);
  MyLabel.Anchor(vpTop, -10);
  MyLabel.Color := Black;
  Button.InsertFront(MyLabel);

  Scene := T2DScene.Create(Application);
  Scene.Load('castle-data:/dragon/dragon.json');
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;
  Scene.PlayAnimation('flying', true);

  SceneManager := T2DSceneManager.Create(Application);
  SceneManager.FullSize := false;
  SceneManager.Width := 390;
  SceneManager.Height := 350;
  SceneManager.Anchor(hpMiddle);
  SceneManager.Anchor(vpBottom, 10);
  SceneManager.Items.Add(Scene);
  SceneManager.MainScene := Scene;
  { below adjusted to the scene size and position }
  SceneManager.ProjectionAutoSize := false;
  SceneManager.ProjectionWidth := 3000;
  SceneManager.ProjectionOriginCenter := true;
  SceneManager.RequiredCamera.SetView(
    Vector3(0, 500, T2DSceneManager.DefaultCameraZ),
    Vector3(0, 0, -1),
    Vector3(0, 1, 0));
  Button.InsertFront(SceneManager);

  Application.Run;
end.
