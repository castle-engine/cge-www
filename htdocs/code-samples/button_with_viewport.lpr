uses SysUtils, CastleVectors, CastleCameras,
  CastleColors, CastleSceneCore, CastleScene, CastleFilesUtils, CastleViewport,
  CastleUIControls, CastleWindow, CastleControls;
var
  Window: TCastleWindow;
  Button: TCastleButton;
  MyLabel: TCastleLabel;
  Viewport: TCastleViewport;
  Scene: TCastleScene;
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

  Scene := TCastleScene.Create(Application);
  Scene.Setup2D;
  Scene.Load('castle-data:/dragon/dragon.json');
  Scene.PreciseCollisions := true;
  Scene.PlayAnimation('flying', true);

  Viewport := TCastleViewport.Create(Application);
  Viewport.Setup2D;
  Viewport.Transparent := true;
  Viewport.FullSize := false;
  Viewport.Width := 390;
  Viewport.Height := 350;
  Viewport.Anchor(hpMiddle);
  Viewport.Anchor(vpBottom, 10);
  Viewport.Items.Add(Scene);
  { below adjusted to the scene size and position }
  Viewport.Camera.Orthographic.Width := 3000;
  Viewport.Camera.Orthographic.Origin := Vector2(0.5, 0.5);
  Viewport.Camera.SetView(
    Vector3(0, 500, TCastleViewport.Default2DCameraZ),
    Vector3(0, 0, -1),
    Vector3(0, 1, 0));
  Button.InsertFront(Viewport);

  Application.Run;
end.
