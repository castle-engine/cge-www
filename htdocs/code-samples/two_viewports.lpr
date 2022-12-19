uses SysUtils, CastleColors, CastleSceneCore, CastleScene, CastleFilesUtils,
  CastleWindow, CastleViewport, CastleControls, CastleUIControls,
  CastleCameras, CastleVectors;
var
  Window: TCastleWindow;
  Navigation: TCastleWalkNavigation;
  MainViewport: TCastleViewport;
  Scene: TCastleScene;
  AdditionalViewport: TCastleViewport;
  AdditionalViewportContainer: TCastleRectangleControl;
begin
  Window := TCastleWindow.Create(Application);
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;
  Window.Open;

  Scene := TCastleScene.Create(Application);
  Scene.Load('castle-data:/level1.x3d');
  Scene.PreciseCollisions := true;

  MainViewport := TCastleViewport.Create(Application);
  MainViewport.AutoCamera := true;
  MainViewport.Anchor(hpLeft, 10);
  MainViewport.Anchor(vpBottom, 10);
  MainViewport.Width := 800;
  MainViewport.Height := 748;
  MainViewport.Items.Add(Scene);
  MainViewport.Items.MainScene := Scene; // makes AutoCamera use camera from level1.x3d
  Window.Controls.InsertFront(MainViewport);

  Navigation := TCastleWalkNavigation.Create(Application);
  Navigation.MoveSpeed := 10;
  MainViewport.InsertFront(Navigation);

  { otherwise, inputs are only passed
    when mouse cursor is over the MainViewport. }
  Window.Container.ForceCaptureInput := MainViewport;

  AdditionalViewportContainer := TCastleRectangleControl.Create(Application);
  AdditionalViewportContainer.FullSize := false;
  AdditionalViewportContainer.Anchor(hpLeft, 820);
  AdditionalViewportContainer.Anchor(vpBottom, 10);
  AdditionalViewportContainer.Width := 256;
  AdditionalViewportContainer.Height := 256;
  AdditionalViewportContainer.Color := Silver;
  Window.Controls.InsertFront(AdditionalViewportContainer);

  AdditionalViewport := TCastleViewport.Create(Application);
  AdditionalViewport.FullSize := true;

  // move AdditionalViewport.Camera instance to MainViewport.Items
  AdditionalViewport.Items.Remove(AdditionalViewport.Camera);
  MainViewport.Items.Add(AdditionalViewport.Camera);
  AdditionalViewport.Items := MainViewport.Items;

  AdditionalViewport.Transparent := true;
  AdditionalViewport.Camera.SetView(
    Vector3(5, 92.00, 0.99),
    Vector3(0, -1, 0),
    Vector3(0, 0, 1));
  AdditionalViewportContainer.InsertFront(AdditionalViewport);

  Application.Run;
end.
