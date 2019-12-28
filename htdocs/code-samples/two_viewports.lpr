uses SysUtils, CastleColors, CastleSceneCore, CastleScene, CastleFilesUtils,
  CastleWindow, CastleViewport, CastleControls, CastleUIControls,
  CastleCameras, CastleVectors;
var
  Window: TCastleWindowBase;
  MainViewport: TCastleViewport;
  Scene: TCastleScene;
  AdditionalViewport: TCastleViewport;
  AdditionalViewportContainer: TCastleRectangleControl;
begin
  Window := TCastleWindowBase.Create(Application);
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;
  Window.Open;

  { Add a black background underneath. You must always draw on
    the whole window area, otherwise it's contents are undefined.
    In this program, we don't have a full-size viewport filling the whole
    screen, so we use TCastleSimpleBackground to clear the screen. }
  Window.Controls.InsertFront(TCastleSimpleBackground.Create(Application));

  Scene := TCastleScene.Create(Application);
  Scene.Load('castle-data:/level1.x3d');
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;

  MainViewport := TCastleViewport.Create(Application);
  MainViewport.AutoCamera := true;
  MainViewport.Left := 10;
  MainViewport.Bottom := 10;
  MainViewport.Width := 800;
  MainViewport.Height := 748;
  MainViewport.Items.Add(Scene);
  MainViewport.Items.MainScene := Scene;
  MainViewport.NavigationType := ntWalk;
  MainViewport.WalkCamera.MoveSpeed := 10;
  Window.Controls.InsertFront(MainViewport);

  { otherwise, inputs are only passed
    when mouse cursor is over the MainViewport. }
  Window.Container.ForceCaptureInput := MainViewport;

  AdditionalViewportContainer := TCastleRectangleControl.Create(Application);
  AdditionalViewportContainer.FullSize := false;
  AdditionalViewportContainer.Left := 820;
  AdditionalViewportContainer.Bottom := 10;
  AdditionalViewportContainer.Width := 256;
  AdditionalViewportContainer.Height := 256;
  AdditionalViewportContainer.Color := Silver;
  Window.Controls.InsertFront(AdditionalViewportContainer);

  AdditionalViewport := TCastleViewport.Create(Application);
  AdditionalViewport.FullSize := false;
  AdditionalViewport.Left := 10;
  AdditionalViewport.Bottom := 10;
  AdditionalViewport.Width := 236;
  AdditionalViewport.Height := 236;
  AdditionalViewport.Items := MainViewport.Items;
  AdditionalViewport.Transparent := true;
  AdditionalViewport.Camera.SetView(
    Vector3(5, 92.00, 0.99),
    Vector3(0, -1, 0),
    Vector3(0, 0, 1));
  AdditionalViewportContainer.InsertFront(AdditionalViewport);

  Application.Run;
end.
