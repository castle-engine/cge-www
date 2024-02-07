{ Example code that creates and shows two TCastleViewport instances
  to show the same 3D model (TCastleScene) from different cameras.

  See https://castle-engine.io/multiple_viewports_to_display_one_world
  for more information about using multiple viewports.

  For the purpose of this demo, we set up everything from code,
  i.e. we create instances of everything using Pascal code.
  For a real application, we encourage to rather design it using CGE editor,
  and load a design file.
  Follow the standard engine "New Project" templates and see
  https://castle-engine.io/views about the simple ways to load a design.
  To do this in TCastleControl, see also https://castle-engine.io/control_on_form ,
}

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
  MainViewport.Anchor(hpLeft, 10);
  MainViewport.Anchor(vpBottom, 10);
  MainViewport.Width := 800;
  MainViewport.Height := 748;
  MainViewport.Items.Add(Scene);
  Window.Controls.InsertFront(MainViewport);

  // Set initial camera
  MainViewport.Camera.SetWorldView(
    Vector3(3.25, 2.00, -5.42), // position
    Vector3(-0.81, 0.00, 0.59), // direction
    Vector3(0.00, 1.00, 0.00)  // up (current)
  );

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
