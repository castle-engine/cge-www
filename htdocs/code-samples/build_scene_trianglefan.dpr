{ Build shape from triangles using TTriangleFanSetNode. }

uses CastleWindow, CastleViewport, CastleScene, X3DNodes, CastleFilesUtils,
  CastleColors, CastleVectors, CastleTimeUtils, CastleSceneCore;

var
  Scene: TCastleScene;

function BuildScene: TX3DRootNode;
var
  Shape: TShapeNode;
  Geometry: TTriangleFanSetNode;
  Material: TPhysicalMaterialNode;
  Coord: TCoordinateNode;
begin
  Coord := TCoordinateNode.Create;
  Coord.SetPoint([
    // 1st fan
    Vector3(-1, -1, 0),
    Vector3( 1, -1, 0),
    Vector3( 1,  1, 0), // 1st triangle of 1st fan ends here

    Vector3(0.6, 1, 0), // 2nd triangle of 1st fan ends here
    Vector3(0.2, 1, 0), // 3rd triangle of 1st fan ends here
    Vector3(-0.2, 1, 0), // 4th triangle of 1st fan ends here

    // 2nd fan
    Vector3(-1, 2, 0),
    Vector3( 1, 2, 0),
    Vector3( 1, 4, 0), // 1st triangle of 2nd fan ends here

    Vector3(0.6, 4, 0), // 2nd triangle of 2nd fan ends here
    Vector3(0.2, 4, 0), // 3rd triangle of 2nd fan ends here
    Vector3(-0.2, 4, 0) // 4th triangle of 2nd fan ends here
  ]);

  Geometry := TTriangleFanSetNode.CreateWithShape(Shape);
  Geometry.Solid := false; // see them from both sides
  Geometry.Coord := Coord;
  // define 2 fans
  Geometry.SetFanCount([6, 6]);

  Material := TPhysicalMaterialNode.Create;
  Material.BaseColor := YellowRGB;

  Shape.Appearance := TAppearanceNode.Create;
  Shape.Appearance.Material := Material;

  Result := TX3DRootNode.Create;
  Result.AddChildren(Shape);
end;

var
  Window: TCastleWindow;
  Viewport: TCastleViewport;
  Headlight: TCastleDirectionalLight;
begin
  Window := TCastleWindow.Create(Application);
  Window.Open;

  Headlight := TCastleDirectionalLight.Create(Application);
  Headlight.Intensity := 10;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.Camera.SetView(
    Vector3(0, 0, 10), // position
    Vector3(0, 0, -1), // look direction
    Vector3(0, 1, 0) // up
  );
  // add a headlight
  Viewport.Camera.Add(Headlight);
  Window.Controls.InsertFront(Viewport);

  Scene := TCastleScene.Create(Application);
  Scene.Load(BuildScene, true);
  // makes it easier to see triangle
  Scene.RenderOptions.WireframeEffect := weSolidWireframe;

  Viewport.Items.Add(Scene);

  Application.Run;
end.
