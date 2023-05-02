{ Build shape from triangles using TTriangleSetNode. }

uses CastleWindow, CastleViewport, CastleScene, X3DNodes, CastleFilesUtils,
  CastleColors, CastleVectors, CastleTimeUtils, CastleSceneCore;

var
  Scene: TCastleScene;

function BuildScene: TX3DRootNode;
var
  Shape: TShapeNode;
  Geometry: TTriangleSetNode;
  Material: TPhysicalMaterialNode;
  Coord: TCoordinateNode;
begin
  Coord := TCoordinateNode.Create;
  // for TTriangleSetNode, each 3 points define a triangle
  Coord.SetPoint([
    Vector3(-1, -1, 0),
    Vector3( 1, -1, 0),
    Vector3( 1,  1, 0),

    Vector3(-1, 2, 0),
    Vector3( 1, 2, 0),
    Vector3( 1, 4, 0)
  ]);

  Geometry := TTriangleSetNode.CreateWithShape(Shape);
  Geometry.Solid := false; // see them from both sides
  Geometry.Coord := Coord;

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

  Viewport.Items.Add(Scene);

  Application.Run;
end.
