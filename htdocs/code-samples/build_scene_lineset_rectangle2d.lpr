{ Build 2D scene with a textured Rectangle2D and outline using LineSet,
  and rotate it. }

uses CastleWindow, CastleViewport, CastleScene, X3DNodes, CastleFilesUtils,
  CastleColors, CastleVectors, CastleTimeUtils, CastleSceneCore;

var
  LifeTime: TFloatTime;
  Scene: TCastleScene;
  Transform: TTransformNode;

function BuildScene: TX3DRootNode;
var
  RectShape: TShapeNode;
  RectGeometry: TRectangle2DNode;
  RectTexture: TImageTextureNode;

  OutlineShape: TShapeNode;
  OutlineCoords: TCoordinateNode;
  OutlineGeometry: TLineSetNode;
  Material: TMaterialNode;
begin
  Transform := TTransformNode.Create;

  RectGeometry := TRectangle2DNode.CreateWithShape(RectShape);
  RectGeometry.Size := Vector2(200, 200);

  RectTexture := TImageTextureNode.Create;
  RectTexture.SetUrl(['castle-data:/face.png']);

  RectShape.Appearance := TAppearanceNode.Create;
  RectShape.Appearance.Texture := RectTexture;

  Transform.AddChildren(RectShape);

  OutlineCoords := TCoordinateNode.Create;
  OutlineCoords.SetPoint([
    // Z = 1 to be on top of RectShape that has Z = 0
    Vector3(-100, -100, 1),
    Vector3( 100, -100, 1),
    Vector3( 100,  100, 1),
    Vector3(-100,  100, 1),
    Vector3(-100, -100, 1)
  ]);

  OutlineGeometry := TLineSetNode.CreateWithShape(OutlineShape);
  OutlineGeometry.Coord := OutlineCoords;
  OutlineGeometry.SetVertexCount([OutlineCoords.FdPoint.Count]);

  Material := TMaterialNode.Create;
  Material.EmissiveColor := YellowRGB;
  OutlineShape.Material := Material;

  Transform.AddChildren(OutlineShape);

  Result := TX3DRootNode.Create;
  Result.AddChildren(Transform);
end;

procedure WindowUpdate(Container: TUIContainer);
begin
  LifeTime := LifeTime + Container.Fps.SecondsPassed;

  // update rotation every frame
  Transform.Rotation := Vector4(0, 0, 1, LifeTime * 2);

  // Note: in this case, since you just rotate whole Scene,
  // you could also rotate it like this:
  //Scene.Rotation := Vector4(0, 0, 1, LifeTime * 2);
  // There's no need for TTransformNode in this case.
end;

var
  Window: TCastleWindowBase;
  Viewport: TCastleViewport;
begin
  Window := TCastleWindowBase.Create(Application);
  Window.Open;

  Viewport := TCastleViewport.Create(Application);
  Viewport.Setup2D;
  Viewport.FullSize := true;
  Viewport.Camera.Orthographic.Height := 1000;
  Viewport.Camera.Orthographic.Origin := Vector2(0.5, 0.5);
  Window.Controls.InsertFront(Viewport);

  Scene := TCastleScene.Create(Application);
  Scene.Setup2D;
  Scene.Load(BuildScene, true);

  Viewport.Items.Add(Scene);

  Window.OnUpdate := @WindowUpdate;

  Application.Run;
end.
