{ Build 2D scene with a textured Rectangle2D and outline using LineSet,
  and rotate it. }

uses CastleWindow, Castle2DSceneManager, X3DNodes, CastleFilesUtils,
  CastleColors, CastleVectors, CastleTimeUtils, CastleSceneCore;

var
  LifeTime: TFloatTime;
  Scene: T2DScene;
  Transform: TTransformNode;

function BuildScene: TX3DRootNode;
var
  RectShape: TShapeNode;
  RectGeometry: TRectangle2DNode;
  RectTexture: TImageTextureNode;

  OutlineShape: TShapeNode;
  OutlineCoords: TCoordinateNode;
  OutlineGeometry: TLineSetNode;
begin
  Transform := TTransformNode.Create;

  RectGeometry := TRectangle2DNode.CreateWithShape(RectShape);
  RectGeometry.Size := Vector2(200, 200);

  RectTexture := TImageTextureNode.Create;
  RectTexture.SetUrl([ApplicationData('face.png')]);

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

  OutlineShape.Material := TMaterialNode.Create;
  OutlineShape.Material.EmissiveColor := YellowRGB;

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
  Window: TCastleWindowCustom;
  SceneManager: T2DSceneManager;
begin
  Window := TCastleWindowCustom.Create(Application);
  Window.Open;

  SceneManager := T2DSceneManager.Create(Application);
  SceneManager.Transparent := false;
  SceneManager.FullSize := true;
  SceneManager.ProjectionAutoSize := false;
  SceneManager.ProjectionHeight := 1000;
  SceneManager.ProjectionOriginCenter := true;
  Window.Controls.InsertFront(SceneManager);

  Scene := T2DScene.Create(Application);
  Scene.Load(BuildScene, true);
  Scene.ProcessEvents := true;
  SceneManager.Items.Add(Scene);

  Window.OnUpdate := @WindowUpdate;

  Application.Run;
end.
