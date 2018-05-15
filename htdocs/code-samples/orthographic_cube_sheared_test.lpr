{ Test of orthographic projection (OrthoViewpoint node) showing a few copies
  of a special mesh (that looks a bit like a box, but with back side shifted). }

uses SysUtils,
  CastleWindow, CastleScene, CastleSceneManager,
  CastleColors, CastleVectors, CastleFilesUtils, X3DNodes, CastleTransform;

procedure AddTexturedShearedCube(
  const RootNode: TX3DRootNode; const Translation: TVector3);
const
  BackFaceShift: TVector2 = (Data: (0.5, 0.5));
var
  Faces: TIndexedFaceSetNode;
  Shape: TShapeNode;
  Transform: TTransformNode;
  Coordinate: TCoordinateNode;
  TextureCoordinate: TTextureCoordinateNode;
  Texture: TImageTextureNode;
begin
  Coordinate := TCoordinateNode.Create;
  Coordinate.SetPoint([
    Vector3(-1, -1, 1),
    Vector3( 1, -1, 1),
    Vector3( 1,  1, 1),
    Vector3(-1,  1, 1),

    Vector3(-1 + BackFaceShift.X, -1 + BackFaceShift.Y, -1),
    Vector3( 1 + BackFaceShift.X, -1 + BackFaceShift.Y, -1),
    Vector3( 1 + BackFaceShift.X,  1 + BackFaceShift.Y, -1),
    Vector3(-1 + BackFaceShift.X,  1 + BackFaceShift.Y, -1)
  ]);

  TextureCoordinate := TTextureCoordinateNode.Create;
  TextureCoordinate.SetPoint([
    Vector2(0, 0),
    Vector2(1, 0),
    Vector2(1, 1),
    Vector2(0, 1)
  ]);

  { This creates TIndexedFaceSetNode,
    with a TShapeNode that contains it
    (Shape.Geometry will be set to Faces),
    with a TTransformNode that contains it
    (Transform first and only child will be Shape).

    See X3D IndexedFaceSet documentation for the meaning of all this:
    http://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/geometry3D.html#IndexedFaceSet
    https://castle-engine.io/x3d_implementation_geometry3d.php
  }
  Faces := TIndexedFaceSetNode.CreateWithTransform(Shape, Transform);
  Faces.Coord := Coordinate;
  Faces.TexCoord := TextureCoordinate;
  Faces.Solid := false; // to see it from any side
  Faces.SetCoordIndex([
    // front face
    0, 1, 2, 3, -1,
    // back face; you don't need to see this face
    // 4, 5, 6, 7, -1
    // two side faces that you want to see
    1, 5, 6, 2, -1,
    3, 2, 6, 7, -1
  ]);
  Faces.SetTexCoordIndex([
    // texture mapping at each face
    0, 1, 2, 3, -1,
    0, 1, 2, 3, -1,
    0, 1, 2, 3, -1
  ]);

  Transform.Translation := Translation;

  Shape.Appearance := TAppearanceNode.Create;

  Texture := TImageTextureNode.Create;
  Texture.SetUrl([ApplicationData('textures/test_texture.png')]);
  Shape.Appearance.Texture := Texture;

  RootNode.AddChildren(Transform);
end;

procedure AddOrthoViewpoint(const RootNode: TX3DRootNode);
var
  OrthoViewpoint: TOrthoViewpointNode;
begin
  { See about OrthoViewpoint:
    http://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/navigation.html#OrthoViewpoint
    https://castle-engine.io/x3d_implementation_navigation.php

    Note: we could set OrthoViewpoint.Position and Orientation now,
    to control the initial camera position/direction/up.
    But instead, we leave them at default,
    and configure them later by WalkCamera.SetView.
    Both approaches are fine, use whichever one seems more comfortable. }

  OrthoViewpoint := TOrthoViewpointNode.Create;
  OrthoViewpoint.SetFieldOfView([-5, -5, 5, 5]);
  RootNode.AddChildren(OrthoViewpoint);
end;

var
  Window: TCastleWindow;
  Scene: TCastleScene;
  RootNode: TX3DRootNode;
begin
  Window := TCastleWindow.Create(Application);
  Window.Open;

  RootNode := TX3DRootNode.Create;
  AddOrthoViewpoint(RootNode);
  AddTexturedShearedCube(RootNode, Vector3(0, 0, 0));
  AddTexturedShearedCube(RootNode, Vector3(2, 0, 0));
  AddTexturedShearedCube(RootNode, Vector3(0, 2, 0));
  AddTexturedShearedCube(RootNode, Vector3(5, 5, 0));

  Scene := TCastleScene.Create(Application);
  Scene.Load(RootNode, true);
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;
  Window.SceneManager.Items.Add(Scene);
  Window.SceneManager.MainScene := Scene;

{
  // configure initial camera view
  Window.SceneManager.WalkCamera.SetView(
    Vector3(0, 0, 10),
    Vector3(0, 0, -1),
    Vector3(0, 1, 0)
  );
  // do not allow user to move camera by arrow keys etc.
  Window.SceneManager.WalkCamera.Input := [];
}

  // for debug: configure initial camera view, let user rotate the scene
  Window.SceneManager.ExamineCamera.SetView(
    Vector3(3, 3, 10),
    Vector3(0, 0, -1),
    Vector3(0, 1, 0)
  );

  Application.Run;
end.
