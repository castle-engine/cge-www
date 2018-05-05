uses CastleVectors, CastleWindow, X3DNodes, CastleScene;

var
  PointSet: TPointSetNode;
  PointCoordinates: TCoordinateNode;
  PointShape: TShapeNode;
  Root: TX3DRootNode;
  Window: TCastleWindow;
  Scene: TCastleScene;
begin
  Window := TCastleWindow.Create(Application);
  Window.Open;

  PointCoordinates := TCoordinateNode.Create;
  PointCoordinates.SetPoint(
    [Vector3(0, 0, 0),
     Vector3(0, 1, 0),
     Vector3(1, 1, 0),
     Vector3(1, 0, 0),
     Vector3(0.5, 0.5, 0)
   ]);

  PointSet := TPointSetNode.Create;
  PointSet.Coord := PointCoordinates;

  PointShape := TShapeNode.Create;
  PointShape.Geometry := PointSet;

  Root := TX3DRootNode.Create;
  Root.AddChildren(PointShape);

  { You can now save Root to X3D file,
    or render Root (loading it to Scene, adding Scene to Window.SceneManager).
    Or both, as the example below shows.

    Notes about memory management:

    - the PointCoordinates are owned by PointSet,
    - PointSet is owned by PointShape,
    - PointShape is owned by Root,
    - Root is owned by Scene (because we call Scene.Load with 2nd parameter "true"),
    - and Scene is owned by Application.

    So in this example, everything will be automatically freed when
    Application is freed (which happens in CastleWindow unit finalization).
    If you would not call "Scene.Load(Root, true)", then you should free Root
    manually at the end, by "FreeAndNil(Root)".
  }

  Save3D(Root, 'my_points.x3d');

  Scene := TCastleScene.Create(Application);
  Scene.Load(Root, true);
  Scene.ProcessEvents := true;
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.Attributes.PointSize := 10;

  Window.SceneManager.Items.Add(Scene);

  Application.Run;
end.
