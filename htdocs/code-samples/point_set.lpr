uses CastleVectors, CastleWindow, X3DNodes, X3DLoad, CastleScene, CastleViewport,
  CastleCameras;

var
  PointSet: TPointSetNode;
  PointCoordinates: TCoordinateNode;
  PointShape: TShapeNode;
  Root: TX3DRootNode;
  Window: TCastleWindow;
  Scene: TCastleScene;
  Viewport: TCastleViewport;
begin
  Window := TCastleWindow.Create(Application);
  Window.Open;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.AutoCamera := true;
  Viewport.Navigation := TCastleExamineNavigation.Create(Application);
  Window.Controls.InsertFront(Viewport);

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
    or render Root (loading it to Scene, adding Scene to TCastleViewport).
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

  SaveNode(Root, 'my_points.x3d');

  Scene := TCastleScene.Create(Application);
  Scene.Load(Root, true);
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.RenderOptions.PointSize := 10;

  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;

  Application.Run;
end.
