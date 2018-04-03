{
  Copyright 2018 Michalis Kamburelis.
  No warranty.
  This example is under a permissive Apache 2.0 license,
  https://www.apache.org/licenses/LICENSE-2.0 .
  Feel free to modify and reuse.

  ----------------------------------------------------------------------------
}

{ Example program that builds a scene with an animated sphere,
  using Castle Game Engine and Object Pascal.
  See https://castle-engine.io/x3d_implementation_interpolation.php
  for description what the nodes used here (like TimeSensor) do. }

uses CastleWindow, CastleSceneManager, X3DNodes,
  CastleColors, CastleVectors, CastleScene;

function BuildScene: TX3DRootNode;
var
  Shape: TShapeNode;
  Sphere: TSphereNode;
  Transform: TTransformNode;
  TimeSensor: TTimeSensorNode;
  PositionInterpolator: TPositionInterpolatorNode;
  Route1, Route2: TX3DRoute;
begin
  Result := TX3DRootNode.Create;

  Sphere := TSphereNode.CreateWithTransform(Shape, Transform);
  { Note that assigning to Shape.Material is a shortcut for creating
    Shape.Appearance, and assigning to Shape.Appearance.Material. }
  Shape.Material := TMaterialNode.Create;
  Shape.Material.DiffuseColor := YellowRGB;
  Result.AddChildren(Transform);

  TimeSensor := TTimeSensorNode.Create('MyAnimationName');
  TimeSensor.CycleInterval := 4;
  Result.AddChildren(TimeSensor);

  PositionInterpolator := TPositionInterpolatorNode.Create;
  PositionInterpolator.SetKey([0, 0.75, 1]);
  PositionInterpolator.SetKeyValue([Vector3(0, 0, 0), Vector3(10, 0, 0), Vector3(10, 10, 0)]);
  Result.AddChildren(PositionInterpolator);

  Route1 := TX3DRoute.Create;
  Route1.SetSourceDirectly(TimeSensor.EventFraction_Changed);
  Route1.SetDestinationDirectly(PositionInterpolator.EventSet_Fraction);
  Result.AddRoute(Route1);

  Route2 := TX3DRoute.Create;
  Route2.SetSourceDirectly(PositionInterpolator.EventValue_Changed);
  Route2.SetDestinationDirectly(Transform.FdTranslation.EventIn);
  Result.AddRoute(Route2);
end;

var
  Window: TCastleWindowCustom;
  SceneManager: TCastleSceneManager;
  Scene: TCastleScene;
begin
  Window := TCastleWindowCustom.Create(Application);
  Window.Open;

  SceneManager := TCastleSceneManager.Create(Application);
  SceneManager.FullSize := true;
  Window.Controls.InsertFront(SceneManager);

  Scene := TCastleScene.Create(Application);
  Scene.Load(BuildScene, true);
  Scene.ProcessEvents := true;
  Scene.PlayAnimation('MyAnimationName', paLooping);
  SceneManager.Items.Add(Scene);
  SceneManager.MainScene := Scene;

  // move camera, to better see the animation
  SceneManager.RequiredCamera.Position := Vector3(0, 0, 30);

  Application.Run;
end.
