{
  Copyright 2018-2024 Michalis Kamburelis.
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

uses CastleWindow, CastleViewport, X3DNodes, CastleCameras,
  CastleColors, CastleVectors, CastleScene;

function BuildScene: TX3DRootNode;
var
  Shape: TShapeNode;
  Material: TMaterialNode;
  //Sphere: TSphereNode; // unused
  Transform: TTransformNode;
  TimeSensor: TTimeSensorNode;
  PositionInterpolator: TPositionInterpolatorNode;
  Appearance: TAppearanceNode;
begin
  Result := TX3DRootNode.Create;

  Material := TMaterialNode.Create;
  Material.DiffuseColor := YellowRGB;

  Appearance := TAppearanceNode.Create;
  Appearance.Material := Material;

  {Sphere := }TSphereNode.CreateWithTransform(Shape, Transform);
  Shape.Appearance := Appearance;
  Result.AddChildren(Transform);

  TimeSensor := TTimeSensorNode.Create('MyAnimationName');
  TimeSensor.CycleInterval := 4;
  Result.AddChildren(TimeSensor);

  PositionInterpolator := TPositionInterpolatorNode.Create;
  PositionInterpolator.SetKey([0, 0.75, 1]);
  PositionInterpolator.SetKeyValue([Vector3(0, 0, 0), Vector3(10, 0, 0), Vector3(10, 10, 0)]);
  Result.AddChildren(PositionInterpolator);

  Result.AddRoute(TimeSensor.EventFraction_Changed, PositionInterpolator.EventSet_Fraction);
  Result.AddRoute(PositionInterpolator.EventValue_Changed, Transform.FdTranslation.EventIn);
end;

var
  Window: TCastleWindow;
  Viewport: TCastleViewport;
  Scene: TCastleScene;
begin
  Window := TCastleWindow.Create(Application);
  Window.Open;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.InsertFront(TCastleExamineNavigation.Create(Application));
  Window.Controls.InsertFront(Viewport);

  // add headlight
  Viewport.Camera.Add(TCastleDirectionalLight.Create(Application));

  Scene := TCastleScene.Create(Application);
  Scene.Load(BuildScene, true);
  Scene.PlayAnimation('MyAnimationName', true);
  Viewport.Items.Add(Scene);

  // move camera, to better see the animation
  Viewport.Camera.Translation := Vector3(0, 0, 30);

  Application.Run;
end.
