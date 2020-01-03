{ Demo of Group, Transform, Switch nodes. }

uses SysUtils,
  CastleWindow, CastleScene, CastleViewport,
  CastleColors, CastleVectors, CastleFilesUtils, X3DNodes, CastleKeysMouse;

var
  Switch: TSwitchNode;

function BuildRootNode: TX3DRootNode;
var
  GroupBoxes: TGroupNode;
  Box: TBoxNode;
  BoxTransform: TTransformNode;
  BoxShape: TShapeNode;
  BoxMaterial: TMaterialNode;
  TransformSwitch: TTransformNode;
  SwitchChildShape: TShapeNode;
begin
  Result := TX3DRootNode.Create;

  GroupBoxes := TGroupNode.Create;
  Result.AddChildren(GroupBoxes);

  { create red box }

  Box := TBoxNode.CreateWithTransform(BoxShape, BoxTransform);
  Box.Size := Vector3(0.75, 0.75, 0.75);

  { TBoxNode.CreateWithTransform is a shortcut for:

    Box := TBoxNode.Create;
    BoxShape := TShapeNode.Create;
    BoxShape.Geometry := Box;
    BoxTransform := TTransformNode.Create;
    BoxTransform.AddChildren(BoxShape);
  }

  BoxTransform.Translation := Vector3(1, 0, 0);

  BoxMaterial := TMaterialNode.Create;
  BoxMaterial.DiffuseColor := RedRGB;

  { Assigning Shape.Material is a shortcut to
    - creating TAppearanceNode,
    - placing it in Shape.Appearance
    - placing the material in Shape.Appearance.Material. }
  BoxShape.Material := BoxMaterial;

  GroupBoxes.AddChildren(BoxTransform);

  { create green box }

  Box := TBoxNode.CreateWithTransform(BoxShape, BoxTransform);
  Box.Size := Vector3(0.75, 0.75, 0.75);
  BoxTransform.Translation := Vector3(2, 0, 0);
  BoxMaterial := TMaterialNode.Create;
  BoxMaterial.DiffuseColor := GreenRGB;
  BoxShape.Material := BoxMaterial;
  GroupBoxes.AddChildren(BoxTransform);

  { create blue box }

  Box := TBoxNode.CreateWithTransform(BoxShape, BoxTransform);
  Box.Size := Vector3(0.75, 0.75, 0.75);
  BoxTransform.Translation := Vector3(3, 0, 0);
  BoxMaterial := TMaterialNode.Create;
  BoxMaterial.DiffuseColor := BlueRGB;
  BoxShape.Material := BoxMaterial;
  GroupBoxes.AddChildren(BoxTransform);

  { create translated Switch node with children }

  TransformSwitch := TTransformNode.Create;
  TransformSwitch.Translation := Vector3(2, -2, 0);
  Result.AddChildren(TransformSwitch);

  Switch := TSwitchNode.Create;
  Switch.WhichChoice := 0; // initially
  TransformSwitch.AddChildren(Switch);

  TSphereNode.CreateWithShape(SwitchChildShape);
  SwitchChildShape.Material := TMaterialNode.Create; // assign any material, to make it lit
  Switch.AddChildren(SwitchChildShape);

  TConeNode.CreateWithShape(SwitchChildShape);
  SwitchChildShape.Material := TMaterialNode.Create; // assign any material, to make it lit
  Switch.AddChildren(SwitchChildShape);

  TCylinderNode.CreateWithShape(SwitchChildShape);
  SwitchChildShape.Material := TMaterialNode.Create; // assign any material, to make it lit
  Switch.AddChildren(SwitchChildShape);
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.IsKey(keyS) then
  begin
    // Switch.WhichChoice cycles from -1 to 2.
    Switch.WhichChoice := Switch.WhichChoice + 1;
    if Switch.WhichChoice > 2 then
      Switch.WhichChoice := -1;
  end;
end;

var
  Window: TCastleWindowBase;
  Viewport: TCastleViewport;
  Scene: TCastleScene;
begin
  Window := TCastleWindowBase.Create(Application);
  Window.Open;
  Window.OnPress := @WindowPress;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.AutoCamera := true;
  Viewport.AutoNavigation := true;
  Window.Controls.InsertFront(Viewport);

  Scene := TCastleScene.Create(Application);
  Scene.Load(BuildRootNode, true);
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;

  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;

  Application.Run;
end.
