<?php
  require_once 'tutorial_common.php';
  tutorial_header('Simple loading of 3D models');
?>

We will now load a 3D model from file. This is also a one approach to load a simple game level, although you will learn a more advanced way later.

If you use a VRML/X3D file, this model doesn't have to be static --- it can already include animated stuff, 3D sounds, scripts, and such.

- If you just want to try a ready 3D file, download this 3D file (link to *standalone* vrml file, in a single x3d file, with no textures etc.). You can open it first with view3dscene to see how it looks like.

Or copy more interesting 3D model from castle_game_engine/examples/vrml/models/: copy bridge_final.x3dv, bridge.wrl, and textures/ subdirectory to your project.

- If you want, you can of course make your own new level. You can generally use any 3D modeler and export to any 3D format we can handle. Our other tutorial "how to make new level for CASTLE-2-NAME-HERE" (right now: DRAFT.modeling_tutorial.txt) describes from the basics how to use open-source Blender to create a level and export it to VRML/X3D. Highly advised reading! :)

To load a 3D model, change your code to this:

<?php echo pascal_highlight(
'var
  Window: TCastleWindow;
  Scene: TCastleScene;
begin
  Scene := TCastleScene.Create(Application);
  Scene.Load(\'my_scene.x3d\');
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;

  Window := TCastleWindow.Create(Application);
  Window.SceneManager.Items.Add(Scene);
  Window.SceneManager.MainScene := Scene;

  Window.OpenAndRun;
end.'); ?>

At the beginning we create a new instance of TCastleScene, and load it's contents from a file. Scene.Spatial determines what spatial structures (octrees for now) are created, the value [ssRendering, ssDynamicCollisions] is the most flexible one (it allows to speed up the rendering by frustum culling, detect collisions between player and level, and it adapts to a dynamic level that may have some animated parts). Scene.ProcessEvents activates animating VRML/X3D models (you can remove it if you know your level is, and always will be, static).

The level is added to the scene manager. The level is also set as the "MainScene" of scene manager, this means that some central settings (like initial camera position, initial headlight status and such) can be obtained from this scene.

<?php
  tutorial_footer();
?>
