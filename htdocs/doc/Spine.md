Castle Game Engine <https://castle-engine.io/> has an advanced support for Spine <http://esotericsoftware.com/> 2D animations.

* [Intro](#intro)
* [How to use Spine animations in your own games](#how-to-use-spine-animations-in-your-own-games)
   * [Overview of Spine and 2D usage in the engine](#overview-of-spine-and-2d-usage-in-the-engine)
* [Creating Spine models](#creating-spine-models)
* [Spine features support](#spine-features-support)
   * [Skins](#skins)
   * [Atlases](#atlases)
   * [Other features](#other-features)
   * [Missing (contributions are welcome!)](#missing-contributions-are-welcome)

Intro
====

Spine is a great software to design 2D animations for games, using modern techniques (bones, skins, meshes etc.). Animations done this way are lightweight: their loading and rendering is very efficient, even on mobile devices like Android or iOS. But they are also very flexible:

* they look smooth regardless of the playback speed,
* you can reuse the same animations by changing skins,
* you can change attachments during animation,
* and more (see Spine website for exhaustive list :).

You can use [view3dscene](https://castle-engine.io/view3dscene.php) to open Spine animations.

You can also add Spine animations using the [editor](https://castle-engine.io/manual_editor.php), just add a `TCastleScene` and set the scene `URL` to point to the JSON file.

1. Some sample Spine animations are in [demo models](https://github.com/castle-engine/demo-models) in subdirectory [spine](https://github.com/castle-engine/demo-models/tree/master/spine). They are also used by some examples, like [examples/animations/play_animation](https://github.com/castle-engine/castle-engine/tree/master/examples/animations/play_animation) and by our "New Project" -> "2D game" template.
2. Just open a project exported from Spine (as a JSON file) using [view3dscene](https://castle-engine.io/view3dscene.php) *File->Open* .
3. To run a named animation, use *Animations* panel in view3dscene.

[![Dragon Spine - Castle Game Engine demo](http://img.youtube.com/vi/AuI4zgmT-YQ/0.jpg)](http://www.youtube.com/watch?v=AuI4zgmT-YQ)

How to use Spine animations in your own games
====

Overview of Spine and 2D usage in the engine
----

See our [engine documentation](https://castle-engine.io/documentation.php) for an overview how to use the engine.

When you run our [editor](https://castle-engine.io/manual_editor.php), create a _"New Project"_ and use template _"2D Game"_. This shows a viewport with mutliple Spine scenes. A Spine scene is just a `TCastleScene` instance and you add it to `TCastleViewport` to make it visible, animated etc.

To play a Spine animation, use `TCastleScene.PlayAnimation` method. Use `TCastleScene.Animations` to list named animations. These are called "named animations" in our engine, and you can actually use them with any 3D model, not only Spine. See [the manual about playing animations](https://castle-engine.io/manual_scene.php).

Creating Spine models
====

Basically, just create Spine models/animations as usual, and export them to JSON. Often it is comfortable to test your animations first in [view3dscene](https://castle-engine.io/view3dscene.php) before trying them out in your game.

You can also [export from Dragon Bones](https://castle-engine.io/creating_data_dragon_bones.php).

It is advised (although not required) to create an atlas when you export to JSON, as rendering with atlas will be more efficient. Atlas options:

- Make sure to select "Bleed" at atlas creation options (it is necessary to avoid visible seams during rendering).

- It is simpler to use "Atlas Per Skeleton", although we also support "Single Atlas For All Skeletons". The latter is more efficient when multiple skeletons share the images, but requires loading by a special URL like <tt>my_animation.json#atlas:my_atlas_name</tt> (see below).

Spine features support
====

Skins
----

* You can choose a skin at loading, by loading a filename like <tt>my_animation.json#skin:my_skin_name</tt> instead of <tt>my_animation.json</tt>. 
* By default we use the first (non-default) skin.
* If you don't know the list of possible skins: you can see them in Spine, or in our [log](https://castle-engine.io/manual_log.php).
* For backward compatibility, instead of <tt>my_animation.json#skin:my_skin_name</tt> we also support <tt>my_animation.json#my_skin_name</tt>. But the latter form is deprecated.

Atlases
----

We fully support all Spine texture atlas options. 

* This includes handling multiple atlas pages, stripping whitespace, rotating... so you can pack your atlases really tight.
* We also support _"single atlas for multiple skeletons"_ feature of Spine. You can use it when exporting (it can be beneficial, may allow to reuse texture space better). To do this, specify atlas name explicitly, like this: <tt>my_animation.json#atlas:my_atlas_name</tt> . This will read atlas from file <tt>my_atlas_name.atlas</tt> in the same directory as <tt>my_animation.json</tt> file.
    To specify both skin, and atlas name, separate them by a comma, like this: <tt>my_animation.json#skin:my_skin_name,atlas:my_atlas_name</tt> .
* You can also work without the atlas (we expect images then to be inside <tt>images/<attachment-name>.png</tt>).

Other features
----

* Support for all Spine versions, JSONs exported by Spine 3.x and 4.x are tested.
* Of course, complete rendering of skeleton with attachments, and complete playback of normal bone animations (hierarchy of translations, rotations, scaling). All animations are imported from Spine, and can be selected by `TCastleSceneCore.PlayAnimation(...)` method.
* Animation of slot timeline attachment, this allows to change the attachment during animation, e.g. to make something appear/disappear or for frame-by-frame animations.
* Animation of slot color and transparency.
* Animating slot draw order (draw order timeline).
* Bezier curves for animating parameters.﻿
* Support for "mesh" attachment.
* Partial support for "skinnedmesh" attachment. The skinnedmesh in setup pose is affected by bones correctly, and during animation it moves as a whole (is affected by parent bone transformation). Missing is the ability to recalculate vertexes affected by bones during animation (that is, to deform skinnedmesh during animation).
* Spine "paths" are fully supported. Internally they are converted to `NurbsCurve` X3D nodes, fully supported by the engine -- you can render them, or query them for any purpose. They are invisible by default (`Shape.render` = false), since that is the intention of Spine paths, but you can make them visible easily if needed. From view3dscene, use _"Edit->Reveal All Hidden Shapes"_ to show them.
* [Cross-fading between animations (Spine and otherwise) is supported since Castle Game Engine 6.5](https://castle-engine.io/wp/2018/03/21/animation-blending/)

- Our implementation of Spine converts the Spine project under the hood into an X3D node graph. This means that Spine animation is a full 3D model, and uses stuff like X3D interpolators for animation. This also means that you can use all existing X3D rendering/processing tools to extend the Spine animation (for example, you can use sensors, texture settings, screen effects, you can cast shadows!, you can use shaders etc.).

- Note that you could also create an X3D file with your 2D world, where you insert multiple Spine animations with X3D <tt>Inline</tt> node. This allows to combine multiple 2D and 3D models already at data level.

Missing (contributions are welcome!)
---

Some more exotic Spine animation features are not implemented (yet!). If you would like to see support for them [please submit an issue](https://github.com/castle-engine/castle-engine/issues) or ask on [Castle Game Engine forum](https://sourceforge.net/p/castle-engine/discussion/). Or submit a pull request :). Many of the features below are a matter of adding some reading and conversion to X3DLoadInternalSpine unit.

* Attachment type "regionsequence" (should be easy, but needs test models; in principle, this should be exactly how you can use things like "xxx@counter(4).png" in X3D MovieTexture with our engine, see <https://castle-engine.io/x3d_extensions.php#section_ext_movie_from_image_sequence> ).
* Attachment type "boundingbox" (easy to do, convert to X3D TransformSensor; will need special code to actually use it for something).
* Events and Event Timeline (easy to do; note that events don't do anything by themselves, they just allow animator to "name" some events on the timeline; we would convert them to some sensor that can be watched (using X3D ROUTE) by user code).
* Animating vertexes of meshes using FFD. Should be easy, this is just CoordinateInterpolator in X3D.
* Animating vertexes of skinned meshes to deform them by the bones. Should be easy, as it is already started --- skinned meshes are correctly calculated in setup pose. It remains to calculate them for all frames of related bones, and for frames where FFD changes, and use CoordinateInterpolator.
* IK (Inverse Kinematics). Difficulty unknown, this is a new Spine feature.

[![ Spine 2D animations rendered using Castle Game Engine (view3dscene)](http://img.youtube.com/vi/liNwT96HUqI/0.jpg)](http://www.youtube.com/watch?v=liNwT96HUqI)
[![New Spine features in Castle Game Engine, including slot timeline attachments](http://img.youtube.com/vi/7JZhrJ4N2Pg/0.jpg)](http://www.youtube.com/watch?v=7JZhrJ4N2Pg)
