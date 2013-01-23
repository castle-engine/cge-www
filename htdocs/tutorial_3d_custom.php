<?php
  require_once 'tutorial_common.php';
  tutorial_header('Define other 3D objects');
?>

6.3. (After creatures and items section, section "More stuff in your 3D world?")

  Our creature and items classes in CastleCreatures and CastleItems units are not magic, they do not get any special treatment from the scene manager. Instead, they just extend the base 3D classes in Castle3D unit. There are ready classes to represent 3D things, that can move, can collide with other 3D stuff and such. If you find that our creatures/items design is not enough for you, or maybe you want to have in your 3D world something that doesn't really fit the creature/item definition, you can always derive new classes from the Castle3D classes. Like T3DAlive, T3DOrient, T3DTransform. Methods Move, MoveAllowed, Height, LineOfSight allow you to add any kind of movement/intelligence to any 3D object.

------------------------------------------------------------------------------
7. More manual way of adding new items

((Maybe remove this chapter / rewrite this?

In new engine version, we have comfortable classes to store item information (TInventoryItem) and visible item on level (TItemOnLevel) and creature (TCreature). However, for advanced usage, it's possible that the traditional concepts of "items that can be picked", "creatures" will not suit you particular game. So the chapter is still somewhat relevant: it's important that our 3D system is designed around flexible T3D class, that you can group (T3DList) and transform (T3DTransform). You can freely design your own descendants of T3DXxx classes, and have all the flexibility you want in creating... well, anything you want in your 3D world.

So, this chapter is probably still relevant, but it should be simplified and rewritten.
))

For the sake of our engine, an item is just a 3D object. So basically, you can just add any T3D descendant (like TCastleScene or TCastlePrecalculatedAnimation) to a scene manager, exactly like we added a level scene in previous section.

However, you usually have a lot of item instances showing the same item. For example, you may place hundreds of potions on a level, and you probably want to add/delete potions on your level quickly. At the same time, you want to load (read from disk, and optimize for OpenGL rendering) only a single potion item. Each potion visible on the level just renders the same 3D model, possibly moved/rotated in a different way.

This means that want to think about two concepts separately: a <b>type</b> of item (like potion, sword, etc.) and an <b>instance</b> of this item in visible 3D world (like "potion lying beside the front door", "other potion hidden behind a secret corridor", "yet another potion not visible yet but in player's backpack", "a sword lying somewhere" and such). Don't confuse the terms <b>type</b> and <b>instance</b> here with ObjectPascal concepts. In our program, we will want to have a class (and it's instance) representing a potion, and a class (and many instances) representing each place where potion exists.

Our engine allows you to do this in a flexible way. We have many 3D objects that keep only references to other 3D objects. In particular, T3DTransfrom keeps a reference to other 3D object(s), and transforms (translates (moves), rotates, scales) them.

So you can create an instance of TCastleScene where you load an item, like potion. Then you create as many as you like instances of T3DTransform, where you add your potion as a children. Additionally, at T3DTransform, you can setup the transformation (like translation) of your item.

<?php echo pascal_highlight(
'var
  PotionType: TCastleScene;
  PotionInstance1, PotionInstance2: T3DTransform;

{ ... somewhere in begin...end block ... }
  PotionType := TCastleScene.Create(Application);
  PotionType.Load(\'my_scene.x3d\');
  PotionType.ProcessEvents := true; // if the item model has interactive/animated parts by VRML/X3D events

  PotionInstance1 := T3DTransform.Create(Application);
  PotionInstance1.Translation := Vector3Single(1, 2, 3);
  PotionInstance1.Add(PotionType);
  SceneManager.Items.Add(PotionInstance1);

  PotionInstance2 := T3DTransform.Create(Application);
  PotionInstance2.Translation := Vector3Single(4, 5, 6);
  PotionInstance2.Add(PotionType);
  SceneManager.Items.Add(PotionInstance2);'); ?>

For more advanced uses, you may want to create your own class for TPotionType (descending from TCastleScene, and adding additional information, for example is this a "life potion", "mana potion", "cure potion" etc. &mdash; we assume these potions have different 3D models, but also share some common parameters and so they want to be treated under a common class TPotionType). Also, you may want to create your own class for TPotionInstance (descending from T3DTransform, and adding additional information like "how much percent of this potion is used up (drink)"). No surprises here, you can do all this by following standard ObjectPascal syntax and conventions, example:

<?php echo pascal_highlight(
'type
  TPotionEffect = (peLife, peMana, peCurePoison);

  { A potion properties and 3D model. }
  TPotionType = class(TCastleScene)
    Effect: TPotionEffect;
    CreaturesMayPickItUp: boolean;
    FlameResistant: boolean;
  end;

  { A potion instance. This always refers to exactly one item inside
    of class TPotionType (you can use "(Items[0] as TPotionType)" to get it,
    you can introduce a method here to get it comfortably).
    When it is visible on a level (that is, added to SceneManager items
    graph), the Translation determines it\'s position on a level. }
  TPotionInstance = class(T3DTransform)
    PercentUsed: Integer;
  end;

var
  LifePotionType,
  ManaPotionType: TPotionType;
  LifePotionInstance1,
  LifePotionInstance2,
  ManaPotionInstance1: TPotionInstance;

{ ... somewhere in begin...end block ... }
  LifePotionType := TPotionType.Create(Application);
  LifePotionType.Load(\'life_potion.x3d\');
  LifePotionType.Effect := peLife;

  ManaPotionType := TPotionType.Create(Application);
  ManaPotionType.Load(\'mana_potion.x3d\');
  LifePotionType.Effect := peMana;

  LifePotionInstance1 := TPotionInstance.Create(Application);
  LifePotionInstance1.Translation := Vector3Single(1, 2, 3);
  LifePotionInstance1.Add(LifePotionType);
  LifePotionInstance1.PercentUsed := 50;
  SceneManager.Items.Add(LifePotionInstance1);

  LifePotionInstance2 := TPotionInstance.Create(Application);
  LifePotionInstance2.Translation := Vector3Single(4, 5, 6);
  LifePotionInstance2.Add(LifePotionType);
  LifePotionInstance1.PercentUsed := 12;
  SceneManager.Items.Add(LifePotionInstance2);

  ManaPotionInstance1 := TPotionInstance.Create(Application);
  ManaPotionInstance1.Translation := Vector3Single(4, 5, 6);
  ManaPotionInstance1.Add(ManaPotionType);
  SceneManager.Items.Add(ManaPotionInstance1);'); ?>

<?php
  tutorial_footer();
?>
