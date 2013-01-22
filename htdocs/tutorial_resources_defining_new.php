You can also derive simple descendants of CastleCreatures classes, to customize their behavior. For example, "The Castle" customizes walk-attack creature to give both ranged and melee attacks to the Spider Queen, to make werewolves howl from time to time, and such. See castle1 GameCreatures.pas unit sources. We plan to add a simple FPS game to the engine examples, to illustrate this nicer.

  You can start your customizations from full-features classes, like TWalkAttackCreatureResource and TWalkAttackCreature. Or you can take the basic TCreatureResource and TCreature, and extend them to your liking.

  This is a good moment to browse the API reference of mentioned CastleCreatures classes, if you haven't already. See
  * [TWalkAttackCreatureResource], [TWalkAttackCreature]
  * [TMissileCreatureResource], [TMissileCreature]
  * [TStillCreatureResource], [TStillCreature]
  * [TCreatureResource], [TCreature]
  * And see the class hierarchy descending from TCreatureResource and TCreature in [class hierarchy diagram]

  Everything is designed to give you a lot of properties to set (most of them are also settable by resource.xml files mentioned above) and a lot of methods to override. See not only the TCreature* classes, but also see above T3D classes in Castle3D unit, for various things that you can override and use.

    You can code new creatures/items behaviors by deriving new classes
    from our existing classes in CastleCreatures / CastleItems / Castle3D units.
    This is the most flexible way to customize everything about a creature/item.

    You usually override two classes to define a new creature/item:

    1.The resource class (descendant of T3DResource, like TCreatureResource
      or TItemResource). The resource class defines the behavior of the creature,
      what properties does it have. For example, you can image a
      TWerewolfResource that is derived from TWalkAttackCreatureResource
      and adds the ability to make a howling sound from time to time.
      TWerewolfResource would introduce a property like HowlingSoundName
      to be able to define sound for it.
      Or imagine a TPotionResource that is derived from TItemResource
      and add a properties saying which player attribute (health, mana, stamina)
      is regenerated and how much.

      The resource class can be registered, like

        RegisterResourceClass(TWerewolfResource, 'Werewolf');
        RegisterResourceClass(TPotionResource, 'Potion');

      which allows this class to be referenced inside resource.xml files
      using the type attribute, like type="Werewolf".
      Many resource.xml files may use the same type="xxx",
      they only must have a different name="xxx".
      Every resource.xml file that uses type="Werewolf" will make
      a new instance of the TWerewolfResource
      class to be created at the Resources.LoadFromFiles call.

      This way you can imagine creating a couple of resource.xml files
      that define a couple of resource instances:

      [[
      <resource
        name="WerewolfRookie"
        type="Werewolf"
        default_max_life="10.0"
        sound_howling="werewolf_rookie_howling">
        <model>...</model>
      </resource>
      ]]

      [[
      <resource
        name="WerewolfBoss"
        type="Werewolf"
        default_max_life="1000000.0"
        sound_howling="werewolf_boss_howling">
        <model>...</model>
      </resource>
      ]]

      [[
      <resource
        name="SmallLifePotion"
        type="Potion"
        regenerate_stat="Life"
        regenerate_amount="10.0">
        <model>...</model>
      </resource>
      ]]

      [[
      <resource
        name="LargeLifePotion"
        type="Potion"
        regenerate_stat="Life"
        regenerate_amount="50.0">
        <model>...</model>
      </resource>
      ]]

      [[
      <resource
        name="ManaPotion"
        type="Potion"
        regenerate_stat="Mana"
        regenerate_amount="10.0">
        <model>...</model>
      </resource>
      ]]

      As you can see in the above examples, you can use the same
      resource class in many ways. Pratically speaking, you only need to create
      a new resource class (like TWerewolfResource) when you really need
      to introduce a new behavior that needs to be implemented using ObjectPascal.
      Otherwise, if what you want can be achieved by tweaking the value
      of a property of an existing class, then you don't need new class,
      you only need to create new resource.xml file that refers to the same
      class but sets different value for given property.

      After calling Resources.LoadFromFiles, the Resources list will be filled
      with instances of appropriate classes. You can find them by name,
      e.g.

      [[
      var
        WerewolfRookie: TWerewolfResource;
      ...
        WerewolfRookie := Resources.FindName('WerewolfRookie') as TWerewolfResource
      ]]

      Defining new property in a resource class usually means
      defining a normal ObjectPascal property and overriding T3DResource.LoadFromFile
      to load the value of this property. See existing units
      like CastleCreatures and CastleItems for a lot of examples.

      An instance of a resource class will be shared by all
      occurences of this creature/item in the 3D world.

    2.The second class will be used to represent a single occurence
      of this creature/item in the 3D world. This has a reference
      to the appropriate resource (for shared information)
      and can have it's own properties, specific to this current instance.
      For example, creatures have their current Life.

      You can imagine that a potion instance could have a property saying
      e.g. how much of it was drunk (if you want to allow player to drink
      only parts of the potions).

      The resource class indicates the related non-resource class by the
      TCreatureResource.CreatureClass or
      TItemResource.ItemClass method (although it's not necessary,
      but usually you want to define non-resource class together with
      resource class to implement the behavior you need).
      These are used by
      TCreatureResource.CreateCreature and TItemResource.CreateItem methods,
      which you can use to create creature/item occurence by code:

      [[
      type
        TWerewolfResource = class(TWalkAttackCreatureResource)
        public
          function CreatureClass: TCreatureClass; override;
          ...
        end;

        TWerewolf = class(TWalkAttackCreature)
          ...
        end;

      function TWerewolfResource.CreatureClass: TCreatureClass;
      begin
        Result := TWerewolf;
      end;

      ...
      { and if you want to create werewolves programmatically
        (not just by placing "placeholders" on level 3D model) then do this: }
      for I := 1 to 100 do
        WerewolfRookie.CreateCreature(SceneManager.Items,
          Vector3Single(1, 2, 3) { position }
          Vector3Single(1, 0, 0) { direction });
    ]]

    There are many possible classes to override. Overriding
    the more specialized (finished) classes, like TWalkAttackCreature*,
    is nice to merely tweak some detail.
    Overriding the more basic classes, like TCreature*, is good if you're
    prepared to implement something (like creature AI) completely from scratch
    on your own.

    Overriding the really basic classes from Castle3D unit (and optionally
    also CastleResources, if you want your 3D object to be associated with
    a resource) is for advanced usage, when you want to define a 3D
    object within your game that doesn't really fit our creatures/items
    definitions. Castle3D contains a lot of classes to make it easy
    to create your own, dynamic 3D objects.

