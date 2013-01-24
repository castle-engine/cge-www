<?php
require_once 'castle_engine_functions.php';
tutorial_header('Extending existing creatures / items classes');
?>

<p>You can derive descendants of <?php api_link('CastleCreatures', 'CastleCreatures.html'); ?> and
<?php api_link('CastleItems', 'CastleItems.html'); ?> classes,
to customize the behavior of creatures and items. There are a lot of methods
to override and behaviors to customize. For example,
you can make something interesting happen when you use an item
(like heal the player), or when creature creature state changes
(e.g. creatures  explodes into other creatures when dying). See
<tt>examples/fps_game/fps_game.lpr</tt> for an example that customizes what happens when using a
medkit item.

<p>You can start your customizations from full-features classes, like
 <?php api_link('TWalkAttackCreatureResource', 'CastleCreatures.TWalkAttackCreatureResource.html'); ?> and
 <?php api_link('TWalkAttackCreature', 'CastleCreatures.TWalkAttackCreature.html'); ?>.
Or you can take the basic
 <?php api_link('TCreatureResource', 'CastleCreatures.TCreatureResource.html'); ?> and
 <?php api_link('TCreature', 'CastleCreatures.TCreature.html'); ?>,
and extend them to your liking.

<p>This is a good moment to browse the classes inside
<?php api_link('CastleCreatures', 'CastleCreatures.html'); ?> and
<?php api_link('CastleItems', 'CastleItems.html'); ?> unit,
if you haven't already. Some of the important creature/item classes:

<ul>
  <li><?php api_link('TCreatureResource', 'CastleCreatures.TCreatureResource.html'); ?> working with
      <?php api_link('TCreature', 'CastleCreatures.TCreature.html'); ?>
    <ul>
      <li><?php api_link('TWalkAttackCreatureResource', 'CastleCreatures.TWalkAttackCreatureResource.html'); ?> working with
          <?php api_link('TWalkAttackCreature', 'CastleCreatures.TWalkAttackCreature.html'); ?>
      <li><?php api_link('TMissileCreatureResource', 'CastleCreatures.TMissileCreatureResource.html'); ?> working with
          <?php api_link('TMissileCreature', 'CastleCreatures.TMissileCreature.html'); ?>
      <li><?php api_link('TStillCreatureResource', 'CastleCreatures.TStillCreatureResource.html'); ?> working with
          <?php api_link('TStillCreature', 'CastleCreatures.TStillCreature.html'); ?>
    </ul>
  <li><?php api_link('TItemResource', 'CastleItems.TItemResource.html'); ?> working with
      <?php api_link('TInventoryItem', 'CastleItems.TInventoryItem.html'); ?>
    <ul>
      <li><?php api_link('TItemWeaponResource', 'CastleItems.TItemWeaponResource.html'); ?> working with
          <?php api_link('TItemWeapon', 'CastleItems.TItemWeapon.html'); ?>
    </ul>
</ul>

<h2>Why are there two classes (TXxxResource and TXxx) for everything?</h2>

<p>A "resource" is an information shared by all creatures/items of given type.

<ol>
  <li><p>For example you can have two instances of <tt>TCreatureResource</tt>: <tt>Werewolf</tt>
    and <tt>Knight</tt>. (Actually, they would probably be instances of
    <tt>TWalkAttackCreatureResource</tt>,
    as <tt>TCreatureResource</tt> is abstract.) Using them you can create and place
    on your your level millions of actual werewolves and knights
    (instances of <tt>TWalkAttackCreature</tt>).
    Every werewolf on the level will have potentially different life (fully healed
    vs almost dead) and state (attacking, walking, dying and such), but all werewolves will share the same
    resource, so e.g. all werewolves will use the same dying animation
    (<tt>TWalkAttackCreatureResource.DieAnimation</tt>) and dying sound
    (<tt>TCreatureResource.SoundDie</tt>).

  <li><p>A similar example for items: you can have two instances of class <tt>TItemResource</tt>:
    <tt>Sword</tt> and <tt>LifePotion</tt>. (Actually, <tt>TItemWeaponResource</tt>, which is a descendant
    of <tt>TItemResource</tt>, sounds like a better candidate for the <tt>Sword</tt>.)
    Using them, you can create millions of actual swords and life potions,
    and place them of your level (as well as in inventories of creatures/players).
    Every life potion (<tt>TInventoryItem</tt> instance)
    may keep some individual information (for example, how much of the potion
    is already used/drunk), but all life potions will share the same
    <tt>TItemResource</tt> instance, so e.g. they all will be displayed using the same model
    on 3D level (<tt>TItemResource.BaseAnimation</tt>) and the same image in 2D inventory
    (<tt>TItemResource.Image</tt>).
</ol>

<p>Everything is designed to give you a lot of properties to set (most
of them are also settable by <tt>resource.xml</tt> files) and a
lot of methods to override. All creatures and items descend from
common classes in <?php api_link('Castle3D', 'Castle3D.html'); ?> unit,
so see also there for various things that you can override and use.

<p>You can code new creatures/items behaviors by deriving new classes
from our existing classes in
<?php api_link('CastleCreatures', 'CastleCreatures.html'); ?> and
<?php api_link('CastleItems', 'CastleItems.html'); ?> and
<?php api_link('Castle3D', 'Castle3D.html'); ?> units.
This is the most flexible way to customize everything about a creature/item.

<p>You usually override two classes to define a new creature/item:

<ol>
  <li><p>The resource class (descendant of <tt>T3DResource</tt>,
    like <tt>TCreatureResource</tt>
    or <tt>TItemResource</tt>). The resource class defines
    the shared information for the whole creature/item kind.
    For example, you can image a
    <tt>TWerewolfResource</tt> that is derived from <tt>TWalkAttackCreatureResource</tt>
    and adds the ability to make a howling sound from time to time.
    <tt>TWerewolfResource</tt> would introduce a property like <tt>HowlingSoundName</tt>
    to be able to define sound for it.
    Or imagine a <tt>TPotionResource</tt> that is derived from <tt>TItemResource</tt>
    and add a properties saying which player attribute (health, mana, stamina)
    is regenerated and how much.

    <p>The resource class can be registered, like

<?php echo pascal_highlight(
'uses ..., CastleResources;

...
RegisterResourceClass(TWerewolfResource, \'Werewolf\');
RegisterResourceClass(TPotionResource, \'Potion\');'); ?>

    <p>which allows this class to be referenced inside <tt>resource.xml</tt> files
    using the type attribute, like <tt>type="Werewolf"</tt>.
    Many resource.xml files may use the same <tt>type="xxx"</tt>,
    they only must have a different <tt>name="xxx"</tt>.
    Every <tt>resource.xml</tt> file that uses <tt>type="Werewolf"</tt> will make
    a new instance of the <tt>TWerewolfResource</tt>
    class to be created at the <tt>Resources.LoadFromFiles</tt> call.

    <p>This way you can imagine creating a couple of <tt>resource.xml</tt> files
    that define a couple of resource instances:

<?php echo xml_highlight(
'<resource
  name="WerewolfRookie"
  type="Werewolf"
  default_max_life="10.0"
  sound_howling="werewolf_rookie_howling">
  <model>...</model>
</resource>'); ?>

<?php echo xml_highlight(
'<resource
  name="WerewolfBoss"
  type="Werewolf"
  default_max_life="1000000.0"
  sound_howling="werewolf_boss_howling">
  <model>...</model>
</resource>'); ?>

<?php echo xml_highlight(
'<resource
  name="SmallLifePotion"
  type="Potion"
  regenerate_stat="Life"
  regenerate_amount="10.0">
  <model>...</model>
</resource>'); ?>

<?php echo xml_highlight(
'<resource
  name="LargeLifePotion"
  type="Potion"
  regenerate_stat="Life"
  regenerate_amount="50.0">
  <model>...</model>
</resource>'); ?>

<?php echo xml_highlight(
'<resource
  name="ManaPotion"
  type="Potion"
  regenerate_stat="Mana"
  regenerate_amount="10.0">
  <model>...</model>
</resource>'); ?>

    <p>As you can see in the above examples, you can use the same
    resource class in many ways. Practically speaking, you only need to create
    a new resource class (like <tt>TWerewolfResource</tt>) when you really need
    to introduce a new behavior that needs to be implemented using ObjectPascal.
    Otherwise, if what you want can be achieved by tweaking the value
    of a property of an existing class, then you don't need new resource class,
    you only need to create new <tt>resource.xml</tt> file that refers to the same
    class but sets different value for given property.

    <p>After calling <tt>Resources.LoadFromFiles</tt>, the
    <tt>Resources</tt> list will be filled
    with instances of appropriate classes. You can find them by name,
    e.g.

<?php echo pascal_highlight(
'var
  WerewolfRookie: TWerewolfResource;
  ...
  WerewolfRookie := Resources.FindName(\'WerewolfRookie\') as TWerewolfResource;'); ?>

    <p>Defining a new property in a resource class usually means
    defining a normal ObjectPascal property and overriding
    <tt>T3DResource.LoadFromFile</tt>
    to load the value of this property. See existing units
    like <tt>CastleCreatures</tt> and <tt>CastleItems</tt> for a lot of examples.

  <li><p>The second class will be used to represent a single occurrence
    of this creature/item in the 3D world. This has a reference
    to the appropriate resource (for shared information)
    and can have it's own properties, specific to this current instance.
    For example, creatures have their current <tt>Life</tt>.

    <p>You can imagine that a potion instance could have a property saying
    e.g. how much of it was drunk (if you want to allow player to drink
    only parts of the potions).

    <p>The resource class indicates the related non-resource class by the
    <tt>TCreatureResource.CreatureClass</tt> or
    <tt>TItemResource.ItemClass</tt> method (although it's not necessary,
    but usually you want to define non-resource class together with
    resource class to implement the behavior you need).
    These are used by
    <tt>TCreatureResource.CreateCreature</tt> and <tt>TItemResource.CreateItem</tt> methods,
    which you can use to create creature/item occurrence by code:

<?php echo pascal_highlight(
'type
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
    Vector3Single(1, 0, 0) { direction });'); ?>

    <p>There are many possible classes to override. Overriding
    the more specialized (finished) classes, like <tt>TWalkAttackCreature*</tt>,
    is nice to merely tweak some detail.
    Overriding the more basic classes, like <tt>TCreature*</tt>, is good if you're
    prepared to implement something (like creature AI) completely from scratch
    on your own.

    <p>Overriding the really basic classes from <tt>Castle3D</tt> unit (and optionally
    also <tt>CastleResources</tt>, if you want your 3D object to be associated with
    a resource) is for advanced usage, when you want to define a 3D
    object within your game that doesn't really fit our creatures/items
    definitions. <tt>Castle3D</tt> contains a lot of classes to make it easy
    to create your own, dynamic 3D objects.
  </li>
</ol>

<?php
tutorial_footer();
?>
