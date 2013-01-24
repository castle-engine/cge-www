<?php
require_once 'castle_engine_functions.php';
tutorial_header('Using existing creatures / items classes');
?>

<p>For starters, you can just use the existing creature and item classes in
<?php api_link('CastleCreatures', 'CastleCreatures.html'); ?> and
<?php api_link('CastleItems', 'CastleItems.html'); ?> units.
There is no ObjectPascal code you need to write &mdash; simply create
<tt>resource.xml</tt> files describing your creatures/items
and referencing their 3D models/animations.
See <?php echo a_href_page('creating resources guide', 'creating_data_resources'); ?>.
To add initial creatures/items on the level, place on the level 3D model
special "placeholder" items,
see <?php echo a_href_page('creating levels guide', 'creating_data_levels'); ?>.

<h2>Creating creatures / items during the game</h2>

<p>If you want to create new creatures or items dynamically during the game,
a little code will be necessary.

<p>First, let's create (spawn) a new creature at an arbitrary position.
This assumes we have a creature named <tt>Knight</tt>, that is there
must be a <tt>resource.xml</tt> file with <tt>name="Knight"</tt>.

<?php echo pascal_highlight(
'uses ..., CastleVectors, CastleCreatures;

...
var
  Position, Direction: TVector3Single;
  CreatureResource: TCreatureResource;
begin
  Position := Vector3Single(1, 2, 3);
  Direction := Vector3Single(1, 0, 0);

  CreatureResource := Resources.FindName(\'Knight\') as TCreatureResource;
  { CreateCreature creates TCreature instance and adds it to SceneManager.Items.
    We could store CreateCreature result, but in this case we just ignore it. }
  CreatureResource.CreateCreature(SceneManager.Items, Position, Direction);
end;'); ?>

<p>Creating an item is similar, except that we have an intermediate step
where we get
<?php api_link('TInventoryItem', 'CastleItems.TInventoryItem.html'); ?>
 instance. This can be either wrapped inside
<?php api_link('TItemOnWorld', 'CastleItems.TItemOnWorld.html'); ?>
 instance to put it on level, or it can be added to someone's inventory.

<?php echo pascal_highlight(
'uses ..., CastleVectors, CastleItems;

var
  Position: TVector3Single;
  ItemResource: TItemResource;
  Item: TInventoryItem;
begin
  Position := Vector3Single(1, 2, 3);

  ItemResource := Resources.FindName(\'MedKit\') as TItemResource;

  { ItemResource.CreateItem(<quantity>) creates new TInventoryItem instance. }
  Item := ItemResource.CreateItem(1);
  { PutOnWorld method creates TItemOnWorld (that "wraps" the TInventoryItem
    instance) and adds it to SceneManager.Items.
    We could store PutOnWorld result, but in this case we just ignore it. }
  Item.PutOnWorld(SceneManager.Items, Position);

  { You may of course shorten it like this: }
  // ItemResource.CreateItem(1).PutOnWorld(SceneManager.Items, Position);

  { You could instead add the item directly to someone\'s inventory, like this: }
  // Player.PickItem(Item);
  // Player.PickItem(ItemResource.CreateItem(1));
end;'); ?>

<h2>Overview of existing resource classes</h2>

<dl>
  <dt><?php api_link('TWalkAttackCreatureResource', 'CastleCreatures.TWalkAttackCreatureResource.html'); ?></dt>

  <dd><p>Creature with walk-attack state intelligence.
    Such creature tracks the enemy
    (remembers last seen enemy 3D position, walks/flies to it, possibly
    through sectors/waypoints — so it can pass through narrow doors in a
    labyrinth or walk over a narrow bridge), attacks the enemy from the
    right distance (this can be either a melee attack, or shooting a
    missile — which adds a missile to the 3D world), eventually runs from
    the enemy (when enemy is too close and/or our creature health is low).

    <p>There are a lot of settings to achieve particular behavior,
    e.g. cowardly/brave, offensive/defensive, melee/ranged, etc.
  </dd>

  <dt><?php api_link('TMissileCreatureResource', 'CastleCreatures.TMissileCreatureResource.html'); ?></dt>

  <dd><p>A "missile" intelligence
    that blindly goes into the given direction (possibly
    with gravity and/or homing (close-on-target) features). On impact,
    missile may explode, hurting player and/or other creatures. Any kind
    of missile, like arrow or lighting bolt, is represented as such
    "missile creature", that flies independently of the shooter.
  </dd>

  <dt><?php api_link('TStillCreatureResource', 'CastleCreatures.TStillCreatureResource.html'); ?></dt>

  <dd><p>Creature just standing still. It can
    still show some looping animation, but there is no fancy logic behind it.
    It can die.
    This is one way to make destructible level parts.
  </dd>

  <dt><?php api_link('TItemResource', 'CastleItems.TItemResource.html'); ?></dt>

  <dd><p>Basic item that can be kept in the inventory. You usually
    want to extend this class, otherwise the item doesn't do anything
    except occupying space in the inventory... Although, if you all you want
    is something like keys/keycards in the game, then this is enough.</p>
  </dd>

  <dt><?php api_link('TItemWeaponResource', 'CastleItems.TItemWeaponResource.html'); ?></dt>

  <dd><p>Weapon that can be equipped. Very configurable in <tt>resource.xml</tt>
    file, can make a melee attack, or immediately shoot,
    or fire a missile (the last case means that we create new creature
    of <?php api_link('TMissileCreatureResource', 'CastleCreatures.TMissileCreatureResource.html'); ?>
    type), may need ammunition or not.
  </dd>
</dl>

<?php
tutorial_footer();
?>
