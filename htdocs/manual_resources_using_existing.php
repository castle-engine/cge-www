<?php
require_once 'castle_engine_functions.php';
castle_header('Using creatures and items');
?>

<p>You can use the existing creature and item classes in
<?php api_link('CastleCreatures', 'CastleCreatures.html'); ?> and
<?php api_link('CastleItems', 'CastleItems.html'); ?> units.
To do this, define your resources (creatures/items) in the <code>resource.xml</code>
files, and call

<?php echo pascal_highlight(
'Resources.LoadFromFiles;'); ?>

<p>(from unit <code>CastleResources</code>) at initialization.
That's i! <b>See <code>castle_game_engine/examples/fps_game/data/</code>
for sample creatures/items</b>. You can copy their <code>resource.xml</code> files
(and accompanying 3D models) to your project, as a starting point.

<p>See <?php echo a_href_page('creating resources guide', 'creating_data_resources'); ?>
 for the detailed documentation what can be used in <code>resource.xml</code> file.
To add initial creatures/items on the level, place on the level 3D model
special "placeholder" items,
see <?php echo a_href_page('creating levels guide', 'creating_data_levels'); ?>.

<h2>Creating creatures / items during the game</h2>

<p>Let's create (spawn) a new creature at an arbitrary position using Object Pascal
code.
This assumes we have a creature named <code>Knight</code>, that is there
must be a <code>resource.xml</code> file with <code>name="Knight"</code>.

<?php echo pascal_highlight(
'uses ..., CastleVectors, CastleCreatures;

procedure SpawnMyCreature;
var
  Position, Direction: TVector3;
  CreatureResource: TCreatureResource;
begin
  Position := Vector3(1, 2, 3);
  Direction := Vector3(1, 0, 0);

  CreatureResource := Resources.FindName(\'Knight\') as TCreatureResource;
  { CreateCreature creates TCreature instance and adds it to the level.
    We could store CreateCreature result, but in this case we just ignore it. }
  CreatureResource.CreateCreature(Level, Position, Direction);
end;

// Remember to actually call SpawnMyCreature
// from wherever you want in your game.
// For example, from the OnPress event.'); ?>

<p>Creating an item is similar, except that we have an intermediate step
where we get
<?php api_link('TInventoryItem', 'CastleItems.TInventoryItem.html'); ?>
 instance. This can be either wrapped inside
<?php api_link('TItemOnWorld', 'CastleItems.TItemOnWorld.html'); ?>
 instance to put it on level, or it can be added to someone's inventory.

<?php echo pascal_highlight(
'uses ..., CastleVectors, CastleItems;

procedure SpawnMyItemOnLevel;
var
  Position: TVector3;
  ItemResource: TItemResource;
  Item: TInventoryItem;
begin
  Position := Vector3(1, 2, 3);

  ItemResource := Resources.FindName(\'MedKit\') as TItemResource;

  { ItemResource.CreateItem(<quantity>) creates new TInventoryItem instance. }
  Item := ItemResource.CreateItem(1);
  { PutOnWorld method creates TItemOnWorld (that "wraps" the TInventoryItem
    instance) and adds it to the level (in effect, adding it to Viewport.Items).
    We could store PutOnWorld result, but in this case we just ignore it. }
  Item.PutOnWorld(Level, Position);

  { You may of course shorten it like this: }
  // ItemResource.CreateItem(1).PutOnWorld(Level, Position);

  { You could instead add the item directly to someone\'s inventory, like this: }
  // Player.PickItem(Item);
  // Player.PickItem(ItemResource.CreateItem(1));
end;

// Remember to actually call SpawnMyItemOnLevel
// from wherever you want in your game.
// For example, from the OnPress event.'); ?>

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

  <dd><p>Weapon that can be equipped. Very configurable in <code>resource.xml</code>
    file, can make a melee attack, or immediately shoot,
    or fire a missile (the last case means that we create new creature
    of <?php api_link('TMissileCreatureResource', 'CastleCreatures.TMissileCreatureResource.html'); ?>
    type), may need ammunition or not.
  </dd>
</dl>

<?php
castle_footer();
?>
