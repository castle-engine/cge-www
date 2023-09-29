<?php
require_once 'castle_engine_functions.php';
castle_header('Resources (creatures and items)');

$toc = new TableOfContents(
  array(
    new TocItem('Resource file (resource.xml)', 'resource_xml'),
    new TocItem('Resource type', 'resource_type'),
    new TocItem('Place resource model above the ground', 'ground'),
    new TocItem('Animations of resources', 'animations'),
      new TocItem('Deprecated: Separate animations in separate files', 'deprecated_animations', 1),
  )
);
?>

<?php echo $toc->html_toc(); ?>
<?php echo $toc->html_section(); ?>

<p><code>resource.xml</code> files define the properties of resources: creatures and items.
Below is a sample <code>resource.xml</code> file,
with links to documentation for every attribute.

<ul>
  <li>(Almost) every attribute is optional, so in practice
    there's no need to specify them all in your <code>resource.xml</code> files.
  <li>Note that sample below shows properties for resource of type <code>WalkAttack</code>
    (indicating <?php echo removedCgeRef('TWalkAttackCreatureResource'); ?>
    class), there are other resource types (for creatures and items)
    with a little different properties.
  <li>See <?php echo a_href_page('manual about resources', 'manual_resources'); ?>
    for information how to initialize resources (creatures and items) from such files.
</ul>

<?php echo xml_highlight(
'<?xml version="1.0"?>

<resource
  [[CastleResources.T3DResource.html#Name|name]]="RequiredCreatureName"
  type="WalkAttack"
  [[CastleResources.T3DResource.html#Orientation|orientation]]="default"
  [[CastleCreatures.TCreatureResource.html#KnockBackSpeed|knockback_speed]]="1.0"
  [[CastleCreatures.TCreatureResource.html#CollidesWhenDead|collides_when_dead]]="False"
  [[CastleCreatures.TCreatureResource.html#KnockBackDistance|knockback_distance]]="4.0"
  [[CastleCreatures.TCreatureResource.html#Flying|flying]]="False"
  [[CastleCreatures.TCreatureResource.html#SoundDieTiedToCreature|sound_die_tied_to_creature]]="True"
  [[CastleCreatures.TCreatureResource.html#DefaultMaxLife|default_max_life]]="100.0"
  [[CastleCreatures.TCreatureResource.html#Radius|radius]]="0.0"
  [[CastleCreatures.TCreatureResource.html#MiddleHeight|middle_height]]="0.5"
  [[CastleCreatures.TCreatureResource.html#SoundSuddenPain|sound_sudden_pain]]=""
  [[CastleCreatures.TCreatureResource.html#SoundDie|sound_die]]=""
  [[CastleCreatures.TCreatureResource.html#ScaleMin|scale_min]]=""
  [[CastleCreatures.TCreatureResource.html#ScaleMax|scale_max]]=""
  [[CastleCreatures.TWalkAttackCreatureResource.html#MoveSpeed|move_speed]]="1.0"
  [[CastleCreatures.TWalkAttackCreatureResource.html#MinLifeLossToHurt|min_life_loss_to_hurt]]="0.0"
  [[CastleCreatures.TWalkAttackCreatureResource.html#ChanceToHurt|chance_to_hurt]]="1.0"
  [[CastleCreatures.TWalkAttackCreatureResource.html#MaxHeightAcceptableToFall|max_height_acceptable_to_fall]]="1.5"
  [[CastleCreatures.TWalkAttackCreatureResource.html#RandomWalkDistance|random_walk_distance]]="10.0"
  [[CastleCreatures.TWalkAttackCreatureResource.html#RemoveDead|remove_dead]]="False"
  [[CastleCreatures.TWalkAttackCreatureResource.html#PreferredDistance|preferred_distance]]="2.0"
  [[CastleCreatures.TWalkAttackCreatureResource.html#SmellDistance|smell_distance]]="0.0"
  [[CastleResources.T3DResource.html#AlwaysPrepared|always_prepared]]="False"
  [[CastleResources.T3DResource.html#FallSpeed|fall_speed]]="10.0"
  [[CastleResources.T3DResource.html#GrowSpeed|grow_speed]]="5.0"
  [[CastleResources.T3DResource.html#ReceiveShadowVolumes|receive_shadow_volumes]]="True"
  [[CastleResources.T3DResource.html#CastShadowVolumes|cast_shadow_volumes]]="True"
  >

  <!-- See lower on this page for explanation how to set animations. -->
  <model url="my-creature.gltf"
    [[CastleResources.T3DResource.html#Pool|pool]]="auto"
    [[CastleResources.T3DResource.html#DefaultAnimationTransition|default_animation_transition]]="0.0"
  >
    <[[CastleCreatures.TWalkAttackCreatureResource.html#IdleAnimation|idle]]         animation_name="Idle" />
    <[[CastleCreatures.TWalkAttackCreatureResource.html#IdleToWalkAnimation|idle_to_walk]] animation_name="IdleToWalk" />
    <[[CastleCreatures.TWalkAttackCreatureResource.html#WalkAnimation|walk]]         animation_name="Walk" />
    <[[CastleCreatures.TWalkAttackCreatureResource.html#FireMissileAnimation|fire_missile]] animation_name="FireMissile" />
    <[[CastleCreatures.TWalkAttackCreatureResource.html#AttackAnimation|attack]]       animation_name="Attack" />
    <[[CastleCreatures.TWalkAttackCreatureResource.html#DieAnimation|die]]          animation_name="Die" />
    <[[CastleCreatures.TWalkAttackCreatureResource.html#DieBackAnimation|die_back]]     animation_name="DieBack" />
    <[[CastleCreatures.TWalkAttackCreatureResource.html#HurtAnimation|hurt]]         animation_name="Hurt" />
  </model>

  <attack
    [[CastleCreatures.TCreatureResource.html#AttackKnockbackDistance|knockback_distance]]="0.0"
    [[CastleCreatures.TWalkAttackCreatureResource.html#AttackTime|time]]="0.0"
    [[CastleCreatures.TWalkAttackCreatureResource.html#AttackMaxDistance|max_distance]]="2.0"
    [[CastleCreatures.TWalkAttackCreatureResource.html#AttackMaxAngle|max_angle]]="0.523598776"
    [[CastleCreatures.TWalkAttackCreatureResource.html#AttackMinDelay|min_delay]]="2.0"
    [[CastleCreatures.TWalkAttackCreatureResource.html#AttackSoundHit|sound_hit]]=""
    [[CastleCreatures.TWalkAttackCreatureResource.html#AttackSoundStart|sound_start]]="" >
    <damage
      [[CastleCreatures.TCreatureResource.html#AttackDamageConst|const]]="0.0"
      [[CastleCreatures.TCreatureResource.html#AttackDamageRandom|random]]="0.0" />
  </attack>

  <fire_missile
    [[CastleCreatures.TWalkAttackCreatureResource.html#FireMissileTime|time]]="0.0"
    [[CastleCreatures.TWalkAttackCreatureResource.html#FireMissileMaxDistance|max_distance]]="30.0"
    [[CastleCreatures.TWalkAttackCreatureResource.html#FireMissileMaxAngle|max_angle]]="0.523598776"
    [[CastleCreatures.TWalkAttackCreatureResource.html#FireMissileMinDelay|min_delay]]="2.0"
    [[CastleCreatures.TWalkAttackCreatureResource.html#FireMissileSound|sound]]=""
    [[CastleCreatures.TWalkAttackCreatureResource.html#FireMissileName|name]]=""
    [[CastleCreatures.TWalkAttackCreatureResource.html#FireMissileHeight|height]]="0.5" />

  <fall>
    <sound
      [[CastleCreatures.TCreatureResource.html#FallMinHeightToSound|min_height]]="1.0"
      [[CastleCreatures.TCreatureResource.html#FallSound|name]]="creature_fall" />
    <damage
      [[CastleCreatures.TCreatureResource.html#FallMinHeightToDamage|min_height]]="5.0"
      [[CastleCreatures.TCreatureResource.html#FallDamageScaleMin|scale_min]]="0.8"
      [[CastleCreatures.TCreatureResource.html#FallDamageScaleMax|scale_max]]="1.2" />
  </fall>

  <run_away
    [[CastleCreatures.TWalkAttackCreatureResource.html#RunAwayLife|life]]="0.3"
    [[CastleCreatures.TWalkAttackCreatureResource.html#RunAwayDistance|distance]]="10.0" />

  <visibility
    [[CastleCreatures.TWalkAttackCreatureResource.html#VisibilityAngle|angle]]="2.094395102" />
</resource>'); ?>

<?php echo $toc->html_section(); ?>

<p>The <code>type</code> attribute determines the exact class (ObjectPascal
implementation) used to instantiate this resource.
You can use the same type many types of course,
for example you can define many creatures of type <code>WalkAttack</code>
or <code>Missile</code> and many items of type <code>Item</code>.

<p>This type determines the behavior that is coded in ObjectPascal
&mdash; like creature artificial intelligence, whether item can be equipped,
what happens when item is used and so on.

<p>The type also determines available attributes and animations of this resource.
For example, only creature type <code>WalkAttack</code> (or it's descendants)
have the <code>&lt;attack&gt;</code> animation. See the properties of resource
classes to know what is available:

<ul>
  <li><?php echo removedCgeRef('T3DResource'); ?>
    <ul>
      <li><?php echo removedCgeRef('TCreatureResource'); ?>
        <ul>
          <li><?php echo removedCgeRef('TWalkAttackCreatureResource'); ?>
          <li><?php echo removedCgeRef('TMissileCreatureResource'); ?>
          <li><?php echo removedCgeRef('TStillCreatureResource'); ?>
        </ul>
      <li><?php echo removedCgeRef('TItemResource'); ?>
        <ul>
          <li><?php echo removedCgeRef('TItemWeaponResource'); ?>
        </ul>
    </ul>
</ul>

<?php echo $toc->html_section(); ?>

<p>Resources models (creatures, items and such) should be modeled
around 0,0,0 point. In case of resources using gravity (items and non-flying
creatures), they will be placed on the ground relative to the 0 level
of their model. In other words, if you
want your model to float slightly above the ground, you can just move it
higher above the 0 level. If the model is slightly below 0 level, it will sink
into the ground. This is usually the most comfortable approach.

<p>For flying resources (not using gravity),
this doesn't matter, basically you can place 0,0,0 wherever you
like. See <?php echo cgeRef('TCastleTransform.MiddleHeight'); ?>
 for precise details.

<?php echo $toc->html_section(); ?>

<p>Resources, like creatures and items, display an animation
appropriate to their current state. For example, a creature state may be
<i>"idle"</i> or <i>"attacking"</i> or <i>"dying"</i>,
and it will cause appropriate animation.
A developer can also add additional animation types to the creature or item
(by creating a <?php echo removedCgeRef('T3DResourceAnimation'); ?>
 instance and adding it to a
<?php echo removedCgeRef('T3DResource'); ?> descendant).

<p>Inside the <code>&lt;model&gt;</code> element of the creature/item
<code>resource.xml</code> file you specify from where to load particular
animations.
You should use one file for all the animations, like
<a href="blender">glTF file exported from Blender</a>
with Blender actions for each required animation.
See the example inside the
<a href="https://github.com/castle-engine/castle-engine/tree/master/examples/animations/resource_animations/">resource_animations engine example</a>.
The <code>data/</code> subdirectory of it shows examples of how you can define <code>&lt;model&gt;</code>,
discussed below. It is also a great program to test your own
creatures/items animations (before using in the actual game), you
can load their <code>resource.xml</code> using the <i>"Add resource..."</i> button and
directly play loaded animations.

<p>You declare it in resource.xml file like this:

<?php echo xml_highlight(
'<model url="model.x3d" pool="10">
  <stand animation_name="TimeSensorStand"/>
  <walk  animation_name="TimeSensorWalk"/>
</model>'); ?>

<p>The <i>"animation_name"</i> recognizes animations defined in various
formats (and expressed in X3D using <code>TimeSensor</code>),
just like the engine
<?php echo cgeRef('TCastleSceneCore.PlayAnimation'); ?>
 method. You can see these animations in the
<?php echo a_href_page('view3dscene', 'view3dscene'); ?>
 by turning on <i>Animations</i> panel.

<!--p>This is nice if your authoring software can store multiple
animations inside a single file, and each animation is controlled
by a different X3D TimeSensor node. This is the most natural way to
record multiple animations in a single X3D file. We will detect
animation length from the TimeSensor.cycleInterval, and we'll simulate
sending appropriate time and fraction_changed from this TimeSensor to
activate the desired moment of the desired animation.

<p>If your model is a <a href="https://castle-engine.io/spine">Spine</a>
2D animation file, then our loader will
automatically convert them to X3D TimeSensors when reading,
and things will work perfectly our of the box.
So you can just use Spine animation names.
-->

<!--p>TODO: While not required, together with this we also recommend using
 T3DResource.Pool
property. This usually yields much more natural
-->

<p>The looping is done automatically for animations that require it (like
walk). The value of <code>loop</code> attribute in castle-anim-frames file,
or <code>TimeSensor.loop</code> field in X3D, is ignored.

<?php echo $toc->html_section(); ?>

<p>The <b>deprecated</b> way (do not use in new games!) to define animations
is to use a separate model for each animation state, like this:

<?php echo xml_highlight(
'<model>
  <stand url="stand.x3d" animation_name="MainTimeSensor"/>
  <walk  url="walk.x3d"  animation_name="MainTimeSensor"/>
</model>'); ?>

<p>You can omit the <code>animation_name</code>,
it is then assumed to be just <code>'animation'</code>,
which is the default animation name we read from <?php echo a_href_page('castle-anim-frames', 'castle_animation_frames'); ?>
 and MD3 (Quake 3 engine format) files.
If you use X3D files, the animation name should just match the
TimeSensor node name (given like <code>&lt;TimeSensor DEF="MyAnimation"&gt;</code>
in X3D XML files).

<p>Example:

<?php echo xml_highlight(
'<model>
  <stand url="stand.castle-anim-frames"/>
  <walk  url="walk.castle-anim-frames"/>
</model>'); ?>
</ol>

<!--

<p><i>Design notes about X3D TimeSensor usage</i>: All creatures of a given kind
must share the same resources. E.g. imagine you have a creature type
"werewolf" (defined by a resource.xml file with name="Werewolf"
inside, resulting in TCastleResource instance with
TCastleResource.Name="Werewolf"). You can instantiate this creature
many times on the level, and all visible werewolves will actually use
the same resource underneath. That is why we use TimeSensors by
directly sending their time/fraction_changed, instead of just
activating them by TimeSensor.startTime: in the latter case, all
werewolves visible on the level would be forced to be in the same
state (and moment) of the animation.


  <li><p>When you want to use this creature on particular level(s),
    you should add it to <code>prepare_resources</code> in <code>level.xml</code>
    file. Alternatively, if the creature should be always loaded
    when the player is present (for example, special creature <code>Arrow</code>
    should be always loaded, as the player may come to any level with a bow
    and shoot some arrows), then it should have <code>always_prepare</code> in
    it's <code>resource.xml</code>.</p></li>

  <li><p>If you want to use <?php echo a_href_page_hashlink('shadow volumes', 'x3d_extensions',
    'section_ext_shadows'); ?>, and you want the creature to cast shadows
    by shadow volumes, the creature model must be a correct 2-manifold.
    This means that every edge must have exactly two neighboring faces,
    so that the shape is a correct closed volume.
    Also, faces must be oriented consistently (e.g. CCW outside).
    This requirement is usually quite naturally satisfiable for creature
    models, and consistent ordering allows you to use backface culling which
    is a good thing on it's own.</p>

    <p>You can inspect whether your model is detected as a 2-manifold
    by <?php echo a_href_page('view3dscene', 'view3dscene'); ?>:
    see menu item <i>Help -&gt; Manifold Edges Information</i>.
    To check which edges are actually detected as border you can use
    <i>View -&gt; Fill mode -&gt; Silhouette and Border Edges</i>,
    manifold silhouette edges are displayed yellow and border edges
    (you want to get rid of them) are blue.</p>

    <p>In Blender, you can easily detect why the mesh is not
    manifold by <i>Select non-manifold</i> command (in edit mode).
    Also, remember that faces must be ordered consistently CCW
    &mdash; I think that in some cases <i>Recalculate normals outside</i>
    may be needed to reorder them properly.
</ul>

-->

<?php
castle_footer();
?>
