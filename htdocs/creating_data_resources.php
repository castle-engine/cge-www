<?php
require_once 'castle_engine_functions.php';
creating_data_header('Resources (creatures and items)');

$toc = new TableOfContents(
  array(
    new TocItem('Resource file (resource.xml)', 'resource_xml'),
    new TocItem('Resource type', 'resource_type'),
    new TocItem('Orientation of resource 3D model above the ground', 'orientation'),
    new TocItem('Animations of resources', 'animations'),
  )
);
?>

<?php echo $toc->html_toc(); ?>
<?php echo $toc->html_section(); ?>

<p><tt>resource.xml</tt> files define the properties of resources: creatures and items.
Below is a sample <tt>resource.xml</tt> file,
with links to documentation for every attribute.

<ul>
  <li>(Almost) every attribute is optional, so in practice
    there's no need to specify them all in your <tt>resource.xml</tt> files.
  <li>Note that sample below shows properties for resource of type <tt>WalkAttack</tt>
    (indicating <?php api_link('TWalkAttackCreatureResource', 'CastleCreatures.TWalkAttackCreatureResource.html'); ?>
    class), there are other resource types (for creatures and items)
    with a little different properties.
  <li>See <?php echo a_href_page('tutorial about resources', 'tutorial_resources'); ?>
    for information how to initialize resources (creatures and items) from such files.
</ul>

<?php echo xml_highlight(
'<?xml version="1.0"?>

<resource
  [[CastleResources.T3DResource.html#Name|name]]="RequiredCreatureName"
  type="WalkAttack"
  [[CastleCreatures.TCreatureResource.html#KnockBackSpeed|knockback_speed]]="1.0"
  [[CastleCreatures.TCreatureResource.html#KnockBackDistance|knockback_distance]]="4.0"
  [[CastleCreatures.TCreatureResource.html#Flying|flying]]="False"
  [[CastleCreatures.TCreatureResource.html#SoundDieTiedToCreature|sound_die_tied_to_creature]]="True"
  [[CastleCreatures.TCreatureResource.html#DefaultMaxLife|default_max_life]]="100.0"
  [[CastleCreatures.TCreatureResource.html#Radius|radius]]="0.0"
  [[CastleCreatures.TCreatureResource.html#MiddleHeight|middle_height]]="0.5"
  [[CastleCreatures.TCreatureResource.html#SoundSuddenPain|sound_sudden_pain]]=""
  [[CastleCreatures.TCreatureResource.html#SoundDie|sound_die]]=""
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
  [[CastleResources.T3DResource.html#CastShadowVolumes|cast_shadow_volumes]]="True">

  <!-- See lower on this page for explanation how to export animations
       and define <model> element. Below we only show all possible attributes,
       in practice you will not want to set them all. -->
  <model url="main.x3d">
    <[[CastleCreatures.TWalkAttackCreatureResource.html#IdleAnimation|idle]]         url="idle.x3d"         time_sensor="TimeSensorIdle" />
    <[[CastleCreatures.TWalkAttackCreatureResource.html#IdleToWalkAnimation|idle_to_walk]] url="idle_to_walk.x3d" time_sensor="TimeSensorIdleToWalk" />
    <[[CastleCreatures.TWalkAttackCreatureResource.html#WalkAnimation|walk]]         url="walk.x3d"         time_sensor="TimeSensorWalk" />
    <[[CastleCreatures.TWalkAttackCreatureResource.html#FireMissileAnimation|fire_missile]] url="fire_missile.x3d" time_sensor="TimeSensorFireMissile" />
    <[[CastleCreatures.TWalkAttackCreatureResource.html#AttackAnimation|attack]]       url="attack.x3d"       time_sensor="TimeSensorAttack" />
    <[[CastleCreatures.TWalkAttackCreatureResource.html#DieAnimation|die]]          url="die.x3d"          time_sensor="TimeSensorDie" />
    <[[CastleCreatures.TWalkAttackCreatureResource.html#DieBackAnimation|die_back]]     url="die_back.x3d"     time_sensor="TimeSensorDieBack" />
    <[[CastleCreatures.TWalkAttackCreatureResource.html#HurtAnimation|hurt]]         url="hurt.x3d"         time_sensor="TimeSensorHurt" />
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

<p>The <tt>type</tt> attribute determines the exact class (ObjectPascal
implementation) used to instantiate this resource.
You can use the same type many types of course,
for example you can define many creatures of type <tt>WalkAttack</tt>
or <tt>Missile</tt> and many items of type <tt>Item</tt>.

<p>This type determines the behavior that is coded in ObjectPascal
&mdash; like creature artificial intelligence, whether item can be equipped,
what happens when item is used and so on.

<p>The type also determines available attributes and animations of this resource.
For example, only creature type <tt>WalkAttack</tt> (or it's descendants)
have the <tt>&lt;attack&gt;</tt> animation. See the properties of resource
classes to know what is available:

<ul>
  <li><?php api_link('T3DResource', 'CastleResources.T3DResource.html'); ?>
    <ul>
      <li><?php api_link('TCreatureResource', 'CastleCreatures.TCreatureResource.html'); ?>
        <ul>
          <li><?php api_link('TWalkAttackCreatureResource', 'CastleCreatures.TWalkAttackCreatureResource.html'); ?>
          <li><?php api_link('TMissileCreatureResource', 'CastleCreatures.TMissileCreatureResource.html'); ?>
          <li><?php api_link('TStillCreatureResource', 'CastleCreatures.TStillCreatureResource.html'); ?>
        </ul>
      <li><?php api_link('TItemResource', 'CastleItems.TItemResource.html'); ?>
        <ul>
          <li><?php api_link('TItemWeaponResource', 'CastleItems.TItemWeaponResource.html'); ?>
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
like. See <?php api_link('T3DCustomTransform.MiddleHeight', 'Castle3D.T3DCustomTransform.html#MiddleHeight'); ?>
 for precise details.

<?php echo $toc->html_section(); ?>

<p>3D resources, like creatures and items, display various 3D
animations. Depending on the creature state, like standing / attacking
/ dying, we may want to display different animation of the given
creature instance. We define these animations using the <tt>&lt;model&gt;</tt>
element of creature/item <tt>resource.xml</tt> file.

<p>As a developer, you can
also create <?php api_link('T3DResourceAnimation', 'CastleResources.T3DResourceAnimation.html'); ?>
 instance adding it to a
<?php api_link('T3DResource', 'CastleResources.T3DResource.html'); ?> descendant,
 this way you can add new animations for your own/extended resources.
At this point, I highly advice you compile and run the
<tt>examples/resource_animations</tt> example program from the engine sources.
The <tt>data/</tt> subdirectory of it shows examples of how you can define <tt>&lt;model&gt;</tt>,
discussed below. It is also a great program to test your own
creatures/items animations (before using in the actual game), you
can load their <tt>resource.xml</tt> using the <i>"Add resource..."</i> button and
directly play loaded animations.

<p>There are three approaches to indicate animations
in <tt>&lt;model&gt;</tt> element in <tt>resource.xml</tt> file.
Which one to choose depends on what 3D
modeler / exporter you use to design your models:

<ol>
  <li><p>The best way (best for memory and loading time, which is really
    important in these situations) is to use a single X3D model, with many
    X3D TimeSensors representing different animations. You declare it in
    resource.xml file like this:

<?php echo xml_highlight(
'<model url="model.x3d">
  <stand time_sensor="TimeSensorStand"/>
  <walk time_sensor="TimeSensorWalk"/>
</model>'); ?>

    <p>This is nice if your 3D modeler / exporter can record multiple
    animations inside a single X3D file, and each animation is controlled
    by a different X3D TimeSensor node. This is the most natural way to
    record multiple animations in a single X3D file. We will detect
    animation length from the TimeSensor.cycleInterval, and we'll simulate
    sending appropriate time and fraction_changed from this TimeSensor to
    activate the desired moment of the desired animation.

    <p>Unfortunately, I don't know of any open-source 3D modeler / exporter
    right now that can nicely produce such multiple animations in a single
    X3D file. I plan to extend Blender X3D exporter to allow this in the
    future.

  <li><p>You can also use a separate X3D model for each animation state, like this:

<?php echo xml_highlight(
'<model>
  <stand url="stand.x3d" time_sensor="MainTimeSensor"/>
  <walk url="walk.x3d" time_sensor="MainTimeSensor"/>
</model>'); ?>

  <li><p>You can also use a precalculated animation for each animation,
    from <?php echo a_href_page('KAnim', 'kanim_format'); ?> or MD3 (Quake 3 engine format) file. This is
    useful if your 3D modeler / exporter cannot produce animated X3D files
    at all, but it can export to kanim (see <?php echo a_href_page('our Blender to KAnim
    exporter', 'blender'); ?>) or MD3. In the worst case, you can also just export a
    couple of still frames and write the xxx.kanim file in a text editor,
    because the kanim format is a trivial XML file that just describes a
    transition between a couple of still 3D models. Internally, we'll use
    TCastlePrecalculatedAnimation for each animation state.

    <p>Example:

<?php echo xml_highlight(
'<model>
  <stand url="stand.kanim"/>
  <walk url="walk.kanim"/>
</model>'); ?>

    <p>This is probably the most comfortable approach to export animations
    from Blender to our engine. <b>For now</b> &mdash; in the future we
    hope to extend Blender X3D exporter to store whole animation inside a
    single X3D file.
</ol>

<p>To describe above three cases in more precise manner for developers:</p>

<ul>
  <li><p>(Case 3. above) When animation state, like <tt>&lt;stand&gt;</tt> or <tt>&lt;walk&gt;</tt>,
    doesn't have a <tt>time_sensor</tt> attribute &mdash; then it must have
    <tt>url</tt> attribute, and we use precalculated animation,
    TCastlePrecalculatedAnimation, to play it. Suitable for kanim and
    X3D animations. Suitable also when the model is just a still 3D
    model, as then TCastlePrecalculatedAnimation simply renders it.

  <li><p>(Case 2. above) Otherwise, if an animation state like <tt>&lt;stand&gt;</tt> or
    <tt>&lt;walk&gt;</tt> has both <tt>time_sensor</tt> and <tt>url</tt>, then we load it to a
    TCastleScene and use appropriate TimeSensor to play the animation.

  <li><p>(Case 1. above) Otherwise, if an animation state like <tt>&lt;stand&gt;</tt> or
    <tt>&lt;walk&gt;</tt> has only <tt>time_sensor</tt>, then we use a 3D model defined at
    <tt>&lt;model&gt;</tt> element to choose and play appropriate animation. This also
    means using TCastleScene and appropriate TimeSensor to play it,
    but this time it's a single TCastleScene potentially shared by various
    animations.
</ul>

<p>In some situations, we have to know the animation duration (for
example, to know when <tt>&lt;attack&gt;</tt> animation ends and we should get back
to <tt>&lt;stand&gt;</tt> or <tt>&lt;walk&gt;</tt> state).

<ul>
  <li><p>For TCastlePrecalculatedAnimation, the animation always starts from
    the local time 0, goes to the last time (time of last <tt>&lt;frame&gt;</tt> in
    kanim file). Then it eventually goes backward, it backwards="true"
    in kanim file. So we know the duration by looking at frames time and
    backwards property: TimeEnd + (if Backwards then TimeEnd-TimeBegin
    else 0).

    <p>So using backwards="true" in KAnim works, useful for some animations
    when you do some gesture and then go back to original position by
    reversing this gesture &mdash; e.g. dog-like creature biting.

  <li><p>For TCastleScene and TimeSensor: in this case, X3D
    TimeSensor.cycleInterval gives us animation duration.
</ul>

<p>The looping is done automatically for animations that require it (like
walk). So using loop attribute in kanim file, or loop field for
TimeSensor is not necessary (it's ignored).

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

<!--

  <li><p>When you want to use this creature on particular level(s),
    you should add it to <tt>prepare_resources</tt> in <tt>level.xml</tt>
    file. Alternatively, if the creature should be always loaded
    when the player is present (for example, special creature <tt>Arrow</tt>
    should be always loaded, as the player may come to any level with a bow
    and shoot some arrows), then it should have <tt>always_prepare</tt> in
    it's <tt>resource.xml</tt>.</p></li>

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
creating_data_footer();
?>
