<?php
require_once 'castle_engine_functions.php';
creating_data_header('Resources (creatures and items)');
?>

------------------------------------------------------------------------------
TODO: fill default values below, link all attributes to appropriate properties.

<?php echo xml_highlight(
'<?xml version="1.0"?>

<resource
  name="TestCreature"
  type="WalkAttack"
  knockback_speed="1.2"
  knockback_distance="3.4"
  flying="True"
  sound_die_tied_to_creature="True"
  default_max_life="5.6"
  radius="7.8"
  middle_height="6.7"
  sound_sudden_pain="test_sound_6"
  sound_die="test_sound_7"
  move_speed="1.2"
  min_life_loss_to_hurt="3.4"
  chance_to_hurt="0.56"
  max_height_acceptable_to_fall="5.6"
  random_walk_distance="7.8"
  remove_dead="True"
  preferred_distance="9.1"
  always_prepared="True"
  fall_speed="1.2"
  grow_speed="3.4"
  receive_shadow_volumes="False"
  cast_shadow_volumes="False">

  <model file_name="main.x3d">
    <idle         file_name="idle.x3d"         time_sensor="TimeSensorIdle" />
    <idle_to_walk file_name="idle_to_walk.x3d" time_sensor="TimeSensorIdleToWalk" />
    <walk         file_name="walk.x3d"         time_sensor="TimeSensorWalk" />
    <fire_missile file_name="fire_missile.x3d" time_sensor="TimeSensorFireMissile" />
    <attack       file_name="attack.x3d"       time_sensor="TimeSensorAttack" />
    <die          file_name="die.x3d"          time_sensor="TimeSensorDie" />
    <die_back     file_name="die_back.x3d"     time_sensor="TimeSensorDieBack" />
    <hurt         file_name="hurt.x3d"         time_sensor="TimeSensorHurt" />
  </model>

  <attack
    knockback_distance="4.5"
    time="7.8"
    max_distance="9.1"
    max_angle="2.3"
    min_delay="4.5"
    sound_hit="test_sound_6"
    sound_start="test_sound_7" >
    <damage
      const="9.1"
      random="2.3" />
  </attack>

  <fire_missile
    time="1.2"
    max_distance="3.4"
    max_angle="5.6"
    min_delay="7.8"
    sound="test_sound_8"
    name="TestMissileCreature"
    height="0.12" />

  <fall>
    <sound
      min_height="7.8"
      name="test_sound_5" />
    <damage
      min_height="1.2"
      scale_min="3.4"
      scale_max="5.6" />
  </fall>

  <run_away
    life="1.2"
    distance="3.4" />

  <visibility
    angle="5.6" />
</resource>'); ?>

------------------------------------------------------------------------------
Specifically about resource.xml:

- Defines a creature or an item resource. The system is extensible,
  so it can actually define other 3D resources that are part of the world
  in your games.

  The root element is <resource>.

- name: the unique object name to indicate initial position of this creature in
  the level 3D file. IOW, this determines Blender object name
  to choose this creature type. It must be unique among all resources
  (creature and items resources). For all (current and future) uses it should
  be a valid VRML/X3D and ObjectPascal identifier, and also we reserve
  underscores and digits for some tricks.
  So stick to only (English) letters.

- type: determines the exact class (ObjectPascal implementation)
  used to instantiate this creature resource.
  It doesn't have to be unique. E.g. creature type "Missile"
  or generic item type "Item" are used by many resources.

  The type determines the behavior that is coded in ObjectPascal
  &mdash; like creature artificial intelligence and whether item can be equipped.

- The type also determines other available attributes of this resource.
  For example, only creature type "WalkAttack" (or it's descendants,
  like "Alien") have the "attack_animation" attribute.

  For the documentation and default values of properties that you can
  set on a creature or item, see T3DResource descendants in the engine:
  TCreatureResource (and descendants) for creatures,
  TItemResource (and descendants) for items.
  They have lot's of properties, and almost all their properties
  can be set by appopriate XML attribute.

- radius: (default 0.0) Radius used for collision detection with this creature.
  If you don't set radius in resource.xml file (or set it to default value 0.0),
  we will calculate a sensible default radius based on the bounding box
  of the creature.

- middle_height: (default 0.5) Position of eyes of the creature,
  used for various collision detection routines.
  See T3DCustomTransform.MiddleHeight documentation.

  Game developers can use the Castle3D.RenderDebug3D variable to easily
  visualize the bounding sphere (and other things) around resources.
  The bounding sphere is centered around the point derived from "middle_height"
  setting and with given (or automatically calculated) "radius".

- flying: False or True to indicate if creature / item is affected by gravity.

  Missile creatures (resources with type="Missile",
  indicating TMissileCreatureResource implementation,
  or other type indicating your custom class descending from TMissileCreatureResource)
  are an exception here: they ignore this setting.
  Missiles have special approach to gravity (see direction_fall_speed)
  and are not affected by normal gravity settings.

- fall_speed: the speed (in units per second) of falling down because of gravity.
  Default is 10 (see CastleResources.DefaultFallSpeed constant).

  Note that the gravity direction is controlled by your level 3D model,
  see "Which way is up" section in the engine tutorial.

  Currently, falling down of creatures and items just uses this constant speed.
  In the future, we plan to add properties to control mass and air friction
  and perform more physically-correct simulation of falling down.

  This has no effect for resources (creatures or items) with flying="True".
  This also has no effect for missile creatures (their flying="Xxx" is ignored,
  as documented above).

- grow_speed: the speed (in units per second) of growing.
  Default is 5 (see CastleResources.DefaultGrowSpeed constant).

  The "growing" is used to allow non-flying creatures to climb stairs.
  The creature can move whenever a sphere (see "middle_height" and "radius"
  settings mentioned above) can move. This means that part of the bounding
  box (part of the T3DCustomTransform.PreferredHeight) may temporarily
  "sink" into the ground. The growing, controlled by this property,
  allows the creature to go up.

- direction_fall_speed: (default 0) The gravity of missiles.
  This works by gradually changing the missile direction to point downward
  (in the same direction where gravity pulls).

  (Only for missiles, that is: resources with type="Missile",
  indicating TMissileCreatureResource implementation,
  or other type indicating your custom class descending from TMissileCreatureResource.)

- <model> element describes 3D models and animations of the creature/items.
  More information about it on DRAFT.modeling_tutorial.txt.

------------------------------------------------------------------------------
Orientation:

<p>Resources models (creatures, items and such) should be modelled
around 0,0,0 point. In case of resources using gravity, they will be
placed on the ground relative to the 0 height. In other words, if you
want your model to float slightly above the ground, just move it
higher above the 0 level. For resources flying (not using gravity),
this doesn't matter, basically you can place 0,0,0 wherever you
like. See <a>MiddleHeight</a> API documentation for precise details.

------------------------------------------------------------------------------
3D resources, like creatures and items, display various 3D
animations. Depending on the creature state, like standing / attacking
/ dying, we may want to display different animation of the given
creature instance. We define these animations using the <model>
element of creature/item resource.xml file. (As a developer, you can
also create T3DResourceAnimation class in T3DResource descendants, to
load more animations in this manner, and use these animations however
you like.)

-> At this point, I highly advice you compile and run the
   resource_animations example program from the engine sources. It's
   inside castle_game_engine/examples/resource_animations/. The data/
   subdirectory shows examples of how you can define <model>,
   discussed below. It is also a great program to test your own
   creatures/items animations (before using in the actual game), you
   can load their resource.xml using the "Add resource..." button and
   directly play loaded animations.

There are three approaches, and which one to choose depends on what 3D
modeler / exporter you use to design your models:

1. The best way (best for memory and loading time, which is really
important in these situations) is to use a single X3D model, with many
X3D TimeSensors representing different animations. You declare it in
resource.xml file like this:

  <model file_name="model.x3d">
    <stand time_sensor="TimeSensorStand"/>
    <walk time_sensor="TimeSensorWalk"/>
  </model>

This is nice if your 3D modeler / exporter can record multiple
animations inside a single X3D file, and each animation is controlled
by a different X3D TimeSensor node. This is the most natural way to
record multiple animations in a single X3D file. We will detect
animation length from the TimeSensor.cycleInterval, and we'll simulate
sending appropriate time and fraction_changed from this TimeSensor to
activate the desired moment of the desired animation.

Unfortunately, I don't know of any open-source 3D modeler / exporter
right now that can nicely produce such multiple animations in a single
X3D file. I plan to extend Blender X3D exporter to allow this in the
future.

2. You can also use a separate X3D model for each animation state, like this:

  <model>
    <stand file_name="stand.x3d" time_sensor="MainTimeSensor"/>
    <walk file_name="walk.x3d" time_sensor="MainTimeSensor"/>
  </model>

3. You can also use a precalculation animation for each animation,
from <a href=>kanim</a> or MD3 (Quake 3 engine format) file. This is
useful if your 3D modeler / exporter cannot produce animated X3D files
at all, but it can export to kanim (see <a href=">our Blender to kanim
exporter</a> or MD3. In the worst case, you can also just export a
couple of still frames and write the xxx.kanim file in a text editor,
because the kanim format is a trivial XML file that just describes a
transition between a couple of still 3D models. Internally, we'll use
TCastlePrecalculatedAnimation for each animation state.

Example:

  <model>
    <stand file_name="stand.kanim"/>
    <walk file_name="walk.kanim"/>
  </model>

This is probably the most comfortable approach to export animations
from Blender to our engine. <b>For now</b> &mdash; in the future we
hope to extend Blender X3D exporter to store whole animation inside a
single X3D file.


To describe above three cases in more precise manner:

- (Case 3. above) When animation state, like <stand> or <walk>,
  doesn't have a time_sensor attribute &mdash; then it must have
  file_name attribute, and we use precalculated animation,
  TCastlePrecalculatedAnimation, to play it. Suitable for kanim and
  X3D animations. Suitable also when the model is just a still 3D
  model, as then TCastlePrecalculatedAnimation simply renders it.

- (Case 2. above) Otherwise, if an animation state like <stand> or
  <walk> has both time_sensor and file_name, then we load it to a
  TCastleScene and use appropriate TimeSensor to play the animation.

- (Case 1. above) Otherwise, if an animation state like <stand> or
  <walk> has only time_sensor, then we use a 3D model defined at
  <model> element to choose and play appropriate animation. This also
  means using TCastleScene and appropriate TimeSensor to play it, but
  now it's a single TCastleScene potentially shared by various
  animations.

In some situations, we have to know the animation duration (for
example, to know when <attack> animation ends and we should get back
to <stand> or <walk> state).

- For TCastlePrecalculatedAnimation, the animation always starts from
  the local time 0, goes to the last time (time of last <frame> in
  kanim file). Then it eventually goes backward, it backwards="true"
  in kanim file. So we know the duration by looking at frames time and
  backwards property: TimeEnd + (if Backwards then TimeEnd-TimeBegin
  else 0).

  So using backwards="true" in kanim works, useful for some animations
  when you do some gesture and then go back to original position by
  reversing this gesture &mdash; e.g. dog-like creature biting.
  
- For TCastleScene and TimeSensor: in this case, X3D
  TimeSensor.cycleInterval gives us animation duration.

The looping is done automatically for animations that require it (like
walk). So using loop attribute in kanim file, or loop field for
TimeSensor is not necessary (it's ignored).

Design notes about X3D TimeSensor usage: All creatures of a given kind
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
------------------------------------------------------------------------------
TODO: old castle-development text, to be simplified

Items

<ul>
  <li><p>About the "placeholder" objects on the level:

    <p>You can place items on the level by placing a "placeholder" object
    on the level with appropriate name.

    <p>When loading, we search for shape nodes that have a parent node
    named like "CasRes&lt;item-resource-name&gt;&lt;quantity&gt;_&lt;ignored&gt;".
    Where "&lt;item-resource-name&gt;" is "LifePotion" or "Sword" or any
    other TItemResource.Name value (see CastleItems unit),
    "&lt;quantity&gt;" is, well, the integer quantity
    ("1" is assumed if "&lt;quantity&gt;" is omitted), and "&lt;ignored&gt;"
    is just anything that will be ignored (you can use this
    to make object names in your model unique,
    which is obviously desirable).

<!--
(new Blender allows longer names.

    <p>Some reasoning about convention above: Blender's names
    have quite limited length, that's why CamelCase is used
    for "&lt;item-resource-name&gt;" and components are generally "glued"
    without any "_" or "-" or " " between.
-->

    <p>Such "placeholder" object is removed from the actual level and
    instead I insert into level an item. Item position is determined
    by placeholder lowest Z and middle X,Y line (see above).

    <p>You can easily insert such "placeholder" with Blender &mdash; just insert
    any shape (I usually insert Cube, and set it's rendering to
    "wireframe" in Blender), and then edit Blender's mesh name
    as appropriate.

  <li><p>Prepare appropriate 2D image of item (to be shown in inventory slots
    and such).
    <!--
    (Once I tried just automatically rendering models inside inventory slots
    but this doesn't look good enough). You can do it however you like.
    -->
    For example, you can do this by opening
    the model in <?php echo a_href_page('view3dscene', 'view3dscene') ?>,
    setting your camera as desired and taking a screenshot
    (see "Display -> Screenshot ..." menu options).
    Or use "Display -> Raytrace".
    It's a very good idea to store the screenshot camera for the future
    (in case we need to redo the image later),
    by using "Console -> Print Current Camera (Viewpoint)..."
    and adding it to your 3D model file.

    <p><?php echo a_href_page('"The Castle"', 'castle') ?> game
    requires all item images to have a size 95 x 95, but that is a specific
    requirement of this game (because of how it displays the inventory).
    Each game has it's own way to display the inventory, so these requirements
    may be different for other games.

    <p>This image filename should be referenced by the <tt>image="xxx"</tt>
    attribute in the root element of item <tt>resource.xml</tt> file.

  <li><p>For an item that can be equipped as a weapon, you also have
    to prepare two scenes showing the weapon in "ready" state (equipped)
    and "attack" (attack animation). Both of these 3D objects may be static
    or animated (through precalculated animation or (TODO) VRML/X3D events).
    You will almost always want to make the "attack" model animated,
    but you can also make the "ready" model animated (e.g. to show some fire
    flickering on the weapon or such; just don't overdo it, as such animations
    can become distracting from the player).

  <li><p>Finally, to really add the item to the game add it's <tt>resource.xml</tt>
    file. See README_about_index_xml_files.txt for more documentation
    about what is possible there. New item can be automatically placed
    on a level, using placeholders named "CasRes" + item name.
    There is no need to modify the game executable.
</ul>

------------------------------------------------------------------------------

<ul>
  <li><p>Many notes about items apply also to creatures:

    <ul>
      <li>Like with items, initial creatures positions can be set
        by placing a "placeholder" on the level.
        Creature position is determined by placeholder lowest Z and
        middle X,Y line.

        <p>If <tt>&lt;creature-life&gt;</tt> part is not present, the default
        MaxLife value (taken from this creature's <tt>resource.xml</tt> file,
        attribute <tt>default_max_life</tt>) will be used.
        You can also use this feature to place already dead corpses on the level
        (e.g. the Doom E1M1 level uses this):
        just specify <tt>&lt;creature-life&gt;</tt> as 0 (zero).

        <p>Initial creature looking direction
        is determined by the transformation of the placeholder object.
        See <tt>placeholder_reference_direction</tt> in README_about_index_xml_files.txt
        for details.

      <li>For more customizations, see developers documentation
        about CastleCreatures unit.

      <li>For simple customizations you can set various creature
        parameters by editing <tt>data/creatures/xxx/resource.xml</tt> file.
    </ul>
  </li>

  <li><p>When you want to use this creature on particular level(s),
    you should add it to <tt>prepare_resources</tt> in <tt>level.xml</tt>
    file. Alternatively, if the creature should be always loaded
    when the player is present (for example, special creature <tt>Arrow</tt>
    should be always loaded, as the player may come to any level with a bow
    and shoot some arrows), then it should have <tt>always_prepare</tt> in
    it's <tt>resource.xml</tt>.</p></li>

  <li><p>For testing various animations of creatures (and adjusting their time
    parameters) you can use <?php echo a_href_page('view3dscene', 'view3dscene') ?>.

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

  <li><p>Notes for animations: beware. Animations are done with a method
    that essentially makes their rendering very fast (rendering each
    animation frame should be as fast as if it was a still scene).
    However it costs much in terms of memory consumption (and in
    "Loading creatures" time). So be sure to test how much memory is consumed
    by the game after your new animation is loaded. (using system monitors
    or such).</p>
  </li>
</ul>

<?php
creating_data_footer();
?>
