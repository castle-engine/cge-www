<?php
require_once 'castle_engine_functions.php';
manual_header('Player');
?>

<p>You can create an instance of <?php api_link('TPlayer', 'CastlePlayer.TPlayer.html'); ?>
 and assign to <?php api_link('TGameSceneManager.Player', 'CastleLevels.TGameSceneManager.html#Player'); ?>.
This provides various features:

<ol>
  <li><p>It keeps track of player life, owned items and more.

  <li><p>It automatically updates some navigation methods based
    on whether player in swimming, dead etc.

  <li><p>By default player is also a central enemy of all hostile creatures
    created using <?php api_link('CastleCreatures', 'CastleCreatures.html'); ?>
    unit. This is configurable (by
    overriding
    <?php api_link('TCreature.Enemy', 'CastleCreatures.TCreature.html#Enemy'); ?>).
</ol>

<p>Note that the player instance is not necessary for basic 3D navigation
(the only thing really necessary is a camera, which is automatically created and
placed in <?php api_link('TCastleViewport.Camera', 'CastleViewport.TCastleViewport.html#Camera'); ?>).

<p>To load a Player do this:

<?php echo pascal_highlight(
'uses ..., CastlePlayer;
var
  SceneManager: TGameSceneManager;
  Player: TPlayer;

...
SceneManager := TGameSceneManager.Create(Application);
SceneManager.FullSize := true;
Window.Controls.InsertFront(SceneManager);

Player := TPlayer.Create(Application);
SceneManager.Items.Add(Player);
SceneManager.Player := Player;'); ?>

<p>It's best to do this (assign <code>SceneManager.Player</code>) before
<code>SceneManager.LoadLevel</code>, this way <code>Player.Navigation</code> is automatically
configured as <code>SceneManager.Navigation</code> and it follows level's properties
like <code>PreferredHeight</code> (from level's <code>NavigationInfo.avatarSize</code>).

<h2>3D models relative to player</h2>

<p>Player is a descendant of
<?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?>,
which means that you can add
additional 3D objects as it's children, like
<code>Player.Add(Some3DObject)</code>. These 3D objects will always be rendered
relative to the player, so they will move along with the player. This
is an easy way to add 3D weapon models and similar things to your
display. Although, for weapons, we actually handle it automatically
for you, using
<?php api_link('TPlayer.EquippedWeapon', 'CastlePlayer.TPlayer.html#EquippedWeapon'); ?>
 model.
But you can also add/remove additional 3D objects this way
(e.g. a 3D torch that is held by the player).

<p>As an additional feature, all 3D objects that are children of player
will be rendered on top of other 3D world. This means that even
if you 3D weapon model is large (like a long sword pointing out from
camera), it will never go "inside the wall". You can turn this feature
on/off by <?php api_link('TPlayer.RenderOnTop', 'CastlePlayer.TPlayer.html#RenderOnTop'); ?> property.

<p>Aside from special <?php api_link('TPlayer.RenderOnTop', 'CastlePlayer.TPlayer.html#RenderOnTop'); ?>
 behavior, the 3D objects
that are children of player are rendered and processed just like all other 3D
stuff. For example, they can be animated (by <code>TCastleScene</code> with
<code>TCastleScene.ProcessEvents := true</code> and
<code>TCastleScene.PlayAnimation</code> method).

<p>Note that the player 3D objects <b>do not</b> make the player
collision sphere (aka camera radius) larger. If you want to make the
collision sphere larger, you can do it by placing a <code>NavigationInfo</code>
node in a level 3D file, and adjusting the 1st item of <code>avatarSize</code>
field &mdash; it determines the camera radius.
See <code>examples/fps_game/data/example_level/example_level_final.x3dv</code>
for an example VRML/X3D configuring the player.

<p style="margin-left: 1em"><b>Alternative method:</b>
There is an alternative way to place things relative to player view:
use X3D ProximitySensor node. See
<code>demo_models/sensors_environmental/follow_camera_by_proximity_sensor.x3dv</code>
in <?php echo a_href_page('our demo VRML/X3D models', 'demo_models'); ?>
 for a simple example how to code it in X3D. This allows you to place
the 3D things that are relative to player inside a larger X3D file,
together e.g. with normal level geometry (which may be an advantage or
disadvantage, depending on what you want). The disadvantage is that we do
not implement layers in X3D now, so such geometry will overlap with 3D
level geometry (unless it will always fit within camera radius).
</p>

<h2>Load player configuration from XML file</h2>

<p><!--Although you can directly adjust player properties using code,-->
Sometimes it's nice to give content creators a way to modify player
behavior without touching the game source code.
To allow this you can load player configuration by
<?php api_link('TPlayer.LoadFromFile', 'CastlePlayer.TPlayer.html#LoadFromFile'); ?>
 method. See <?php echo a_href_page('creating player data', 'creating_data_player'); ?>
 for a sample and documentation how player configuration file looks like.</p>

<?php
manual_footer();
?>
