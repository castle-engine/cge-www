<?php
require_once 'castle_engine_functions.php';
tutorial_header('Player');
?>

<p>For a full-featured game it is good to have a central Player
object. This is used for various things:

<ol>
  <li><p>It will make camera automatically tied to the Player,
    making it a first-person perspective game. (More camera approach,
    like third-person view, will be available later.)

  <li><p>By default player is also a central enemy of all hostile creatures
    created using CastleCreatures unit. This is configurable (by
    overriding TCastleCreature.Enemy).
</ol>

<p>Note that the Player instance is not necessary for basic 3D navigation
(the only thing really necessary is camera, automatically created and
placed in TCastleAbstractViewport.Camera). But for games, it's usually
most comfortable to create and use it.

<p>To load a Player do this:

<?php echo pascal_highlight(
'var
  Player: TPlayer;
...
  Player := TPlayer.Create(SceneManager);
  SceneManager.Items.Add(Player);
  SceneManager.Player := Player;'); ?>

<p>It's best to do this (assign SceneManager.Player) before
SceneManager.LoadLevel, this way Player.Camera is automatically
configured as SceneManager.Camera and it follows level's properties
like PreferredHeight (from level's NavigationInfo.avatarSize).

<p>Player is a descendant of T3DList, which means that you can add
additional 3D objects as it's children, like
Player.Add(Some3DObject). These 3D objects will always be rendered
relative the the player, so they will move along with the player. This
is an easy way to add 3D weapon models and similar things to your
display. In fact, we do it automatically for
TCastlePlayer.EquippedWeapon 3D model. But you can also add/remove
additional 3D objects this way.

<p>As an additional feature, all 3D objects that are children of player
will always be rendered on top of other 3D world. This means that even
if you 3D weapon model is large (like a long sword pointing out from
camera), it will never go "inside the wall". You can turn this feature
on/off by TCastlePlayer.RenderOnTop property.

<p>Aside from special TCastlePlayer.RenderOnTop behavior, the 3D objects
that are children of player are rendered and processed as all other 3D
stuff. For example, they can animate (by using
TCastlePrecalculatedAnimation or by using TCastleScene with
<tt>TCastleScene.ProcessEvents := true</tt>).

<p>Note that the player 3D objects <b>do not</b> make the player
collision sphere (aka camera radius) larger. If you want to make the
collision sphere larger, you can do it by placing a NavigationInfo
node in every level 3D file, and adjusting the 1st item of avatarSize
field &mdash; it determines the camera radius.

<p style="margin-left: 1em"><b>Alternative method:</b>
There is an alternative way to place things relative to player view:
use X3D ProximitySensor node. See
demo_models/sensors_environmental/follow_camera_by_proximity_sensor.x3dv
in <?php echo a_href_page('our demo VRML/X3D models', 'demo_models'); ?>
 for a simple example how to code it in X3D. This allows you to place
the 3D things that are relative to player inside a larger X3D file,
together e.g. with normal level geometry (which may be an advantage or
disadvantage, depending what you want). The disadvantage is that we do
not implement layers in X3D now, so such geometry will overlap with 3D
level geometry (unless it will always fit within camera radius).
</p>

<p>You can also load <tt>player.xml</tt> file by <tt>TPlayer.LoadFromFile</tt>
to load various player properties from an XML configuration file.
See <?php echo a_href_page('creating player', 'creating_data_player'); ?>
 for details.</p>

<?php
tutorial_footer();
?>
