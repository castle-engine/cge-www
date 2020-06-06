<?php
require_once 'castle_engine_functions.php';
manual_header('Which way is up?');

$toc = new TableOfContents(
  array(
    new TocItem('The short explanation: +Y is up', 'short'),
    new TocItem('How to rotate your models from +Z to +Y', 'rotate'),
    new TocItem('How to use Blender with either up = +Y or +Z conventions', 'blender'),
    new TocItem('Detailed discussion', 'details'),
    new TocItem('If +Y means "up", then what should the "forward" be? +Z or -Z?', 'z'),
  )
);
?>

<p>There are various conventions for which vector is "up" in 3D world
and, consequently, which vector corresponds to the "direction" the
creature/player is facing.

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>By default, our engine follows the convention that "up" is in +Y axis.

<p>This is consistent with glTF, X3D and other 3D formats. Exporters from 3D authoring
software are ready for this &mdash; e.g. Blender glTF, X3D exporters by default
rotate models to change +Z axis (traditional "up" vector in Blender) to
the +Y axis. So things <i>just work</i>.

<?php echo $toc->html_section(); ?>

<p>If you have existing models oriented such that +Z is "up",
but you would like to use the engine default convention that +Y is "up",
you can simply rotate them. You can rotate things in <i>Castle Game Engine</i>
using the <?php api_link('TCastleTransform.Rotation', 'CastleTransform.TCastleTransform.html#Rotation'); ?>
 property. Note that <?php api_link('TCastleScene', 'CastleScene.TCastleScene.html'); ?>
 also descends from <?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?>,
 so you can directly rotate a scene.

<p>To rotate each single model from +Z to +Y, just set rotation for every TCastleScene:

<?php echo pascal_highlight('// rotate by -90 degrees around X axis
Scene.Rotation := Vector4(1, 0, 0, -Pi/2);'); ?>

<p>Here's a complete source code:

<?php echo pascal_highlight_file('code-samples/rotate_1.lpr'); ?>

<p>The above option is best if you prefer to think <i>"+Y direction is up"</i> as soon as possible.

<p>Alternatively, you could rotate a whole group of models (with their local transformations) from +Z to +Y, by using TCastleTransform as a group:

<?php echo pascal_highlight_file('code-samples/rotate_2.lpr'); ?>

<p>Note that you can also rotate things in <i>Castle Game Engine</i> using <?php api_link('TTransformNode.Rotation', 'X3DNodes.TTransformNode.html#Rotation'); ?>. But in this case, <?php api_link('TCastleTransform.Rotation', 'CastleTransform.TCastleTransform.html#Rotation'); ?> is simpler to use.

<?php echo $toc->html_section(); ?>

<ul>
  <li><p>If you want to follow <i>"+Y axis is up"</i> convention (easier,
    i.e. you don't really need to do anything):

    <ol>
      <li><p>When exporting from Blender (levels, creatures etc.), let it
        rotate the model, i.e. change +Z to +Y.
        This is the default behavior of the glTF and X3D exporters.

      <li><p>Make sure the <code>Viewpoint</code> node in X3D (default camera)
        indicates +Y as the up vector.
        This is the default X3D value. You can always
        just remove the Blender's camera and setup the default camera position
        by code.

        <p>You can also set the viewpoint using the
        <?php echo a_href_page('view3dscene', 'view3dscene'); ?> feature
        <i>"Console -&gt; Print Current Camera (Viewpoint)"</i>.
        Paste the generated <code>Viewpoint</code> code into your X3D file
        (or into an X3D "wrapper" file, that includes another X3D using the <code>Inline</code> node).

      <li><p>Leave <?php api_link('TCastleTransform.DefaultOrientation', 'CastleTransform.TCastleTransform.html#DefaultOrientation'); ?> at the default value: <code>otUpYDirectionZ</code>
        (best for glTF export).
        Also <code>otUpYDirectionMinusZ</code> (best for X3D export) indicates +Y up.
        The point is: a value like <code>otUpYDirection*</code>.
    </ol>
  </li>

  <li><p>If you want to follow <i>"+Z axis is up"</i> convention:

    <ol>
      <li><p>When exporting from Blender (levels, creatures etc.), always
        select to <i>not</i> rotate the model, i.e. keep Blender's original
        coordinate system.

        <p>If you use Blender glTF exporter, just <i>uncheck</i> the <i>"+Y Up"</i> checkbox
        when exporting.

        <p>If you use Blender X3D exporter, set in the exporter settings:
        <ul>
          <li><i>Forward</i> = Y Forward</li>
          <li><i>Up</i> = Z Up</li>
        </ul>

      <li><p>Make sure you use <?php echo a_href_page('view3dscene', 'view3dscene'); ?> (or other VRML/X3D editor) to
        generate a <code>Viewpoint</code> in your level that makes gravity
        working in the Z axis.

        <p>You can set the viewpoint using the
        <?php echo a_href_page('view3dscene', 'view3dscene'); ?> feature
        <i>"Console -&gt; Print Current Camera (Viewpoint)"</i>, just make
        sure to set earlier the <i>"Navigation -&gt; Set Up (and Gravity Up) +Z"</i>.

      <li><p>Set <?php api_link('TCastleTransform.DefaultOrientation', 'CastleTransform.TCastleTransform.html#DefaultOrientation'); ?> := <code>otUpZDirectionMinusY</code>.
    </ol>
  </li>
</ul>

<?php echo $toc->html_section(); ?>

<p>There are two common conventions for the "up" vector:</p>

<ol>
  <li><p>Most 3D modeling software, like <a href="http://www.blender.org/">Blender</a>,
    prefers the "up" vector to be +Z.

    <p>This is natural if you think about a level map spread on the XY plane.
    The Z coordinate is representing the height above the ground.

  <li><p>glTF, X3D, default OpenGL camera,
    and various game engines (Castle Game Engine, Unity3d, Unreal Engine)
    by default prefer the "up" vector to be +Y.

    <p>This makes sense if you think about the screen as being the XY plane.
    Then if you look straight (in the right-hand coordinate system,
    as used by X3D, OpenGL and Castle Game Engine)
    you look in the -Z direction.

    <p>One argument in favor of this convention is that the default camera makes
    sense for both 2D and 3D games. For 2D games, X goes to the right,
    Y goes up, and Z doesn't matter (or is used to position layers relative
    to each other). For 3D games, again X goes to the right,
    again Y goes up, and Z represents the depth.
    When your game character jumps, the Y coordinate increases &mdash;
    whether it's a 2D or 3D game.
</ol>

<p>As you can imagine, other conventions are possible, as you can pick
any 3D vector as "up", and pick anything orthogonal to it as "direction".</p>

<p>While our engine defaults to the convention "+Y is up",
it is actually configurable:

<ul>
  <li><p>Following
    <a href="http://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/navigation.html#X3DViewpointNode">the X3D standard about X3DViewpointNode</a>,
    the gravity is working along <i>the negative Y-axis of
    the coordinate system of the currently bound X3DViewpointNode node</i>.

  <li><p>In practice, you can just set the viewpoint using the
    <?php echo a_href_page('view3dscene', 'view3dscene'); ?> feature
    <i>"Console -&gt; Print Current Camera (Viewpoint)"</i>.
    Before doing it, you can use the <i>"Navigation -&gt; Set Up (and Gravity Up) ..."</i> menu item.
    The generated <code>Viewpoint</code> node will have correct settings.

  <li><p>Alternatively you can set the camera using <code>TCastleCamera.SetView</code>
    method with an explicit <code>GravityUp</code> parameter.
</ul>

<p>To make things work smoothly, you usually want to
use the same conventions for "up" throughout your asset creation process.
Be wary of
this when creating <?php api_link('TCastleTransform', 'CastleTransform.TCastleTransform.html'); ?> instances in the engine, when exporting
3D models from Blender, when setting viewpoint (with gravity) in
whatever way etc.</p>

<p>Note that Blender (and other 3D modeling software?) by default rotates
models when exporting to X3D, to turn +Z into +Y. On one hand, this
means that some things will "just work" (you use +Z convention inside
Blender, and you get +Y convention inside VRML/X3D). On the other
hand, this may create a little confusion if you manually probe some
positions in Blender model and type them in X3D code (or ObjectPascal
code using our engine): since Blender rotated the models, we
necessarily "see" a different coordinate system than what you see in
Blender.</p>

<p>For this reason, you may decide to disable this "rotation on export"
feature, and instead decide to work with VRML/X3D files where +Z is
"up".</p>

<p>VRML/X3D is flexible in this regard: although the default is to have
up in +Y, the specification says that up is +Y transformed by the
<code>Viewpoint</code> node transformation, and we honour it. In short, this means
that gravity is configurable in VRML/X3D file. You can setup your
camera in <?php echo a_href_page('view3dscene', 'view3dscene'); ?>,
use <i>"Navigation -> Set Up (and Gravity Up) ..."</i>, then use
<i>"Console -> Print Current Camera..."</i> option, and copy-paste the
generated code from the console to your VRML/X3D file. This will set a
<code>Viewpoint</code> with desired up vector, which will be correctly used by our
engine (and other good VRML/X3D browsers actually) for gravity.</p>

<p>The notion of direction/up is used by our engine in two places:</p>

<ol>
  <li><p>Gravity pulls things (player, items, creatures...) down in the -up
    vector. We automatically detect this based on the gravity vector of
    the <code>Viewpoint</code> inside your <?php api_link('Viewport.Items.MainScene', 'CastleScene.TCastleRootTransform.html#MainScene'); ?>
    (you usually
    want to set this to your level 3D model). This means that we follow
    VRML/X3D specification, and gravity vector is derived from the 3D
    model of your level. You can use e.g. <?php echo a_href_page('view3dscene', 'view3dscene'); ?>
    to generate
    <code>Viewpoint</code> node with a desired gravity vector. You can read this vector
    by looking at <code>TCastleViewport.Camera.GravityUp</code>.

  <li><p>If you use <code>TCastleTransform.Direction</code>
    and <code>TCastleTransform.Up</code> properties to rotate your models
    (which is the most natural way to transform creatures, player, and items)
    then they need to know what is your default orientation.
    That is, how does the model look like when rotation is at zero.

    <p>You configure this using the <code>TCastleTransform.Orientation</code> property.
</ol>

<p>Usually, you want to just set
<?php api_link('TCastleTransform.DefaultOrientation', 'CastleTransform.TCastleTransform.html#DefaultOrientation'); ?>, and then
it will be used for all your models.

<?php echo $toc->html_section(); ?>

<p>I'm glad you asked. Of course, things are never easy and there are 2 competing choices
for this (that you may indeed encounter).

<p>Default Blender X3D exporter uses one convention:
<ul>
  <li>Blender up (+Z) turns into +Y,
  <li>Blender forward turns into <b>-Z (negative Z)</b>.
</ul>

<p>Default Blender glTF, and Wavefront OBJ and other exporters use a different convention:
<ul>
  <li>Blender up (+Z) turns into +Y,
  <li>Blender forward turns into <b>+Z (positive Z)</b>.
</ul>

<p>So they both rotate to make up be "+Y", but they rotate in different ways,
producing different results for what happens with model "front" (assuming you used
Blender's default suggested orientation for "front").

<p>To account for this, and keep <?php api_link('TCastleTransform.Direction', 'CastleTransform.TCastleTransform.html#Direction'); ?> working
in an intuitive way, you can adjust <?php api_link('TCastleTransform.Orientation', 'CastleTransform.TCastleTransform.html#Orientation'); ?> or even
<?php api_link('TCastleTransform.DefaultOrientation', 'CastleTransform.TCastleTransform.html#DefaultOrientation'); ?>.

<p>By default they match Blender's glTF exporter, in CGE 6.5 and newer.
In the earlier engine versions they matched the Blender's X3D exporter.
See our
<a href="https://github.com/castle-engine/castle-engine/wiki/Upgrading-to-Castle-Game-Engine-7.0">Upgrading to Castle Game Engine 7.0</a> notes.

<?php
manual_footer();
?>
