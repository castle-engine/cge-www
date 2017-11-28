<?php
require_once 'castle_engine_functions.php';
manual_header('Which way is up?');
?>

<p>There are various conventions for which vector is "up" in 3D world
and, consequently, which vector corresponds to the "direction" the
creature/player is facing.

<h2>The short answer</h2>

<p>By default, our engine follows the convention that "up" is in +Y axis.

<p>This is consistent with X3D. And exporters from 3D authoring
software are ready for this &mdash; e.g. Blender X3D exporter by default
rotates models to change +Z axis (traditional "up" vector in Blender) to
the +Y axis. So things <i>just work</i>.

<h2>How to use <a href="http://www.blender.org/">Blender</a> with up = Y or Z conventions</h2>

<ul>
  <li><p>If you want to follow <i>"+Y axis is up"</i> convention (easier,
    i.e. you don't really need to do anything):

    <ol>
      <li><p>When exporting from Blender (levels, creatures etc.), let it
        rotate the model, i.e. change +Z to +Y.
        This is the default behavior of the X3D exporter.
        You can verify that it happens by checking in the exporter settings that:
        <ul>
          <li><i>Forward</i> = Z Forward</li>
          <li><i>Up</i> = Y Up</li>
        </ul>

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

      <li><p>Leave <?php api_link('TCastleTransform.DefaultOrientation', 'CastleTransform.TCastleTransform.html#DefaultOrientation'); ?> at the default value:
        <code>otUpYDirectionMinusZ</code>.
    </ol>
  </li>

  <li><p>If you want to follow <i>"+Z axis is up"</i> convention:

    <ol>
      <li><p>When exporting from Blender (levels, creatures etc.), always
        select to <i>not</i> rotate the model, i.e. keep Blender's original
        coordinate system. To do this, set in the exporter settings:
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
        sure to set earlier the <i>"Navigation -&gt; Walk and Fly Settings -&gt;
        Change Gravity Up Vector"</i> to <code>0 0 1</code>.

      <li><p>Set <?php api_link('TCastleTransform.DefaultOrientation', 'CastleTransform.TCastleTransform.html#DefaultOrientation'); ?> := <code>otUpZDirectionMinusY</code>.
    </ol>
  </li>
</ul>

<h2>Detailed discussion about the issue</h2>

<p>There are two common conventions:</p>

<ol>
  <li>Most 3D modeling software, like <a href="http://www.blender.org/">Blender</a>, prefers the up direction
    to be +Z, and direction to be +X. This is usually sensible when you
    think about a flat level, with XY representing the map view, and
    additional Z adding the height.

  <li>On the other hand, VRML/X3D and default OpenGL camera prefers the
    up to be +Y, and direction to be -Z. This makes sense if you think
    about the screen as being the XY plane, so then if you look straight
    (in the right-hand coordinate system) you look in -Z direction.
</ol>

<p>You can also easily imagine other conventions, as you can really pick
any 3D vector as "up", and pick anything orthogonal to it as
"direction".</p>

<p>Our engine supports various such conventions, we do not force you to
follow any particular one. To make things work smoothly, you want to
keep the same conventions throughout your process &mdash; be wary of
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
use <i>"Navigation -> Walk and Fly settings ->
Change Up Vector"</i>, input any vector you want (like "0 0 1"), then use
<i>"Console -> Print Current Camera..."</i> option, and copy-paste the
generated code from the console to your VRML/X3D file. This will set a
<code>Viewpoint</code> with desired up vector, which will be correctly used by our
engine (and other good VRML/X3D browsers actually) for gravity.</p>

<p>The notion of direction/up is used by our engine in two places:</p>

<ol>
  <li><p>Gravity pulls things (player, items, creatures...) down in the -up
    vector. We automatically detect this based on the gravity vector of
    the <code>Viewpoint</code> inside your <?php api_link('TCastleSceneManager.MainScene', 'CastleSceneManager.TCastleSceneManager.html#MainScene'); ?>
    (you usually
    want to set this to your level 3D model). This means that we follow
    VRML/X3D specification, and gravity vector is derived from the 3D
    model of your level. You can use e.g. <?php echo a_href_page('view3dscene', 'view3dscene'); ?>
    to generate
    <code>Viewpoint</code> node with a desired gravity vector. You can read this vector
    by looking at <code>TCastleSceneManager.GravityUp</code>,
    <code>TCastleSceneManager.Camera.GravityUp</code>, and
    <code>World.GravityUp</code> (from any
    <code>TCastleTransform</code> code), these are all equal.

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

<?php
manual_footer();
?>
