<?php
require_once 'castle_engine_functions.php';
tutorial_header('Which way is up?');
?>

<p>There are various conventions for which vector is "up" in 3D world
and, consequently, which vector corresponds to the "direction" the
creature/player is facing.

<p>Quick guide for Blender:

<ul>
  <li><p>If you want to follow "+Y is up" convention (easier,
    i.e. you don't really need to do anything):

    <ol>
      <li>When exporting from Blender (levels, creatures etc.), let it
        rotate the model, i.e. change +Z to +Y. This is actually the
        default for X3D exporter since some time.

      <li>Make sure your Viewpoint indicates +Y as up vector. This is
        actually the default VRML/X3D value.

      <li>Leave T3DOrient.DefaultOrientation at default
        otUpYDirectionMinusZ.
    </ol>
  </li>

  <li><p>If you want to follow "+Z is up" convention:

    <ol>
      <li>When exporting from Blender (levels, creatures etc.), always
        select to *not* rotate the model, i.e. keep Blender's coordinate
        system.

      <li>Make sure you use view3dscene (or other VRML/X3D editor) to
        generate a Viewpoint in your level that makes gravity working in Z
        axis.

      <li>Set T3DOrient.DefaultOrientation := otUpZDirectionX.
    </ol>
  </li>
</ul>

<p>Detailed discussion about what the issue: There are two common
conventions:</p>

<ol>
  <li>Most 3D modeling software, like Blender, prefers the up direction
    to be +Z, and direction to be -Y. This is usually sensible when you
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
this when creating T3DOrient instances in the engine, when exporting
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
Viewpoint node transformation, and we honour it. In short, this means
that gravity is configurable in VRML/X3D file. You can setup your
camera in view3dscene, use "Navigation -> Walk and Fly settings ->
Change Up Vector", input any vector you want (like "0 0 1"), then use
"Console -> Print Current Camera..." option, and copy-paste the
generated code from the console to your VRML/X3D file. This will set a
Viewpoint with desired up vector, which will be correctly used by our
engine (and other good VRML/X3D browsers actually) for gravity.</p>

<p>The notion of direction/up is used by our engine in two places:</p>

<ol>
  <li><p>Gravity pulls things (player, items, creatures...) down in the -up
    vector. We automatically detect this based on the gravity vector of
    the Viewpoint inside your TCastleSceneManager.MainScene (you usually
    want to set this to your level 3D model). This means that we follow
    VRML/X3D specification, and gravity vector is derived from the 3D
    model of your level. You can use e.g. view3dscene to generate
    Viewpoint node with a desired gravity vector. You can read this vector
    by looking at TCastleSceneManager.GravityUp,
    TCastleSceneManager.Camera.GravityUp, and World.GravityUp (from any
    T3D code), these are always equal.

  <li><p>Oriented 3D things, like creatures, player, and items (and anything
    else derived from T3DOrient class) are always oriented such that their
    local direction/up is matching the vectors defined by
    T3DOrient.Orientation property. Although the publicly visible
    properties like TCreature.Up are automatically set to be *usually*
    equal to the World.GravityUp (in case of flying creatures, it may
    actually be different sometimes). But you still have to set the
    T3DOrient.Orientation yourself, to make the local creature model
    correctly match these vectors. You want to set it such that typical up
    of your creatures changes into World.GravityUp (that is, creature's up
    remains untransformed).
</ol>

<p>Usually, you want to just set T3DOrient.DefaultOrientation, and then
it will be used for all your models.

<?php
tutorial_footer();
?>
