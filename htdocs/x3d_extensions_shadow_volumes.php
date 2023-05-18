<?php
require_once 'castle_engine_functions.php';
require_once 'x3d_extensions_functions.php';

castle_header('Shadow Volumes');

$toc = new TableOfContents(array(
  new TocItem('Introduction', 'introduction'),
  new TocItem('Examples', 'examples'),
  new TocItem('Features', 'features'),
  new TocItem('Shadow casters 3D geometry must be 2-manifold (closed volume)', 'manifold'),
    new TocItem('Optional: Treat whole scene as 2-manifold', 'scene_manifold', 1),
  new TocItem('Adjust what casts shadows', 'shadow_caster'),
  new TocItem('Advanced: Control from X3D', 'x3d'),
    new TocItem('Example models', 'examples_x3d', 1),
    new TocItem('X3D fields <code>shadowVolumes</code> and <code>shadowVolumesMain</code>', 'x3d_fields_light', 1),
    new TocItem('X3D field <code>Appearance.shadowCaster</code>', 'x3d_field_shadow_caster', 1),
  //new TocItem('Advanced: Control stencil buffer', 'stencil'),
));
?>

<?php echo pretty_heading($page_title);  ?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p><i>Shadow volumes</i> are a method of rendering dynamic shadows. <i>"Dynamic"</i> means that everything (light source, shadow casters, shadow receivers) can move and change each frame and the shadows will reflect that.

<p>To activate them in <i>Castle Game Engine</i>,
just set the light's <?php echo cgeRef('TCastlePunctualLight.Shadows', 'Shadows'); ?> property to <code>true</code>.
You can do this in the editor, and observe the effects immediately.

<?php
  echo cgeImg('block', array(
    array('filename' => 'shadows_editor.png', 'titlealt' => 'Shadow volumes in editor'),
    array('filename' => 'shadows_screenshot.png', 'titlealt' => 'Shadow volumes example'),
  ));
?>

<p>Important limitations:

<ul>
  <li>
    <p>The models that cast shadows must be <a href="#section_manifold">2-manifold</a>.
      This means that every edge has exactly 2 (not more, not less)
      neighbor faces, so the whole shape is a closed volume.

  <li>
    <p>Right now <i>only one light source</i> can cast shadows using
      shadow volumes. Do not set
      <?php echo cgeRef('TCastlePunctualLight.Shadows', 'Shadows'); ?> to <code>true</code>
      on multiple lights.
</ul>

<?php echo $toc->html_section(); ?>

<p>Check out these CGE examples:

<ul>
  <li>
    <p><a href="https://github.com/castle-engine/castle-engine/tree/master/examples/viewport_and_scenes/shadows">examples/viewport_and_scenes/shadows</a> - simple example showing shadow volumes from different light source types.
  <li>
    <p><i>"3D FPS game"</i> template for new projects in CGE editor also has a light casting shadows by default.
  <li>
    <p><a href="https://github.com/castle-engine/castle-engine/tree/master/examples/viewport_and_scenes/shadow_volumes_whole_scene_manifold">examples/viewport_and_scenes/shadow_volumes_whole_scene_manifold</a> - demonstrates <?php echo cgeRef('TCastleRenderOptions.WholeSceneManifold'); ?> feature.
</ul>

<?php echo $toc->html_section(); ?>

<p>Featues of <i>shadows by shadow volumes</i> (in particular, how does this technique compare to <a href="x3d_extensions_shadow_maps.php">shadow maps</a>):</p>

<ul>
  <li><p>Shadow volumes produce <b>hard shadows</b>. That's both an advantage (they are as sharp as your geometry, no problems with texture resolution like in shadow maps) and disadvantage (when simulating large area lights, hard shadows may look unrealistic).</p></li>

  <li><p><b>Shadow volumes are easier to activate.</b> To see the shadows, just choose one light in the scene (probably the brightest one) and set <?php echo cgeRef('TCastlePunctualLight.Shadows', 'Shadows'); ?> to <code>true</code>.

    <p><b>That's it.</b> Compared to <a href="x3d_extensions_shadow_maps.php">shadow maps</a>, you don't need to tweak anything to deal with shadow maps resolution, bias etc.</p>
  </li>

  <li><p><b>Shadow volumes require the shadow casters to be 2-manifold.</b> So you need to be more careful when modeling. Shadow volumes usually don't work if you try to activate them on a random 3D scene. <!-- &mdash; you will often need to fix scenes to be 2-manifold. --> More about this below.</p></li>

  <li><p><b>It's difficult to compare the speed of "shadow volumes" vs "shadow maps". Both techniques are expensive in different ways.</b> On one hand, shadow volumes require extra rendering passes (additional rendering of "shadow quads" to the stencil buffer, and then additional render to color buffer, for each shadow-casting light). On the other hand, shadow maps require updating the shadow textures (one for each light source).</p></li>

  <li><p>Our current shadow volumes implementation allows for only <b>one light casting shadow volumes</b>. (This may be improved some day. Give us a shout at the forum if needed and <a href="donate.php">support the engine development</a> to make this happen sooner.)<!-- But shadow volumes become impractical anyway for more than a couple of lights, as each additional light requires an extra rendering pass.)--></p>

  <p>Note that shadow volumes will require more and more rendering passes when multiple light sources may cast shadows.

  <p>In contrast, <a href="x3d_extensions_shadow_maps.php">shadow maps</a> support as many lights as you want already, and each light only requires to keep yet another shadow texture up-to-date.</p>
  </li>

  <li><p>Note that <b>it's perfectly fine to use both shadow volumes and shadow maps in a single scene</b>. <!--So you can use shadow volumes to cast a hard shadow from some particularly bright and distant light (like a sun during the summer day), and add shadow maps for some soft shadows.--></p></li>

  <li><p><b>Applying a texture with transparency (alpha testing) on the object has no effect on the shadows cast.</b>

  <?php
  echo cgeImg('float', array(
    array('filename' => 'leaf.png', 'titlealt' => 'Leaf texture with transparent parts'),
  ));
  ?>

  <p>E.g. if you have a leaf designed as a simple quad, with a leaf texture that has transparent parts &mdash; the shadows cast by shadow volumes will look like cast by quads, they will not reflect the leaf shape. Similarly if you make a fence as a plane with a texture.

  <p>In general, shadow volumes cannot account for the object visual appearance like a texture. They only account for the object topology (polygons, edges, vertices).

  <p>In contrast, <a href="x3d_extensions_shadow_maps.php">shadow maps</a> interact with objects using texture with alpha-testing nicely (fully transparent places will not cast shadows).

  <p>Note that <b>shadow casters may use partial transparency (by alpha testing or alpha blending)</b>. They can use material with non-zero <code>transparency</code>, they can use textures with alpha channel. These shapes render OK, they just cast shadows just as if they were completely opaque.

  <!--p>(For programmers: reasoning may be found in
  <code>TCastleScene.RenderSilhouetteShadowVolume</code> comments,
  see <code>glDepthFunc(GL_NEVER)</code> notes. For transparent triangles,
  light/dark caps must always be drawn, even in Z-pass approach.)-->
</ul>

<?php echo $toc->html_section(); ?>

<p>By default, for shadow volumes, <b>all shapes that cast shadows must be 2-manifold</b>.
This means that every edge has exactly 2 (not more, not less)
neighbor faces, so the whole shape is a closed volume.
Also, faces must be oriented consistently (e.g. CCW outside).
This requirement is often quite naturally satisfied for natural
objects.

<p>Note that satisfying the above requirements (2-manifold, consistent ordering)
means that you can also use <i>backface culling</i> which improves rendering performance.
You typically turn on <i>backface culling</i> in your 3D authoring software
(e.g. check it in <a href="blender">Blender</a> material settings)
which will export it to the glTF or X3D file.
Our engine will automatically follow this information.

<?php /*

<p>In earlier engine/view3dscene versions, it was allowed
for some part of the model to not be perfectly 2-manifold.
But some rendering problems are unavoidable in this case. See
<a href="https://castle-engine.io/vrml_engine_doc/output/xsl/html/chapter.shadows.html">chapter "Shadow Volumes"</a>
(inside < ?php echo a_href_page("engine documentation",'engine_doc'); ? >)
for description.
<!--
Since < ?php echo a_href_page('view3dscene', 'view3dscene'); ? >
 3.12.0, your model must be perfectly 2-manifold to cast any shadows
for shadow volumes.
-->

*/ ?>

<p>You can inspect whether your shapes are detected as a 2-manifold
by <?php echo a_href_page('view3dscene', 'view3dscene'); ?>:
 see menu item <i>"Help -&gt; Manifold Edges Information"</i>.
To check which edges are actually detected as "border edges" use
<i>"View -&gt; Fill mode -&gt; Silhouette and Border Edges"</i>,
manifold silhouette edges are displayed yellow and border edges
(you want to get rid of them!) are blue.</p>

<p>You can also check manifold edges in <a href="blender">Blender</a>:
you can easily detect why the mesh is not
manifold by <i>"Select -&gt; Select All By Trait -&gt; Non Manifold"</i> command
(press F3 to find this command in edit mode).
Also, remember that faces must be ordered consistently CCW
&mdash; in some cases <i>"Recalculate normals outside"</i>
(this actually changes vertex order in Blender)
may be needed to reorder them properly.

<?php
echo cgeImg('block', array(
  array('filename' => 'blender_non_manifold.png', 'titlealt' => 'Select Non Manifold in Blender'),
));
?>

<p>Note that <b>each shape must be 2-manifold</b> (by default, when
<?php echo cgeRef('TCastleRenderOptions.WholeSceneManifold'); ?>
is <code>false</code>).
<!--(since <i>Castle Game Engine 6.0.0</i>). -->
It's not enough (it's also not necessary) for the <i>whole scene (<?php echo cgeRef('TCastleScene'); ?>)</i>
to be 2-manifold.

<p>The <i>shape</i> is a smallest unbreakable unit that we pass to GPU when rendering. It corresponds exactly to glTF <i>primitive</i> and X3D <?php echo cgeRef('TShapeNode'); ?>. When working with <a href="blender">Blender</a>, each <i>Blender object</i> corresponds to as many shapes as you use materials in that Blender object (so, usually just one).

<p>Putting the requirement to be 2-manifold on <i>shape</i>, not on <i>scene</i>, has advantages and disadvantages:

<ul>
  <li><p><i>Advantage:</i> We prepare and render shadow volumes per-shape,
    so we work efficiently with dynamic models.
    Transforming a shape (move, rotate...),
    or changing the active shapes
    <!-- (e.g. by changing <code>Switch.whichChoice</code>) -->
    has zero cost, no data needs to be recalculated.

  <li><p><i>Advantage:</i> We can avoid rendering per-shape. We can reject shadow volume
    rendering for shape if we know shape's shadow
    will never be visible from the current camera view.

  <li><p><i>Advantage:</i> Not the whole scene needs to be 2-manifold.
    If a shape is 2-manifold, it casts shadow.
    If your scene has both 2-manifold and non-2-manifold shapes,
    it will work OK, just only a subset of shapes (the 2-manifold ones)
    will cast shadows.

  <li><p><i>Disadvantage:</i> The whole shape must be 2-manifold.
    You cannot create 2-manifold scenes by summing multiple non-2-manifold shapes.
</ul>

<?php echo $toc->html_section(); ?>

<p>If you set <?php echo cgeRef('TCastleRenderOptions.WholeSceneManifold', 'MyScene.RenderOptions.WholeSceneManifold'); ?> to <code>true</code> then the <b>whole <?php echo cgeRef('TCastleScene'); ?> must be 2-manifold</b>. In turn, we don't require in this case that each <i>shape</i> is 2-manifold separately.

<p>This is useful when your model is composed from multiple shapes that <i>together</i> are 2-manifold. For example, if your mesh in <a href="blender">Blender</a> is 2-manifold but it uses multiple materials. In such case, exporting it to glTF or X3D splits each mesh into multiple shapes. Each shape is <i>not</i> 2-manifold but whole scene is.

<p>Use <?php echo cgeRef('TCastleRenderOptions.WholeSceneManifold'); ?> to cast shadows from such models. See the <i>Alpaca</i> model from <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/viewport_and_scenes/shadow_volumes_whole_scene_manifold">examples/viewport_and_scenes/shadow_volumes_whole_scene_manifold</a> for an example.

<?php
  echo cgeImg('block', array(
    array('filename' => 'whole_scene_manifold_editor.png', 'titlealt' => 'WholeSceneManifold example in editor'),
  ));
?>

<p>Note that when <?php echo cgeRef('TCastleRenderOptions.WholeSceneManifold', 'MyScene.RenderOptions.WholeSceneManifold'); ?> is <code>true</code>, right now we <i>assume</i> that your whole scene is 2-manifold. We do not check it! You have to make sure it is true (e.g. check in <a href="blender">Blender</a> using <i>"Select -&gt; Select All By Trait -&gt; Non Manifold"</i> mentioned above). If the scene is not actually 2-manifold, rendering artifacts will appear.

<?php echo $toc->html_section(); ?>

<p>By default, every shape that is 2-manifold casts shadows, and every scene with <?php echo cgeRef('TCastleRenderOptions.WholeSceneManifold'); ?> = <code>true</code> casts shadows.

<p>To stop some objects from casting shadows, set <?php echo cgeRef('TCastleTransform.CastShadows'); ?> to <code>false</code>.

<?php echo $toc->html_section(); ?>

<?php
  echo cgeImg('block', array(
    array('filename' => 'fountain_shadows_0.png', 'titlealt' => 'Fountain level model, with shadow volumes.'),
    array('filename' => 'fountain_shadows_1.png', 'titlealt' => 'The same fountain level model, with shadow volumes. After some interactive fun with moving/rotating stuff around :)'),
    // array('filename' => "shadows_dynamic_2.png", 'titlealt' => 'Dynamic shadows demo'),
    // array('filename' => "castle_screen_3.png", 'titlealt' =>  'Werewolves with shadows'),
    // array('filename' => "castle_shadows_fountain.png", 'titlealt' =>  'Castle "fountain" level with shadows'),
  ));
?>

<p><b>NOTE: This section is relevant only for X3D authors that directly edit X3D nodes.
Most <i>Castle Game Engine</i> users can ignore it.
Just control the shadows by toggling <?php echo cgeRef('TCastlePunctualLight.Shadows', 'Shadows'); ?>
 property.</b>

<p>You can use <a href="vrml_x3d.php">X3D</a> nodes to design your lights and shadow volumes on them.
In the simplest case, just activate shadow volumes on a light source
by setting fields <code>shadowVolumes</code> and <code>shadowVolumesMain</code> both to <code>TRUE</code>.

<?php echo $toc->html_section(); ?>

<p>Demo 3D models that use dynamic shadow volumes are
inside our <?php echo a_href_page('demo models',
'demo_models'); ?>, see subdirectory <code>shadow_volumes/</code>.
Open them with <?php echo a_href_page('view3dscene', 'view3dscene'); ?>
 and play around.

<?php echo $toc->html_section(); ?>

<p>To all X3D light nodes, we add two fields:

<?php
  echo node_begin('*Light');
  $node_format_fd_type_pad = 7;
  $node_format_fd_name_pad = 16;
  $node_format_fd_def_pad = 5;
  $node_format_fd_inout_pad = 8;
  echo
  node_dots('all normal *Light fields') .
  node_field('SFBool', '[in,out]', 'shadowVolumes' , 'FALSE') .
  node_field('SFBool', '[in,out]', 'shadowVolumesMain' , 'FALSE',
    'meaningful only when shadowVolumes = TRUE') .
  node_end();
?>

<p>The idea is that shadows are actually projected from only one light source
(with shadow volumes, number of light sources is limited,
since more light sources mean more rendering passes; for now, I decided
to use only one light). The scene lights are divided into three groups:
<ol>
  <li><p>First of all, there's one and exactly one light
    that makes shadows. Which means that shadows are made
    where this light doesn't reach. This should usually be the
    dominant, most intensive light on the scene.

    <p>This is taken as the first light node with
    <code>shadowVolumesMain</code> and <code>shadowVolumes</code> = <code>TRUE</code>.
    Usually you will set <code>shadowVolumesMain</code> to <code>TRUE</code>
    on only one light node.</li>

  <li><p>There are other lights that don't determine <b>where</b>
    shadows are, but they are turned off where shadows are.
    This seems like a nonsense from "realistic" point of view
    &mdash; we turn off the lights,
    even though they may reach given scene point ?
    But, in practice, it's often needed to put many lights
    in this group. Otherwise, the scene could be so light,
    that shadows do not look "dark enough".

    <p>All lights with <code>shadowVolumes</code> = <code>TRUE</code> are
    in this group. (As you see, the main light has to have
    <code>shadowVolumes</code> = <code>TRUE</code> also, so the main light
    is always turned off where the shadow is).</li>

  <li>Other lights that light everything. These just
    work like usual X3D lights, they shine everywhere
    (actually, according to X3D light scope rules).
    Usually only the dark lights should be in this group.

    <p>These are lights with <code>shadowVolumes</code> = <code>FALSE</code>
    (default).</li>
</ol>

<p>Usually you have to experiment a little to make the shadows look
good. This involves determining which light should be the main light
(<code>shadowVolumesMain</code> = <code>shadowVolumes</code> = <code>TRUE</code>),
and which lights should be just turned off inside the shadow
(only <code>shadowVolumes</code> = <code>TRUE</code>).
This system tries to be flexible, to allow you to make
shadows look good &mdash; which usually means "dark, but
not absolutely unrealistically black".

<p>In <?php echo a_href_page('view3dscene', 'view3dscene'); ?>
 you can experiment with this using <i>Edit -> Lights Editor</i>.</p>

<p>If no "main" light is found
(<code>shadowVolumesMain</code> = <code>shadowVolumes</code> = <code>TRUE</code>)
then shadows are turned off on this model.</p>

<p><i>Trick:</i> note that you can set the main light
to have <code>on</code> = <code>FALSE</code>. This is the way to make "fake light"
&mdash; this light will determine the shadows position (it will
be treated as light source when calculating shadow placement),
but will actually not make the scene lighter (be sure to set
for some other lights <code>shadowVolumes</code> = <code>TRUE</code> then).
This is a useful trick when there is no comfortable main light on the scene,
so you want to add it, but you don't want to make the scene
actually brighter.</p>

<?php /*

<p><i>To be deprecated some day: currently
<code>shadowVolumes</code> and <code>shadowVolumesMain</code> are the only
way to get shadow volumes. However, we plan in the future to instead
make our <a href="x3d_extensions_shadow_maps.php#section_light_shadows_on_everything">X3DLightNode.shadows field (currently only for shadow maps)</a>
usable also for shadow volumes. The <code>shadowVolumes*</code> will become
deprecated then.</i></p>

*/ ?>

<?php echo $toc->html_section(); ?>

<p>If you edit X3D nodes, you can control what casts shadows for each particular shape using the <code>Appearance.shadowCaster</code> field. <a href="x3d_extensions_shadow_maps.php#section_shadow_caster">See <code>Appearance.shadowCaster</code> documentation</a>.

<?php
// echo $toc->html_section();
/* Section removed, as we now always use 8-bit stencil buffer.

<p><b>There is nothing you need to do now. We enable 8-bit stencil buffer
by default.</b>

<p>In order for shadow volumes to work, we need a <i>stencil buffer</i> initialized.

<p>To request stencil buffer explicitly, you need to set
<?php echo cgeRef('TCastleWindow.StencilBits'); ?>
 or
<?php echo cgeRef('TCastleControl.StencilBits'); ?>
 to something non-zero.
The number of bits should be large enough to track all the possible objects that may cast a shadow at a given pixel.
In practice, in most reasonable cases, using 8 bits (256 possible objects) is enough. Like this:

<?php echo pascal_highlight(
'Window.StencilBits := 8;'); ?>

<p>You need to set this before the <code>Window.Open</code> call.
In a <a href="manual_cross_platform.php">typical cross-platform CGE application</a>
you would do this in your <code>gameinitialize.pas</code> <code>initialization</code> section.

*/ ?>

<?php
  castle_footer();
?>
