<?php
require_once 'castle_engine_functions.php';
require_once 'x3d_extensions_functions.php';

castle_header('Shadow Volumes', array(
  'path' => array('vrml_x3d', 'x3d_larger_extensions', 'x3d_extensions_shadow_volumes')
));

$toc = new TableOfContents(array(
  new TocItem('Intro', 'intro'),
  new TocItem('Features', 'features'),
  new TocItem('Requirements: geometry must be 2-manifold', 'requirements'),
  new TocItem('Specify what lights cast shadows for shadow volumes (fields <code>shadowVolumes</code> and <code>shadowVolumesMain</code> for light nodes)', 'ext_shadows_light'),
  new TocItem('Optionally specify shadow casters (<code>Appearance.shadowCaster</code>)', 'shadow_caster'),
));
?>

<?php echo pretty_heading($page_title);  ?>

<p>Contents:
<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<?php
  echo castle_thumbs(array(
    array('filename' => 'fountain_shadows_0.png', 'titlealt' => 'Fountain level model, with shadow volumes.'),
    array('filename' => 'fountain_shadows_1.png', 'titlealt' => 'The same fountain level model, with shadow volumes. After some interactive fun with moving/rotating stuff around :)'),
    // array('filename' => "shadows_dynamic_2.png", 'titlealt' => 'Dynamic shadows demo'),
    array('filename' => "castle_screen_3.png", 'titlealt' =>  'Werewolves with shadows'),
    array('filename' => "castle_shadows_fountain.png", 'titlealt' =>  'Castle &quot;fountain&quot; level with shadows'),
  ));
?>

<p><i>Shadow volumes</i> are another method of rendering dynamic
shadows in our engine.

<p><b>Demo 3D models that use dynamic shadow volumes</b> are
inside our <?php echo a_href_page('demo models',
'demo_models'); ?>, see subdirectory <code>shadow_volumes/</code>.
Open them with <?php echo a_href_page('view3dscene', 'view3dscene'); ?>
 and play around!

<?php echo $toc->html_section(); ?>

<p>Featues of <i>shadows by shadow volumes</i> (especially when compared
with <i>shadow maps</i>):</p>

<ul>
  <li><p>Shadow volumes produce <b>hard shadows</b>. That's both an advantage (they are as sharp as your geometry, no problems with texture resolution like in shadow maps) and disadvantage (when simulating large area lights, hard shadows may look unrealistic).</p></li>

  <li><p><b>Shadow volumes are much easier to activate...</b> To see the shadows, just choose one light in the scene (probably the brightest one) and set it's fields <code>shadowVolumes</code> and <code>shadowVolumesMain</code> both to <code>TRUE</code>. <b>That's it.</b> Compared to shadow maps, you don't need to tweak anything to deal with shadow maps resolution, bias etc.</p>
  </li>

  <li><p><b>... but shadow volumes require your models to be 2-manifold.</b> So you need to be more careful when modeling. Shadow volumes usually don't work if you try to activate them on a random 3D scene. <!-- &mdash; you will often need to fix scenes to be 2-manifold. --> More about this below.</p></li>

  <li><p><b>It's difficult to compare the speed of "shadow volumes" vs "shadow maps". Both techniques are expensive in totally different ways.</b> On one hand, shadow volumes are more expensive as they require extra rendering passes (additional rendering of "shadow quads" to the stencil buffer, and then additional render to color buffer, for each shadow-casting light). On the other hand, there is no need for a costly per-pixel shader over every shadow receiver (as in shadow maps).</p></li>

  <li><p>Our current shadow volumes implementation allows for only <b>one light casting shadow volumes</b>. (This may be improved some day. Give us a shout at the forum if needed. Contributions or <a href="donate.php">donations</a> toward this goal are welcome.)<!-- But shadow volumes become impractical anyway for more than a couple of lights, as each additional light requires an extra rendering pass.)--></p></li>

  <li><p>Note that <b>it's perfectly fine to use both shadow volumes and shadow maps in a single scene</b>. <!--So you can use shadow volumes to cast a hard shadow from some particularly bright and distant light (like a sun during the summer day), and add shadow maps for some soft shadows.--></p></li>

  <li><p><b>Shadow volumes do not take transparency by alpha-testing textures into account</b>. E.g. you cannot use them to make shadows from leaves on a trees (where a leaf is typically modeled as a simple quad, covered with a leaf texture).
</ul>

<?php echo $toc->html_section(); ?>

<p>For shadow volumes, <b>all shapes of the model
that are shadow casters must be 2-manifold</b>.
This means that every edge has exactly 2 (not more, not less)
neighbor faces, so the whole shape is a closed volume.
Also, faces must be oriented consistently (e.g. CCW outside).
This requirement is often quite naturally satisfied for natural
objects. Note that the consistent ordering allows you to use backface culling
(<code>solid=TRUE</code> in VRML/X3D), which
is a good thing on it's own.</p>

<?php /*

<p>In earlier engine/view3dscene versions, it was allowed
for some part of the model to not be perfectly 2-manifold.
But some rendering problems are unavoidable in this case. See
<a href="http://castle-engine.sourceforge.net/vrml_engine_doc/output/xsl/html/chapter.shadows.html">chapter "Shadow Volumes"</a>
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
 see menu item <i>Help -&gt; Manifold Edges Information</i>.
To check which edges are actually detected as "border edges" use
<i>View -&gt; Fill mode -&gt; Silhouette and Border Edges</i>,
manifold silhouette edges are displayed yellow and border edges
(you want to get rid of them!) are blue.</p>

<p>You can also check manifold edges in <a href="http://www.blender.org/">Blender</a>:
you can easily detect why the mesh is not
manifold by <i>Select non-manifold</i> command (in edit mode).
Also, remember that faces must be ordered consistently CCW
&mdash; in some cases <i>Recalculate normals outside</i>
(this actually changes vertex order in Blender)
may be needed to reorder them properly.

<p>Remember that <b>each shape must be 2-manifold</b> (since <i>Castle Game Engine 6.0.0</i>).
It's not enough (it's also not necessary) for the whole scene
to be 2-manifold. This has advantages and disadvantages:

<ul>
  <li><p><i>Advantage:</i> We prepare and render shadow volumes per-shape,
    so we work efficiently with dynamic models.
    Transforming a shape (move, rotate...),
    or changing the active shapes (e.g. by changing <code>Switch.whichChoice</code>)
    has zero cost, no data needs to be recalculated.

  <li><p><i>Advantage:</i> We can avoid rendering per-shape. We can reject shadow volume
    rendering for shape if we know shape's shadow
    will never be visible from the current camera view.

  <li><p><i>Advantage:</i> Not the whole scene needs to be 2-manifold.
    If a shape is 2-manifold, it casts shadow.
    Your scene can have both 2-manifold and non-2-manifold shapes,
    it will work Ok, just only a subset of shapes will cast shadows.

  <li><p><i>Disadvantage:</i> The whole shape must be 2-manifold.
    If you depended (in engine &lt; 6.0.0) on the fact
    that you can build 2-manifold model from multiple non-2-manifold shapes &mdash; it
    will not work anymore. <!--This is especially constraining because in X3D,
    shape must have the same material (color, transparency etc.).
    You may need to use textures with alpha channel and more tricks
    now to satisfy the 2-manifold requirement.
    -->

  <!--
  <li>Also there's no more ShareManifoldAndBorderEdges method, which means that rendering
    shadow volumes using TCastlePrecalculatedAnimation performance drops.
    But luckily, TCastlePrecalculatedAnimation is deprecated now.
    But in exchange, you can load castle-anim-frames/MD3 and everything to TCastleScene,
    and rendering shadow volumes using TCastleScene is now superb.
  -->
</ul>

<p>Note that <b>shadow casters may be transparent</b> (have material with
<code>transparency</code> &gt; 0), this is handled perfectly.

<!--
<p>However, note that <i>all opaque shapes must
be 2-manifold</i> and separately <i>all transparent shapes must
be 2-manifold</i>. For example, it's Ok to have some transparent
box cast shadows over the model. But it's not Ok to have a shadow casting
box composed from two separate VRML/X3D shapes: one shape defining
one box face as transparent, the other shape defining
the rest of box faces as opaque.
-->

<!--p>(For programmers: reasoning may be found in
<code>TCastleScene.RenderSilhouetteShadowVolume</code> comments,
see <code>glDepthFunc(GL_NEVER)</code> notes. For transparent triangles,
light/dark caps must always be drawn, even in Z-pass approach.)-->

<?php echo $toc->html_section(); ?>

<p>To all VRML/X3D light nodes, we add two fields:

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
    work like usual VRML lights, they shine everywhere
    (actually, according to VRML light scope rules).
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

<p>Everything by default is a shadow caster (as long as it's 2-manifold). To stop some objects from casting shadows, set their <code>Appearance.shadowCaster</code> field to <code>false</code>. <a href="x3d_extensions_shadow_maps.php#section_shadow_caster">See <code>Appearance.shadowCaster</code> documentation</a>.

<?php
  castle_footer();
?>
