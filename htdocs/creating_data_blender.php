<?php
// It covers the "next" manual link...
//define('CASTLE_GITHUB_NAME', 'cge-blender');

require_once 'castle_engine_functions.php';
creating_data_header('Exporting from Blender', array(
  'social_share_image' => 'blender_castle_anim_frames_export.png',
));

$toc = new TableOfContents(
  array(
    new TocItem('Export to X3D', 'x3d'),
    new TocItem('Export to Castle Animation Frames (castle-anim-frames)', 'castle_anim_frames'),
    new TocItem('Actions and Frames', 'actions_and_frames', 1),
    new TocItem('Exporting Various Animations Types to castle-anim-frames', 'castle_anim_frames_hints', 1),
    new TocItem('Rendering Skyboxes and Static Cube Environment Maps', 'render_skybox'),
  )
);

echo castle_thumbs(array(
  array('filename' => 'blender_castle_anim_frames_install.png', 'titlealt' => 'Installation of castle-anim-frames Export Script in Blender'),
  array('filename' => 'blender_castle_anim_frames_export.png', 'titlealt' => 'Options of Exporting to castle-anim-frames'),
  array('filename' => 'lizardman_animations.png', 'titlealt' => 'Lizardman Animations Exported from Blender'),
));
?>

<?php echo $toc->html_toc(); ?>
<?php echo $toc->html_section(); ?>

<p><a href="http://www.blender.org/">Blender</a> is a magnificent
free open-source 3D modelling software. Latest Blender versions
include a working X3D exporter, so you can export your content from Blender
out-of-the-box and open it with our engine.
We advice testing the X3D models by opening them with
<?php echo a_href_page('view3dscene', 'view3dscene') ?>.</p>

<div class="download jumbotron">
    <a class="btn btn-primary btn-lg" href="https://github.com/castle-engine/castle-engine/wiki/Blender">Some hints about exporting from Blender to X3D are here</a>
</div>

<p>We have our own X3D exporter version, which is based on the Blender X3D exporter
and adds some fixes and also <b>support for <a href="x3d_implementation_texturing_extensions.php#section_ext_common_surface_shader">CommonSurfaceShader</a> exporting</b>.
In time, we want to submit these modifications to the official Blender code.
For now, install our modifications by copying two files over (overwriting) the original Blender exporter,
which is in <code>&lt;blender-installation-dir&gt;/2.XX/scripts/addons/io_scene_x3d/</code>.

<div class="download jumbotron">
    <a class="btn btn-primary btn-lg" href="https://raw.githubusercontent.com/castle-engine/cge-blender/master/x3d_exporter/export_x3d.py"><span class="glyphicon glyphicon-download" aria-hidden="true"></span><br>Download 1st file of our X3D exporter (export_x3d.py)</a>
    &nbsp;
    <a class="btn btn-primary btn-lg" href="https://raw.githubusercontent.com/castle-engine/cge-blender/master/x3d_exporter/__init__.py"><span class="glyphicon glyphicon-download" aria-hidden="true"></span><br>Download 2nd file of our X3D exporter (__init__.py)</a>
</div>

<?php /*
We publish here some improvements / fixes / documentation for
Blender's X3D exporter.</p>
*/ ?>

<?php /*

<h2>X3D exporter, for Blender &gt;= 2.68a</h2>

<ul>
  <li><p>< ?php echo_svn_blender_file('blender25_x3d/export_x3d.py') ? >:
    Download the actual exporter.

    <p>Copy it over (overwrite) the original Blender exporter,
    which is in <code>&lt;blender-installation-dir&gt;/2.68/scripts/addons/io_scene_x3d/export_x3d.py</code>.
    <!--You will need to restart Blender (if currently running to load the new exporter.-->

    <p><i>Note</i>: as of Blender 2.68, our custom exporter doesn't serve much
    purpose. All the past bugfixes have been applied to the
    Blender standard X3D exporter. The only feature our exporter script
    has over the standard exporter is the magic treatment of images named
    <code>xxx_normalmap</code>, and we don't really advice using this
    (better use < ?php echo a_href_page('material_properties.xml',
    'creating_data_material_properties'); ? >).

  <li><p>< ?php echo_svn_blender_file('blender25_x3d/x3d_blender_exporter_notes.txt') ? >:
    Detailed notes how the exporter (both original distributed in Blender
    and modified by me) works, how you should setup your model.
    At the end, contains notes about our modifications.
</ul>

<p>Blender people: feel welcome to take my fixes / changes,
and apply them to Blender sources. Michalis will try to report them when
he has time.
Also feel free to take my notes, and use/convert them for documentation
anywhere on Blender site, wiki etc. Permission to use my notes
on any license required for official Blender wiki / docs contents is granted.</p>
*/ ?>

<?php echo $toc->html_section(); ?>

<p>The <i>Blender X3D exporter</i> cannot handle animations for now.
To solve this, use the exporter below to export animations to
a <?php echo a_href_page("Castle Animation Frames format",
'castle_animation_frames'); ?>. The <code>.castle-anim-frames</code> files can be read by our engine
and will play animations.

<div class="download jumbotron">
    <a class="btn btn-primary btn-lg" href="https://raw.githubusercontent.com/castle-engine/cge-blender/master/export_castle_anim_frames.py"><span class="glyphicon glyphicon-download" aria-hidden="true"></span><br>Download Castle Animation Frames exporter</a>
</div>

<p>Install it like every other Blender addon:</p>

<ol>
  <li>Use the comfortable <i>File -&gt; User Preferences (opens new window) -&gt; Addons (tab)
    -&gt; Install Addon... (button at the bottom)</i>.
    Or just copy the file directly to the
    <code>scripts/addons/</code> directory.
  <li>Enable it, by clicking the checkbox at <i>"Import-Export: Export Castle Animation Frames"</i>
    in the Addons window.
</ol>

<!-- <p>Tested with <i>Blender &gt;= 2.68</i>. -->

<?php echo $toc->html_section(); ?>

<p><i>Actions</i> are Blender containers for animations. Each Blender object may have many actions, like <i>walk</i>, <i>run</i>, <i>die</i>... A new action is automatically created on an object (if needed) when you insert a keyframe.

<p>In Blender, <i>it matters to which object you attach an action</i>. Action describes the complete animation of a given Blender object. If you try to reuse the same action on two different objects, you will find that they animate (move, rotate...) the same way. If you want every object to animate in a different way, <i>you will usually use an action only on a single object</i>. I explain this, to make it clear that <i>Blender actions do not span multiple objects</i>, which is a little different than what we need (we want to export a series of animations, and each animation should just apply to the whole scene).

<p>When exporting the animation, you can select an object to <i>export all the actions of this object</i>. <b>If your scene includes an armature with some actions, we automatically select it as the object from which to take actions</b> (you can deselect it, if desired). The range of exported frames is determined by the minimum and maximum keyframe set in this action (that's how Blender calculates <code>action.frame_range</code> in Python).<!--  All other objects will animate according to their current actions, but this special object will have different action  -->

<p>When you don't select any such object then we export the whole animation (from <i>Start</i> to <i>End</i> frames that you set on the <i>Timeline</i>). The resulting animation will be called just "<code>animation</code>" in this case (this is useful if you run animations from code, using the <?php api_link('PlayAnimation', 'CastleSceneCore.TCastleSceneCore.html#PlayAnimation'); ?> method). This is perfectly reasonable in many situations:
<ul>
  <li>if you don't have actions in your scene (if your animation is ruled only by physics), <!--, like <i>Rigid Body</i> or <i>Cloth</i> or <i>Particles</i-->
  <li>or if you just want to export the current actions of all the objects,
  <li>or if you configured the animation using Blender's NLA editor.
</ul>

<?php echo $toc->html_section(); ?>

<p>Thanks to the simplicity of the <code>.castle-anim-frames</code> format,
<b>this format exports every kind of Blender animation to our engine</b>:

<ul>
  <li>You can <b>transform</b> whole objects,
  <li>You can animate using an <b>armature</b> (skeleton) attached to a skinned mesh or disjoint objects,
  <li>You can <b>deform the mesh in any way (shape keys, hooks)</b>,
  <li>You can use <b>fluid simulation</b>,
  <li>You can use <b>physics</b> (rigid body, soft body, cloth; make sure to <i>play the complete animation in Blender right before exporting, to make it cached</i>),
  <li>You can animate <b>material properties</b> (e.g. color or transparency),
  <li>You can even animate <b>particles</b> (select the <i>"Make Duplicates Real (Export Particles)"</i> checkbox)!
</ul>

<p>The <code>castle-anim-frames</code> animations render smoothly when the models are <i>"structurally equal"</i>, which means that you should only animate things that can be interpolated. <!-- (are expressed as floating-point values). --> You should not change a topology (e.g. create, destroy faces or whole objects) from one frame to another, at least <i>you should not change it too often</i>. In particular:

<ul>
  <li>You may want to avoid using Blender <i>"fluid simulation"</i>, as it always instantiates a different mesh topology every frame.
  <li>Avoid having <i>"Triangulate"</i> or <i>"Decimate"</i> modifiers on a stack <i>after</i> an animating modifier (like an <i>"Armature"</i>), as they will change the mesh topology differently every frame. Using these modifiers earlier on the stack is better.
  <li>If you export particles, make sure that all the particles exist in every animation frame. It's easiest to achieve this by selecting to <i>Render</i> both the <i>"Unborn"</i> and <i>"Dead"</i> particles.
</ul>

<p>Ignoring these advices will make the animation "jump" at certain frames, since the engine will not be able to interpolate between the frames you provided. Sometimes this is OK (e.g. when you really change one object to something completely different), but sometimes this is a bad artifact. Use the <?php echo a_href_page('view3dscene', 'view3dscene') ?> with <code>--debug-log</code> command-line option, and watch for warnings about the model not being <i>"structurally equal"</i>, if you want to eliminate such issues.

<p>The <code>castle-anim-frames</code> exporter uses the X3D exporter (the original one, provided with Blender, or our custom one if installed) to export the static data. So if something doesn't look like you want after exporting, consult the advices above about using the X3D exporter. You can always export, as a test, a single frame of your animation to X3D, to check does it look OK.

<p>TODO: Right now the interpolation is not done using the proper X3D interpolators at runtime, like <code>PositionInterpolator</code>. Instead we interpolate (create intermediate frames) at the load time, then put the resulting frames inside a <code>Switch</code> node, animated using the <code>IntegerSequencer</code>. The nodes inside all <code>Switch</code> nodes are shared smartly, but still the memory usage is much higher than it could be.

<p>For the sake of the collision detection, each animation is treated as it's bounding box. Calculating detailed collision trees for every animation frame would be too time-consuming. TODO: We can improve it one day to collide as a <i>different</i> bounding box, corresponding to the current animation frame, not to the whole animation.

<?php echo $toc->html_section(); ?>

<p>You can render a set of six images that can be used as a skybox (<code>Background</code> in X3D, see <a href="x3d_implementation_environmentaleffects.php">documentation of the <i>Environmental effects component</i></a>) or a cube map texture (<code>ComposedCubeMapTexture</code> in X3D, see <a href="x3d_implementation_cubemaptexturing.php">documentation of the <i>Cube map environmental texturing component</i></a>). We have a small Python script for Blender that renders the images to the appropriate names following the X3D conventions (front, back, top,....), and a simple X3D test scenes that allow to test that the resulting images indeed work as a skybox or a cubemap texture (e.g. you can open them in view3dscene).

<div class="download jumbotron">
    <a class="btn btn-primary btn-lg" href="https://github.com/castle-engine/cge-blender/tree/master/render_skybox"><span class="glyphicon glyphicon-download" aria-hidden="true"></span><br>Download render_skybox.py script, and test files</a>
</div>

<?php
creating_data_footer();
?>
