<?php
// It covers the "next" manual link...
//define('CASTLE_GITHUB_NAME', 'cge-blender');

require_once 'castle_engine_functions.php';
creating_data_header('Exporting from Blender', array(
  'social_share_image' => 'blender_castle_anim_frames_export.png',
));

$toc = new TableOfContents(
  array(
    new TocItem('Export to glTF 2', 'gltf'),
    new TocItem('Export to X3D', 'x3d'),
    new TocItem('Export to Castle Animation Frames (castle-anim-frames)', 'castle_anim_frames'),
    new TocItem('Actions and Frames', 'actions_and_frames', 1),
    new TocItem('Exporting Various Animations Types to castle-anim-frames', 'castle_anim_frames_hints', 1),
    new TocItem('Rendering Skyboxes and Static Cube Environment Maps', 'render_skybox'),
    new TocItem('Obsolete: Export to X3D from Blender 2.7', 'x3d_old'),
  )
);

echo castle_thumbs(array(
  array('filename' => 'blender_castle_anim_frames_install.png', 'titlealt' => 'Installation of castle-anim-frames Export Script in Blender'),
  array('filename' => 'blender_castle_anim_frames_export.png', 'titlealt' => 'Options of Exporting to castle-anim-frames'),
  array('filename' => 'lizardman_animations.png', 'titlealt' => 'Lizardman Animations Exported from Blender'),
));
?>

<?php echo $toc->html_toc(); ?>

<p><a href="http://www.blender.org/">Blender</a> is a magnificent
free open-source 3D modelling software.

<?php echo $toc->html_section(); ?>

<p><i>Castle Game Engine</i> supports the <i>glTF 2.0</i> format,
and new <a href="https://www.blender.org/">Blender</a> has an exporter to glTF 2.0.
Just export using the <i>File -&gt; Export -&gt; glTF 2.0</i> menu item
and open the resulting file (in <code>.glb</code> or <code>.gltf</code> formats)
using any engine tool (like <a href="view3dscene.php">view3dscene</a>).

<p><a href="creating_data_model_formats.php#section_gltf">Read here for the details about our glTF support</a>.
In short: we support most features for static models (including textures and physical materials),
and we support simple animations by transformations.
More support for glTF animations (by skinning and morphing) will come soon
(for now you have to use other formats for it, like X3D or castle-anim-frames).

<p><a href="https://docs.blender.org/manual/en/dev/addons/import_export/scene_gltf2.html">Blender documentation of the glTF exporter</a> is useful. The exporter is <a href="https://github.com/KhronosGroup/glTF-Blender-IO">developed by Khronos on GitHub</a>.

<p>Apply <a href="https://github.com/KhronosGroup/glTF-Blender-IO/pull/991">this fix</a> if you're going to use glTF combined with castle-anim-frames (see below).

<?php echo $toc->html_section(); ?>

<p><a href="https://www.blender.org/">Blender</a> includes exporter for X3D out-of-the-box.

<p>It doesn't support animations.
Since Blender 2.8 it is unfortunately even more limited,
it doesn't support any textures either.

<p>There are also bugs.
Be sure to use the latest Blender versions (2.82 or later), and consider appplying these
fixes yourself:
<a href="https://developer.blender.org/D7183">fix applying modifiers</a>,
<a href="https://developer.blender.org/D7186">fix backface culling export</a>.

<?php echo $toc->html_section(); ?>

<p>To export <i>any</i> kind of animation from Blender, use the exporter to our <?php echo a_href_page("Castle Animation Frames format", 'castle_animation_frames'); ?>. The <code>.castle-anim-frames</code> files can be read by our engine and will play animations.

<p>Internally they are a set of X3D or glTF files.

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

<?php echo $toc->html_section(); ?>

<p>Blender versions earlier than 2.80
included a working X3D exporter out-of-the-box.
So you can export your content from Blender to X3D open it with our engine.</p>

<div class="download jumbotron">
    <a class="btn btn-primary btn-lg" href="https://github.com/castle-engine/castle-engine/wiki/Blender">Some hints about exporting from Blender to X3D are here</a>
</div>

<p>We have our own X3D exporter version, which is based on the Blender X3D exporter
and adds some fixes and also <b>support for <a href="x3d_implementation_texturing_extensions.php#section_ext_common_surface_shader">CommonSurfaceShader</a> exporting</b>.

<div class="download jumbotron">
    <a class="btn btn-primary btn-lg" href="https://raw.githubusercontent.com/castle-engine/cge-blender/master/x3d_exporter/castle_engine_x3d.zip"><span class="glyphicon glyphicon-download" aria-hidden="true"></span><br>Download our X3D exporter</a>
</div>

<p>A short installation instruction: just install it like any other Blender add-on (<i>"Install from file..."</i> on <i>"Add-ons"</i> tab).

<p>A <i>very detailed</i> installation instruction:

<ol>
  <li>In Blender, click the <i>"File -&gt; User Preferences"</i> menu item.
  <li>Go to the <i>"Add-ons"</i> tab.
  <li>Click the button <i>"Install from file..."</i> at the bottom of the window that appears.
  <li>Select the <code>castle_engine_x3d.zip</code> file you downloaded, and confirm by clicking <i>"Install from file..."</i> near the top-right corner.
  <li>Find the script box e.g. by searching for <code>"castle"</code> in the search box. You should see a script named <i>"Web3D X3D/VRML2 format (Castle Game Engine Importer/Exporter)"</i>. Activate this script by ticking the checkbox right near it's name.
  <li>Optionally, you can click now <i>"Save User Settings"</i> at the bottom to have this script automatically enabled next time you open Blender.
  <li>Make sure it works: look into the <i>"File -&gt; Export"</i> submenu, it should have now a new option <i>"X3D Extensible 3D (.x3d) (Castle Game Engine Exporter)"</i>
</ol>

<?php
creating_data_footer();
?>
