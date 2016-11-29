<?php
define('CASTLE_GITHUB_NAME', 'cge-blender');

require_once 'castle_engine_functions.php';
creating_data_header('Creating data in Blender');

$toc = new TableOfContents(
  array(
    new TocItem('Export to X3D', 'x3d'),
    new TocItem('Export to Castle Animation Frames (castle-anim-frames)', 'castle_anim_frames'),
    new TocItem('Hints about exporting animations to castle-anim-frames', 'castle_anim_frames_hints', 1),
    new TocItem('Rendering skyboxes and static cube environment maps', 'render_skybox'),
  )
);
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
and adds some small fixes. In time, we hope to have them submitted to official Blender code.
Install this by copying over (overwriting) the original Blender exporter,
which is in <code>&lt;blender-installation-dir&gt;/2.XX/scripts/addons/io_scene_x3d/export_x3d.py</code>.

<div class="download jumbotron">
    <a class="btn btn-primary btn-lg" href="https://raw.githubusercontent.com/castle-engine/cge-blender/master/x3d_exporter/export_x3d.py"><span class="glyphicon glyphicon-download" aria-hidden="true"></span><br>Download our X3D exporter</a>
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

<p>Thanks to the simplicity of the <code>.castle-anim-frames</code> format,
<b>this format exports every kind of Blender animation to our engine</b>:

<ul>
  <li>You can <b>transform</b> whole objects,
  <li>You can animate using an <b>armature</b> (skeleton) attached to a skinned mesh or disjoint objects,
  <li>You can <b>deform the mesh in any way (shape keys, hooks)</b>,
  <li>You can use <b>fluid simulation</b>,
  <li>You can use <b>physics</b> (rigid body, soft body, cloth),
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

<p>TODO: Note that right now the interpolation is not done using the X3D interpolators. This means that the memory usage is much higher than it could be. Also, detailed collision structures are not generated for all but the 1st frame. Otherwise, initializing collisions for a long series of nodes was really time-consuming. We have to implement actual conversion from a series of nodes -&gt; interpolators to have proper collisions with castle-anim-frames contents.

<?php echo $toc->html_section(); ?>

<p>You can render a set of six images that can be used as a skybox (<code>Background</code> in X3D) or a cube map texture (<code>ComposedCubeMapTexture</code> in X3D). We have a small Python script for Blender that renders the images to the appropriate names following the X3D conventions (front, back, top,....), and a simple X3D test scenes that allow to test that the resulting images indeed work as a skybox or a cubemap texture (e.g. you can open them in view3dscene).

<div class="download jumbotron">
    <a class="btn btn-primary btn-lg" href="https://github.com/castle-engine/cge-blender/tree/master/render_skybox"><span class="glyphicon glyphicon-download" aria-hidden="true"></span><br>Download render_skybox.py script, and test files</a>
</div>

<?php
creating_data_footer();
?>
