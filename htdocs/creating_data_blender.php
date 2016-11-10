<?php
define('CASTLE_GITHUB_NAME', 'cge-blender');

require_once 'castle_engine_functions.php';
creating_data_header('Creating data in Blender');

/*
function echo_svn_blender_file($filename)
{
  echo '<a href="' . sf_checkout_link(false, 'blender/' . $filename) . '">' . $filename . '</a>';
}
*/
?>

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

<p>The <i>Blender X3D exporter</i> cannot handle animations for now.
To solve this, use the exporter below to export animations to
a <?php echo a_href_page("KAnim (Castle Game Engine animations) format",
'kanim_format'); ?>. The <code>.kanim</code> files can be read by our engine
and will play animations. Thanks to the simplicity of the kanim format,
this approach supports every kind of Blender animation (transform, through armature or not,
deform in any way, fluids, physics, material animations...).

<div class="download jumbotron">
    <a class="btn btn-primary btn-lg" href="https://raw.githubusercontent.com/castle-engine/cge-blender/master/export_kanim.py"><span class="glyphicon glyphicon-download" aria-hidden="true"></span><br>Download KAnim exporter</a>
</div>

<p>Install it like every other Blender addon:</p>

<ol>
  <li>Use the comfortable <i>File -&gt; User Preferences (opens new window) -&gt; Addons (tab)
    -&gt; Install Addon... (button at the bottom)</i>.
    Or just copy the file directly to the
    <code>scripts/addons/</code> directory.
  <li>Enable it, by clicking the checkbox at <i>"Import-Export: Export KAnim"</i>
    in the Addons window.
</ol>

<p>Tested with <i>Blender &gt;= 2.68</i>.

<?php
creating_data_footer();
?>
