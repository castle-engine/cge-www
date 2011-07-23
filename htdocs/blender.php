<?php
  require_once 'vrmlengine_functions.php';

  vrmlengine_header("Blender X3D exporter (with Kambi modifications)",
    NULL, array('blender'));

function echo_svn_blender_file($filename)
{
  echo '<a href="' .
    'http://vrmlengine.svn.sourceforge.net/viewvc/*checkout*/vrmlengine/trunk/blender/' .
    $filename . '">' . $filename . '</a>';
}
?>

<?php
  echo pretty_heading('Blender X3D exporter', NULL,
    'with Kambi modifications');
?>

<p><a href="http://www.blender.org/">Blender</a> is a magnificent
free open-source 3D modelling software. Latest Blender version already
includes an X3D exporter, so you can export your content and open it with
our engine tools like <?php echo a_href_page('view3dscene', 'view3dscene') ?>.
We publish here some improvements / fixes / documentation for
Blender's X3D exporter.</p>

<h2>X3D exporter, for Blender 2.58</h2>

<ul>
  <li><p><?php echo_svn_blender_file('blender25_x3d/export_x3d.py') ?>:
    Download the actual exporter.

    <p>Copy it over (overwrite) the original Blender exporter,
    which is in <tt>&lt;blender-installation-dir&gt;/2.58/scripts/addons/io_scene_x3d/export_x3d.py</tt>.
    <!--You will need to restart Blender (if currently running to load the new exporter.-->

    <p>Note that for 2.58 exporter, I advice to use <i>Triangulate=Off</i>.
    There are some problems with triangulation: it cannot preserve vertex sharing
    in case of some UVs or vertex colors (<a href="http://projects.blender.org/tracker/index.php?func=detail&amp;aid=27773&amp;group_id=9&amp;atid=498">#27773, in svn it now forces Normals=On</a>),
    it cannot preserve creaseAngle (<a href="http://projects.blender.org/tracker/?func=detail&amp;aid=27769&amp;group_id=9&amp;atid=127">#27769</a>).
    Using <i>Normals=On</i> makes <i>Triangulate=On</i> better,
    but sometimes normals are still incorrect in my experience. I try to investigate
    and submit to Blender relevant patches or bugs, for now with official 2.58
    it's safer to just use <i>Triangulate=Off</i> in my experience.

  <li><p><?php echo_svn_blender_file('blender25_x3d/x3d_blender_exporter_notes.txt') ?>:
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

<h2>VRML 97 (2.0) and KAnim exporters, for Blender 2.4x</h2>

<p>(Please note that I don't use exporters below anymore,
they probably will not work with Blender 2.5x or newer,
and I probably will not update them. X3D is the future,
and X3D exporter above should be your preferred option.
We also hope one day to see animation export to X3D,
and then our "KAnim hack" will no longer be useful.)</p>

<ul>
  <li><p>Script <?php echo_svn_blender_file('kambi_vrml97_export.py') ?>
    (requires accompanying <?php echo_svn_blender_file('kambi_vrml97_export_base.py') ?>) :</p>

    <p>Customized version of <tt>vrml97_export.py</tt> script
    (distributed with newer Blender versions, originally from
    <a href="http://kimballsoftware.com/blender/">here</a>).
    Various customizations to improve exporting VRML 2.0 (aka 97) models:
    creaseAngle exporting (from Blender's set smooth/set solid/auto smooth/degr
    controls), full texture filename (including possibly relative path prefix)
    is written, corrected twoside detection,
    Background is proper VRML, also the model is <i>not</i>
    rotated to change +Z axis to +Y.
    Search for "Kambi" string inside files to know more.</p>

    <p>The essential exporter class is inside
    <tt>kambi_vrml97_export_base.py</tt>, to be shared by <tt>kanim_export.py</tt>.</p>

    <p>May not work with Blender &lt; 2.44.</p>

    <p>TODO: some fixes here should be submitted and hopefully merged into
    VRML 2.0 exporter included with Blender.
  </li>

  <li>
    <p><?php echo_svn_blender_file('vrml_97_exporter_notes.txt') ?> :</p>

    <p>Some details important for anyone who exports VRML 2.0 from Blender,
    using original or our script versions. Also explains some
    reasoning behind changes in <tt>kambi_vrml97_export[_base].py</tt>
    as compared to original exporter.</p>
  </li>

  <li>
    <p><?php echo_svn_blender_file('kanim_export.py') ?>
    (requires accompanying <?php echo_svn_blender_file('kambi_vrml97_export_base.py') ?>) :</p>

    <p>Exporter to
    <?php echo a_href_page("kanim (Kambi VRML engine animations) format",
      'kanim_format'); ?>. Thanks to Grzegorz Hermanowicz for starting this !</p>
  </li>
</ul>

<h2>Subversion</h2>

<p>You can grab all this stuff from SVN:
<pre class="terminal small">
<?php echo sf_checkout_link(true, 'blender'); ?>
</pre>


<?php
  vrmlengine_footer();
?>
