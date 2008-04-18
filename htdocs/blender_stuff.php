<?php
  require_once 'vrmlengine_functions.php';

  common_header("Blender VRML stuff", LANG_EN,
    "Various stuff related to Blender and VRML exporters, " .
    "especially for Kambi VRML game engine");

function echo_svn_blender_file($filename)
{
  echo '<a href="' .
    'http://vrmlengine.svn.sourceforge.net/viewvc/*checkout*/vrmlengine/trunk/blender/' .
    $filename . '">' . $filename . '</a>';
}
?>

<?php
  echo pretty_heading("Blender VRML stuff");
?>

<p>Various utilities for Blender, related to VRML :

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

<p>You can grab all this stuff from SVN:
<pre>
<?php echo sf_checkout_link(true, 'blender'); ?>
</pre>


<?php
  if (!IS_GEN_LOCAL) {
    $counter = php_counter("blender_stuff", true);
  };

  common_footer();
?>
