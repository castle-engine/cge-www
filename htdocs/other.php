<?php
  require_once 'vrmlengine_functions.php';
  vrmlengine_header('Other documentation');

  echo pretty_heading($page_title);
?>

<p>Some more information, randomly related to the engine and
the programs developed with it:</p>

<ul>
  <li><a href="http://apps.sourceforge.net/mediawiki/vrmlengine/">Wiki
    of Kambi VRML game engine</a>
    (<b>Please</b> contribute to it, and then we'll make the wiki link more visible!)
  <li><?php echo a_href_page(
    "Blender VRML/X3D exporters", "blender_stuff"); ?></li>
</ul>

<p><a href="http://michalis.ii.uni.wroc.pl/~michalis/vrmlengine-snapshots/">Nightly
builds of vrmlengine binaries are available.</a>
These are build automatically every night
using current SVN code. Use at your own risk.</p>

<?php vrmlengine_footer(); ?>
