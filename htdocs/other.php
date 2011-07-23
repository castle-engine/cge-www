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

  <li>Programmers may be interested in my notes about:
    <ul>
      <li><a href="http://vrmlengine.svn.sourceforge.net/viewvc/*checkout*/vrmlengine/trunk/kambi_vrml_game_engine/examples/vrml/bump_mapping/README">Bump
        mapping techniques</a>,</li>
      <li><a href="http://vrmlengine.svn.sourceforge.net/viewvc/*checkout*/vrmlengine/trunk/kambi_vrml_game_engine/examples/glwindow/shading_langs/README">Shading
        languages (some info about ARB vertex/fragment assembly programs,
        and a lot of GLSL)</a>,</li>
      <li><a href="https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/kambi_vrml_game_engine/examples/vrml/terrain/TERRAIN_GENERATION_NOTES.txt">Basic terrain (noise) generation.</a>
    </ul>
    I wrote these documents when I was learning these techniques myself,
    they contain some summary and my findings, and links to other useful resources.
    You can take a look into our engine sources (in <tt>kambi_vrml_game_engine/examples/..</tt>
    directories, just look at the URLs of these documents) to see practical
    implementation of all these things.
  </li>
</ul>

<p><a href="http://michalis.ii.uni.wroc.pl/~michalis/vrmlengine-snapshots/">Nightly
builds of vrmlengine binaries are available.</a>
These are build automatically every night
using current SVN code. Use at your own risk.</p>

<?php vrmlengine_footer(); ?>
