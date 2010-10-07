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
    "Blender VRML stuff", "blender_stuff"); ?></li>
  <li><?php echo a_href_page(
    "Standard command-line options understood by all OpenGL programs",
    "opengl_options"); ?>
  <li><?php echo a_href_page(
    'Notes related to programs using OpenAL', 'openal_notes'); ?>
  <li><?php echo a_href_page(
    "Command-line options understood by all programs here",
    "common_options"); ?>

  <li><?php echo a_href_page('Versioning scheme', 'versioning'); ?>

  <li><?php echo current_www_a_href_size('All Windows DLLs used by programs here',
    'miscella/win32_dlls.zip'); ?>

  <li><?php echo a_href_page('Dependencies on Mac OS X',
    'macosx_requirements'); ?>

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

<p>See also <a href="http://sourceforge.net/projects/vrmlengine">vrmlengine
project page on SourceForge</a>.

<p><a href="http://michalis.ii.uni.wroc.pl/~michalis/vrmlengine-snapshots/">Nightly
builds of vrmlengine binaries are available.</a>
These are build automatically every night
using current SVN code. Use at your own risk.</p>

<p>For amusement (mostly), you can watch commits to vrmlengine through
<a href="http://cia.vc/stats/project/vrmlengine">Kambi VRML game engine on
cia.vc</a>.</p>

<?php vrmlengine_footer(); ?>
