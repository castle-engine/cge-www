<?php
require_once 'castle_engine_functions.php';
require_once 'x3d_extensions_functions.php';

castle_header('Compositing Shaders', NULL,
  array('vrml_x3d', 'x3d_extensions', 'compositing_shaders'));

$toc = new TableOfContents(array(
  new TocItem('Intro', 'intro'),
  new TocItem('Examples', 'examples'),
  new TocItem('Documentation', 'docs'),
));

echo castle_thumbs(array(
  array('filename' => 'volumetric_animated_fog_all.png', 'titlealt' => 'Volumetric fog'),
  array('filename' => 'volumetric_animated_fog_no_fog.png', 'titlealt' => 'Scene for the volumetric fog, here visible with fog turned off'),
  array('filename' => 'volumetric_animated_fog_no_light.png', 'titlealt' => 'Volumetric fog, with normal lighting turned off'),
  array('filename' => 'fancy_light_spot_shape.png', 'titlealt' => 'Textured spot light with shadow'),
  array('filename' => 'flowers.png', 'titlealt' => 'Flowers bending under the wind, transformed on GPU in object space'),
  array('filename' => 'fresnel_and_toon.png', 'titlealt' => 'Toon and Fresnel effects combined'),
  array('filename' => 'noise.png', 'titlealt' => '3D and 2D smooth noise on GPU, wrapped in ShaderTexture'),
));

echo pretty_heading($page_title);
?>

<p>Contents:</p>
<?php echo $toc->html_toc(); ?>
<?php echo $toc->html_section(); ?>

<p>This in an extension of <?php echo a_href_page('X3D', 'vrml_x3d'); ?> that allows
to use shaders to define <i>effects</i> on 3D shapes, textures and lights.
You basically provide small pieces of shading language code that
will be seamlessly integrated with the engine internal shaders (and with each other).
This allows to create graphic effects using
the powerful shader langauges (like OpenGL Shading Language, GLSL, in case of <i>Castle Game Engine</i>).
Contrary to the traditional approach (using <code>ComposedShader</code> node,
see <?php echo a_href_page(
'shader component support', 'x3d_implementation_shaders'); ?>),
our system allows to create the effects easily. There is no need
to replicate existing functionality (you don't have to reimplement existing lighting or texturing operations
in your shaders, if you don't want to modify them).
Your shader effects automatically cooperate with each other and with standard
rendering features.</p>

<?php echo $toc->html_section(); ?>

<p>The examples are available inside
<?php echo a_href_page('our VRML/X3D demo models', 'demo_models'); ?>.
Download them, and look inside the subdirectory <code>compositing_shaders</code> there.
Also the <code>water</code> subdirectory contains water implementation
using our effects.</p>

<p>You can open the example models with any of our engine tools, like
<?php echo a_href_page('view3dscene', 'view3dscene'); ?>.</p>

<?php echo $toc->html_section(); ?>

<ol>
  <li><a href="http://castle-engine.sourceforge.net/compositing_shaders_doc/html/">"Compositing Shaders in X3D" documentation, HTML version</a>. This is my Ph.D. thesis, enjoy :)</li>
  <li><a href="http://castle-engine.sourceforge.net/compositing_shaders_doc/compositing_shaders_doc.pdf">"Compositing Shaders in X3D" documentation, PDF version</a>. Exactly the same content as above HTML version, just in PDF &mdash; probably much better for printing.</li>
  <li><?php echo current_www_a_href_size('Slides from my presentation (on TPCG11) about the compositing shaders', 'compositing_shaders_slides.pdf'); ?>.</li>
  <li><?php echo current_www_a_href_size('Slides in Polish from my presentation (on seminar on ii.uni.wroc.pl)', 'compositing_shaders_sem_dokt_polish.pdf'); ?>. These slides have roughly the same content as above TPCG11 slides, but in Polish.</li>
  <li>My paper about this idea was also accepted to the <a href="http://www.eguk.org.uk/TPCG11/">Theory and Practice of Computer Graphics 2011 conference</a>. I don't have permission to redistribute it here publicly, but basically it's a shortcut of my Ph.D. thesis. Anyway, you can buy it from <a href="http://diglib.eg.org/EG/DL/LocalChapterEvents/TPCG/TPCG11">EG digital library (along with other papers from TPCG11 proceedings)</a>.</li>
  <li>Of course, you can also get the sources of above documents:
    <ul>
      <li>My Ph.D. thesis sources (in DocBook) can be downloaded from SVN URL:<br/>
        <code>http://svn.code.sf.net/p/castle-engine/code/trunk/papers/compositing_shaders_doc/</code></li>
      <li>Slides and conference paper (in TeX) are alongside, on SVN URL:<br/>
        <code>http://svn.code.sf.net/p/castle-engine/code/trunk/papers/compositing_shaders/</code></li>
    </ul>
  </li>
</ol>

<p>Finally, if you're interested in more information,
<?php echo michalis_mailto('send me a mail'); ?>.
And be sure to investigate the examples mentioned above!</p>

<?php castle_footer(); ?>
