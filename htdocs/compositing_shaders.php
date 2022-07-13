<?php
require_once 'castle_engine_functions.php';
require_once 'x3d_extensions_functions.php';

castle_header('Compositing Shaders');

$toc = new TableOfContents(array(
  new TocItem('Intro', 'intro'),
  new TocItem('Examples', 'examples'),
  new TocItem('Documentation', 'docs'),
  new TocItem('Conference paper and slides (a bit outdated)', 'old_docs'),
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

<p>I'm incredibly happy to note that we have <i>two</i> independent
implementations of this concept now:

<ol>
  <li>Inside
    <?php echo a_href_page('Castle Game Engine', 'index'); ?> and
    <?php echo a_href_page('view3dscene', 'view3dscene'); ?>.
    By Michalis Kamburelis, the author of this paper.
  <li>Inside <a href="http://freewrl.sourceforge.net/">FreeWRL</a>,
    another excellent VRML / X3D browser.
    By Doug Sanden.
</ol>

<?php echo $toc->html_section(); ?>

<ol>
  <li><a href="https://castle-engine.io/compositing_shaders_doc/html/">"Compositing Shaders in X3D" documentation, HTML version</a>.</li>
  <li><a href="https://castle-engine.io/compositing_shaders_doc/compositing_shaders_doc.pdf">"Compositing Shaders in X3D" documentation, PDF version</a>.
    <!--
    Exactly the same content as above HTML version, just in PDF &mdash; probably much better for printing.</li>
    -->
  <li>Source of this documentation is in <a href="https://github.com/castle-engine/cge-documentation/tree/master/compositing_shaders_doc">cge-documentation, compositing_shaders_doc subdirectory</a>.
</ol>

<p>If you have any questions, please
<a href="talk.php">ask on Castle Game Engine forum or Discord</a>.</p>

<?php echo $toc->html_section(); ?>

<p>The original description of this idea was Michalis Kamburelis' Ph.D. thesis. (Although never formally defended, due to lack of time.)

<p>I was also presenting it on <a href="http://www.eguk.org.uk/TPCG11/">Theory and Practice of Computer Graphics 2011 conference</a> and at <a href="https://ii.uni.wroc.pl/">seminar at Institure of Computer Science, Wrocław</a>.

<p>So I have some additional content about this idea, that may be useful -- but be careful, as it was not updated in the last years. While the idea and syntax remained unchanged, some advised PLUG names have changed.

<ol>
  <li><?php echo current_www_a_href_size('Slides from my presentation (TPCG11 conference) about compositing shaders', 'compositing_shaders_slides.pdf'); ?>.</li>
  <li><?php echo current_www_a_href_size('Paper (TPCG11 conference) about compositing shaders', 'miscella/compositing_shaders.pdf'); ?>.</li>
  <li><?php echo current_www_a_href_size('Slides in Polish from my presentation (on seminar on ii.uni.wroc.pl)', 'compositing_shaders_sem_dokt_polish.pdf'); ?>. These slides have roughly the same content as above TPCG11 slides, but in Polish.</li>
</ol>

<?php castle_footer(); ?>
