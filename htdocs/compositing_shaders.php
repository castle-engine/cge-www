<?php
require_once 'castle_engine_functions.php';
require_once 'x3d_extensions_functions.php';

castle_header('Shader Effects (Compositing Shaders)');

$toc = new TableOfContents(array(
  new TocItem('Intro', 'intro'),
  new TocItem('Examples for Pascal developers', 'examples_pascal'),
  new TocItem('Examples for X3D authors', 'examples_x3d'),
  new TocItem('Nodes implementations', 'implementations'),
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

<p><i>Castle Game Engine</i> defines a few
nodes that allow to use shaders to define <i>effects</i> on 3D shapes, textures and lights.

<p>You basically provide small pieces of shading language code
(OpenGL Shading Language, GLSL) that
will be seamlessly integrated with the engine internal shaders (and with each other).
This allows to create graphic effects using
the powerful shader languages.
Contrary to the traditional approach of using shaders (using <code>ComposedShader</code> nodes,
see <?php echo a_href_page(
'shader component support', 'x3d_implementation_shaders'); ?>),
this new system allows to create the effects more easily. There is no need
to replicate existing functionality (e.g. you don't have to reimplement existing lighting or texturing operations
in your shaders, if you don't want to modify them).
Moreover, your shader effects automatically cooperate with each other and with standard
rendering features, hence why we call this feature <i>compositing shaders</i>.</p>

<?php echo $toc->html_section(); ?>

<p>For engine developers using Pascal:

<ul>
  <li>
    <p>The most important example is <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/viewport_and_scenes/shader_effects">Using Shader Effects to implement rendering effects that enhance the standard rendering (examples/viewport_and_scenes/shader_effects)</a>. It shows applying shader effects on shapes.

  <li>
    <p>See also <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/viewport_and_scenes/shader_effects_on_texture">Shader Effects On Texture (examples/viewport_and_scenes/shader_effects_on_texture)</a>. This focuses on applying the effects on textures. As always, follow the <a href="https://github.com/castle-engine/castle-engine/tree/master/examples/viewport_and_scenes/shader_effects_on_texture#readme">README of the example</a> to know all the details.
</ul>

<?php echo $toc->html_section(); ?>

<p>The example models are available inside
<?php echo a_href_page('our demo models', 'demo_models'); ?>.
Download them, and look inside the subdirectory <code>compositing_shaders</code> there.
Also the <code>water</code> subdirectory contains water implementation
using our effects.</p>

<p>You can open the example models with any of our engine tools, like
<a href="castle-model-viewer">Castle Model Viewer</a>.</p>

<?php echo $toc->html_section(); ?>

<p>The <a href="https://castle-engine.io/compositing_shaders_doc/html/">specification
of these effects</a> is an extension of the <?php echo a_href_page('X3D', 'doc/x3d'); ?> standard.
We have <i>two</i> independent implementations of this concept now:

<ol>
  <li>Inside
    <?php echo a_href_page('Castle Game Engine', 'index'); ?> and
    <a href="castle-model-viewer">Castle Model Viewer</a>.
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
