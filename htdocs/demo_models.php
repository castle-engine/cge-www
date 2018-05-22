<?php
  define('CASTLE_GITHUB_NAME', 'demo-models');

  require_once 'castle_engine_functions.php';
  require_once 'x3d_implementation_common.php';

  vrmlx3d_header("Demo models");

  $toc = new TableOfContents(
    array(
      new TocItem('About', 'about'),
      new TocItem('Development', 'development'),
      new TocItem('Authors', 'authors'),
    ));

  echo pretty_heading($page_title, VERSION_DEMO_MODELS);
  echo castle_thumbs(array(
    /* shader pipeline */
    array('filename' => 'rhan_shrine_5_everything.png', 'titlealt' => 'Shinto shrine model, from http://opengameart.org/content/shrine-shinto-japan , with multiple shadow maps enabled'),
    array('filename' => 'metallic_shiny.png', 'titlealt' => 'Shiny dark metallic material under multiple lights, with per-pixel lighting.'),
    array('filename' => 'volumetric_animated_fog_all.png', 'titlealt' => 'Volumetric fog'),
    array('filename' => 'fancy_light_spot_shape.png', 'titlealt' => 'Textured spot light with shadow'),
    array('filename' => 'castle_overburn.png', 'titlealt' => 'Castle &quot;overburn&quot; simple effect.'),
    array('filename' => 'shadows_chopper_and_house.png', 'titlealt' => 'Shadow volumes from chopper over a house scenery. Chopper can be moved, rotated, scaled by mouse.'),
    array('filename' => 'fountain_shadows_0.png', 'titlealt' => 'Fountain level model, with shadow volumes.'),
    array('filename' => 'fountain_shadows_1.png', 'titlealt' => 'The same fountain level model, with shadow volumes. After some interactive fun with moving/rotating stuff around :)'),
  ));
?>

<div class="download jumbotron">
    <!-- We use a div with min-width,
         to make sure the text "Support on Patreon"
         fits on the button. -->
    <div style="display: block; min-width: 20em; text-align: center;">

    <?php
    if (CASTLE_ENVIRONMENT == 'offline')
    {
      echo '<p><a href="' . CURRENT_URL . $this_page_name .
        '" class="btn btn-primary btn-lg">Download demo models from our WWW page</a></p>';
    } else
    {
      ?>

      <?php echo sf_download('<span class="glyphicon glyphicon-download" aria-hidden="true"></span><br>Download demo models', 'demo_models-' . VERSION_DEMO_MODELS . '.zip'); ?>

      <?php echo download_donate_footer(); ?>
      <?php
    }
    ?>

    </div>
</div>

<p>Documentation:

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>This is our collection of demo 3D models.
Some are impressive 3D models, some (most) are simple models demonstrating some
technical feature.
Most of the models are in X3D and VRML formats,
but in general various formats handled by
 <?php echo a_href_page('view3dscene', 'view3dscene'); ?> and
 <?php echo a_href_page('Castle Game Engine', 'index'); ?> are used.
At the beginning of most files (remember that you can open X3D files
in normal text editor too) you can find
comments explaining what this file demonstrates.

<p>These models were created to test
<?php echo a_href_page('our Castle Game Engine', 'index'); ?>,
but many of them should also be handled by other conforming
X3D and VRML browsers.</p>

<p>Files inside <code>warnings/</code> subdirectories are deliberately invalid
in some ways. Good X3D browser should display a sensible warning
message and (when possible) continue (omitting only the problematic part).</p>

<?php echo $toc->html_section(); ?>

<p>Comments and contributions to these demos are most welcome.
See <?php echo a_href_page('forum', 'forum'); ?> for ways to contact us.
If you have a cool 3D model, something interesting,
or maybe just something difficult for our engine, please share it with us!
<!--Posting a screenshot or a video of your model is most welcome.-->
We love to see how you use our tools :)
<!--
To include your model in these demos, or to report a bug, you will of course
have to actually send your 3D model source.--></p>

<p>Feel free to expand, modify, redistribute these test files
&mdash; they are covered by GNU GPL &gt;= 2 license.</p>

<p>You can always download the very current version of these models from <a href="https://github.com/castle-engine/demo-models">our GitHub project</a>, like this:

<pre>git clone https://github.com/castle-engine/demo-models.git</pre>

<?php echo $toc->html_section(); ?>

<p>Most of the 3D models here were created by Kambi (Michalis Kamburelis).
Simple test models were simply manually crafted in a text editor.
The more complicated meshes were created in
<a href="http://www.blender3d.org/">Blender</a> (see
our <?php echo a_href_page('Blender exporting documentation', 'creating_data_blender'); ?>),
you will find the .blend source files inside.
For 2D art I usually use <a href="http://www.gimp.org/">GIMP</a>.
Skies generated with <a href="http://www.planetside.co.uk/">Terragen</a>.</p>

<p>Large/important contributions:</p>

<ul>
  <li><p><i>Victor Amat</i> provided a lot of interesting demos.
    To mention some:
    flat mirrors by <code>RenderedTexture</code> (see <code>x3d/rendered_texture/</code>),
    Screen Space Ambient Occlusion (see <code>shadow_maps/ssao*</code>),
    shadow maps tests (including demo to visualize bias/scale,
    <code>shadow_maps/shadow_bias.x3dv</code>),
    <code>vrml_2/camera_{orient,rot}.wrl</code>,
    <code>orientation_interpolator_alum_box.wrl</code>,
    test textures for spherical mapping testing (<code>textures/spheremap-*.jpg</code>).
    Thousand thanks!</p></li>

  <li><p><i>Stephen H. France</i> provided various tests for <code>TimeSensor</code>,
    <code>Extrusion</code> and others. Thanks!</p></li>

  <li><p>Various models were based on
    <a href="http://opengameart.org/">OpenGameArt.org</a> models,
    also some textures were taken from there. This is a great site
    where you can find models and textures on clear open-source licenses
    (GPL or CC), thousand thanks go to it's many contributors.

    <p>Examples include
    <a href="http://opengameart.org/content/shrine-shinto-japan">Shinto Shrine</a>
    (used for compositing shaders demo inside <code>compositing_shaders/shinto_shrine/</code>).
    Also a lot of textures, see <code>compositing_shaders/textures/AUTHORS.txt</code>.</p>
  </li>

  <li><p><i>Orin Palmer</i> created plane model in 3DS, see <code>3ds/p47d.3ds</code>
    and <code>3ds/p47d.txt</code>.</p></li>

  <li><p>Many textures inside <code>textures/castle/</code>
    are from <a href="http://www.wolfiesden.com/golgotha/golgotha.asp">public domain textures from unfinished game "Golgotha"</a><!-- (previously on http://www.jonathanclark.com/, but host seems dead now)-->
    </p></li>

  <li>lion bump mapping textures from
    <a href="http://www-static.cc.gatech.edu/grads/d/davidp/parallax_mapping/">Philippe
    David steep parallax mapping code</a>, based on <a href="http://graphics.cs.brown.edu/games/SteepParallax/index.html">paper
    about steep mapping</a>.

  <li><p>See also <code>AUTHORS.txt</code> files inside the archive for detailed
    and complete listing of authors and sources.</p></li>
</ul>

<p>Thanks to everyone!</p>

<?php
  vrmlx3d_footer();
?>
