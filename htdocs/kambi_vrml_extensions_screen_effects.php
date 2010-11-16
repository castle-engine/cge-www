<?php
  require_once 'vrmlengine_functions.php';
  require_once 'kambi_vrml_extensions_functions.php';

  vrmlengine_header('Screen Effects', NULL,
    array('vrml_x3d', 'kambi_vrml_extensions', 'kambi_vrml_extensions_screen_effects'));

// $toc = new TableOfContents(array());
// $toc->echo_numbers = true;

  echo vrmlengine_thumbs(array(
    array('filename' => 'screen_effect_trees.png', 'titlealt' => 'Another screen effect example'),
    array('filename' => 'screen_effects_demo3.png', 'titlealt' => 'Demo of three ScreenEffects defined in VRML/X3D, see screen_effects.x3dv'),
    array('filename' => 'screen_effect_headlight_and_gamma.png', 'titlealt' => 'Screen effect: headlight, gamma brightness (on DOOM E1M1 level remade for our Castle)'),
    array('filename' => 'screen_effect_grayscale_negative.png', 'titlealt' => 'Screen effect: grayscale, negative (on Tremulous ATCS level)'),
    array('filename' => 'screen_effect_castle_hall_0.png', 'titlealt' => 'Castle Hall screen: no effects'),
    array('filename' => 'screen_effect_castle_hall_1.png', 'titlealt' => 'Castle Hall screen: edge detection effect, with some gamma and negative'),
  ));

  echo pretty_heading($page_title);

  // <!--p>Contents:
  // < ?php echo $toc->html_toc(); ? >
  // < ?php echo $toc->html_section(); ? -->
?>

<p><i>Screen effects</i> allow you to create many graphic effects
by post-processing the rendered image. For demos, see:</p>

<ul>
  <li><p><?php echo a_href_page('view3dscene', 'view3dscene') ?>
    menu <i>View -&gt; Screen Effects</i>. Note that you can activate
    many effects at the same time.</p></li>

  <li><p>VRML/X3D file <a href="https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/kambi_vrml_test_suite/x3d/screen_effects.x3dv">screen_effects.x3dv</a>.
    You should download full
    <?php echo a_href_page('Kambi VRML test suite', 'kambi_vrml_test_suite'); ?>
    and open file <tt>x3d/screen_effects.x3dv</tt> there, to see the complete
    demo with an example castle model underneath.</p></li>
</ul>

<?php
  vrmlengine_footer();
?>
