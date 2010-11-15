<?php
  require_once 'vrmlengine_functions.php';
  require_once 'kambi_vrml_extensions_functions.php';

  vrmlengine_header('Screen Effects', NULL,
    array('vrml_x3d', 'kambi_vrml_extensions', 'kambi_vrml_extensions_screen_effects'));

// $toc = new TableOfContents(array());
// $toc->echo_numbers = true;
?>

<?php echo pretty_heading($page_title);  ?>

<!--p>Contents:
< ?php echo $toc->html_toc(); ? >

< ?php echo $toc->html_section(); ? -->


<?php
  vrmlengine_footer();
?>
