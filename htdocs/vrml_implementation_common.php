<?php

/* Common functions for pages describing VRML/X3D status
   of particular X3D component. */

require_once 'vrmlengine_functions.php';

function vrmlx3d_header($a_page_title)
{
  vrmlengine_header($a_page_title, NULL, array('vrml_x3d'));
}

function vrmlx3d_footer()
{
  vrmlengine_footer();
}

function x3d_status_header($x3d_component_name, $x3d_spec_page_url)
{
  vrmlengine_header($x3d_component_name .
    ' component - implementation status - Kambi VRML game engine',
    NULL, array('vrml_x3d', 'vrml_implementation_status'));

  echo pretty_heading($x3d_component_name . ' component');

  echo '<div class="useful_link">See also <a href="http://web3d.org/x3d/specifications/ISO-IEC-19775-1.2-X3D-AbstractSpecification/Part01/components/' .
    $x3d_spec_page_url . '.html">X3D ' .
    'specification of ' . $x3d_component_name . ' component</a></div>';
}

function x3d_status_footer()
{
  vrmlengine_footer();
}
?>
