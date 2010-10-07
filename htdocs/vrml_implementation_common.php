<?php

/* Common functions for pages describing VRML/X3D status
   of particular X3D component.
   Define 'X3D_COMPONENT_NAME' before including. */

require_once 'vrmlengine_functions.php';

function vrmlx3d_header($a_page_title)
{
  vrmlengine_header($a_page_title, NULL, array('vrml_x3d'));
}

function vrmlx3d_footer()
{
  vrmlengine_footer();
}

function x3d_status_header()
{
  vrmlengine_header(X3D_COMPONENT_NAME .
    ' component - implementation status - Kambi VRML game engine',
    NULL, array('vrml_x3d', 'vrml_implementation_status'));

  global $page_title;
  echo pretty_heading(X3D_COMPONENT_NAME . ' component');
}

function x3d_status_footer()
{
  vrmlengine_footer();
}
?>
