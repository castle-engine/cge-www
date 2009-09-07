<?php

/* Common functions for pages describing VRML/X3D status
   of particular X3D component.
   Define 'X3D_COMPONENT_NAME' before including. */

require_once 'vrmlengine_functions.php';

function x3d_status_header()
{
  common_header(X3D_COMPONENT_NAME . ' component - implementation status - Kambi VRML game engine', LANG_EN);

  global $page_title;
  echo pretty_heading('<span style="font-size: medium">' .
    a_href_page('VRML/X3D implementation status', 'vrml_implementation_status') .
    ' :</span><br/>' . X3D_COMPONENT_NAME . ' component');
}

function x3d_status_footer()
{
  common_footer();
}

?>
