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

function x3d_status_header($x3d_component_name, $x3d_spec_page_url, $component_intro)
{
  vrmlengine_header($x3d_component_name .
    ' component - implementation status - Castle Game Engine',
    NULL, array('vrml_x3d', 'vrml_implementation_status'));

  echo pretty_heading($x3d_component_name . ' component');

  global $x3d_component_url;
  $x3d_component_url = 'http://web3d.org/x3d/specifications/ISO-IEC-19775-1.2-X3D-AbstractSpecification/Part01/components/' .
    $x3d_spec_page_url . '.html';

  echo '<div class="x3d_component_intro">
    <p class="paragraph_first">' . $component_intro . '</p>
    <p class="paragraph_last">See also <a href="' . $x3d_component_url .
    '">X3D specification of ' . $x3d_component_name . ' component</a>.</p></div>';
}

function x3d_status_footer()
{
  vrmlengine_footer();
}

/* Display name of VRML/X3D node, linked to it's description
   in X3D specification. This relies that the node's component was previously
   declared by x3d_status_header. */
function x3d_node_link($node_name)
{
  global $x3d_component_url;
  return '<tt><a href="' . $x3d_component_url . '#' . $node_name . '">' .
    $node_name . '</a></tt>';
}
?>
