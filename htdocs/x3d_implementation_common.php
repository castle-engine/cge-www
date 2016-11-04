<?php

/* Common functions for pages describing VRML/X3D status
   of particular X3D component. */

require_once 'castle_engine_functions.php';

function vrmlx3d_header($a_page_title)
{
  castle_header($a_page_title, array(
    'path' => array('vrml_x3d')
  ));
}

function vrmlx3d_footer()
{
  castle_footer();
}

function x3d_status_header($x3d_component_name, $x3d_spec_page_url, $component_intro)
{
  castle_header($x3d_component_name .' component', array(
    'path' => array('vrml_x3d', 'x3d_implementation_status')
  ));

  echo pretty_heading($x3d_component_name . ' component');

  global $x3d_component_url;
  $x3d_component_url = x3d_spec_latest_url($x3d_spec_page_url);

  echo '<div class="x3d_component_intro">
    <p class="paragraph_first">' . $component_intro . '</p>
    <p class="paragraph_last">See also <a href="' . $x3d_component_url .
    '">X3D specification of the ' . $x3d_component_name . ' component</a>.</p></div>';
}

function x3d_extensions_header($x3d_component_name, $base_component_page,
  $x3d_spec_page_url, $component_intro)
{
  $base_component_page = 'x3d_implementation_' . $base_component_page;
  castle_header($x3d_component_name .' component - extensions', array(
    'meta_description' => 'Castle Game Engine (and view3dscene) extensions to the ' . $x3d_component_name .' X3D component',
    'path' => array('vrml_x3d', 'x3d_implementation_status', $base_component_page)
  ));

  echo pretty_heading($x3d_component_name . ' component - extensions');

  global $x3d_component_url;
  $x3d_component_url = x3d_spec_latest_url($x3d_spec_page_url);

  echo '<div class="x3d_component_intro">
    <p class="paragraph_first">' . $component_intro . '</p>
    <p class="paragraph_last">See also ' . a_href_page('documentaton of supported nodes of the ' . $x3d_component_name . ' component', $base_component_page) .
      ' and <a href="' . $x3d_component_url .
    '">X3D specification of the ' . $x3d_component_name . ' component</a>.</p></div>';
}

function x3d_status_footer()
{
  castle_footer();
}

/* Display name of VRML/X3D node, linked to it's description
   in X3D specification. This relies that the node's component was previously
   declared by x3d_status_header. */
function x3d_node_link($node_name)
{
  global $x3d_component_url;
  return '<code><a href="' . $x3d_component_url . '#' . $node_name . '">' .
    $node_name . '</a></code>';
}
?>
