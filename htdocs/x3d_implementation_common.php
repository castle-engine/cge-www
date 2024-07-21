<?php

/* Common functions for pages describing VRML/X3D status
   of particular X3D component. */

require_once 'castle_engine_functions.php';

function x3d_status_header($x3d_component_name, $component_name_for_url, $component_intro)
{
  castle_header($x3d_component_name .' component');

  echo pretty_heading($x3d_component_name . ' component');

  global $x3d_current_component_name_for_url;
  $x3d_current_component_name_for_url = $component_name_for_url;

  $x3d_component_url = x3d_spec_latest_url($component_name_for_url);

  echo '<div class="x3d_component_intro">
    <p class="paragraph_first">' . $component_intro . '</p>
    <p class="paragraph_last">See also <a href="' . $x3d_component_url .
    '">X3D specification of the ' . $x3d_component_name . ' component</a>.</p></div>';
}

function x3d_extensions_header($x3d_component_name, $base_component_page,
  $component_name_for_url, $component_intro, $social_share_image = NULL)
{
  $base_component_page = 'x3d_implementation_' . $base_component_page;

  $header_parameters = array(
    'meta_description' => 'Castle Game Engine (and view3dscene) extensions to the ' . $x3d_component_name .' X3D component'
  );
  if ($social_share_image != '') {
    $header_parameters['social_share_image'] = $social_share_image;
  }
  castle_header($x3d_component_name .' component - extensions', $header_parameters);

  echo pretty_heading($x3d_component_name . ' component - extensions');

  global $x3d_current_component_name_for_url;
  $x3d_current_component_name_for_url = $component_name_for_url;

  $x3d_component_url = x3d_spec_latest_url($component_name_for_url);

  echo '<div class="x3d_component_intro">
    <p class="paragraph_first">' . $component_intro . '</p>
    <p class="paragraph_last">See also ' . a_href_page('documentaton of supported nodes of the ' . $x3d_component_name . ' component', $base_component_page) .
      ' and <a href="' . $x3d_component_url .
    '">X3D specification of the ' . $x3d_component_name . ' component</a>.</p></div>';
}

function x3d_node_cgeRef($node_name)
{
  $node_name_pascal = 'T' . $node_name . 'Node';
  return '<small>(Pascal API: ' .
    cgeRef($node_name_pascal) .
    ')</small>';
}

/* Display X3D node, linked to X3D specification and to Pascal API docs.
   This depends you used x3d_status_header or x3d_extensions_header
   to set current X3D component name.

   $spec_version value is like for x3d_spec_latest_url:
   NULL (latest stable spec) or 'draft' (latest draft spec).
*/
function x3d_node_link($node_name, $spec_version = NULL)
{
  global $x3d_current_component_name_for_url;
  return x3d_node_link2($node_name, $x3d_current_component_name_for_url, $spec_version);
}

/* Display X3D node, linked to X3D specification and to Pascal API docs.

   $spec_version value is like for x3d_spec_latest_url:
   NULL (latest stable spec) or 'draft' (latest draft spec).
*/
function x3d_node_link2($node_name, $component_name_for_url, $spec_version = NULL)
{
  $x3d_component_url = x3d_spec_latest_url($component_name_for_url, $node_name, $spec_version);
  return '<code><a href="' . $x3d_component_url . '">' .
    $node_name . '</a></code>' . x3d_node_cgeRef($node_name);
}
?>
