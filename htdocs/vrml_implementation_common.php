<?php

/* Common functions for pages describing VRML/X3D status
   of particular X3D component.
   Define 'X3D_COMPONENT_NAME' before including. */

require_once 'vrmlengine_functions.php';

function vrmlx3d_header($a_page_title)
{
  common_header($a_page_title, LANG_EN);
  echo '<table class="layout">
    <col class="content">
    <col class="sidebar">
    <tr><td class="layout">';
}

function vrmlx3d_footer()
{
  echo '</td><td class="layout">';
  vrmlx3d_sidebar();
  echo '</td></tr></table>';
  common_footer();
}

function x3d_status_header()
{
  vrmlx3d_header(X3D_COMPONENT_NAME .
    ' component - implementation status - Kambi VRML game engine');

  global $page_title;
  echo pretty_heading('<span style="font-size: medium">' .
    a_href_page('VRML/X3D implementation status', 'vrml_implementation_status') .
    ' :</span><br/>' . X3D_COMPONENT_NAME . ' component');
}

function x3d_status_footer()
{
  vrmlx3d_footer();
}

function vrmlx3d_sidebar()
{
  /* TODO: make current not link */
  ?>
<div class="sidebar">
  <div class="sidebar_title">
    <small><?php echo a_href_page('Kambi VRML game engine', 'index'); ?></small>
    <h2><?php echo a_href_page('VRML / X3D support', 'vrml_x3d'); ?></h2>
  </div>
  <ul>
    <li><?php echo a_href_page('Extensions', 'kambi_vrml_extensions'); ?></li>
    <li><?php echo a_href_page('Test suite', 'kambi_vrml_test_suite'); ?></li>
    <li><?php echo a_href_page('Implementation status', 'vrml_implementation_status'); ?>
      <ul>
        <li><?php echo a_href_page('Core'                            , 'vrml_implementation_core'                ); ?>  </li>
        <li><?php echo a_href_page('Time'                            , 'vrml_implementation_time'                ); ?>  </li>
        <li><?php echo a_href_page('Networking'                      , 'vrml_implementation_networking'          ); ?>  </li>
        <li><?php echo a_href_page('Grouping'                        , 'vrml_implementation_grouping'            ); ?>  </li>
        <li><?php echo a_href_page('Rendering'                       , 'vrml_implementation_rendering'           ); ?>  </li>
        <li><?php echo a_href_page('Shape'                           , 'vrml_implementation_shape'               ); ?>  </li>
        <li><?php echo a_href_page('Geometry3D'                      , 'vrml_implementation_geometry3d'          ); ?>  </li>
        <li><?php echo a_href_page('Geometry2D'                      , 'vrml_implementation_geometry2d'          ); ?>  </li>
        <li><?php echo a_href_page('Text'                            , 'vrml_implementation_text'                ); ?>  </li>
        <li><?php echo a_href_page('Lighting'                        , 'vrml_implementation_lighting'            ); ?>  </li>
        <li><?php echo a_href_page('Texturing'                       , 'vrml_implementation_texturing'           ); ?>
          <ul>
            <li><?php echo a_href_page_hashlink('Clarifications to X3D multi-texturing', 'vrml_implementation_texturing', 'section_multi_texturing'); ?></li>
            <li><?php echo a_href_page_hashlink('DDS (DirectDraw Surface)', 'vrml_implementation_texturing', 'section_dds'); ?></li>
          </ul>
        </li>
        <li><?php echo a_href_page('Interpolation'                   , 'vrml_implementation_interpolation'       ); ?>  </li>
        <li><?php echo a_href_page('Pointing device sensor'          , 'vrml_implementation_pointingdevicesensor'); ?>  </li>
        <li><?php echo a_href_page('Key device sensor'               , 'vrml_implementation_keydevicesensor'     ); ?>  </li>
        <li><?php echo a_href_page('Environmental sensor'            , 'vrml_implementation_environmentalsensor' ); ?>  </li>
        <li><?php echo a_href_page('Navigation'                      , 'vrml_implementation_navigation'          ); ?>  </li>
        <li><?php echo a_href_page('Environmental effects'           , 'vrml_implementation_environmentaleffects'); ?>  </li>
        <li><?php echo a_href_page('H-Anim'                          , 'vrml_implementation_hanim'               ); ?>  </li>
        <li><?php echo a_href_page('NURBS'                           , 'vrml_implementation_nurbs'               ); ?>  </li>
        <li><?php echo a_href_page('Scripting'                       , 'vrml_implementation_scripting'           ); ?>  </li>
        <li><?php echo a_href_page('Event utilities'                 , 'vrml_implementation_eventutilities'      ); ?>  </li>
        <li><?php echo a_href_page('Programmable shaders'            , 'vrml_implementation_shaders'             ); ?>  </li>
        <li><?php echo a_href_page('CAD geometry'                    , 'vrml_implementation_cadgeometry'         ); ?>  </li>
        <li><?php echo a_href_page('Texturing3D'                     , 'vrml_implementation_texturing3d'         ); ?>  </li>
        <li><?php echo a_href_page('Cube map environmental texturing', 'vrml_implementation_cubemaptexturing'    ); ?>  </li>
      </ul>
    </li>
    <li><?php echo a_href_page('NIST conformace test suite', 'nist_vrml_test_suite'); ?></li>
    <li><?php echo a_href_page('KambiScript language reference', 'kambi_script'); ?></li>
    <li><?php echo a_href_page('Kanim (precalculated animations) file format', 'kanim_format'); ?></li>
    <li><?php echo a_href_page('VRML / X3D time origin considered uncomfortable', 'vrml_time_origin_considered_uncomfortable'); ?>
  </ul>
</div>
<?php
}

?>
