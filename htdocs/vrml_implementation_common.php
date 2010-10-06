<?php

/* Common functions for pages describing VRML/X3D status
   of particular X3D component.
   Define 'X3D_COMPONENT_NAME' before including. */

require_once 'vrmlengine_functions.php';

function vrmlx3d_header($a_page_title)
{
  vrmlengine_header($a_page_title, NULL, 'vrmlx3d_sidebar');
}

function vrmlx3d_footer()
{
  vrmlengine_footer();
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
  return '
<div class="sidebar">
  <div class="sidebar_title">
    <small>' . a_href_page('Kambi VRML game engine', 'index') . '</small>
    <h2>' . a_href_page('VRML / X3D support', 'vrml_x3d') . '</h2>
  </div>
  <ul>
    <li>' . a_href_page('Extensions', 'kambi_vrml_extensions') . '</li>
    <li>' . a_href_page('Test suite', 'kambi_vrml_test_suite') . '</li>
    <li>' . a_href_page('Implementation status', 'vrml_implementation_status') . '
      <ul>
        <li>' . a_href_page('Core'                            , 'vrml_implementation_core'                ) . '</li>
        <li>' . a_href_page('Time'                            , 'vrml_implementation_time'                ) . '</li>
        <li>' . a_href_page('Networking'                      , 'vrml_implementation_networking'          ) . '</li>
        <li>' . a_href_page('Grouping'                        , 'vrml_implementation_grouping'            ) . '</li>
        <li>' . a_href_page('Rendering'                       , 'vrml_implementation_rendering'           ) . '</li>
        <li>' . a_href_page('Shape'                           , 'vrml_implementation_shape'               ) . '</li>
        <li>' . a_href_page('Geometry3D'                      , 'vrml_implementation_geometry3d'          ) . '</li>
        <li>' . a_href_page('Geometry2D'                      , 'vrml_implementation_geometry2d'          ) . '</li>
        <li>' . a_href_page('Text'                            , 'vrml_implementation_text'                ) . '</li>
        <li>' . a_href_page('Lighting'                        , 'vrml_implementation_lighting'            ) . '</li>
        <li>' . a_href_page('Texturing'                       , 'vrml_implementation_texturing'           ) . '
          <ul>
            <li>' . a_href_page_hashlink('Clarifications to X3D multi-texturing', 'vrml_implementation_texturing', 'section_multi_texturing') . '</li>
            <li>' . a_href_page_hashlink('DDS (DirectDraw Surface)', 'vrml_implementation_texturing', 'section_dds') . '</li>
          </ul>
        </li>
        <li>' . a_href_page('Interpolation'                   , 'vrml_implementation_interpolation'       ) . '</li>
        <li>' . a_href_page('Pointing device sensor'          , 'vrml_implementation_pointingdevicesensor') . '</li>
        <li>' . a_href_page('Key device sensor'               , 'vrml_implementation_keydevicesensor'     ) . '</li>
        <li>' . a_href_page('Environmental sensor'            , 'vrml_implementation_environmentalsensor' ) . '</li>
        <li>' . a_href_page('Navigation'                      , 'vrml_implementation_navigation'          ) . '</li>
        <li>' . a_href_page('Environmental effects'           , 'vrml_implementation_environmentaleffects') . '</li>
        <li>' . a_href_page('H-Anim'                          , 'vrml_implementation_hanim'               ) . '</li>
        <li>' . a_href_page('NURBS'                           , 'vrml_implementation_nurbs'               ) . '</li>
        <li>' . a_href_page('Scripting'                       , 'vrml_implementation_scripting'           ) . '</li>
        <li>' . a_href_page('Event utilities'                 , 'vrml_implementation_eventutilities'      ) . '</li>
        <li>' . a_href_page('Programmable shaders'            , 'vrml_implementation_shaders'             ) . '</li>
        <li>' . a_href_page('CAD geometry'                    , 'vrml_implementation_cadgeometry'         ) . '</li>
        <li>' . a_href_page('Texturing3D'                     , 'vrml_implementation_texturing3d'         ) . '</li>
        <li>' . a_href_page('Cube map environmental texturing', 'vrml_implementation_cubemaptexturing'    ) . '</li>
      </ul>
    </li>
    <li>' . a_href_page('NIST conformace test suite', 'nist_vrml_test_suite') . '</li>
    <li>' . a_href_page('KambiScript language reference', 'kambi_script') . '</li>
    <li>' . a_href_page('Kanim (precalculated animations) file format', 'kanim_format') . '</li>
    <li>' . a_href_page('VRML / X3D time origin considered uncomfortable', 'vrml_time_origin_considered_uncomfortable') . '</li>
  </ul>
</div>';
}

?>
