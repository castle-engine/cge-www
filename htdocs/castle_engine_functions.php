<?php /* -*- mode: kambi-php -*- */

/*
   Copyright 2001-2023 Michalis Kamburelis.

   This file is part of "Castle Game Engine Website".

   "Castle Game Engine Website" is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   "Castle Game Engine Website" is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with "Castle Game Engine Website"; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

   ---------------------------------------------------------------------------
*/

/* PHP functions common for castle-engine WWW pages. */

/* The production (not development, offline) website URL. */
define('CASTLE_PROD_URL', 'https://castle-engine.io/');

/* Calculate CASTLE_ENVIRONMENT constant. */
function castle_detect_environment()
{
  /* CASTLE_ENVIRONMENT equal "offline"
     means that we will set CURRENT_URL to '' and do not connect
     to the outside world for resources (scripts, images, including analytics).
     All the links lead to remote (to CASTLE_PROD_URL/xxx.php),
     this is realized by kambi_common.php .

     Useful to generate documentation working offline
     (for PasDoc docs in CGE release).

     Note that some things cannot work correctly in this mode:
     - Stuff that can be included by WordPress will be included
       incorrectly, as WordPress base path is in any subdirectory,
       like "2017/02/12/hello-world/".
     - RSS feed generated will have relative URLs,
       which means it would not work when embedded on other servers.
     - og:image contents should be an absolute URL (Facebook warns
       about it otherwise).
  */
  // For now, the environment "offline" is never auto-detected here.

  /* CASTLE_ENVIRONMENT equal "development"
     means that we are testing the page on development server,
     not on production server. It removes *some* stuff that embeds online
     content on our webpages, see castle_engine_externals.php.
     The idea is to test our webpages through http://localhost/ easily.
  */
  if (isset($_SERVER['SERVER_NAME']) &&
      ($_SERVER['SERVER_NAME'] == '127.0.0.1' ||
       $_SERVER['SERVER_NAME'] == 'localhost')) {
    define('CASTLE_ENVIRONMENT', 'development');
    return;
  }

  define('CASTLE_ENVIRONMENT', 'production');
}

if (!defined('CASTLE_ENVIRONMENT')) {
  castle_detect_environment();
}

if (CASTLE_ENVIRONMENT != 'production') {
  /* Helpful for debugging, to see PHP warnings, notices. */
  error_reporting(E_ALL);
  ini_set('display_errors', 'stderr');
}

/* Calculate CURRENT_URL constant. */
switch (CASTLE_ENVIRONMENT) {
  case 'offline':
    define('CURRENT_URL', '');
    break;
  case 'development':
    define('CURRENT_URL', 'http://localhost:8777/');
    break;
  case 'production':
    define('CURRENT_URL', CASTLE_PROD_URL);
    break;
  default:
    throw new ErrorException('Unrecognized CASTLE_ENVIRONMENT value: ' . CASTLE_ENVIRONMENT);
}

// other constants
define('SF_UNIX_NAME', 'castle-engine'); // used only by download links.
// unused: define('MAILING_LIST_URL',    'https://lists.sourceforge.net/lists/listinfo/castle-engine-main');
define('FORUM_URL',           'https://forum.castle-engine.io/');
define('PATREON_URL',         'https://patreon.com/castleengine');

// returned by ../latest.zip
define('CGE_LATEST_DOWNLOAD', 'https://github.com/castle-engine/castle-engine/archive/snapshot.zip');

// bump this each time you change castle-engine.css, to work with CloudFlare caching (or you can purge CloudFlare cache manually)
define('CASTLE_ENGINE_CSS_VERSION', 43);

define('TWITTER_HANDLE', 'castleengine'); // https://twitter.com/castleengine/

// unused: define('CGE_LATEST_UNSTABLE_DOWNLOAD', 'https://github.com/castle-engine/castle-engine/releases/tag/snapshot');

function reference_link()
{
  global $castle_apidoc_url;
  return $castle_apidoc_url . 'index.html';
}

/* PHP file path to "CGE root"
   (where castle-engine-website-base/ is a subdirectory).
   The original PHP file that handles the request can define it at the beginning,
   and require this file (...functions.php) using:

     require_once $castle_php_path . 'castle_engine_functions.php';

   This may be used to set include path correctly.
*/
global $castle_php_path;
if (empty($castle_php_path)) {
  $castle_php_path = '';
}

/* Set path to include PHP files.
   - Because some time ago on SourceForge, requiring castle-engine-website-base/kambi_common.php
     was not reliable without this (sometimes fails, sometimes not).
     Using set_include_path to include the castle-engine-website-base/ fixed the issue.
   - Because we actually depend on it for geshi. */
global $castle_php_path;
set_include_path(get_include_path() . PATH_SEPARATOR . $castle_php_path . 'castle-engine-website-base/');
global $castle_wordpress;
if (empty($castle_disable_cge_geshi)) {
  set_include_path(get_include_path() . PATH_SEPARATOR . $castle_php_path . 'geshi/');
}

/* Michalis' email address. Using the constant makes it easier to change
   the address everywhere at once, and saves me from accidentaly
   making typos in my email. */
define('MICHALIS_EMAIL', 'michalis@castle-engine.io');

require_once 'castle-engine-website-base/kambi_common.php';
require_once 'generated_versions.php';
require_once 'castle_engine_externals.php';
require_once 'castle_engine_books.php';
if (empty($castle_disable_cge_geshi)) {
  require_once 'geshi.php';
}

define('CASTLE_GENERATE_OFFLINE',
  isset($_GET['CASTLE_GENERATE_OFFLINE']) &&
  $_GET['CASTLE_GENERATE_OFFLINE'] == 'true');

define('S_INSTALLATION_INSTRUCTIONS_SHORT',
  'No installation is required. Just download and unpack these archives wherever
  you want, and run the program inside. The documentation
  (this web page) is also included inside
  (look in the <code>documentation/</code> subdirectory) for offline viewing.');

define('FPC_CFG_DOCS', 'see <a href="http://www.freepascal.org/docs-html/user/usersu10.html">FPC documentation <i>"Configuration file"</i> to know where you can find your <code>fpc.cfg</code> file</a>');

global $castle_apidoc_url;
if (CASTLE_ENVIRONMENT == 'development') {
  $castle_apidoc_url = CASTLE_PROD_URL . 'apidoc/html/';
} else {
  $castle_apidoc_url = page_url('apidoc/html/');
}

global $site_title;
$site_title = 'Castle Game Engine';

/* Complete sitemap of our website.
   This determines a lot of navigational stuff: header menu, sidebar,
   breadcrumbs.

   An array, mapping page basename => into the information about the page
   and it's subitems.
   Page basename may also contain (after hash) an anchor inside this page.
   (Although this is not allowed for now for top-level items, but it may
   be fixed when needed.)

   Page information is another array, that contains

   - title: html safe (with character entities already escaped etc.)
     title of the page. Required.
     Used for everything (header menu (if no title-for-header-menu),
     sidebar, breadcrumbs.)

   - title-for-header-menu: optional alternative to title, used (if set) by
     header menu.

   - hint: optional tooltip ("title" over <a> in HTML, don't confuse
     with our title). Used now only over the header menu.

   - sidebar: optional. If set, and true, then this is the "root"
     of the sidebar. The algorithm used to display the sidebar looks
     for the 1st item in the path (from sitemap root) that has
     sidebar=true. If no such item, then no sidebar.
     Sidebar follows the items defined in 'sub'.

   - sub: optional array of pages under this page.

   - url: target URL (ready to be used as HTML attribute, i.e. already escaped).
     Optional, if not present we will generate proper link to internal page
     using the key as page basename.
*/
global $castle_sitemap;
$castle_sitemap = array(
  'doc/download' => array('title' => 'Download'),
  'doc/features' => array('title' => 'Features'),

  'news' => array('title' => 'News',
    'url' => page_url('wp/'),
    /* The 'news' menu is a parent menu for
       - old news items, generated by old_news.php,
         that adds also 'sub' and 'sidebar' of this.
         The 'sub' is only calculated by old_news.php,
         since it may be time-consuming (requires reading large $news table),
         and is not needed on other pages.
       - new news, implemented by going to Wordpress.
         Clicking on "News" in main menu should go there.
    */
  ),

  'documentation' => array('title' => 'Documentation',
    /* Dropdown contents are determined by "sub" below.
       In the past we allowed to specify "dropdown" contents explicitly,
       but it was weird for users, e.g.https://castle-engine.io/documentation.php
       really should show the same ToC as the menu dropdown. */
    'dropdown' => true,
    'sidebar' => true,
    'sub' => array(
      'doc/bad_chess' => array('title' => 'Recommended Tutorial: 3D Physics Fun'),
      'manual_intro' => array('title' => 'Manual',
        'sub' => array(
          'doc/install' => array('title' => 'Install'),
          'doc/build_first' => array('title' => 'Build your first application'),
          'doc/view_events' => array('title' => 'Designing user interface and handling events (press, update) within the view',
            'sub' => array(
              'doc/views' => array('title' => 'Managing Views'),
            )
          ),
          'doc/viewport_and_scenes' => array('title' => 'Viewport with scenes, camera, navigation',
            'sub' => array(
              'doc/viewport_3d' => array('title' => 'Tutorial: Designing a 3D world'),
              'doc/viewport_2d' => array('title' => 'Tutorial: Designing a 2D world'),
              'doc/camera' => array('title' => 'Camera'),
              'doc/navigation' => array('title' => 'Navigation'),
              'doc/viewport_and_scenes_from_code' => array('title' => 'Writing code to modify scenes and transformations'),
              'doc/behaviors' => array('title' => 'Behaviors'),
              'doc/multiple_viewports_to_display_one_world' => array('title' => 'Multiple viewports to display one world'),
            ),
          ),
          'doc/user_interface' => array('title' => 'User interface',
            'sub' => array(
              'manual_text' => array('title' => 'Text and fonts'),
              'doc/loading_image' => array('title' => 'Customize loading image'),
              'manual_2d_ui_custom_drawn' => array('title' => 'Custom drawn 2D controls: player HUD'),
            )
          ),
          'doc/editor' => array('title' => 'Editor',
            'sub' => array(
              'doc/custom_components' => array('title' => 'Custom Components in Editor'),
              'doc/reuse_design' => array('title' => 'Components to reuse a design in other designs'), // TCastleDesign and TCastleTransformDesign -- too long names would make horizontal scroll on mobile
            )
          ),
          'manual_2d_games' => array('title' => '2D games',
            'sub' => array(
              'doc/sprite_sheets' => array('title' => 'Sprite sheets'),
              'doc/using_images' => array('title' => 'Images'),
              'doc/tiled_maps' => array('title' => 'Tiled maps'),
              'doc/how_to_render_2d' => array('title' => 'How to render 2D games with images and sprites'),
            )
          ),
          'doc/physics' => array('title' => 'Physics'),
          'manual_sound' => array('title' => 'Sound',
            'sub' => array(
              'doc/openal' => array('title' => 'OpenAL'),
              'doc/fmod' => array('title' => 'FMOD'),
            ),
          ),
          'manual_user_prefs' => array('title' => 'Persistent data (user preferences, savegames)'),
          'doc/save_screen' => array('title' => 'Save screen (screenshot)'),
          'manual_network' => array('title' => 'Network, downloading and using URLs'),
          'doc/log' => array('title' => 'Logging'),
          'manual_cross_platform' => array('title' => 'Cross-platform (desktop, mobile, consoles...) projects',
            'sub' => array(
              'manual_data_directory' => array('title' => 'Data directory'),
              'doc/project_manifest' => array('title' => 'CastleEngineManifest&ZeroWidthSpace;.xml'), // using zero-width space to avoid making mobile version with horizontal scroll
              'manual_castle_settings' => array('title' => 'Customize UI scaling, font, warmup cache by CastleSettings.xml'),
              'doc/build_tool' => array('title' => 'Build Tool'),
              'doc/touch_input' => array('title' => 'Touch Input'),
            ),
          ),
          'manual_platforms' => array('title' => 'Platforms details',
            'sub' => array(
              'doc/android' => array('title' => 'Android'),
              'doc/android_faq' => array('title' => 'Android FAQ'),
              'doc/android_services' => array('title' => 'Android Services (Google Play Games and many more...)'),
              'doc/adding_new_android_services' => array('title' => 'Adding New Android Services'),
              'doc/ios' => array('title' => 'iOS'),
              'doc/ios_services' => array('title' => 'iOS Services (Apple Game Center and many more...)'),
              'doc/in_app_purchases' => array('title' => 'In-App Purchases (Android, iOS)'),
              'doc/nintendo_switch' => array('title' => 'Nintendo Switch'),
              'doc/macos' => array('title' => 'macOS'),
            ),
          ),
          'manual_optimization' => array('title' => 'Optimization',
            'sub' => array(
              'doc/occlusion_culling' => array('title' => 'Occlusion Culling'),
              'doc/profiling_using_valgrind' => array('title' => 'Profiling Using Valgrind'),
              'doc/memory_leaks' => array('title' => 'Detecting Memory Leaks'),
            )
          ),
          'doc/how_to_make_rendering_prettier' => array('title' => 'Making rendering prettier',
            'sub' => array(
              'doc/bump_mapping' => array('title' => 'Bump Mapping (Normal Maps)'),
              'doc/shadow_volumes' => array('title' => 'Shadow Volumes'),
              'doc/background' => array('title' => 'Background (skybox, sky and ground)'),
              'doc/fog' => array('title' => 'Fog'),
              'doc/blending' => array('title' => 'Blending'),
              'doc/color_space' => array('title' => 'Color Space (Gamma Correction)'),
              'manual_alpha_bleeding' => array('title' => 'Alpha Bleeding'),
              'doc/sketchfab' => array('title' => 'Sketchfab'),
            )
          ),
          'doc/control_on_form' => array('title' => 'Engine on a form (VCL, FMX, LCL) using TCastleControl'),
          'manual_automatic_builds' => array('title' => 'Automatic Builds (Continuous Integration and Delivery)',
            'sub' => array(
              'doc/github_actions' => array('title' => 'GitHub Actions (automatic builds for GitHub projects)'),
              'doc/gitlab_ci' => array('title' => 'GitLab CI/CD (automatic builds for GitLab projects)'),
              'doc/docker' => array('title' => 'Docker (easily get CGE, compilers, texture compression tools)'),
              'doc/jenkins' => array('title' => 'Jenkins (automatic builds by your Jenkins server)'),
            )
          ),
          'manual_ide' => array('title' => 'Pascal IDEs',
            'sub' => array(
              'doc/lazarus' => array('title' => 'Lazarus'),
              'doc/delphi' => array('title' => 'Delphi'),
              'doc/vscode' => array('title' => 'Visual Studio Code'),
            ),
          ),
          'manual_miscellaneous' => array('title' => 'Miscellaneous',
            'sub' => array(
              'manual_up' => array('title' => 'Which way is up?'),
              'manual_transformation_hierarchy' => array('title' => 'Transformation hierarchy'),
              'doc/castlewindow_backends' => array('title' => 'CastleWindow Backends'),
              'doc/threads' => array('title' => 'Threads'),
              'compiling_from_source' => array('title' => 'Compiling from source'),
              'supported_compilers' => array('title' => 'Supported compilers and IDEs'),
              'doc/coding_traps' => array('title' => 'Coding Traps'),
              'doc/units_map' => array('title' => 'Units Map'),
              // 'engine_doc' => array('title' => 'Internals documentation'), // too outdated
              'doc/dedicated_gpu' => array('title' => 'Dedicated GPU'),
              'doc/fpcupdeluxe' => array('title' => 'fpcupdeluxe'),
              'doc/fpmake' => array('title' => 'FpMake and FpPkg'),
              'doc/license' => array('title' => 'License'),
            )
          ),
          'helping' => array('title' => 'Helping in engine development',
            'sub' => array(
              'donate_other' => array('title' => 'Donate'),
              'doc/roadmap' => array('title' => 'Roadmap'),
              'doc/coding_conventions' => array('title' => 'Coding Conventions'),
            )
          ),
          'manual_high_level_3d_classes' => array('title' => 'Deprecated: Utilities for typical 3D games',
            'sub' => array(
              'manual_3d_utlities_overview' => array('title' => 'Overview'),
              'manual_game_level' => array('title' => 'Loading game level'),
              'manual_player' => array('title' => 'Player'),
              'manual_resources' => array('title' => 'Defining creatures and items'),
              'manual_resources_using_existing' => array('title' => 'Using creatures and items'),
              'manual_resources_extending' => array('title' => 'Extending creatures and items classes'),
            ),
          ),
        ),
      ),
      'creating_data_intro' => array('title' => 'Creating Game Data',
        'sub' => array(
          'creating_data_export' => array('title' => 'Exporting 3D and 2D models',
            'sub' => array(
              'creating_data_model_formats' => array('title' => 'Supported model formats',
                'sub' => array(
                  'doc/gltf' => array('title' => 'glTF'),
                  'doc/md3' => array('title' => 'MD3'),
                ),
              ),
              'doc/blender' => array('title' => 'Blender'),
              'creating_data_3dsmax' => array('title' => '3ds Max'),
              'creating_data_maya' => array('title' => 'Maya'),
              'doc/spine' => array('title' => 'Spine'),
              // Dragon Bones seems not maintained anymore, with broken website and even downloads.
              //'creating_data_dragon_bones' => array('title' => 'Dragon Bones'),
            )
          ),
          'creating_data_auto_generated_textures' => array('title' => 'Auto-generated compressed and scaled textures'),
          'creating_data_xml' => array('title' => 'Deprecated: XML files describing game data for typical 3D games',
            'sub' => array(
              'creating_data_levels' => array('title' => 'Levels'),
              'creating_data_resources' => array('title' => 'Resources (creatures and items)'),
              'creating_data_player' => array('title' => 'Player configuration'),
            )
          ),
          'creating_data_sound' => array('title' => 'Deprecated: Sounds XML description'),
        )
      ),
      'reference' => array('title' => 'API Reference', 'url' => reference_link()),
      'doc/why_pascal' => array('title' => 'Why Pascal?'),
      'doc/modern_pascal' => array('title' => 'Modern Object Pascal Introduction',
        'sub' => array(
          //'doc/modern_pascal' => array('title' => 'English (HTML)'),
          'modern_pascal_en_pdf' => array('title' => 'PDF', 'url' => CASTLE_PROD_URL . 'modern_pascal_introduction.pdf'),

          'doc/modern_pascal_translations' => array(
            'title' => 'Translations',
            'sub' => array(
              'modern_pascal_bg_html' => array('title' => 'Bulgarian', 'url' => CASTLE_PROD_URL . 'modern_pascal_introduction_bg.html'),
              //TODO:'modern_pascal_bg_pdf' => array('title' => 'Bulgarian (PDF)', 'url' => CASTLE_PROD_URL . 'modern_pascal_introduction_bg.pdf'),

              'modern_pascal_chinese' => array('title' => 'Chinese (WIP)', 'url' => CASTLE_PROD_URL . 'modern_pascal_introduction_chinese.pdf'),

              'modern_pascal_russian_html' => array('title' => 'Russian', 'url' => CASTLE_PROD_URL . 'modern_pascal_introduction_russian.html'),
              //TODO:'modern_pascal_russian_pdf' => array('title' => 'Russian (PDF)', 'url' => CASTLE_PROD_URL . 'modern_pascal_introduction_russian.pdf'),

              'doc/modern_pascal_spanish' => array('title' => 'Spanish'),

              'modern_pascal_ukrainian_html' => array('title' => 'Ukrainian (WIP)', 'url' => CASTLE_PROD_URL . 'modern_pascal_introduction_ukrainian.html'),
              //TODO:'modern_pascal_ukrainian_pdf' => array('title' => 'Ukrainian (WIP) (PDF)', 'url' => CASTLE_PROD_URL . 'modern_pascal_introduction_ukrainian.pdf'),
            )
          ),

          'doc/learn_pascal' => array('title' => 'More Resources to Learn Pascal'),
        )
      ),
      'doc/castle_game_engine_for_unity_developers' => array('title' => 'Overview for Unity Developers'),

      'vrml_x3d' => array('title' => 'Scene Graph (X3D)',
        'sub' => array(
          'demo_models' => array('title' => 'Demo models'),
          'x3d_implementation_status' => array('title' => 'Standard X3D Nodes',
            'sub' => array(
              'x3d_implementation_core'                 => array('title' => 'Core'                            ),
              'x3d_implementation_time'                 => array('title' => 'Time',
                'sub' => array(
                  'x3d_implementation_time_extensions' => array('title' => 'Extensions'),
                ),
              ),
              'x3d_implementation_networking'           => array('title' => 'Networking'                      ),
              'x3d_implementation_grouping'             => array('title' => 'Grouping'                        ),
              'x3d_implementation_rendering'            => array('title' => 'Rendering',
                'sub' => array(
                  'x3d_implementation_rendering_extensions' => array('title' => 'Extensions'),
                ),
              ),
              'x3d_implementation_shape'                => array('title' => 'Shape',
                'sub' => array(
                  'x3d_implementation_shape_extensions' => array('title' => 'Extensions'),
                ),
              ),
              'x3d_implementation_geometry3d'           => array('title' => 'Geometry3D',
                'sub' => array(
                  'x3d_implementation_geometry3d_extensions' => array('title' => 'Extensions'),
                ),
              ),
              'x3d_implementation_geometry2d'           => array('title' => 'Geometry2D'                      ),
              'x3d_implementation_text'                 => array('title' => 'Text',
                'sub' => array(
                  'x3d_implementation_text_extensions' => array('title' => 'Extensions'),
                ),
              ),
              'x3d_implementation_sound'                => array('title' => 'Sound'                           ),
              'x3d_implementation_lighting'             => array('title' => 'Lighting'                        ),
              'x3d_implementation_texturing'            => array('title' => 'Texturing',
                'sub' => array(
                  'x3d_implementation_texturing_extensions' => array('title' => 'Extensions'),
                  'x3d_multi_texturing' => array('title' => 'X3D MultiTexturing problems and proposed solutions'),
                ),
              ),
              'x3d_implementation_interpolation'        => array('title' => 'Interpolation ("how to animate things")',
                'sub' => array(
                  'x3d_implementation_interpolation_extensions' => array('title' => 'Extensions'),
                ),
              ),
              'x3d_implementation_pointingdevicesensor' => array('title' => 'Pointing device sensor'          ),
              'x3d_implementation_keydevicesensor'      => array('title' => 'Key device sensor'               ),
              'x3d_implementation_environmentalsensor'  => array('title' => 'Environmental sensor'            ),
              'x3d_implementation_navigation'           => array('title' => 'Navigation',
                'sub' => array(
                  'x3d_implementation_navigation_extensions' => array('title' => 'Extensions'),
                ),
              ),
              'x3d_implementation_environmentaleffects' => array('title' => 'Environmental effects',
                'sub' => array(
                  'x3d_implementation_environmentaleffects_extensions' => array('title' => 'Extensions'),
                ),
              ),
              'x3d_implementation_hanim'                => array('title' => 'H-Anim'                          ),
              'x3d_implementation_nurbs'                => array('title' => 'NURBS',
                'sub' => array(
                  'x3d_implementation_nurbs#section_homogeneous_coordinates' => array('title' => 'Control points are in homogeneous coordinates'),
                ),
              ),
              'x3d_implementation_scripting'            => array('title' => 'Scripting'                       ),
              'x3d_implementation_eventutilities'       => array('title' => 'Event utilities',
                'sub' => array(
                  'x3d_implementation_eventutilities_extensions' => array('title' => 'Extensions'),
                ),
              ),
              'x3d_implementation_shaders'              => array('title' => 'Programmable shaders'            ),
              'x3d_implementation_cadgeometry'          => array('title' => 'CAD geometry'                    ),
              'x3d_implementation_texturing3d'          => array('title' => 'Texturing3D'                     ),
              'x3d_implementation_cubemaptexturing'     => array('title' => 'Cube map environmental texturing'),
            )
          ),
          'x3d_larger_extensions' => array('title' => 'Larger X3D Extensions',
            'sub' => array(
              'compositing_shaders' => array('title' => 'Shader Effects (Compositing Shaders)'),
              'x3d_extensions_mirror_plane' => array('title' => 'Mirrors on flat objects'),
              'x3d_extensions_screen_effects' => array('title' => 'Screen (Post-Processing) Effects'),
              'x3d_extensions_shadow_maps' => array('title' => 'Shadow Maps'),
              'x3d_extensions_shadow_volumes' => array('title' => 'Shadow Volumes'),
              'x3d_extensions_vrml1' => array('title' => '(Old) VRML 1.0'),
            ),
          ),
          'x3d_extensions' => array('title' => 'Complete list of X3D Extensions'),
          'castle_script' => array('title' => 'CastleScript language reference'),
          'castle_animation_frames' => array('title' => 'Castle Animation Frames (castle-anim-frames) file format'),
          'x3d_time_origin_considered_uncomfortable' => array('title' => 'VRML / X3D time origin considered uncomfortable'),
          'nist_vrml_test_suite' => array('title' => 'NIST conformace test suite'),
        ),
      ),

      'doc/conferences' => array('title' => 'Conferences',
        'sub' => array(
          'gic2022' => array('title' => 'Slides from GIC 2022', 'url' => 'https://castle-engine.io/gic2022'),
          'ipc2023' => array('title' => 'Slides from IPC 2023', 'url' => 'https://castle-engine.io/ipc2023'),
        )
      ),
    ),
  ),

  'videos' => array('title' => 'Videos', 'url' => 'https://www.youtube.com/c/CastleGameEngine'),

  'view3dscene'  => array('hint' => 'Viewer for glTF, X3D, Collada, sprite sheets and other model formats', 'title' => 'Viewer for glTF, X3D...'),

  'talk' => array('hint' => 'Ask for help, report bugs, discuss features', 'title' => 'Forum, Discord...',
    'sub' => array(
      'privacy_policy' => array('title' => 'Privacy Policy'),
    ),
  ),

  // Do not show, for now we focus on Patreon funding.
  // 'donate' => array('title' => 'Donate'),

  'gallery' => array('title' => 'Gallery',
    'dropdown' => true,
    'sub' => array(
      'gallery_games' => array('title' => 'Games',
        'sub' => array(
          'mountains_of_fire' => array('title' => 'Mountains Of Fire'),
          'darkest_before_dawn' => array('title' => 'Darkest Before the Dawn'),
          'castle'                 => array('hint' => 'First-person perspective game, in a dark fantasy setting'   , 'title' => 'The Castle',
            'sidebar' => true,
            'sub' => array(
              'castle-advanced'    => array('title' => 'Additional notes (troubleshooting)'),
              'castle-credits'     => array('title' => 'Credits'),
            ),
          ),
          'malfunction' => array('title' => 'malfunction'),
          'kambi_lines' => array('title' => 'kambi_lines'),
        ),
      ),
      'gallery_tools' => array('title' => 'Tools',
        'sub' => array(
          'convert' => array('title' => 'Convert glTF, OBJ, STL, Collada, 3DS (and other 3D and 2D model formats) to X3D'),
          'convert-output' => array('title' => 'Conversion output'),
          'castle-view-image' => array('title' => 'castle-view-image'),
          'glplotter' => array('title' => 'glplotter'),
          'rayhunter' => array('title' => 'rayhunter',
            'sub' => array(
              'raytr_gallery' => array('title' => 'Small gallery of images rendered using rayhunter'),
            ),
          ),
          'kambi_mgf2inv' => array('title' => 'kambi_mgf2inv'),
          'all_programs_sources' => array('title' => 'All Programs Sources'),
        ),
      ),
      'additional_components' => array(
        'title' => 'Additional Components',
        'hint' => 'Additional components (Pascal code) on top of CGE, that you can use in your games',
      ),
      'assets' => array(
        'title' => 'Assets (3D and 2D Graphics, Sound)',
        'hint' => 'Various assets (graphics, sound) you can use in your games'
      ),

      /* We keep these pages here, to keep them working,
         but honestly they are old and we don't know where to link them from. */
      'common_options' => array('title' => 'Standard command-line options', 'hidden_in_toc' => true),
      'opengl_options' => array('title' => 'Standard command-line options for OpenGL programs', 'hidden_in_toc' => true),
      'versioning' => array('title' => 'Versioning scheme of programs', 'hidden_in_toc' => true),
    ),
  ),
  'doc/credits' => array(
    'title' => '<span class="glyphicon glyphicon-heart" aria-hidden="true"></span>',
    /* Showing this makes the link better for users
       and also makes Google Lighthouse report not complain about it. */
    'hint' => 'Credits'
  )
);

function _castle_bootstrap()
{
  kambi_bootstrap();
}

/* Call this immediately, to modify $castle_sitemap even before calling
   castle_header. */
_castle_bootstrap();

/* Internal for _castle_sidebar* usage.

   Return a formatted link to given page.
   $page is the page basename (like for a_href_page), or a basename(hash)anchor.
   $pageinfo must contain 'title', may contain 'url'.

   Looks at global $page_basename, to avoid turning current page name
   into a link. */
function _castle_sidebar_link($page, $pageinfo)
{
  $pagelink = explode('#', $page);
  if (count($pagelink) == 1)
  {
    global $page_basename;
    /* If it's the current page, don't make a link to it */
    if ($pagelink[0] == $page_basename)
      return $pageinfo['title']; else
    if (isset($pageinfo['url']))
      return '<a href="' . $pageinfo['url'] .'">' . $pageinfo['title'] . '</a>'; else
      return a_href_page($pageinfo['title'], $pagelink[0]);
  } else
  if (count($pagelink) == 2)
    return a_href_page_hashlink($pageinfo['title'], $pagelink[0], $pagelink[1]); else
    return '<b>Invalid sidebar link ' . htmlspecialchars($pagelink) . '</b>';
}

/* Internal for _castle_sidebar* usage.
   Return a <ul> listing items on $sub. */
function _castle_sidebar_menu($sub, $nested = FALSE)
{
  /* Only nested lists use list-no-margin.
     Testcase: ttps://castle-engine.io/creating_data_intro.php. */
  $classes = $nested ? 'list-no-margin' : '';

  $result = '<ol class="' . $classes . '">';
  foreach($sub as $page => $pageinfo)
  {
    if (isset($pageinfo['hidden_in_toc']) && $pageinfo['hidden_in_toc']) {
      continue;
    }

    $result .= '<li>' . _castle_sidebar_link($page, $pageinfo);
    if (isset($pageinfo['sub']))
      $result .= _castle_sidebar_menu($pageinfo['sub'], TRUE);
    $result .= '</li>';
  }
  $result .= '</ol>';
  return $result;
}

/* Return a rendered HTML sidebar. */
function _castle_sidebar($page, $pageinfo)
{
  $result = '
  <div class="sidebar">
    <div class="sidebar_title">' . _castle_sidebar_link($page, $pageinfo) . '</div>';

  if (isset($pageinfo['sub']))
    $result .= _castle_sidebar_menu($pageinfo['sub']);

/*
  $result .= '
    <div style="text-align: center; width: 200px; margin-right: auto; margin-left: auto; padding: 0.1em; border: medium outset #FFF;">
    <p><a href="https://castle-engine.io/donate.php">Donate</a></p>
    <p>' . flattr_button(false) . '</p>
    <p>' . paypal_button() . '</p>
    </div>';
*/

  $result .= '</div>';

  return $result;
}

function _castle_header_menu($current_page)
{
  global $castle_sitemap;

  $result = '
    <!-- Uncomment this for toggable navbar -->
    <!--div class="collapse navbar-collapse" id="main-navbar-collapse-1" -->
    <ul class="nav nav-tabs">';

  foreach($castle_sitemap as $menu_item_page => $menu_item)
  {
    // output <li ...>
    $result .= '<li class="';
    if ($menu_item_page == $current_page) {
      $result .= ' active';
    }
    if (!empty($menu_item['dropdown'])) {
      $result .= ' dropdown';
    }
    $result .= '">';

    // output <a href="..."
    $result .= '<a href="';
    if (!empty($menu_item['dropdown'])) {
      $result .= '#';
    } else
    if (isset($menu_item['url'])) {
      $result .= $menu_item['url'];
    } else {
      $result .= page_url($menu_item_page);
    }
    $result .= '"';

    // output optional <a> dropdown attributes
    if (!empty($menu_item['dropdown'])) {
      $result .= ' class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false"';
    }

    // output title="..."
    if (isset($menu_item['hint'])) {
      $result .= ' title="' . $menu_item['hint'] . '"';
    }

    // output <a> content
    $result .= '>';
    if (isset($menu_item['title-for-header-menu'])) {
      $result .= $menu_item['title-for-header-menu'];
    } else {
      $result .= $menu_item['title'];
    }
    if (!empty($menu_item['dropdown'])) {
      $result .= ' <span class="caret"></span>';
    }
    $result .=  '</a>';

    // output dropdown contents
    if (!empty($menu_item['dropdown'])) {
      if (!isset($menu_item['sub'])) {
        throw new ErrorException('"dropdown" implies we need also "sub" for menu item ' . $menu_item_page);
      }
      $result .= '<ul class="dropdown-menu">';
      foreach ($menu_item['sub'] as $dropdown_item_page => $dropdown_item) {
        // do not show in dropdown items marked hidden_in_toc
        if (!empty($dropdown_item['hidden_in_toc'])) {
          continue;
        }
        $result .= '<li>';

        // output <a href="..."
        $result .= '<a href="';
        if (isset($dropdown_item['url'])) {
          $result .= $dropdown_item['url'];
        } else {
          $result .= page_url($dropdown_item_page);
        }
        $result .= '"';

        // output <a> content
        $result .= '>';
        if (isset($dropdown_item['title-for-header-menu'])) {
          $result .= $dropdown_item['title-for-header-menu'];
        } else {
          $result .= $dropdown_item['title'];
        }
        $result .=  '</a>';

        $result .= '</li>';
      }
      $result .= '</ul>';
    }

    // finish </li>
    $result .= '</li>';
  }
  unset($menu_item);
  unset($menu_item_page);

  $result .= '
    </ul>
    <!-- Uncomment this for toggable navbar -->
    <!--/div-->';

  return $result;
}

function _castle_breadcrumbs($path)
{
  $result = '';

  /* There is no point in showing breadcrumbs containing only "Home"
     (because home link is visible anyway on the header).
     Also, no point in showing breadcrumbs if we're at the 1st page
     in header menu, as then header menu tab indicates the page.
     Also, no point when we're inside a page directly under header menu
     (because then header menu tab + page title shows where we are). */
  if (count($path) > 2)
  {
    global $castle_sitemap;

    $result = '<div class="header_breadcrumbs"><a href="/">Home</a>';

    $path_item_num = 0;
    $path_item = '';
    $path_itemsub = $castle_sitemap;

    while ($path_item_num < count($path) - 1)
    {
      $path_item = $path[$path_item_num];

      if (!isset($path_itemsub[$path_item])) {
        //throw new ErrorException('No page named ' . $path_item . ' at current level of castle_sitemap');
        // return no breadcrumbs for pages outsite of castle_sitemap
        return '';
      }

      $path_itemtitle = $path_itemsub[$path_item]['title'];
      if (isset($path_itemsub[$path_item]['sub']))
        $path_itemsub = $path_itemsub[$path_item]['sub']; else
        $path_itemsub = NULL;

      $result .= ' &#187; ' . a_href_page($path_itemtitle, $path_item);

      $path_item_num ++;
    }

    $result .= '</div>';
  }

  return $result;
}

function _castle_find_in_sitemap($path)
{
  global $castle_sitemap;

  $path_itemsub = $castle_sitemap;
  $result = NULL;

  foreach ($path as $path_item) {
    if (!isset($path_itemsub[$path_item])) {
      //throw new ErrorException('No page named ' . $path_item . ' at current level of castle_sitemap');
      // behave as if the page is at top-level, for pages outsite of castle_sitemap
      return NULL;
    }
    $result = $path_itemsub[$path_item];

    if (isset($result['sub'])) {
      $path_itemsub = $result['sub'];
    } else {
      $path_itemsub = NULL;
    }
  }

  return $result;
}

function castle_toc_from_sitemap()
{
  global $castle_page_path;
  $page_map = _castle_find_in_sitemap($castle_page_path);
  if (!isset($page_map['sub'])) {
    throw new ErrorException('Requested castle_toc_from_sitemap for page that does not have "subitems" according to castle_sitemap');
  }

  $result = '<ol>';
  foreach ($page_map['sub'] as $menu_item_page => $menu_item)
  {
    if (isset($menu_item['hidden_in_toc']) && $menu_item['hidden_in_toc']) {
      continue;
    }

    if (isset($menu_item['url'])) {
      $menu_item_url = $menu_item['url'];
    } else {
      $menu_item_url = page_url($menu_item_page);
    }

    $result .=  '<li><a href="' . $menu_item_url . '">' .
      $menu_item['title'] . '</a></li>';
  }
  $result .= '</ol>';
  return $result;
}

function castle_geshi_header()
{
  global $castle_disable_cge_geshi;
  if (!empty($castle_disable_cge_geshi)) {
    return;
  }

  global $geshi;
  $geshi = new GeSHi();
  $geshi->enable_classes();
  //$geshi->set_overall_class('sourcecode'); // not needed anymore

  echo '<style>' . "\n";

  /* looks like we need to set_language before get_stylesheet,
     otherwise not everything necessary is output. */
  $geshi->set_language('delphi');
  echo $geshi->get_stylesheet(false);
  $geshi->set_language('VRML / X3D');
  echo $geshi->get_stylesheet(false);
  $geshi->set_language('C');
  echo $geshi->get_stylesheet(false);
  $geshi->set_language('XML');
  echo $geshi->get_stylesheet(false);

  echo '</style>' . "\n";
}

function echo_header_bonus ()
{
  ?>
  <link rel="alternate" type="application/rss+xml"
    title="Castle Game Engine - News Feed"
    href="<?php echo page_url('news_feed'); ?>">

  <!--
    Add Inter font, https://developers.google.com/fonts/docs/getting_started .
    Add &display=swap to render text ASAP, see https://web.dev/font-display/?utm_source=lighthouse&utm_medium=lr
  -->
  <!--link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Inter&display=swap"-->

  <link type="text/css" rel="stylesheet" media="all" href="<?php echo page_requisite('castle-engine-website-base/castle-engine.css?version=' . CASTLE_ENGINE_CSS_VERSION); ?>">
  <link type="text/css" rel="stylesheet" media="all" href="<?php echo page_requisite('castle-engine-website-base/castle-asciidoctor.css?version=' . CASTLE_ENGINE_CSS_VERSION); ?>">
  <?php

  if (defined('CASTLE_ENGINE_CUSTOM_CSS')) {
    ?>
    <link type="text/css" rel="stylesheet" media="all" href="<?php echo page_requisite(CASTLE_ENGINE_CUSTOM_CSS); ?>">
    <?php
  }

  castle_geshi_header();

  /* echo flattr_header(); - Flattr not used now */

  /* Add icons, using same HTML code as Wordpress */
  global $castle_wordpress;
  if (empty($castle_wordpress)) {
      ?>
      <link rel="icon" href="<?php echo page_requisite('images/castle_game_engine_icon_fit_in_square.png'); ?>" sizes="256x256">
      <link rel="apple-touch-icon-precomposed" href="<?php echo page_requisite('images/castle_game_engine_icon_fit_in_square.png'); ?>">
      <meta name="msapplication-TileImage" content="<?php echo page_requisite('images/castle_game_engine_icon_fit_in_square.png'); ?>">
      <?php
  }

  /* Add icon, following Google structured data recommendation.
     https://developers.google.com/search/docs/advanced/structured-data/logo
  */
  ?>
  <script type="application/ld+json">
  {
    "@context": "https://schema.org",
    "@type": "Organization",
    "url": "<?php echo CASTLE_PROD_URL; ?>",
    "logo": "<?php echo CASTLE_PROD_URL; ?>images/castle_game_engine_icon.png"
  }
  </script>
  <?php
}

/* Echo a header.
   Set various global variables about this page.

   $parameters fields:
   - 'meta_description' (like for common_header)
   - 'path' (array of strings;
     a list of page names, a path in the tree of $castle_sitemap,
     to the current page.
     The $page_basename is added at the end, if not already there.
     It will be set as global $castle_page_path.)
   - 'social_share_image' (string,
     name of image to be used as Facebook share image (og:image);
     relative to in images/original_size/ , unless it's an absolute URL,
     with a protocol like http[s] etc.)
   - 'subheading_text' (string,
     will be shown in page content, within pretty_heading.
     Only relevant when page is part of some book.)
*/
function castle_header($a_page_title, array $parameters = array())
{
  // calculate global $castle_current_book
  global $castle_current_book;
  global $castle_books;
  global $page_basename;
  $castle_current_book = detect_current_book($page_basename);

  // call castle_book_calculate_pages to determine previous/next links for current book
  if ($castle_current_book !== NULL) {
    castle_book_calculate_pages($castle_current_book);
  }

  // change title, in case we're part of book
  $a_page_title_without_book = $a_page_title;
  if ($castle_current_book !== NULL) {
    $a_page_title = $a_page_title . ' | ' . $castle_books[$castle_current_book]['title'];
  }

  /* call common_header with proper params */
  $common_header_parameters = array();
  if (array_key_exists('meta_description', $parameters)) {
    $common_header_parameters['meta_description'] = $parameters['meta_description'];
  }
  if (array_key_exists('meta_keywords', $parameters)) {
    $common_header_parameters['meta_keywords'] = $parameters['meta_keywords'];
  }
  if (array_key_exists('canonical_url', $parameters)) {
    $common_header_parameters['canonical_url'] = $parameters['canonical_url'];
  } else {
    // Autocalcualate canonical_url based on $this_page_name, if none provided.
    global $this_page_name;
    $common_header_parameters['canonical_url'] = CASTLE_PROD_URL . $this_page_name;
  }
  if (array_key_exists('social_share_image', $parameters)) {
    if (kambi_url_absolute($parameters['social_share_image'])) {
      $social_share_image_url = $parameters['social_share_image'];
    } else {
      $social_share_image_url = page_url('images/original_size/' . $parameters['social_share_image']);
    }
    $common_header_parameters['social_share_image'] = $social_share_image_url;
  }
  if (array_key_exists('publish_date', $parameters)) {
    $common_header_parameters['publish_date'] = $parameters['publish_date'];
  }
  common_header($a_page_title, $common_header_parameters);

  $path = array();
  if (isset($parameters['path'])) {
    $path = $parameters['path'];
  }
  echo_castle_header_suffix($path);

  // output extra header HTML, in case we're part of book
  if ($castle_current_book != NULL) {
    echo book_bar($castle_current_book);
    $subheading_text = isset($parameters['subheading_text']) ? $parameters['subheading_text'] : '';
    echo pretty_heading($a_page_title_without_book, NULL, $subheading_text);
  }
}

/* Helper for _detect_page_path. Returns null if not found. */
function _detect_page_path_core($page_name, $list)
{
  foreach ($list as $list_pagename => $list_pageinfo) {
    if ($list_pagename == $page_name) {
      return array($page_name);
    }
    if (isset($list_pageinfo['sub'])) {
      $result = _detect_page_path_core($page_name, $list_pageinfo['sub']);
      if ($result !== NULL) {
        array_unshift($result, $list_pagename);
        return $result;
      }
    }
  }
  return NULL;
}

/* Find given page name within $castle_sitemap,
   return a path (list of strings) from root to the page.
   If not found, pretends this page is part of root (this makes it easier to add new pages,
   you don't need to put everything into sitemap).
   Never returns an empty list. */
function _detect_page_path($page_name)
{
  global $castle_sitemap;
  $result = _detect_page_path_core($page_name, $castle_sitemap);
  if ($result === NULL) {
    return array($page_name);
    //throw new ErrorException('Page named ' . $page_name . ' not found anywhere in the castle_sitemap');
  }
  return $result;
}

/* Return HTML with width="xxx" height="yyy" of the image, if known. */
function _castle_image_sizes($relative_filename)
{
  global $castle_image_sizes;

  /* Load castle_image_sizes.php only when necessary.
     This way, on production server, when rendering AsciiDoc,
     we never actually load castle_image_sizes.php. */
  if (CASTLE_ENVIRONMENT == 'production') {
    require_once 'castle_image_sizes.php';
  } else {
    // the existence of castle_image_sizes is optional on non-production
    @include_once 'castle_image_sizes.php';
  }

  if (isset($castle_image_sizes) && array_key_exists($relative_filename, $castle_image_sizes)) {
    $img_sizes = $castle_image_sizes[$relative_filename];
    return ' width="' . $img_sizes['width'] . '" height="' . $img_sizes['height'] . '" ';
  } else {
    return '';
  }
}

function _castle_patreon_box()
{
  $patreon_json = @file_get_contents(__DIR__ . '/../patreon/patreon.json');
  /* Check _castle_disable_externals() to avoid adding Patreon data
     to static HTML pieces in castle-engine/doc/pasdoc/html-parts/ .
     These static HTML pieces then go to PasDoc output,
     compositing_shaders_doc, vrml_engine_internals. */
  if (_castle_disable_externals() || $patreon_json === false) {
    $patreon_desc = NULL; // 'Data Not Available';
    $patreon_percent = 0;
  } else {
    $patreon = json_decode($patreon_json);
    $patreon_desc = $patreon->completed_percentage . '%&nbsp;of&nbsp;$' . $patreon->amount_cents/100;
    $patreon_percent = $patreon->completed_percentage;
  }

  $result = '
    <a href="' . PATREON_URL . '" class="navbar-link"
      style="display: inline-block; background-color: #FF424D; border-radius: 0; text-align: center;">
      <div style="color: #ffffff; padding-left: 1em; padding-right: 1em;">
        <span style="vertical-align: middle;">Support&nbsp;us&nbsp;on</span>&nbsp;<img src="' .
          page_requisite('images/patreon-brand/Digital-Patreon-Wordmark_White.webp') .
          '" ' .
          // _castle_image_sizes('images/patreon-brand/wordmark/small/Digital-Patreon-Wordmark_White.webp')
          ' width="80" height="16" ' // hardcode here, to avoid even loading castle_image_sizes.php in some cases
          . '
          alt="Patreon Wordmark"
          style="display: inline-block; vertical-align: middle;"
        >
      </div>';

  if ($patreon_desc !== NULL) {
    $result .= '
      <div style="width: 100%; height: 2em; background-color: #141518; position: relative;">
        <div style="position: absolute; width: ' . $patreon_percent . '%; height: 2em; background-color: #ffffff; border: 2px none;">&nbsp;</div>
        <div style="position: absolute; color: #FF424D; width: 100%; height:100%; display: table; ">
          <span style="display: table-cell; text-align: center; vertical-align: middle;">' . $patreon_desc . '</span>
        </div>
      </div>';
  }

  $result .= '</a>';
  return $result;
}

/* Create a deep copy of given $map, but trimmed to not show all the depth,
   except we do show all depth in $show_full_contents.
   The parts that we don't modify from $map are copied directly, not a deep copy.

   Array deep copy adapted from https://craftytechie.com/how-to-copy-array-in-php/ . */
function _castle_clone_sitemap_and_trim($map, $show_full_contents, $level = 0)
{
  $clone = [];
  foreach($map as $k => $v) {
    if ((isset($show_full_contents[1])) &&
        ($k == $show_full_contents[1]) &&
        // We check $level is even, as our arrays have for PHP 2 conceptual levels,
        // odd levels are with 'sub' etc.
        ($level % 2 != 0))
    {
      /* make a copy (recursive, without any further consideration)
       if it matches $show_full_contents path. */
      $clone[$k] = $v;
    } else
    if (is_array($v)) {
      // If a subarray
      if ($level < 3) {
        $clone[$k] = _castle_clone_sitemap_and_trim($v, $show_full_contents, $level + 1);
      }
    } else
    if (is_object($v)) {
      // If an object
      $clone[$k] = clone $v;
    } else {
      //Other primitive types.
      $clone[$k] = $v;
    }
  }
  return $clone;
}

function echo_castle_header_suffix($path, $enable_sidebar = true)
{
  global $castle_sidebar;
  global $castle_sitemap;
  global $page_basename;
  global $castle_page_path;

  /* calculate $castle_page_path and $path */
  if (count($path) == 0) {
    $path = _detect_page_path($page_basename);
  } else
  if ($path[count($path) - 1] != $page_basename) {
    /* make sure $path ends with $page_basename */
    $path[] = $page_basename;
  }
  $castle_page_path = $path;

  /* traverse $castle_sitemap, along the $path.
     Find which items should be used for a sidebar, if any. */
  $sidebarroot_num = -1;
  $sidebarroot_page = NULL;
  $sidebarroot_info = NULL;
  $sidebarroot_sidebar = false;
  $sidebarroot_sub = $castle_sitemap;
  if ($enable_sidebar) {
    while (!$sidebarroot_sidebar)
    {
      $sidebarroot_num ++;
      if ($sidebarroot_num == count($path))
      {
        /* end of path, nothing wants sidebar */
        $sidebarroot_page = NULL;
        $sidebarroot_info = NULL;
        break;
      } else
      {
        $sidebarroot_page = $path[$sidebarroot_num];

        if (!isset($sidebarroot_sub[$sidebarroot_page])) {
          // return no sidebar for pages outsite of castle_sitemap
          $sidebarroot_page = NULL;
          $sidebarroot_info = NULL;
          break;
          //throw new ErrorException('No page named ' . $sidebarroot_page . ' at current level of castle_sitemap');
        }

        $sidebarroot_info = $sidebarroot_sub[$sidebarroot_page];
        $sidebarroot_sidebar =
          isset($sidebarroot_info['sidebar']) &&
                $sidebarroot_info['sidebar'];
        if (isset($sidebarroot_info['sub']))
          $sidebarroot_sub = $sidebarroot_info['sub']; else
          $sidebarroot_sub = NULL;
      }
    }
  }

  /* make sidebar */
  if ($sidebarroot_page !== NULL && $sidebarroot_info !== NULL)
  {
    $castle_sidebar = _castle_sidebar($sidebarroot_page,
      _castle_clone_sitemap_and_trim($sidebarroot_info, $path));
  } else {
    $castle_sidebar = '';
  }

  if (defined('CASTLE_GITHUB_NAME')) {
    $github_ribbon = '<a href="https://github.com/castle-engine/' . CASTLE_GITHUB_NAME . '" class="github-ribbon"><img src="' . page_requisite('images/forkme_right_orange_ff7600.png') . '" alt="Fork me on GitHub"></a>';
  } else {
    $github_ribbon = '';
  }

  global $main_page;

  $rendered = '
  <nav class="navbar navbar-default ' . ($main_page?'navbar-main-page':'') . '">
    <div class="container-fluid container-nav">
      <!-- Uncomment this for toggable navbar -->
      <!--
      <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#main-navbar-collapse-1" aria-expanded="false">
        <span class="sr-only">Toggle navigation</span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
      </button>
      -->

      <ul class="nav nav-tabs navbar-right">
        <li>' . _castle_patreon_box() . '</li>
      </ul>

      <!--button type="button" class="btn btn-default navbar-btn navbar-right" style="margin-right: 0px;"><a href="' . PATREON_URL . '" class="navbar-link">Support us on<br><img style="height: 40px" src="' . page_requisite('images/patreonlogoorange.png') . '" alt="Patreon"></a></button-->

      <!--p class="navbar-text navbar-right"><a href="' . PATREON_URL . '" class="navbar-link">Support us on<br><img style="height:50px" src="' . page_requisite('images/patreonlogoorange.png') . '" alt="Patreon"></a></p-->

      <div class="navbar-header navbar-brand">
        <a href="/">
          <img alt="Castle Game Engine Logo" src="' .
            page_requisite('images/header_icon.png') . '" ' .
            // _castle_image_sizes('images/header_icon.png')
            ' width="32" height="32" ' // hardcode here, to avoid even loading castle_image_sizes.php in some cases
            . '>
          Castle Game Engine
        </a>
      </div>
      ' .
      // TODO: No idea now where to place this, so it doesn't interfere with "patreon"
      // search_box() .
      '
      ' . _castle_header_menu($path[0]) . '
    </div>

    ' . $github_ribbon . '
  </nav>';

  /* preview version is not maintained anymore
  if (CASTLE_ENVIRONMENT == 'preview') {
    $rendered .= '<div class="alert alert-warning preview-warning" role="alert"><strong>This is a preview!</strong> This is not the official <i>Castle Game Engine</i> website (<a href="' . CASTLE_PROD_URL . '">official website is here</a>). This is only a preview for developers, to see the next website before the release, and to see the documentation for the unstable engine version (from <a href="https://github.com/castle-engine/castle-engine">GitHub master</a>).</div>';
  }
  */

  if ($main_page) {
    $banner_images = array(
      array('src' => 'combined_cge_logo_game', 'alt' => 'Castle Game Engine logo and editor'),
      array('src' => 'combined_cge_logo_game_2', 'alt' => 'Castle Game Engine logo and Escape from the Universe game'),
    );
    $rendered .= '<div class="banner-container">' . "\n";
    $style = ''; // first image is not hidden
    foreach ($banner_images as $banner_image) {
      $image_src = 'images/not_resized/' . $banner_image['src'] . '.webp';
      $rendered .= '<img src="' . $image_src . '" alt="' .
        $banner_image['alt'] . '" ' .
        _castle_image_sizes($image_src) . ' ' .
        $style . '>' . "\n";
      $style = 'style="display: none"'; // subsequent images are hidden initially, this makes better look when loading
    }
    $rendered .= '</div>' . "\n";
  }

  // make sure to start container-fluid for bootstrap container
  if (empty($castle_sidebar)) {
    $rendered .=  _castle_breadcrumbs($path) .
    '<div class="container-fluid">';
  } else {
    $rendered .=
    '<div class="container-fluid">
      <div class="row">
        <div class="col-sm-9 col-sm-push-3 content-near-sidebar">
          ' . _castle_breadcrumbs($path);
  }

  echo $rendered;
}

function castle_footer()
{
  // output extra footer HTML, in case we're part of book
  global $castle_current_book, $castle_books;
  if ($castle_current_book != NULL) {
    echo book_bar($castle_current_book);
  }

  /* This should be done after jQuery JS loaded.
     But, actually it's not needed, it seems.
  ? >
  <script>
  jQuery('.dropdown-toggle').dropdown();
  </script>
  < ?php
  */

  global $castle_sidebar;
  if (empty($castle_sidebar)) {
    echo '</div>';
  } else {
    echo '</div><div class="col-sm-3 col-sm-pull-9 well sidebar">' .$castle_sidebar. '</div></div></div>';
  }

  common_footer(page_requisite('castle-engine-website-base/castle-engine.js'));
}

function echo_footer ()
{
  ?>

  <div class="panel-footer">
    <p>Copyright <a href="https://michalis.xyz/">Michalis Kamburelis</a> and <a href="https://github.com/castle-engine/castle-engine/graphs/contributors">Castle Game Engine Contributors</a>.

    <p>This <a href="https://github.com/castle-engine/cge-www">webpage is also open-source and we welcome pull requests to improve it</a>.

    <?php // using CASTLE_PROD_URL to make sure it works also from API docs ?>
    <p>We use cookies for analytics. See our <a href="<?php echo CASTLE_PROD_URL; ?>privacy_policy.php">privacy policy</a>.
  </div>

  <?php

  /* Insert tracking code */
  echo_piwik_tracking();
  echo_google_analytics_tracking();
}

/* Return SVN URL to appropriate path with repository trunk.
   If $prefix_command is true then also will add 'svn checkout ' text
   at the beginning. */
function sf_checkout_link($prefix_command, $path, $force_https = false)
{
  return
    ($prefix_command ? 'svn checkout ' : '') .
    ($force_https ? 'https' : 'http' ).
    '://svn.code.sf.net/p/castle-engine/code/trunk/' .
    $path;
}

function castle_download_button($title, $url)
{
  return '<a class="btn btn-primary btn-lg" href="' .
    htmlspecialchars($url) .  '">' . $title. '</a>';
}

function sf_download_url($file_name)
{
  return 'http://downloads.sourceforge.net/' . SF_UNIX_NAME . '/' . $file_name;
}

/* Makes a link to a download from SourceForge file release system. */
function sf_download($title, $file_name)
{
  return castle_download_button($title, sf_download_url($file_name));
}

function download_donate_footer()
{
  return /* '
    <hr/>
      Please <a href="' . PATREON_URL . '">support the engine development on Patreon!</a>'; */

      /* <div style="float: left">' . paypal_button(false) . '</div> */

      /* <small>If you like this software, <a href="' . CURRENT_URL . 'donate.php">please consider donating</a>.</small>'; */

    '<a class="btn btn-success btn-lg btn-patreon" href="' . PATREON_URL .
    '"><span class="glyphicon glyphicon-heart" aria-hidden="true"></span> Support us on Patreon</a>';
}

/* This echoes a list to download various versions of the same program.
     < ?php echo castle_download_button("Foo for Linux", ....); ? >
   where $prog_nice_name = Foo.
   The $prog_archive_basename is used to generate download URLs for SourceForge
   (using sf_download_url), ignored if you supply $os_arch_urls for now.

   If $prog_version is '' then the whole -version part will be omitted
   (i.e. $prog_version = '' causes also the dash '-' before version
   to disappear, since this is what you usually want).

   $os_arch_urls is the list of supported OS/architectures (keys)
   and appropriate URLs (values, may be NULL to use SourceForge link).
   Leave NULL (default) to use the default set of OS/architectures hardcoded
   inside, and SourceForge download links. */
function echo_standard_program_download(
  $prog_nice_name, $prog_archive_basename, $prog_version,
  $os_arch_urls = NULL, $extra_bottom_html = '')
{
  global $this_page_name, $os_arch_caption, $os_arch_extension;

  $arch_name_start = $prog_archive_basename;
  if ($prog_version != '')
    $arch_name_start .= '-' . $prog_version;
  $arch_name_start .= '-';

  $nice_name_start = 'Download ' . $prog_nice_name;
  /* no, looks bad:
  if ($prog_version != '')
    $nice_name_start .= ' (' . $prog_version . ')';
  */

  if ($os_arch_urls === NULL) {
    $os_arch_urls = array(
      'win-i386'     => NULL,
      'linux-i386'   => NULL,
      'linux-x86_64' => NULL,
      'macos-x86_64' => NULL,
    );
  }

  $os_arch_caption = array(
    'win-i386'     => ' Windows<br/>(all versions, 32 or 64-bit)',
    'win-x86_64'   => ' Windows<br/>(64-bit)',
    'linux-i386'   => ' Linux<br/>(32-bit, i386)',
    'linux-x86_64' => ' Linux<br/>(64-bit, x86_64)',
    'macos-x86_64' => ' macOS<br/>(64-bit, x86_64)',
  );

  $os_arch_extension = array(
    'win-i386'     => '.zip',
    'win-x86_64'   => '.zip',
    'linux-i386'   => '.tar.gz',
    'linux-x86_64' => '.tar.gz',
    'macos-x86_64' => '.zip',
  );

  $os_arch_icon = array(
    'win-i386'     => 'win',
    'win-x86_64'   => 'win',
    'linux-i386'   => 'linux32',
    'linux-x86_64' => 'linux', // do not use 'linux64', as 64-bit is now obvious standard
    'macos-x86_64' => 'macos'
  );

  echo '<div class="download jumbotron">';

  if (CASTLE_GENERATE_OFFLINE) {
    /* Since the download links contain so many things
       (program version, available OSes, availability on servers
       (github ? sf ?)), it's safer to not put this information
       in the locally generated page (since it changes to often).
       Instead, we can place a link to the WWW page. */
    echo '<p><a href="' . page_url($this_page_name) .
      '" class="btn btn-primary btn-lg">Download ' . $prog_nice_name . ' from our WWW page</a></p>';
  } else
  {
    echo '<div class="download_title">' . $nice_name_start . ':</div>
      <div class="download_platforms_list">';
    foreach ($os_arch_urls as $os_arch => $download_url)
    {
      // calculate non-empty $download_url
      if (empty($download_url)) {
        // for macOS dmg, there is no "-i386" in the filename
        $os_arch_filename = $os_arch;
        if ($os_arch_filename == 'macosx-i386' && $macosx_dmg) {
          $os_arch_filename = 'macosx';
        }
        $download_url = sf_download_url(
          $arch_name_start . $os_arch_filename . $os_arch_extension[$os_arch]);
      }

      echo '<div class="download_platform">';
      echo castle_download_button(
        '<img src="' .
        page_requisite('images/os_icons/' .
          /* This size should be synched with images/Makefile rule */
          $os_arch_icon[$os_arch] . '.png') .
        '" width="64" height="64" alt="' .
        $os_arch_caption[$os_arch] . '"><br/>' .
        $os_arch_caption[$os_arch],
        $download_url);
      echo '</div>' . "\n";
    }
    echo "</div>\n";
  }

  echo $extra_bottom_html .
  download_donate_footer() . '
  </div>';
  /* <!-- This helps Michalis to spend more time on developing our engine, our tools and games :) Details about how you can donate are here</a>.<--> */
}

/* Return html (<table> or <div>) with image links.

   Each $images array item is another associative array:
   - filename: name of the image file (with extension, without path).
     Must exist within original_size/ of our images, and be generated
     to our other sizes (like thumb_size and thumb_const_height_size).
     You can omit this,
     if you instead provide ready URLs in url_full and url_thumb.
   - url_full: URL to the full-size image file.
     If missing, we will derive it from 'filename'.
   - url_thumb: URL to the thumb-size image file.
     If missing, we will derive it from 'filename'.
   - sizes_thumb: if available and non-empty, use this for <img> intrinsic width and height.
     Otherwise, we'll try to deduce it using _castle_image_sizes, but it is impossible
     if url_thumb is also set.
   - titlealt - text used for both title and alt.
     Will be sanitized for HTML, so don't use e.g. &quot;, just write " to display double quot character.
   - html - if set, the rest (except colspan) is ignored,
     and we simply put this html into cell content.
   - colspan (int) - colspan of table cell. It's your responsibility to make it
     look sensible then.

   $align is 'left', 'center' or 'right'.

   $columns may be an int, it then specifies number of columns of the table.
   We will automatically divide $images into rows with $columns images
   (the last row may be left shorter).

   $columns may also be a string 'auto', in which case we will put images
   in a div, and we will fit as many images as the parent width allows.

   The generated links are absolute (starting with CURRENT_URL).
   The important work is always done directly (without need for CSS classes),
   so this content is suitablefor inclusion also in HTML RSS feeds.
*/
function castle_thumbs($images, $columns=1, $align='right', $thumb_size = NULL)
{
  if (count($images) == 0) {
    return '';
  }

  if ($columns === 'auto') {
    $result = '<div class="thumbnails thumbnails-align-' . $align . '">';
  } else {
    $result = '<table class="thumbnails thumbnails-align-' . $align . '"><tr>';
  }

  if ($thumb_size === NULL) {
    $thumb_size = ($columns !== 'auto' ? 'thumb_size' : 'thumb_const_height_size');
  }

  $column_now = 0;
  $image_index = 0;

  foreach ($images as $image)
  {
    if ($columns !== 'auto') {
      if ($column_now == 0 && $image_index != 0) {
        $result .= '</tr><tr>';
      }

      // special trick for the last image when columns > 1:
      // add empty cells, so that the last image is at right.
      if ($column_now == 0 &&
          $image_index == count($images) - 1 &&
          !isset($image['colspan'])) {
        $result .= str_repeat('<td></td>', $columns - 1);
      }

      if (isset($image['colspan']))
        $colspan = ' colspan="' . (int)$image['colspan'] . '"'; else
        $colspan = '';

      $result .= '<td' . $colspan . '>';
    }

    if (isset($image['html']))
    {
      $result .= $image['html'];
    } else
    {
      $regenerate_thumbnails = CASTLE_ENVIRONMENT == 'development';

      // calculate $relative_filename_full, $relative_filename_thumb
      if (array_key_exists('filename', $image)) {
        $relative_filename_full  = 'images/original_size/' . $image['filename'];

        $thumb_pathinfo = pathinfo($image['filename']);
        $thumb_ext = $thumb_pathinfo['extension'];
        // Following images/Makefile, change thumbnails extension to webp
        if ($thumb_ext == 'png' || $thumb_ext == 'jpg') {
          $thumb_ext = 'webp';
        }
        $relative_filename_thumb = 'images/' . $thumb_size . '/' .
          $thumb_pathinfo['filename'] . '.' . $thumb_ext;
      } else {
        $relative_filename_full = NULL;
        $relative_filename_thumb = NULL;
      }

      if ( $regenerate_thumbnails &&
           ($relative_filename_thumb !== NULL) &&
           (!file_exists($relative_filename_thumb))
         )
      {
        /* Regenerate thumbnails.
           Output raport at any place within current doc -- this is only at development,
           doesn't have to look good, just be informative. */
        echo "<pre>\nThumbnail for <b>${image['filename']}</b> does not exist, regenerating thumbnails:\n";
        $command = 'cd images/ && make';
        if (exec($command, $output_lines, $exec_status) === FALSE) {
          die('Failed (exec error) executing ' . htmlspecialchars($command));
        }
        echo htmlspecialchars(implode("\n", $output_lines)) . "\n";
        if ($exec_status != 0) {
          echo 'WARNING: Failed (non-zero status: ' . $exec_status . ') executing: ' . htmlspecialchars($command);
        }
        echo '</pre>';
      }

      if (isset($image['url_full'])) {
        $url_full = $image['url_full'];
      } else {
        $url_full = page_url($relative_filename_full);
      }

      if (isset($image['url_thumb'])) {
        $url_thumb = $image['url_thumb'];
        $size_thumb = '';
      } else {
        $url_thumb =  page_requisite($relative_filename_thumb);
        $size_thumb = _castle_image_sizes($relative_filename_thumb);
      }

      /* If sizes_thumb is set (and not FALSE), it overrides $size_thumb
         (whether it was empty or determined by _castle_image_sizes).
         This allows to output in HTML image sizes for images in Wordpress gallery
         (and having these sizes is good for optimizing CLS). */
      if (!empty($image['sizes_thumb'])) {
        $size_thumb = $image['sizes_thumb'];
      }

      $result .= '
          <a href="' . $url_full . '"
             class="screenshot"
             title="' . htmlspecialchars($image['titlealt']) . '"><img
            style="float: right"
            src="' . $url_thumb . '"
            ' . $size_thumb . '
            alt="' . htmlspecialchars($image['titlealt']) . '"
          ></a>';
    }

    if ($columns !== 'auto') {
      $result .= '</td>';
    }

    // increase $column_now
    if (isset($image['colspan']))
      $column_now += (int)$image['colspan']; else
      $column_now++;
    if ($column_now >= $columns) {
      $column_now = 0;
    }

    $image_index++;
  }

  if ($columns !== 'auto') {
    $result .= '</tr></table>';
  } else {
    $result .= '</div>';
  }

  return $result;
}

/* Thumbnail for this program. Filename and alt/title auto-generated
   from $prog_name. */
function default_program_thumbnail($prog_name)
{
  return castle_thumbs(array(
    array('filename' => $prog_name . '_screen_demo.png', 'titlealt' => 'Image from "' . $prog_name . '"'),
  ));
}

/* DEPENDS_ consts and funcs */

define('DEPENDS', 'Requirements');
define('DEPENDS_OPENGL',
  '<a href="http://www.opengl.org/documentation/implementations/">OpenGL</a>
  <!--
  (on all modern OSes, OpenGL is probably already installed and working on
  your system) -->');
define('DEPENDS_LIBPNG_AND_ZLIB',
  '<a href="http://www.libpng.org/">Libpng</a>,
   <a href="http://www.gzip.org/zlib/">Zlib</a>
   (under Windows appropriate DLL files are already included
   in program\'s archive, so you don\'t have to do anything)');
define('SUGGESTS_OPENAL',
  '<a href="' . CASTLE_PROD_URL . 'openal#_installing_openal">OpenAL</a> is used to play sound
  (under Windows appropriate DLL files are already included
  in program\'s archive, so you don\'t have to do anything)');
define('SUGGESTS_OPENAL_VORBISFILE',
  '<a href="' . CASTLE_PROD_URL . 'openal#_installing_openal">OpenAL</a> and <a href="http://xiph.org/vorbis/">VorbisFile</a> libraries are used to play sound and load OggVorbis sounds
  (under Windows appropriate DLL files are already included
  in program\'s archive, so you don\'t have to do anything)');
define('DEPENDS_UNIX_CASTLE_WINDOW_GTK_2',
  'Under Linux and FreeBSD: <a href="http://www.gtk.org/">GTK+</a> 2');

function depends_par($depends_array)
{
  $result = '';
  foreach($depends_array as $item)
  {
    if ($result != '') $result .= ', ';
    $result .= $item;
  }

  $result = "<p><b>" . DEPENDS . ":</b> $result.";
  return $result;
}

function depends_ul($depends_array)
{
  return array_to_ul($depends_array);
}

/* Highlight XML code, detecting [[xxx|yyy]] as links to API doc.

   We do not use geshi for this now, since as far as I know we cannot do this
   with geshi (geshi's "keyword URLs" are too weak for what we want,
   we want various XML attributes (sometimes named the same, but under different
   elements) to lead to different URL). */
function xml_highlight($source)
{
  $source = str_replace('&', '&amp;', $source);
  $source = str_replace('<', '&lt;', $source);
  $source = str_replace('>', '&gt;', $source);
  $source = str_replace('&lt;!--', '<span class="xml_highlight_comment">&lt;!--', $source);
  $source = str_replace('--&gt;', '--&gt;</span>', $source);
  $source = '<pre class="xml">' . $source . '</pre>';

  // replace [[http:xxx|yyy] with <a href="http:xxx">yyy</a>
  $source = preg_replace('/\\[\\[(https?:.+)\\|(.+)\\]\\]/',
    '<a href="\\1">\\2</a>', $source);

  // replace [[http:xxx] with <a href="http:xxx">xxx</a>
  $source = preg_replace('/\\[\\[(https?:.+)\\]\\]/',
    '<a href="\\1">\\1</a>', $source);

  // replace [[xxx|yyy] with <a href="$castle_apidoc_url/xxx">yyy</a>
  global $castle_apidoc_url;
  $source = preg_replace('/\\[\\[(.+)\\|(.+)\\]\\]/',
    '<a href="' . $castle_apidoc_url . '\\1">\\2</a>', $source);

  return $source;
}

/* Highlight XML code.
   This doesn't detect [[xxx|yyy]] inside (in contrast to xxx_highlight),
   but it makes output more colorful (uses full Geshi possibilities).
*/
function xml_full_highlight($source)
{
  global $geshi;
  $geshi->set_source($source);
  $geshi->set_language('xml');
  $geshi->set_footer_content(''); // remove set_footer_content by pascal_highlight_file
  return $geshi->parse_code();
}

function pascal_highlight($source)
{
  global $geshi;
  $geshi->set_source($source);
  $geshi->set_language('delphi'); // tested: order of set_source, set_language doesn't matter
  $geshi->set_footer_content(''); // remove set_footer_content by pascal_highlight_file
  return $geshi->parse_code();
}

function pascal_highlight_file($file_name, $link_to_download = TRUE)
{
  // trim, to avoid final newline
  $source = trim(file_get_contents($file_name));

  global $geshi;
  $geshi->set_source($source);
  $geshi->set_language('delphi');
  /* Unfortunately, tidy says that this isn't OK, putting <div>
     (with set_footer_content) inside a <pre> */
  if ($link_to_download && !HTML_VALIDATION) {
    $geshi->set_footer_content('<br><a class="download_code" href="' . htmlspecialchars($file_name) . '">// This is a complete source code that you can compile. Download it!</a>');
  } else {
    $geshi->set_footer_content(''); // remove set_footer_content set by previous pascal_highlight_file calls
  }
  return $geshi->parse_code();
}

/* URL to X3D specification specific component.
   When non-empty $anchor, we add this #xxxx to the URL.
   $spec_version may be NULL (latest stable spec) or 'draft' (latest draft spec).
*/
function x3d_spec_latest_url($component_name, $anchor = '', $spec_version = NULL)
{
  if ($anchor != '') {
    $anchor = '#' . $anchor;
  }

  // Return X3D 4.0 now, ignoring $draft value, our materials now heavily depend on X3D 4.
  $spec_base = 'https://www.web3d.org/documents/specifications/19775-1/V4.0/Part01/components/';

  switch ($component_name) { // the HTML name was updated in X3D 4
    case 'enveffects'    : $component_name = 'environmentalEffects'; break;
    case 'interp'        : $component_name = 'interpolators'; break;
    case 'group'         : $component_name = 'grouping'; break;
    case 'envsensor'     : $component_name = 'environmentalSensor'; break;
    case 'env_texture'   : $component_name = 'environmentalTexturing'; break;
    case 'pointingsensor': $component_name = 'pointingDeviceSensor'; break;
    case 'keyboard'      : $component_name = 'keyDeviceSensor'; break;
    case 'utils'         : $component_name = 'eventUtilities'; break;
  }

  return $spec_base . $component_name . '.html' . $anchor;
}

function vrmlx3d_highlight($source)
{
  global $geshi;
  $geshi->set_source($source);
  $geshi->set_language('VRML / X3D');
  $geshi->set_footer_content(''); // remove set_footer_content by pascal_highlight_file
  return $geshi->parse_code();
}

function glsl_highlight($source)
{
  global $geshi;
  $geshi->set_source($source);
  $geshi->set_language('C'); // don't use glSlang, for some reason it's broken for me
  $geshi->set_footer_content(''); // remove set_footer_content by pascal_highlight_file
  return $geshi->parse_code();
}

/* Link from a gallery.
   $page_name may be a complete URL, or a page name for a_href_page. */
function gallery_link($title, $subtitle, $image_name, $page_name)
{
  $image_pathinfo = pathinfo($image_name);
  $image_ext = $image_pathinfo['extension'];
  // Following images/Makefile, change thumbnails extension to webp
  if ($image_ext == 'png' || $image_ext == 'jpg') {
    $image_ext = 'webp';
  }

  $s = '<div class="col-sm-4"><div class="cge-gallery-link">';
  $s .= '<p>' .
    a_href_page(
      '<img src="images/gallery_size/' . $image_pathinfo['filename'] . '.' . $image_ext . '" alt="' .
      htmlspecialchars($title) . '">',
      $page_name
    ) .
    '</p>';
  $s .= '<p class="cge-gallery-link-title">' .
    a_href_page("<b>$title</b>", $page_name) . '</p>' .
    '<p>' . $subtitle . '</p>' .
    '</div></div>';

  echo $s;
}

/* Link from a gallery, that doesn't have nice screenshot. */
function gallery_link_noimage($title, $subtitle, $page_name)
{
  $s = '<li><p class="cge-gallery-link-title">' .
    a_href_page("<b>$title</b>", $page_name) . '</p>' .
    '<p>' . $subtitle . '</p>' .
    '</li>';
  echo $s;
}

/* Show 404 (page not found) error page.
   Show $message (will be sanitized for HTML, should be a whole sentence -- end with dot).
   Automatically sets HTTP response, and dies (breaks script execution).
*/
function castle_fail_404($message)
{
  http_response_code(404);

  castle_header('Page not found', array(
    'canonical_url' => NULL // do not write canonical for 404 pages
  ));
  echo '<div class="alert alert-danger message_404" role="alert"><p>' . htmlspecialchars($message) . '

    <p><a href="/">Go to <b>Castle Game Engine</b> main page.</a>

    <p><a href="talk.php">Looking for something but cannot find the answer? Ask on our forum or chat!</a>
  </div>';
  castle_footer();
  die();
}

/* Return HTML link to given Pascal identifier.
   $title equal to NULL or '' means to use identifier as the title.
   This is documented (as AsciiDoctor macro) in ../README.md.
*/
function cgeRef($identifier, $title = NULL)
{
  global $castle_apidoc_url, $pasdoc;

  // require large apidoc_map_latest.php only when necessary
  require_once 'apidoc_map_latest.php';

  if (empty($title)) {
    $title = $identifier;
  } else {
    /* As an optimization, we don't do this when $title comes from $identifier,
       when we know that Pascal identifier doesn't have {{{ or }}}. */
    $title = str_replace('{{{', '[',
             str_replace('}}}', ']',
             $title));
  }
  if (array_key_exists($identifier, $pasdoc)) {
    $html_filename = $pasdoc[$identifier]['html_filename'];
  } else {
    /* Try case-insensitive search.
       This is slower, and also we try to be consistent (case-sensitive,
       even though we could ignore it in Pascal)
       in the way to specify identifiers in CGE code and docs.
       So we warn about it. */
    $identifier_lower = strtolower($identifier);
    $pasdoc_lower = array_change_key_case($pasdoc, CASE_LOWER);
    $html_filename = $pasdoc_lower[$identifier_lower]['html_filename'];
    if (CASTLE_ENVIRONMENT == 'development') {
      echo '<b>Development Warning</b>: Pascal identifier <code>' . htmlspecialchars($identifier) . '</code> found, but with different case, in docs<br>';
    }
  }
  return '<code><a href="' . $castle_apidoc_url . $html_filename . '">' .
    htmlspecialchars($title) . '</a></code>';
}

/* Return HTML that looks similar to cgeRef for the same arguments,
   but actually has no link,
   and communicates that given identifier is deprecated and no longer documented.
   (Stuff that is outright removed from CGE should not be at all in our docs.) */
function removedCgeRef($identifier, $title = NULL)
{
  if (empty($title)) {
    $title = $identifier;
  }
  return '<code title="Deprecated, not documented anymore">' . htmlspecialchars($title) . '</code>';
}

/* Return HTML to display images (as a block inside regular content, or as a floating block
   on the side).
   This is just a thin wrapper over castle_thumbs now.
   This is documented (as AsciiDoctor macro) in ../README.md.
*/
function cgeImg($placement, $images)
{
  // calculate $columns, $align (args for castle_thumbs) from 1st match
  if ($placement == 'block') {
    $columns = 'auto';
    $align = 'left';
  } else
  if ($placement == 'float') {
    $columns = 1;
    $align = 'right';
  } else {
    throw new ErrorException('Invalid cgeimg placement: ' . $placement);
  }

  return castle_thumbs($images, $columns, $align);
}

/* Replace AsciiDoctor macros like cgeref, cgeimg, documented in ../README.md. */
function castle_replace_asciidoctor_macros($contents)
{
  /* This optionally removes also surrounding HTML <p> and </p> to fix HTML validity
     in case of most common usage: when cge::features-summary[] is used as a separate
     paragraph.

     Note: We use "?:" to make them non-capturing groups, see
     https://www.php.net/manual/en/regexp.reference.subpatterns.php .
     This matters for similar "cgeimg::..." trick, we didn't want to change numbering
     of matching groups.
  */

  $contents = preg_replace_callback('/(?:<p>)?cge::features-summary\[\](?:<\/p>)?/',
    function ($matches) {
      return cge_features_summary();
    },
    $contents);

  $contents = preg_replace_callback('/cge::download-engine\[([^],]*),([^],]*)\]/',
    function ($matches) {
      return cge_download_engine($matches[1], $matches[2]);
    },
    $contents);

  $contents = preg_replace_callback('/cgeref:([A-Za-z0-9_.]+)\[([^]]*)\]/',
    function ($matches) {
      $identifier = $matches[1];
      $title = $matches[2];
      return cgeRef($identifier, $title);
    },
    $contents);

  // raise error on the common misspelling of cgeref macro, with double colon
  $contents = preg_replace_callback('/cgeref::([A-Za-z0-9_.]+)\[([^]]*)\]/',
    function ($matches) {
      throw new ErrorException('Error: cgeref with double colon: ' . $matches[0]);
    },
    $contents);

  $contents = preg_replace_callback('/(?:<p>)?cgeimg::([A-Za-z0-9_.]+)\[([^]]*)\](?:<\/p>)?/',
    function ($matches) {
      $placement = $matches[1];

      $images_str = $matches[2];

      /* We use a simple hack to handle \, and \| in macro contents.
         We replace them to CGE-XXX now, and later (after all exploding) we'll replace them back.
         This is not a reliable solution to quoting with backslashes -- e.g. you can easily
         break it by writing actually CGE-XXX in the macro text.
         But it is simple, and works for our cases. */
      $images_str = str_replace('\\,', 'CGE-COMMA',
                    str_replace('\\|', 'CGE-BAR',
                    str_replace('\\\\', 'CGE-BACKSLASH',
                    $images_str)));

      // Calculate $images from 2nd match.
      $images_strings = explode(',', $images_str);
      $images = array();
      foreach ($images_strings as $image_str) {
        $image_str_split = explode('|', $image_str);
        if (count($image_str_split) != 2) {
          throw new ErrorException('Expected 2 items in image string split by |: ' . $image_str . ', part of larger content ' . $images_str);
        }
        $img_filename = trim($image_str_split[0]);
        $img_titlealt = trim($image_str_split[1]);
        $img_titlealt = str_replace('CGE-COMMA', ',',
                        str_replace('CGE-BAR', '|',
                        str_replace('CGE-BACKSLASH', '\\',
                        $img_titlealt)));
        $images[] = array(
          'filename' => $img_filename,
          'titlealt' => $img_titlealt,
        );
      }

      return cgeImg($placement, $images);
    },
    $contents);

  return $contents;
}

/* Replace in file AsciiDoctor macros like cgeref, cgeimg (documented in ../README.md). */
function castle_replace_asciidoctor_macros_file($file_name)
{
  $contents = file_get_contents($file_name);
  $contents = castle_replace_asciidoctor_macros($contents);
  file_put_contents($file_name, $contents);
}

/* Return HTML text about sources of this application.
   Will use CASTLE_GITHUB_NAME if defined. */
function castle_sources_notice()
{
  $result = 'This is free/open-source software. Developers can ' .
    a_href_page('download sources of this program', 'all_programs_sources') . '.';
  if (defined('CASTLE_GITHUB_NAME')) {
    $result .= ' Or just <a href="https://github.com/castle-engine/' . CASTLE_GITHUB_NAME . '">get the code from GitHub</a>.';
  }
  return $result;
}

/* Show CGE features.

   $feature_heading_level (int) determines the HTML header level for each feature,
   default 3 means we'll use <h3>.
   Adjust it to have good header nesting (search engines like it, to understand the page better).
*/
function cge_features_summary($feature_heading_level = 3)
{
  $features = array(
    array(
      'link' => 'features#_visual_editor',
      'title' => 'Visual editor',
      'description' => 'Design 3D and 2D games and user interfaces (with automatic scaling, anchors). Build and deploy the same project for multiple platforms. Integrate with various IDEs (Lazarus, Delphi, Visual Studio Code...).',
      'image' => '2d_demo_editor.png',
      'image_titlealt' => '2D game in editor',
    ),
    array(
      'link' => 'features#_viewport_with_scenes_camera_navigation_and_other_components',
      'title' => 'Components',
      'description' => 'A lot of components to design <a href="viewport_and_scenes">viewport</a> contents (3D and 2D world, using scenes, cameras, navigation, primitives, lights...) and <a href="user_interface">user interface</a> (buttons, images, labels...).',
      // 'image' => 'viewport_3d_nice_view.png',
      // 'image_titlealt' => 'Viewport with 3D design',
      'image' => 'terrain_component_1.png',
      'image_titlealt' => 'Terrain with water and trees',
    ),
    array(
      'link' => 'features#_data_formats',
      'title' => 'Lots of ways to define data',
      'description' => 'Use 3D models, 2D animations, sprite sheets. Great integration with authoring tools like <a href="blender">Blender</a> or shops like Sketchfab thanks to using open standards like glTF and X3D.',
      'image' => 'gltf_village_outlines.png',
      'image_titlealt' => 'Village scene with outlines in glTF from Sketchfab ( https://sketchfab.com/3d-models/ftm-0970f30574d047b1976ba0aa6f2ef855 by Luis Fernandez )',
    ),
    array(
      'link' => 'features#_graphic_effects',
      'title' => 'Graphic effects',
      'description' => 'Composable shader effects, shadows, mirrors, physically based rendering, bump mapping, gamma correction...',
      'image' => 'barna29_nice_shadows.png',
      'image_titlealt' => 'Real-time water with caustics, reflections, shadows',
    ),
    array(
      'link' => 'features#_cross_platform',
      'title' => 'Cross-platform',
      'description' => 'Target any platform (desktop: Windows, Linux, macOS, FreeBSD, Raspberry Pi, mobile: Android, iOS, console: Nintendo Switch). Work on any desktop platform. WebGL and Oculus (VR) are coming as targets soon.',
      'image' => 'nintendo_switch_3.jpg',
      'image_titlealt' => '"Escape from the Universe" on Nintendo Switch',
    ),
    array(
      'link' => 'features#_native_and_fast_code_using_modern_pascal',
      'title' => 'Clean and fast code',
      'description' => 'Native clean object-oriented programming language with <a href="modern_pascal">modern Object Pascal</a>. Fast builds, fast execution out-of-the-box.',
      'image' => 'combined_fpc_delphi.png',
      'image_titlealt' => 'FPC and Delphi',
    ),
    array(
      'link' => 'features#_build_tool_and_continuous_integration',
      'title' => 'Open source and friendly to continuous integration',
      'description' => 'Tooling friendly to continuous integration. <a href="build_tool">Command-line build tool</a> and <a href="docker">Docker</a> image. Ready files and documentation how to use with <a href="github_actions">GitHub Actions</a>, <a href="gitlab_ci">GitLab CI</a>, <a href="jenkins">Jenkins</a>. Engine is completely open-source and <a href="license">can be used to make any (including proprietary) applications</a>.',
      'image' => 'combined_osi_ci_logos.png',
      'image_titlealt' => 'Open-source, integrated with GitHub, GitLab, Jenkins',
    ),
  );

  $heading = 'h' . (int)$feature_heading_level;

  $result = '';
  $odd = true;
  foreach ($features as $feature) {
    $image_relative_filename = 'images/feature_size/' . pathinfo($feature['image'],  PATHINFO_FILENAME) . '.webp';
    $row_odd = $odd ? 'odd' : 'even';
    $result .= '<div class="feature-row feature-row-' . $row_odd . '">
      <div class="feature-column-image">
        <a href="' . htmlspecialchars($feature['link']) . '"
           title="' . htmlspecialchars($feature['image_titlealt']) . '">
          <img
             class="feature-image"
             alt="' . htmlspecialchars($feature['image_titlealt']) . '"
             src="' . htmlspecialchars($image_relative_filename) . '"
             ' . _castle_image_sizes($image_relative_filename) . '
          >
        </a>
      </div>
      <div class="feature-column-text">
        <' .$heading. ' class="feature-title"><a href="' . htmlspecialchars($feature['link']) . '">' . htmlspecialchars($feature['title']) .  '</a></' .$heading. '>
        <div class="feature-description">' . $feature['description'] .  '</div>
      </div>
    </div>';
    $odd = !$odd;
  }

  /* This clearfix could be realized alternatively by

  .feature-row::after {
    clear: both !important;
  }

  in CSS, but for unknown reason it fails.

  Careful:
  <div class="clearfix" /> is not good, you cannot close <div> this way
  in HTML 5. It would have a visible bad artifact: on main page,
  <div class="container-fluid" ...">  would include also footer,
  and footer would have unwanted left/right padding with white color.
  */
  $result .= '<div class="clearfix"></div>';

  return $result;
}

/* Refer to CGE download on GitHub.

   $version - part of CGE zip filename with version.

   $tag - GitHub release (aka GIT tag).

   E.g.:
   // Download snapshot:
   $version = '7.0-alpha.snapshot';
   $tag = 'snapshot';

   // Download stable:
   $version = '7.0-alpha.2';
   $tag = 'v' . $version;
*/
function cge_download_engine($version, $tag)
{
  $download_prefix_general = 'https://github.com/castle-engine/castle-engine/releases/download/' . $tag . '/';
  $download_prefix = $download_prefix_general . 'castle-engine-' . $version;

  return '
    <div class="centered-download-wrapper">
        <div class="download jumbotron main-cge-download">
            <div class="download_platforms_list">
                <div class="download_platform"><a class="btn btn-primary btn-lg" href="' . $download_prefix_general . 'castle-engine-setup-' . $version . '.exe"><img src="' . CURRENT_URL . '/images/os_icons/win.png" alt="Windows (64-bit, x86_64)" width="64" height="64"><br> Windows<br>(x86_64)</a></div>
                <div class="download_platform"><a class="btn btn-primary btn-lg" href="' . $download_prefix . '-linux-x86_64-bundle.zip"><img src="' . CURRENT_URL . '/images/os_icons/linux.png" alt="Linux (64 bit, x86_64)" width="64" height="64"><br> Linux<br>(x86_64)</a></div>
                <div class="download_platform"><a class="btn btn-primary btn-lg" href="' . $download_prefix . '-linux-arm.zip"><img src="' . CURRENT_URL . '/images/os_icons/raspberry_pi_32.png" alt="Raspberry Pi 32-bit (Linux Arm)" width="91" height="64"><br> Raspberry Pi<br>(Linux Arm)</a></div>
                <div class="download_platform"><a class="btn btn-primary btn-lg" href="' . $download_prefix . '-linux-aarch64.zip"><img src="' . CURRENT_URL . '/images/os_icons/raspberry_pi_64.png" alt="Raspberry Pi 64-bit (Linux Aarch64)" width="91" height="64"><br> Raspberry Pi<br>(Linux Aarch64)</a></div>
                <div class="download_platform"><a class="btn btn-primary btn-lg" href="' . $download_prefix . '-darwin-x86_64.zip"><img src="' . CURRENT_URL . '/images/os_icons/macos.png" alt="macOS (Intel-based macs, 64-bit, x86_64)" width="64" height="64"><br> macOS<br>(x86_64)</a></div>
                <div class="download_platform"><a class="btn btn-primary btn-lg" href="https://castle-engine.io/compiling_from_source.php"><img src="' . CURRENT_URL . '/images/os_icons/freebsd.png" alt="FreeBSD (Sources)" width="64" height="64"><br> FreeBSD<br>(Use the Source Luke)</a></div>
                <div class="download_platform"><a class="btn btn-primary btn-lg" href="https://github.com/castle-engine/castle-engine/"><img src="' . CURRENT_URL . '/images/os_icons/github.png" alt="Source Code on GitHub" width="64" height="64"><br> Source Code<br>(GitHub)</a></div>
            </div>
        </div>
    </div>';
}
