<?php
// define('CASTLE_GITHUB_NAME', 'castle-engine');

require_once 'castle_engine_functions.php';

global $main_page;
$main_page = true;
global $site_title;
$site_title = NULL; // set to NULL, to disable appending to <title>; main page has special title that already includes CGE

castle_header('Castle Game Engine - Free open-source cross-platform 3D 2D game engine with editor and powerful Pascal API', array(
  'meta_description' => META_DESCRIPTION,
  /* Not useful for Google anymore, but maybe other search engines use them. */
  'meta_keywords' => 'game engine, glTF, X3D, Spine, Pascal, cross platform, editor, Nintendo Switch, Linux, Windows, macOS, Android, iOS, PBR, shadows, shaders',
  'canonical_url' => CASTLE_PROD_URL
));

define('CGE_SNAPSHOTS_BASE', 'https://github.com/castle-engine/castle-engine/releases/download/snapshot/');
define('CGE_SNAPSHOTS_VERSION', '7.0-alpha.snapshot');
?>

<!-- Free open-source game engine for <a href="http://www.freepascal.org/">FreePascal and Lazarus</a>. Excellent support for many 3D and 2D data formats, portable (desktops, Android, iOS...), many advanced graphic effects, comfortable API.</p -->

<div class="row">
    <div class="col-sm-8">
        <div class="centered-wrapper">
            <div class="centered">
                <h1 class="main-title">Castle Game Engine</h1>
                <p class="main-subtitle">Cross-platform (desktop, mobile, console) 3D and 2D game engine. Powerful visual editor. Support for glTF, X3D, Spine and more. Fast clean code using modern Pascal. Free and open-source.</p>
                <!--
                The tagline could be linked, but some of these links could them "hijack" user's attention.
                Links will be later.

                <a href="https://github.com/castle-engine/castle-engine/blob/master/COPYING.md">Open-source</a>
                cross-platform 3D / 2D game engine using
                modern Object Pascal</a>
                and open standards like glTF and X3D</a>
                -->
            </div>
        </div>

        <div class="centered-download-wrapper">
            <div class="download jumbotron main-cge-download">
                <div class="download-hints">
                    <p>Download the engine for your system:
                </div>

                <div class="download_platforms_list">
                    <div class="download_platform"><a class="btn btn-primary btn-lg" href="<?php echo CGE_SNAPSHOTS_BASE; ?>castle-engine-<?php echo CGE_SNAPSHOTS_VERSION; ?>-win64-x86_64.zip"><img src="<?php echo CURRENT_URL; ?>/images/os_icons/win.png" alt="Windows (64-bit, x86_64)" width="64" height="64"><br> Windows<br>(x86_64)</a></div>
                    <div class="download_platform"><a class="btn btn-primary btn-lg" href="<?php echo CGE_SNAPSHOTS_BASE; ?>castle-engine-<?php echo CGE_SNAPSHOTS_VERSION; ?>-linux-x86_64.zip"><img src="<?php echo CURRENT_URL; ?>/images/os_icons/linux.png" alt="Linux (64 bit, x86_64)" width="64" height="64"><br> Linux<br>(x86_64)</a></div>
                    <div class="download_platform"><a class="btn btn-primary btn-lg" href="<?php echo CGE_SNAPSHOTS_BASE; ?>castle-engine-<?php echo CGE_SNAPSHOTS_VERSION; ?>-linux-arm.zip"><img src="<?php echo CURRENT_URL; ?>/images/os_icons/raspberry_pi.png" alt="Raspberry Pi (Linux Arm)" width="64" height="64"><br> Raspberry Pi<br>(Linux Arm)</a></div>
                    <div class="download_platform"><a class="btn btn-primary btn-lg" href="<?php echo CGE_SNAPSHOTS_BASE; ?>castle-engine-<?php echo CGE_SNAPSHOTS_VERSION; ?>-darwin-x86_64.zip"><img src="<?php echo CURRENT_URL; ?>/images/os_icons/macos.png" alt="macOS (Intel-based macs, 64-bit, x86_64)" width="64" height="64"><br> macOS<br>(x86_64)</a></div>
                    <div class="download_platform"><a class="btn btn-primary btn-lg" href="https://castle-engine.io/compiling_from_source.php"><img src="<?php echo CURRENT_URL; ?>/images/os_icons/freebsd.png" alt="FreeBSD (Sources)" width="64" height="64"><br> FreeBSD<br>(Use the Source Luke)</a></div>
                    <div class="download_platform"><a class="btn btn-primary btn-lg" href="https://github.com/castle-engine/castle-engine/"><img src="<?php echo CURRENT_URL; ?>/images/os_icons/github.png" alt="Source Code on GitHub" width="64" height="64"><br> Source Code<br>(GitHub)</a></div>
                    <!--div class="download_platform"><a class="btn btn-primary btn-lg" href="<?php echo CURRENT_URL; ?>cge-delphi-beta.zip"><img src="<?php echo CURRENT_URL; ?>/images/os_icons/windows_delphi.png" alt="Beta: Windows version with Delphi support" width="145" height="64"><br>Windows + Delphi Support<br>(Beta)</a></div-->
                </div>

                <?php /*
                echo castle_download_button('<span class="glyphicon glyphicon-download" aria-hidden="true"></span><br>Download<br><p class="download-button-hint">Version 6.5 (Snapshot)</p>',
                  CGE_LATEST_UNSTABLE_DOWNLOAD);
                  */ ?>
                <?php /*
                  echo castle_download_button('<span class="glyphicon glyphicon-leaf" aria-hidden="true"></span><br>Download<br><p class="download-button-hint">6.4 - Stable Release<br>&nbsp;</p>',
                  CGE_LATEST_STABLE_DOWNLOAD);
                  */ ?>

                <div class="download-hints">
                    <p>Next: <a href="manual_install_run_first.php">Install the engine and build your first application</a>.
                </div>

                <?php echo download_donate_footer(); ?>
            </div>
        </div>

        <div class="centered-wrapper">
            <div class="centered">
                <ul>
                    <li>Use <b>any 3D or 2D software</b> to create your models in various formats: <a href="creating_data_model_formats.php">glTF, X3D, VRML<!--?php echo a_href_page('VRML / X3D', 'vrml_x3d'); ?-->, Spine JSON, Collada...</a>
                    <li>Develop <b>cross-platform</b> applications, for desktop (<b>Windows, Linux, macOS, FreeBSD...</b>), mobile (<b>Android, iOS</b>), consoles (<b>Nintendo Switch</b>) and other devices (<b>Raspberry Pi</b>).
                    <li><a href="https://castle-engine.io/manual_editor.php"><b>Visual editor</b></a> to design games UI and to build applications, powerful <a href="https://castle-engine.io/build_tool">command-line <b>build tool</b></a> under the hood.
                    <li>Optimized rendering with a lot of graphic effects (<b>physically-based rendering, shadows, mirrors, bump mapping, shader effects, gamma correction</b>...).
                    <li><b>Build and edit</b> the <a href="vrml_x3d.php">scene graph (X3D)</a> <b>at runtime</b>.
                      <!--Load and save images and X3D graph as needed.-->
                      Create 3D processing, visualization tools and CAD applications.
                    <li>Extensible system for game objects, with <b>physics, creatures with AI and navmesh</b>, and more.
                    <li>Access numerous <b>services, like in-app purchases and game services</b> on mobile devices.
                    <li>Create <b>cross-platform user-interface with anchors and automatic scaling</b>.
                    <?php /* Talking with Eugene and KB, it seems important to mention
                      that it's actively developed language,
                      and that we have native speed. */ ?>
                    <li>Code in <a href="modern_pascal_introduction.html">modern Object Pascal</a>, an efficient OOP language with <a href="https://www.freepascal.org/">FPC (cross-platform open-source compiler)</a> or <a href="https://www.embarcadero.com/products/Delphi">Delphi</a>, compiled to a native optimized code. Use any IDE, like <a href="https://www.lazarus-ide.org/">Lazarus</a>, Delphi, or <a href="https://code.visualstudio.com/">Visual Studio Code</a>.
                </ul>
            </div>
        </div>
    </div>
    <div class="col-sm-4">
        <!-- iframe class="hidden-xs main-page-thumbnail" width="560" height="315" src="https://www.youtube.com/embed/o5q7guVkYVo" frameborder="0" allowfullscreen></iframe -->

        <img src="images/original_size/dragon_large.png"
            alt="2D animation, designed in Spine"
            title="2D animation, designed in Spine"
            class="main-page-thumbnail hidden-xs" />
        <br>

        <a href="images/original_size/dragon_editor.png"
            title="2D game, designed in Castle Game Engine editor"
            class="screenshot">
        <img src="images/original_size/dragon_editor.png"
            alt="2D game, designed in Castle Game Engine editor"
            style="padding: 1em; border: none"
            class="main-page-thumbnail hidden-xs" />
        </a>

        <div class="jumbotron alert alert-info">
            <p><a href="https://discord.gg/GYgKs6f7?event=982744972476432384" role="button">
              Register for 3rd Open Meeting for CGE Users and Developers:<br>
              <span style="text-decoration: underline;">September 17th (2022)</span>
              </a>
            </p>

            <?php echo cgeImg('block', array(
              array('filename' => 'meet_2_michalis.png', 'titlealt' => 'Michalis on 2nd Open Meeting'),
            ));
            ?>
        </div>

        <!-- <img src="images/original_size/rhan_shrine_5_everything.png" -->
        <!--     alt="Bump mapping and shadow maps from multiple light sources" -->
        <!--     title="Bump mapping and shadow maps from multiple light sources" -->
        <!--     class="main-page-thumbnail" -->
        <!--     style="margin-bottom: 0.5em" /> -->
        <!-- <img src="images/original_size/barna29_nice_shadows.png" -->
        <!--     alt="Real-time water with caustics, reflections, shadows" -->
        <!--     title="Real-time water with caustics, reflections, shadows" -->
        <!--     class="main-page-thumbnail" /> -->
        <!-- <img src="images/original_size/castle_spine_screen_9.png" -->
        <!--     alt="Dragon 2D animation designed in Spine" -->
        <!--     title="Dragon 2D animation designed in Spine" -->
        <!--     class="main-page-thumbnail" /> -->
    </div>
</div>

<?php require_once 'index-wp-news.php'; ?>

<hr>

<img src="images/MacStadium-developerlogo.png" alt="MacStadium developer logo" style="width: 100px; float: left; margin: 1em;" />
<p>We're a happy member of <a href="https://www.macstadium.com/opensource">MacStadium Open Source Developer Program</a>.<br>
They provide us access to a remote Mac machine, we use it to develop our <a href="macos">macOS</a> and <a href="ios">iOS</a> targets.

<?php
castle_footer();
?>
