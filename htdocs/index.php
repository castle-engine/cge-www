<?php
// define('CASTLE_GITHUB_NAME', 'castle-engine');

require_once 'castle_engine_functions.php';

global $main_page;
$main_page = true;
global $site_title;
$site_title = NULL; // set to NULL, to disable appending to <title>; main page has special title that already includes CGE

castle_header('Castle Game Engine - Free open-source cross-platform 3D 2D game engine with editor and powerful Pascal API', array(
  'meta_description' => 'Free open-source 3D and 2D game engine. Cross-platform, for desktops (Windows, Linux, macOS, FreeBSD...), mobile (Android, iOS), console (Nintendo Switch). Powerful visual editor. Powerful API for devs. Supports a lot of model formats, like glTF, X3D and Spine. Many rendering features (physically based rendering, shadows, mirrors, gamma correction). Fast code compilation and execution using modern Pascal.',
  /* Not useful for Google anymore, but maybe other search engines use them. */
  'meta_keywords' => 'game engine, glTF, X3D, Spine, Pascal, cross platform, editor, Nintendo Switch, Linux, Windows, macOS, Android, iOS, PBR, shadows, shaders',
  'canonical_url' => CASTLE_PROD_URL,
  'social_share_image' => CASTLE_PROD_URL . 'images/castle_game_engine_icon.png',
));

define('CGE_SNAPSHOTS_BASE', 'https://github.com/castle-engine/castle-engine/releases/download/snapshot/');
define('CGE_SNAPSHOTS_VERSION', '7.0-alpha.snapshot');
?>

<div class="row">
    <div class="col-sm-8">
        <div class="centered-wrapper">
            <div class="centered">
                <h1 class="main-title">Castle Game Engine</h1>

                <?php
                /* Tagline / slogan - super-important quick description of engine.

                - Put our tagline in h2, maybe better for SEO.

                - The tagline could has linked, but some of these links could them "hijack" user's attention.
                  Links will be later.

                  Another old version:

                  <a href="https://github.com/castle-engine/castle-engine/blob/master/COPYING.md">Open-source</a>
                  cross-platform 3D / 2D game engine using
                  modern Object Pascal</a>
                  and open standards like glTF and X3D</a>

                - Another old version:
                  Free open-source game engine for <a href="http://www.freepascal.org/">FreePascal and Lazarus</a>. Excellent support for many 3D and 2D data formats, portable (desktops, Android, iOS...), many advanced graphic effects, comfortable API.
                */ ?>
                <h2 class="main-subtitle">Cross-platform (desktop, mobile, console) 3D and 2D game engine. Powerful visual editor. Support for glTF, X3D, Spine and more. Fast clean code using modern Pascal. Free and open-source.</h2>
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
                    <p>Next: <a href="install">Install the engine and build your first application</a>.
                    <p>If you want to learn by watching: <a href="https://www.youtube.com/watch?v=rPU-IFltcuM">watch a tutorial to the engine</a>.
                </div>

                <?php echo download_donate_footer(); ?>
            </div>
        </div>
    </div>
    <div class="col-sm-4">
        <!-- iframe class="hidden-xs main-page-thumbnail" width="560" height="315" src="https://www.youtube.com/embed/o5q7guVkYVo" frameborder="0" allowfullscreen></iframe -->

        <img src="images/main_page_sidebar/dragon_large.webp"
            alt="2D animation, designed in Spine"
            title="2D animation, designed in Spine"
            class="main-page-thumbnail hidden-xs"
            <?php echo _castle_image_sizes('images/main_page_sidebar/dragon_large.webp'); ?>
        />
        <br>

        <?php /*
        <a href="images/original_size/dragon_editor.png"
            title="2D game, designed in Castle Game Engine editor"
            class="screenshot">
            <img src="images/main_page_sidebar/dragon_editor.webp"
                alt="2D game, designed in Castle Game Engine editor"
                style="padding: 1em; border: none"
                class="main-page-thumbnail hidden-xs"
                < ?php echo _castle_image_sizes('images/main_page_sidebar/dragon_editor.webp'); ? >
            />
        </a>

        <div class="jumbotron alert alert-info">
            <p><a href="https://discord.gg/GYgKs6f7?event=982744972476432384" role="button">
              Register for 3rd Open Meeting for CGE Users and Developers:<br>
              <span style="text-decoration: underline;">September 17th (2022)</span>
              </a>
            </p>

            < ?php echo cgeImg('block', array(
              array('filename' => 'meet_2_michalis.png', 'titlealt' => 'Michalis on 2nd Open Meeting'),
            ));
            ? >
        </div>
        */ ?>

        <?php /*
        <img src="images/original_size/rhan_shrine_5_everything.png"
            alt="Bump mapping and shadow maps from multiple light sources"
            title="Bump mapping and shadow maps from multiple light sources"
            class="main-page-thumbnail"
            style="margin-bottom: 0.5em" />
        <img src="images/original_size/barna29_nice_shadows.png"
            alt="Real-time water with caustics, reflections, shadows"
            title="Real-time water with caustics, reflections, shadows"
            class="main-page-thumbnail" />
        <img src="images/original_size/castle_spine_screen_9.png"
            alt="Dragon 2D animation designed in Spine"
            title="Dragon 2D animation designed in Spine"
            class="main-page-thumbnail" />
        */ ?>
    </div>
</div>

<h1 class="main-page-header">Comfortable visual designer, powerful code execution:</h1>

<?php echo cge_features_summary('center'); ?>

<?php require_once 'index-wp-news.php'; ?>

<hr>

<img src="images/MacStadium-developerlogo.png"
     alt="MacStadium developer logo"
     style="width: 100px; height: auto; float: left; margin: 1em;"
     <?php echo _castle_image_sizes('images/MacStadium-developerlogo.png'); ?>
 />
<p>We're a happy member of <a href="https://www.macstadium.com/opensource">MacStadium Open Source Developer Program</a>.<br>
They provide us access to a remote Mac machine, we use it to develop our <a href="macos">macOS</a> and <a href="ios">iOS</a> targets.

<?php
castle_footer();
?>
