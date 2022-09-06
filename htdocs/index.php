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
  'social_share_image' => CASTLE_PROD_URL . 'images/combined_cge_logo_game.png',
));
?>

<!--h1 class="main-title">Castle Game Engine</h1-->

<?php
/* Tagline / slogan - super-important quick description of engine.

- Put our tagline in h2, maybe better for SEO.

- The tagline could have links, but some of these links could them "hijack" user's attention.
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

<div class="row">
    <div class="col-sm-4">
        <a class="btn btn-primary btn-lg main-page-action" href="download"><span class="glyphicon glyphicon-download" aria-hidden="true"></span> Download</a>
    </div>
    <div class="col-sm-4">
        <a class="btn btn-primary btn-lg btn-info main-page-action" href="install">Read the manual</a>
        <a class="btn btn-primary btn-lg btn-info main-page-action" href="https://www.youtube.com/watch?v=rPU-IFltcuM">Watch the tutorial</a>
    </div>
    <div class="col-sm-4">
        <a class="btn btn-success btn-lg btn-patreon main-page-action" href="<?php echo PATREON_URL; ?>"><span class="glyphicon glyphicon-heart" aria-hidden="true"></span> Support us on Patreon</a>
    </div>
</div>

<h1 class="main-page-header">Comfortable visual designer and powerful code</h1>

<?php echo cge_features_summary(); ?>

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
