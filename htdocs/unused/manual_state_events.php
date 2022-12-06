<?php /*
<div class="jumbotron">
<p><span class="label label-warning">Warning</span> This manual page uses features available only in the <b>unstable <a href="https://github.com/castle-engine/castle-engine">engine version on GitHub</a></b>. Do not read this if you use the <b>stable engine version</b> (downloaded as zip or tar.gz from our pages), or be prepared to make some modifications.

<p>In particular, in the stable engine version, the <code>TDrawableImage</code> class is a little more difficult to use. It needs to be created / destroyed in <code>OnGLContextOpen</code> / <code>OnGLContextClose</code>. <a href="manual_player_2d_controls.php">Details are explained here</a>.
</div>
*/ ?>

<p>Before we dive into full-featured viewports, scenes and 3D,
let's take a quick look at the simple things you can do with our window.
Let's draw some images and handle inputs.

<div class="panel panel-primary">
  <div class="panel-heading">TODO: Outdated manual page</div>
  <div class="panel-body">
    <p>Admittedly this manual page is outdated.
    It shows a way that <i>still works</i> but it is no longer what we advise
    as a most comfortable way to do such things in the engine.
    Have patience as we update our documentation, and in the meantime:

    <p>We advise you now to:</p>

    <ol>
      <li>
        <p>Create a <i>"New Project"</i> in <a href="manual_editor.php">CGE editor</a> and start with the <i>Empty</i> template.
      <li>
        <p>It has a code in <code>code/gamestatemain.pas</code> that shows how to handle keys in <code>TStateMain.Press</code>
          and do something continuously in <code>TStateMain.Update</code>.
      <li>
        <p>You can render an image directly (as described in this manual page, using
          <?php echo cgeRef('TDrawableImage'); ?>)
          by overriding <code>TStateMain.Render</code>.
          Alternatively, you could design (using CGE editor) an image as <code>TCastleImageControl</code>
          and only move it.
    </ol>
  </div>
</div>
------------------------------------------------------------------------------
<p>If you want to go more into the direction of 2D games:

<ul>
  <li><p>See the <?php echo a_href_page('manual about drawing your own 2D controls', 'manual_2d_ui_custom_drawn'); ?>. It has a nice overview of 2D drawing capabilities. You can also use the <?php echo a_href_page('standard 2D controls', 'manual_2d_user_interface'); ?> with a lot of ready functionality.

    <p>It also shows a more flexible way to handle drawing and inputs, by creating new descendants of <?php echo cgeRef('TCastleUserInterface'); ?> (instead of simply attaching to window callbacks).

  <li><p>If you want to use smooth and efficient animations, you can load a 2D model (and animation) from <a href="creating_data_model_formats.php">any supported format (like X3D or glTF or Spine)</a>. To do this:

    <ol>
      <li>Create a <?php echo cgeRef('TCastleViewport'); ?>.
      <li>Call <?php echo cgeRef('TCastleViewport.Setup2D'); ?>.
      <li>Create <?php echo cgeRef('TCastleScene'); ?> instance.
      <li>Call <?php echo cgeRef('TCastleScene.Setup2D'); ?>.
    </ol>

    <p>The following manual chapters focus on <?php echo cgeRef('TCastleScene'); ?> usage, and apply for both 3D and 2D games.

    <p>See the example code <code>castle_game_engine/examples/2d_dragon_spine_android_game/</code> inside the engine.

  <li><p>You can also make inputs user-configurable. To do this, wrap each input in a <?php echo cgeRef('TInputShortcut'); ?> instance. This will store whether the input is a key press or a mouse click, and you can check and change it at runtime. More information is in <?php echo a_href_page('manual about key / mouse shortcuts', 'manual_key_mouse'); ?>.
</ul>
