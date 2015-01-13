<?php

array_push($news,
    array('title' => 'Castle Game Engine 5.1.2 release (fixes to TCastleControl, tutorial, more)',
          'year' => 2015,
          'month' => 1,
          'day' => 11,
          'short_description' => '',
          'guid' => '2015-01-11',
          'description' =>
castle_thumbs(array(
  array('filename' => 'android_emulator.png', 'titlealt' => '&quot;Dragon Spine&quot; running in an Android emulator'),
)) .
'<p>Castle Game Engine version 5.1.2 was just released! This release brings a couple of fixes and improvements to the engine.

<div class="download jumbotron">
<a href="' . CURRENT_URL . 'engine.php" class="btn btn-primary btn-lg">Download Castle Game Engine 5.1.2!</a>
</div>

<ol>
  <li>New useful events are published on <code>TCastleControl</code>:

<pre>
OnOpen: TNotifyEvent
OnClose: TNotifyEvent
OnBeforeRender: TNotifyEvent
OnRender: TNotifyEvent
OnResize: TNotifyEvent
OnPress: TControlInputPressReleaseEvent
OnRelease: TControlInputPressReleaseEvent
OnMotion: TControlInputMotionEvent
OnUpdate: TNotifyEvent
</pre>

     <p>These should be used to watch open/close of context, to watch key/mouse events and to perform continous updates. For detailed documentation of them, see <a href="http://castle-engine.sourceforge.net/apidoc/html/CastleControl.TCastleControlCustom.html">TCastleControlCustom</a> reference.</p>

    <p>Some previously published stuff on <code>TCastleControl</code> is deprecated now and will be removed in next release (sorry, we have to break compatibility &mdash; this old stuff really doesn\'t make much sense, all new code should use new events). Also, the deprecated <code>OnPaint</code> was fixed &mdash; just in case you\'re using it (but please switch to using <code>OnRender</code> soon!).

  <li><p>The beginning of the <a href="http://castle-engine.sourceforge.net/tutorial_intro.php">tutorial</a> was much streamlined. We fixed some wording, removed a lot of useless info, and made the beginning of the tutorial really smooth. Now, you really quickly get to the <i>"I made a 3D game!"</i> stage :)

    <p>Also, the tutorial chapter <a href="http://castle-engine.sourceforge.net/tutorial_castle_scene_transform.php">"Adding a simple moving object" was added</a>.

  <li>Also, as you probaly see, the website got a total facelift, using HTML5 and Bootstrap style. Various pages and menus were rearranged to be more helpful. I hope you enjoy the new website &mdash; please leave a note in the comments :)

  <li>You can save almost 0.7 MB from exe size by undefining CASTLE_EMBED_ALL_3D_FONT_VARIATIONS in some cases, see src/base/castleconf.inc for comments.

  <li>Various testsuite fixes and improvements.

  <li>Improve CastleCurves API, and also mark most of it as deprecated.
</ol>
')
);
