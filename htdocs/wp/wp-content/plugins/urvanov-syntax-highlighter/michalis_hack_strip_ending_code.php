<?php

/* Michalis fix:
It seems that combination of Jetpack Markdown processing + this plugin
is not perfect. At the end, we get newline + </code>, which is stripped by
this plugin (urvanov-...) but additional newline remains.
This additional newline is clearly visible in the output, esp. as the
plugin adds line numbers, so e.g. I want to show 2-line code

```delphi
Controllers.Initialize;
MyWalkNavigation.UseGameController;
```

on https://castle-engine.io/wp/2025/06/08/game-controllers-gamepads-joysticks-new-comfortable-api-and-example-of-walking-and-talking-using-gamepad-from-delphi-summit-2025/

... but without this fix, this would be rendered as 3 lines,
3rd line empty (but taking space, and numbered 3).

Another testcase, with non-Pascal formatting:
https://castle-engine.io/wp/2025/05/24/web-target-progress-editor-on-web-proof-of-concept-openurl-right-mouse-button-many-new-demos-creature-ai-3d-model-viewer-physics-shooting-auto-tests-kraft-and-vampyre-imaging-on-web-wa/

Note: No need to strip initial <code...>, let plugin do it by
"Remove <code> tags surrounding the shortcode content" setting on
https://castle-engine.io/wp/wp-admin/options-general.php?page=urvanov_syntax_highlighter_settings

//$content = preg_replace('/^<code>/i', '', $content);
//$content = preg_replace('/^<code class="language-[a-zA-Z0-9_]+">/i', '', $content);

We only strip ending newline + </code> better:
*/

$content = preg_replace('|\n</code>|i', '', $content);
