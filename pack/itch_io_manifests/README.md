This directory contains manifests to upload our applications to itch.io:
- https://castle-engine.itch.io/castle-game-engine/
- https://castle-engine.itch.io/view3dscene/

See itch.io docs about manifests and butler:
- https://itch.io/docs/itch/integrating/manifest.html
- https://itch.io/docs/itch/integrating/manifest-actions.html

Note: using a single manifest file for all the platforms, and filtering "play" action per platform,
doesn't work for me for unknown reason.
On Linux, it persists in trying to use Windows command with ".exe".
And I triple-checked that I didn't mix them in the manifest.
For now, we use separate manifests for Windows and Linux then.
