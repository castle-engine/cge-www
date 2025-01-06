We are proud to present the first 3 working applications developed with _Castle Game Engine_ for the web! Check them out -- just open the links below, in any www browser (Firefox, Chrome...), on desktop or mobile!

1. [3D viewport, with random cones, animated spot light, dropping boxes with physics, Examine camera navigation](https://castle-engine.io/web-demos/simplest_viewport).

2. [2D "Invaders" game - simple game, using keys, multiple views, 2 difficulty modes](https://castle-engine.io/web-demos/simplest_invaders). Note: This example is not really useful on mobile, just because it relies on the keyboard input.

3. [The first, simplest example that we got running on the web!](https://castle-engine.io/web-demos/simplest)

How is this possible?

- We use the WebAssembly target available in [FPC](https://www.freepascal.org/) and we have a "glue" code generated using [Pas2JS](https://getpas2js.freepascal.org/). We render using WebGL (1.0, with 2.0 features useful but optional), using the [Castle Game Engine](https://castle-engine.io/) code. The game code is cross-platform (the 3 examples above can be recompiled to desktop, mobile, console without any change). The [cgeref id=TCastleWindow] abstracts all platform differences providing rendering and handling input (key, mouse, touch).

- This is not yet available in the CGE `master` branch, not yet available in the downloads! The development is on a branch [webassm_platformer_test](https://github.com/castle-engine/castle-engine/tree/webassm_platformer_test). It should be merged to `master` soon.

- We have a ton of documentation what works already, how it works, what is missing, and what do we plan on the [web platform page](https://castle-engine.io/web). This was quite intensitively updated in the last 2 weeks, so check it out!

Do you like what we do? Spread the word to your friends and [support us on Patreon](https://www.patreon.com/castleengine)! And have fun developing games with our open-source game engine :)

