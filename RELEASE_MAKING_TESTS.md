# Automated and semi-automated tests of Castle Game Engine and tools

The following tests/tasks should be done before release,
to make sure that everything is in top-quality-shape before release.

As much as possible, we run automated tests after every commit,
using both Jenkins and GitHub Actions,
see https://castle-engine.io/jenkins and https://castle-engine.io/github_actions .
So the first step is just to

**Make sure our Jenkins shows OK status for every CGE project.**

## Cleanup repository

Test are there no files with accidental executable attribute:

```
./tools/internal/check_no_accidental_executable
```

Search and remove not needed empty dirs.

```
find-empty-dirs
```

# Basic compilation tests

Test compilation methods from "getting started"
https://castle-engine.io/documentation.php

Check once that most important stuff compiles outside Jenkins too:

```
cd ../castle-engine/
make
make examples
  # Or "make examples-ignore-errors", this compiles everything,
  # instead of stopping on 1st error. Useful to see all errors
  # (instead of potentially retrying "make examples" many times),
  # but you have to watch output (exit status = 0 is meaningless for
  # "make examples-ignore-errors").
make clean # get rid of conflicting files outside Lazarus package output dir
make examples-laz
```

Test some special compilation options:
- ../castle-engine/src/common_includes/castleconf.inc
    - define CASTLE_DISABLE_LIBPNG
    - undefine CASTLE_EMBED_ALL_3D_FONT_VARIATIONS
- ../castle-engine/src/x3d/octreeconf.inc
    - define CONSERVE_TRIANGLE_MEMORY
    - define CONSERVE_TRIANGLE_MEMORY_MORE
For each:
- `make clean && make`
- revert later `castleconf.inc` , `octreeconf.inc`

## Test OpenGLES on desktop

- define OpenGLES in ../castle-engine/src/common_includes/castleconf.inc

- check OpenGLES desktop rendering works on some sample models from view3dscene.

## Test tolerating GPU bugs

- Fake some BuggyXxx vars to be true,
   in ../castle-engine/src/base/opengl/castleglversion.pas
   in particular BuggyVbo being true,
   and check rendering works.

- Test with EnableFixedFunction := true:

    ```
    view3dscene --debug-enable-fixed-function
    ```

## Test fpmake and InstantFPC

Install CGE units following https://castle-engine.io/fpmake , test them.
Test also InstantFPC.

```
# Remember to define `FPCDIR` first, like this:
export FPCDIR=/home/michalis/installed/fpclazarus/current/fpc/lib/fpc/3.2.2/

make test-fpmake
./fpmake --globalunitdir="${FPCDIR}" install
# The CGE installed units should now be known to FPC, no need for any -Fu or @castle-fpc.cfg
ls -Flah /home/michalis/installed/fpclazarus/current/fpc/lib/fpc/3.2.2/units/x86_64-linux/castle-game-engine
fpc -Mobjfpc -Fuexamples/fps_game/code/ examples/fps_game/fps_game.dpr
```

Run the InstantFPC examples in examples/instantfpc/ .

Cleanup:

```
rm -Rf /home/michalis/installed/fpclazarus/current/fpc/lib/fpc/3.2.2/units/x86_64-linux/castle-game-engine
```

## Make sure released zip/tar.gz don't contain any unwanted binary files.

Allowed binaries below:

```
$ make cleanmore
$ find -iname *.jar
./tools/build-tool/data/android/integrated/gradle/wrapper/gradle-wrapper.jar
./tools/build-tool/data/android/base/gradle/wrapper/gradle-wrapper.jar
$ find -iname *.so
./tools/build-tool/data/android/integrated-components/ogg_vorbis/app/src/main/jni/armeabi-v7a/libtremolo.so
./tools/build-tool/data/android/integrated-components/sound/app/src/main/jni/armeabi-v7a/libopenal.so
```

## Android

```
make strip-precompiled-libraries
cd examples/mobile/achievements/
castle-engine --target=android --mode=debug package

# Should use system-wide Gradle, should show a message like
#   Local Gradle wrapper ("gradlew") not found, so we will call the Gradle on $PATH.
#   Make sure you have installed Gradle (e.g. from the Debian "gradle" package), in a version compatible with the Android Gradle plugin (see https://developer.android.com/studio/releases/gradle-plugin.html#updating-gradle ).
```

## Interactive tests

Download for important platforms (esp. Windows that Michalis doesn't use regularly but is often used by users).

- Unpack
- Run editor
- Use Preferences -> Register Lazarus Packages (important, advised in manual)
- Create all templates
  - open designs inside
  - compile F9 with FPC
  - compile F9 with Delphi at least one example
  - click and double-click on some glTF, to make sure preview and view3dscene appear ok
- Build + run some examples
- Open doc/reference/index.html
