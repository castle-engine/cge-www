# Automated and semi-automated tests of Castle Game Engine and tools

The following tests/tasks should be done before release,
to make sure that everything is in top-quality-shape before release.

As much as possible, we run automated tests after every commit,
using `Jenkinsfile*` instructions in our repositories and our Jenkins,
see https://castle-engine.io/cloud_builds_jenkins .
So the first step is just to

**Make sure our Jenkins shows OK status for every CGE project.**

## Cleanup repository

Test are there no files with accidental executable attribute
(may happen if commiting from FAT filesystems on Unix):

```
cd ../castle-engine/
find . '(' -type d -name .git -prune -false ')' -or \
       '(' -type f '(' -iname '*.sh' -or -iname '*~' -or -iname 'gradlew' ')' ')' -or \
       '(' -executable -type f -print ')'
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

## Test fpmake

Remember to define `FPCDIR` first, like export FPCDIR=/home/michalis/installed/fpc/current/lib/fpc/3.0.2/

See

```
make test-fpmake
./fpmake --globalunitdir="${FPCDIR}" install
# The CGE installed units should now be known to FPC, no need for any -Fu or @castle-fpc.cfg
fpc -Mobjfpc examples/fps_game/fps_game.lpr
```

Install CGE units following https://castle-engine.io/fpmake

Run the 2 InstantFPC examples.

# Test dependencies

Run `../cge-scripts/check_units_dependencies`

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

### Android

```
make strip-precompiled-libraries
cd examples/mobile/achievements/
make android

# Should use system-wide Gradle, should show a message like
#   Local Gradle wrapper ("gradlew") not found, so we will call the Gradle on $PATH.
#   Make sure you have installed Gradle (e.g. from the Debian "gradle" package), in a version compatible with the Android Gradle plugin (see https://developer.android.com/studio/releases/gradle-plugin.html#updating-gradle ).
```
