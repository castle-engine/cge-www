# Testing Castle Game Engine before release

## Automated and semi-automated tests of Castle Game Engine and tools

The following tests/tasks should be done before release,
to make sure that everything is in top-quality-shape before release.

As much as possible, we run automated tests after every commit,
using both Jenkins and GitHub Actions,
see https://castle-engine.io/jenkins and https://castle-engine.io/github_actions .
So the first step is just to...

**Make sure our Jenkins shows OK status for every CGE project.**

## Test: Interactive test of basic actions from manual

Idea: make sure that basic actions we recommend on https://castle-engine.io/install work.

Download for important platforms (esp. Windows, that Michalis doesn't use regularly but is often used by users).

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

## Test: custom editor building

```
cd ~/sources/castle-engine/castle-engine/tools/build-tool/data
./custom_editor_template_rebuild.sh
git status # should show no modification
```

Open in CGE editor `examples/advanced_editor/custom_component`, allow to rebuild custom editor,
make sure custom editor defines project-specific component.

## Test: Repository doesn't contain anything unwanted

Test are there no files with accidental executable attribute:

```
./tools/internal/check_no_accidental_executable
```

Search and remove not needed empty dirs.

```
find-empty-dirs
```

## Test: Some obscure compilation options work OK

Test some special compilation options:
- ../castle-engine/src/common_includes/castleconf.inc
    - define `CASTLE_DISABLE_LIBPNG`
    - undefine `CASTLE_EMBED_ALL_3D_FONT_VARIATIONS`
- ../castle-engine/src/x3d/octreeconf.inc
    - define `CONSERVE_TRIANGLE_MEMORY`
    - define `CONSERVE_TRIANGLE_MEMORY_MORE`
For each:
- `make clean && make`
- revert later `castleconf.inc` , `octreeconf.inc`

## Test: OpenGLES on desktop works

- define OpenGLES in ../castle-engine/src/common_includes/castleconf.inc

- check OpenGLES desktop rendering works on some demo-models using view3dscene.

## Test: Various OpenGL support cases, including modern and ancient, are supported

- Test `test_rendering_opengl_capabilities`:

    ```
    ../castle-engine/examples/research_special_rendering_methods/test_rendering_opengl_capabilities/
    castle-engine compile
    castle-engine run # test it shows "automatic"
    castle-engine run -- --capabilities=force-modern
    castle-engine run -- --capabilities=force-fixed-function
    ```

    Change `InternalUpgradeGlslVersion` to `false` and retest. "automatic" should work. "force-modern" can fail (as resulting shaders do not satisfy OpenGL "core").

- Test CGE editor with non-standard rendering (open 3D template):

    ```
    ./castle-editor --capabilities=force-fixed-function
    ./castle-editor --capabilities=force-modern
    ```

- Test `view3dscene` with fixed-function:

    ```
    view3dscene --debug-fixed-function
    ```

## Test: fpmake installation and InstantFPC

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

## Test: Released zip/tar.gz should not contain any unwanted binary files

Allowed binaries below:

```
$ make cleanmore
$ find -iname *.jar
./tools/build-tool/data/android/integrated/gradle/wrapper/gradle-wrapper.jar
./tools/build-tool/data/android/base/gradle/wrapper/gradle-wrapper.jar
$ find -iname *.so
./tools/build-tool/data/android/integrated-services/.../app/src/main/jniLibs/.../lib*.so
# should show 4x libs for services that rely on them, like sound, freetype.
```

## Test: Using system-provided Gradle instead of bundled (Android)

This *will fail* right now on Debian, that contains too old Gradle,
see https://packages.debian.org/sid/gradle .
It is *expected* it will answer something like "Minimum supported Gradle version is 6.5".

```
sudo apt install gradle

make strip-precompiled-libraries
cd examples/mobile/achievements/
castle-engine --target=android --mode=debug package --fast

# Should use system-wide Gradle, should show a message like
#   Local Gradle wrapper ("gradlew") not found, so we will call the Gradle on $PATH.
#   Make sure you have installed Gradle (e.g. from the Debian "gradle" package), in a version compatible with the Android Gradle plugin (see https://developer.android.com/studio/releases/gradle-plugin.html#updating-gradle ).

sudo apt purge gradle # if you installed gradle just for this test
sudo apt --purge autoremove # if you didn't have anything else not pulled by gradle deps
```

## Test: Before view3dscene release, make sure everything auto-generated is up-to-date and respective make targets work

```
cd view3dscene/
make clean-code
make generate-code
```
