# Testing Castle Game Engine before release

## Automated and semi-automated tests of Castle Game Engine and tools

The following tests/tasks should be done before release,
to make sure that everything is in top-quality-shape before release.

As much as possible, we run automated tests after every commit,
using GitHub Actions now.
( Related: see about [using GitHub Actions in your own projects](https://castle-engine.io/github_actions). )
So the first step is just to...

[Make sure our GitHub Actions status is OK](https://github.com/castle-engine/castle-engine/actions).

## Test: Interactive test of basic actions from manual

Idea: make sure that basic actions we recommend on https://castle-engine.io/install work.

Download for important platforms (esp. Windows, that Michalis doesn't use regularly but is often used by users).

- Unpack
- Run editor
- Use Preferences -> Register Lazarus Packages (important, advised in manual)
- Create all templates
    - open designs inside
    - compile F9 with FPC
    - compile F9 with Delphi
    - click and double-click on some glTF, to make sure preview and castle-model-viewer appear ok
- Build + run some examples
- Open doc/reference/index.html

## Test: custom editor building

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
- ../castle-engine/src/scene/octreeconf.inc
    - define `CONSERVE_TRIANGLE_MEMORY`
    - define `CONSERVE_TRIANGLE_MEMORY_MORE`
For each:
- `make clean && make`
- revert later `castleconf.inc` , `octreeconf.inc`

## Test: OpenGLES on desktop works

- define OpenGLES in ../castle-engine/src/common_includes/castleconf.inc

- check OpenGLES desktop rendering works on some demo-models using castle-model-viewer.

## Test: Various OpenGL support cases, including modern and ancient, are supported

- Test `test_rendering_opengl_capabilities`:

    ```
    ../castle-engine/examples/research_special_rendering_methods/test_rendering_opengl_capabilities/
    castle-engine compile
    castle-engine run # test it shows "automatic"
    castle-engine run -- --capabilities=force-modern
    castle-engine run -- --capabilities=force-fixed-function
    ```

    Change `InternalUpgradeGlslVersion` to `false` and retest.
    - with "automatic" it should work.
    - with "force-modern" it *may* (but don't have to) fail (as resulting shaders do not satisfy OpenGL "core").

- Test CGE editor with non-standard rendering (open 3D template):

    ```
    ./castle-editor --capabilities=force-fixed-function
    ./castle-editor --capabilities=force-modern
    ```

- Test `castle-model-viewer` with fixed-function:

    ```
    castle-model-viewer --capabilities=force-fixed-function
    ```

## Test: fpmake installation and InstantFPC

Note: This is known to fail with FPC installed by https://castle-engine.io/fpcupdeluxe .
Use the official installer from
- https://sourceforge.net/projects/freepascal/files/
- like `fpc-3.2.2.x86_64-linux.tar` on https://sourceforge.net/projects/freepascal/files/Linux/3.2.2/

- Michalis-specific setup:
  ```
  # Install to /home/michalis/installed/fpclazarus/fpc322-official/fpc
  # (makes it a bit consistent with fpcupdeluxe installs).
  michalis-fpclazarus-config # should say that adjusts my environment to FPC official
  fpc

  # Remember to define `FPCDIR` first
  export FPCDIR=/home/michalis/installed/fpclazarus/fpc322-official/fpc/lib/fpc/3.2.2/

  # Generate fpc.cfg to point to proper units
  ~/installed/fpclazarus/fpc322-official/fpc/bin/fpcmkcfg -d basepath="$FPCDIR" -o ~/.fpc.cfg
  ```

Note: The completeness of `fpmake.pp` is also automatically tested by our `tools/internal/check_packages/` which is in turn run by CI (GitHub Actions). Still, let's test it manually before release to be sure.

Install CGE units following https://castle-engine.io/fpmake , test them.

```
make test-fpmake
./fpmake --globalunitdir="${FPCDIR}" install
# The CGE installed units should now be known to FPC, no need for any -Fu or @castle-fpc.cfg
ls -Flah $FPCDIR/units/x86_64-linux/castle-game-engine
fpc -Mobjfpc -Fu3d_games/explore_impressive_castle/code/ examples/3d_games/explore_impressive_castle/fps_game.dpr
fpc -Mobjfpc -Sh -Sa -gh -gl -Futests/code/testcases/ -Futests/code/tester-castle/ tests/castle_tester_standalone.dpr
```

Run the InstantFPC examples in examples/instantfpc/ :

```
cd examples/instantfpc/
./castle_color_hex_to_pascal ff00ff
./castle_list_files
./castle_open_dialog
```

Cleanup (this is specific to Michalis setup, adjust as you see fit):

```
rm -Rf $FPCDIR/units/x86_64-linux/castle-game-engine
mv ~/.fpc.cfg /home/michalis/installed/fpclazarus/fpc322-official/moved.fpc.cfg
set_fpclazarus_current ...
michalis-fpclazarus-config # should say that adjusts my environment to FPC from fpcupdeluxe
fpc
```

## Test: Released zip/tar.gz should not contain any unwanted binary files

Allowed binaries below:

```
$ make clean
$ find -iname *.jar
./tools/build-tool/data/android/integrated/gradle/wrapper/gradle-wrapper.jar
$ find -iname *.so
./tools/build-tool/data/android/services/.../app/src/main/jniLibs/.../lib*.so
# should show 4x libs for services that rely on them, like freetype, png, ogg_vorbis.
```

## RESIGN FOR NOW: Test: Using system-provided Gradle instead of bundled (Android)

This *will fail* right now on Debian or Ubuntu, that contain too old Gradle,

- see [Debian Gradle version](https://packages.debian.org/sid/gradle).
- see [Ubuntu Gradle version](https://packages.ubuntu.com/oracular/gradle).

It is *expected* that procedure below will answer something like "Minimum supported Gradle version is 6.5". There's no solution -- we just need newer Gradle in Linux distros like Debian/Ubuntu. (We cannot downgrade our Android toolchain to downgrade our Gradle requirements, as our Android toolchain version is really necessary -- we upgrade it to fix other building problems and be able to target latest Android SDKs, required in turn by latest Google Play.)

```
sudo apt install gradle

make strip-precompiled-libraries
cd examples/mobile/game_services/
castle-engine --target=android --mode=debug package --fast

# Should use system-wide Gradle, should show a message like
#   Local Gradle wrapper ("gradlew") not found, so we will call the Gradle on $PATH.
#   Make sure you have installed Gradle (e.g. from the Debian "gradle" package), in a version compatible with the Android Gradle plugin (see https://developer.android.com/studio/releases/gradle-plugin.html#updating-gradle ).

sudo apt purge gradle # if you installed gradle just for this test
sudo apt --purge autoremove # if you didn't have anything else not pulled by gradle deps
```

## Test: Before castle-model-viewer release, make sure everything auto-generated is up-to-date and respective make targets work

```
cd castle-model-viewer/
make clean-code
make generate-code
```

## Test: Interactive test in Delphi (compilation using DPROJ)

* Remove all paths in Delphi settings that are added by our packages ("Tools -> Options" menu item, then "Language -> Delphi -> Library" panel, edit "Library path").
* [Compile and install all packages in Delphi](https://castle-engine.io/delphi_packages)
* Compile examples/delphi/vcl in Delphi (interactively, so using DPROJ)
    * Win32
    * Win64
* Compile examples/delphi/fmx in Delphi (interactively, so using DPROJ)
    * Win32
    * Win64
    * [Linux64](https://castle-engine.io/delphi_linux)
* Compile `examples/3d_games/explore_impressive_castle` in Delphi (interactively, so using DPROJ)
    * Win32
    * Win64
    * [Linux64](https://castle-engine.io/delphi_linux)
* Create a new FMX project with `TCastleControl`.
    * Compiling it should initially fail.
    * Follow https://castle-engine.io/delphi_packages to enable CGE paths for all projects, in Delphi settings.
    * Now compiling new project using `TCastleControl` should work.
* Retest compiling `examples/delphi/vcl`, `examples/3d_games/explore_impressive_castle` once CGE paths are present in Delphi, to be sure it's all OK.

## Test: Lazarus Project Group

Has to be used interactively. Open and compile `most_important_lazarus_packages_and_tools.lpg` in Lazarus IDE.

Install first lazprojectgroups in Lazarus IDE: https://wiki.freepascal.org/Project_Groups .

## Test: VS Code extension, with CGE installed from Windows installer, with bundled FPC

Make sure no CGE / FPC is installed (or visible on env variables) on the system.

Install new CGE using Windows installer.

Make sure VS Code extension, if only you give CGE path in settings, works -- has code completion, knows FPC and CGE units.

## Analyze can we remove some unused data from examples, to make downloads smaller

```
cd /tmp/
find "${CASTLE_ENGINE_PATH}/examples/" \
  '(' -iname CastleEngineManifest.xml ')' -and \
  '(' -execdir bash -c 'echo `pwd`' ';' ')' -and \
  '(' -execdir bash -c 'castle-engine unused-data' ';' ')' > unused-data.txt
```
