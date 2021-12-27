Together with the engine we include a tool "castle-engine" to help with building and packaging your programs for various platforms (standalone, mobile, web browser...).

* [Quick installation instructions](#quick-installation-instructions)
* [Quick usage instructions](#quick-usage-instructions)
* [Creating and using the manifest file for your projects](#creating-and-using-the-manifest-file-for-your-projects)
* [Commands supported by the castle-engine tool](#commands-supported-by-the-castle-engine-tool)
  * [create-manifest](#create-manifest)
  * [compile](#compile)
  * [package](#package)
  * [install](#install)
  * [run](#run)
  * [package-source](#package-source)
  * [clean](#clean)
  * [simple-compile](#simple-compile)
  * [auto-generate-textures](#auto-generate-textures)
  * [auto-generate-clean](#auto-generate-clean)
  * [generate-program](#generate-program)
  * [editor](#editor)

Quick installation instructions
====

1. **Download** [Castle Game Engine](https://castle-engine.io/)

2. **It is simplest to just download the binary release of Castle Game Engine**, then the build tool executable (called `castle-engine`) is already available in the `bin` subdirectory. There's nothing left to do -- it will be automatically used by the CGE editor, just open (or create new) any CGE project and build.

3. **If you get Castle Game Engine as only source code (e.g. from GitHub)** then compile the build tool yourself:

    - By command-line (only FPC is required for this, no need for full Lazarus): 
        ```
        cd .../castle_game_engine/tools/build-tool/
        ./castle-engine_compile.sh
        ```
    - Alternatively, by Lazarus:
      1. Open in Lazarus the package <tt>castle_game_engine/packages/castle_base.lpk</tt> and press "Compile" button in the package window.
      2. Then open in Lazarus the project <tt>castle_game_engine/tools/build-tool/castle-engine.lpi</tt> and use "Compile" command (in the "Run" menu).

4. **(Optional) Adjust the environment variable `$PATH` (only necessary for compilation without CGE editor)**

    - Add to the environment variable `$PATH` the directory where `fpc` executable is. You need to do this, to allow the _build tool_ to find FPC, which is necessary to compile applications.
    - You may also add to `$PATH` the directory where `lazarus` and `lazbuild` are. This is only necessary for some operations (build editor with custom controls, see `castle-engine editor`, for now).
    - You may also add to `$PATH` the directory where `castle-engine` is. This is just for your comfort, to be able to execute <tt>castle-engine</tt> easily.

    If you don't know how to set the environment variable, search the Internet (e.g. <a href="https://www.computerhope.com/issues/ch000549.htm">these are quick instructions how to do it on various Windows versions</a>).

    Note that if you use [Castle Game Engine Editor](https://castle-engine.io/manual_editor.php) to compile your projects, then you don't need to adjust any environment variables. So if this sounds hard for you, just use the visual [Castle Game Engine Editor](https://castle-engine.io/manual_editor.php) instead of the command-line build tool.

4. **(Optional) Define environment variable `$CASTLE_ENGINE_PATH` (only necessary for compilation without CGE editor)**. To compile projects using the build tool, it must be able to find the engine.

    You can define an environment variable `$CASTLE_ENGINE_PATH` to indicate a directory that contains <tt>castle_game_engine</tt> or <tt>castle-engine</tt> directory (with engine sources). This way sources of our engine will be automatically used (and recompiled when necessary, which is also cool for developing engine modifications). This step is optional, the engine path will be automatically detected anyway if you use an official release from our webpage.

<!--
Make sure that the build tool "data" is installed correctly --- on Windows is should be alongside the castle-engine.exe file, on Unix is can be in system-wide location <tt>/usr/local/share/castle-engine</tt> or <tt>/usr/share/castle-engine</tt> .
-->


Quick usage instructions
====

Open a terminal (command-line), and enter the directory containing <tt>CastleEngineManifest.xml</tt> file. This is the main project directory.

Run this command to compile the project (for the current operating system and processor):

~~~~
castle-engine compile
~~~~

Run this to compile and also package (again, for the current operating system and processor):

~~~~
castle-engine package
~~~~

By default we compile in release mode. Use option <code>--mode=debug</code> to compile a debug version (slower, but more friendly for various debuggers).

That's it, you can now use our "build tool" to easily compile and package your games for various platforms:) Read on to learn about more useful commands of the build tool.


Creating and using the manifest file for your projects
====

Create a <tt>CastleEngineManifest.xml</tt> file in your project's directory. See the link:pass:[CastleEngineManifest.xml examples][CastleEngineManifest.xml examples] wiki page for samples and documentation. You can create an trivial starting <tt>CastleEngineManifest.xml</tt> file by executing <tt>castle-engine create-manifest</tt>. The **manifest file describes your project's name, source code**, what files to include in the package and such.

Then you **call <tt>castle-engine</tt> from within your project's directory** (or any subdirectory, we automatically look for <tt>CastleEngineManifest.xml</tt> in parent directories) **to quickly compile, package and clean the project**. See the description of command-line parameters to <tt>castle-engine</tt> below. The tool is integrated with our engine, so it can automatically compile the code correctly, and package it following our usual conventions. The data for the game is automatically packaged. On Windows the required DLL files are automatically included (see also the description of &lt;dependencies&gt; in link:pass:[CastleEngineManifest.xml examples][CastleEngineManifest.xml]). On Android the required Java wrappers and libraries are automatically used.

Special file/directory names:

 * The <code>data</code> subdirectory of the project, if found, is used for game data. It is automatically included in the game package for all platforms. It is accessible at runtime using URLs like `castle-data:/xxx.png` function (see the <a href="https://castle-engine.io/manual_data_directory.php">manual about data directory</a>).

    We filter out some common development files like <tt>\*.xcf</tt> and <tt>\*.blend\*</tt> from the data. See also &lt;include&gt; and &lt;exclude&gt; directives (in link:pass:[CastleEngineManifest.xml examples][CastleEngineManifest.xml]) to add/remove specific files from the package. These allow to add/remove files both inside and outside of data.

 * The <code>data/material_properties.xml</code> file, if found, is assumed to be used for [material properties configuration](https://castle-engine.io/creating_data_material_properties.php). The subcommand <tt>castle-engine auto-generate-textures</tt> will use it to know which textures should be compressed and downscaled. At runtime, load this file to MaterialProperties.URL to actually use the compressed and downscaled versions.

Note that *using the build tool is optional*. The [Castle Game Engine](https://castle-engine.io/) is just a set of Object Pascal units. You can use them in your programs, and compile/package the final program/library however you like. You can directly use Lazarus or command-line FPC to compile your game, and it's a sufficient approach when you develop a standalone game --- since the compiler generates a working executable that you simply run. And you can distribute it however you like.

However, using the build tool is nice to automate some tasks. For example, in case of Android and iOS, "building and packaging" is not a trivial process --- after compiling there are some steps you have to follow to get a final package that you can distribute to install/run your program. The build tool does it for you automatically, **for example it can create fully-working Android apk/aab, you only provide your Pascal source code**.

Main advantages of using the build tool:

* It can trivially easy package your game for Android. Once your code compiles for Android, packaging it to a production-ready file (APK or AAB) file (that you can distribute freely, e.g. upload on Google Play) is trivial. Just call <code>castle-engine package --target=android</code>. <!-- It can also package to a debuggable apk, that can be inspected with debuggers based on "gdb". Commented out, ndk-gdb is not 100% reliable anymore. -->
* It can trivially easy package your game for iOS. Just call <code>castle-engine package --target=ios</code>, and then open the created project in Xcode (to run it in simulator, publish...). You can also use `--package-format=ios-archive-xxx` arguments to create IPA file.
* It also takes care of resources (with icon, version information, manifest) on Windows.
* It can also compile and package your game for desktop operating systems like Linux and Windows. This is comfortable, making sure that the same compilation options and the same packaging rules (what to include / what to exclude) are used when packaging your game for all targets.

Commands supported by the castle-engine tool
====

create-manifest
----

Create link:pass:[CastleEngineManifest.xml examples][CastleEngineManifest.xml] file if it does not exist yet, guessing the project name based on directory name. You can use this <tt>CastleEngineManifest.xml</tt> as a starting point.

compile
----

Compile the project, with the syntax options and optimizations suggested for programs using our engine. 

By default we compile for your current OS (operating system) and processor, so if you're on 32-bit Windows -> you will compile a 32-bit Windows binary, if you're on 64-bit Linux -> you will compile a 64-bit Linux binary and so on. You can use <code>--os</code> and/or <code>--cpu</code> options to cross-compile. Underneath, proper cross-compilation options will be passed to FPC.

For example:

* Call `castle-engine compile --cpu=x86_64` to compile a 64-bit version for the current operating system.
* Call `castle-engine compile --os=linux --cpu=x86_64` to compile a 64-bit version for Linux.
* Windows is a little weird (due to historical conventions beyond FPC), and you have to use `castle-engine compile --os=win64 --cpu=x86_64` (thus, you request 64-bit "twice" in the command-line) to get a 64-bit version on Windows. Use `castle-engine compile --os=win32 --cpu=i386` to get a 32-bit executable for Windows.

Instead of <code>--os</code> and/or <code>--cpu</code> options, you can also use <code>--target</code>. A <i>target</i> is a collection of OS and CPU combinations that typically are distributed together. Right now, these targets are available:

1. <code>custom</code> (the default target), which means that we look at <code>--os</code> and/or <code>--cpu</code> options, and compile for this single OS/CPU.

2. <code>android</code>, which consists of 2 combinations of OS/CPU: Android on ARM (32-bit devices) and Android on Aarch64 (64-bit devices, only if your FPC is capable of compiling to Android/Aarch64 -- FPC 3.3.1 is necessary).

3. <code>ios</code>. By default this consists of 2 combinations of OS/CPU, to include 32-bit and 64-bit iOS devices. Add the `--ios-simulator` option to include 2 more combinations of OS/CPU to include also support for the iOS simulator. See link:pass:[iOS][iOS] to learn more.

4. <code>nintendo-switch</code>, which builds an application for link:pass:[Nintendo Switch][].

Use <code>--mode=debug</code> or <code>--mode=release</code> or <code>--mode=valgrind</code> for a specific compilation mode. By default, it is "release". The "valgrind" mode is for profiling (speed, memory usage) using the excellent [Valgrind tool](http://valgrind.org/).

In all cases, your programs will be compiled with the same options as engine units. We turn the same optimizations as for the engine core. In FPC it which means that we use ObjFpc syntax mode by default.

By default we auto-detect the compiler: using the compiler indicated in link:pass:[CastleEngineManifest.xml examples][CastleEngineManifest.xml] by `compiler="xxx"` option, otherwise using FPC or Delphi (whichever first is found). You can use `--compiler` command-line option to override the compiler choice, like `--compiler=fpc` or `--compiler=delphi`.

You can customize what options we pass to the compiler by:

1. Adding options to the `<custom_options>` in link:pass:[CastleEngineManifest.xml examples][CastleEngineManifest.xml]. This is the good place to define options specific to your project (that should be used by all developers working with this project).

2. Using `--compiler-option` command-line option. For example, `--compiler-option=-dMY_DEFINE` or `--compiler-option=-gl --compiler-option=-gh`. This is the good place to pass options specific to your development system or preferences, that should not be shared by all developers working on this project.

Use <code>--plugin</code> to compile a web browser plugin.

Use <code>--output DIR</code> to place the output files (executable, temporary <code>castle-engine-output</code> subdirectory) in a specified directory. When not given, they are placed in the current project directory. This option is also available for other commands, like `package`, `install` and `run`.

Compiling on Windows will also copy the necessary `.dll` files from the engine to be alongside your `.exe` file. This allows to run the executable afterwards, in any way.

Note: This is not the only possible way to compile programs using our engine (for example, you can also compile and run using Lazarus, which is OK for desktop applications).

package
----

Create an installable package with your application.

Alternatively, on some platforms (iOS), this instead creates _"something as close to the installable package as possible"_, which in case of iOS means that it creates an _Xcode project_.

Use <code>--cpu</code>, <code>--os</code> or <code>--target</code> options to specify target operating system/processor (by default, we package for current standalone platform). When target is `iOS` you can also use `--ios-simulator` option to include iOS simulator support (see link:pass:[iOS][iOS]). Use `--compiler` to override compiler for building, just as for `compile` command.

What exactly is produced by this command depends on the target platform and the `--package-format` option used. 

* `--package-format=default` (used also when no `--package-format=...` was specified):
    * For the standalone platforms, we package to a simple zip / tar.gz archive containing the executable, libraries and data. For Windows, we create zip, otherwise tar.gz.
    * For the Android (when <code>--target=android</code> or <code>--os=android --cpu=arm/aarch64</code>), we create a complete apk with your application, ready to be installed and uploaded to Google Play!
    * For iOS (when <code>--target=ios</code>), we create an Xcode project, that you can run and publish using Xcode. 
* `--package-format=zip`: Pack files into a zip file.
* `--package-format=tar.gz`: Pack files into a tar.gz file.
* `--package-format=directory`: Put files into a directory. This is useful if you plan to further process this directory, e.g. pack it with your own scripts.

* Additional Android-only options:
    * `--package-format=android-apk`: Create an APK file. This is right now the equivalent to `--package-format=default` and it is just the default behavior when target/OS is Android. It's the standard way to build applications for Android. It also allows you to manually install the app on your Android device.
    * `--package-format=android-app-bundle`: Create an [Android App Bundle (AAB)](https://developer.android.com/platform/technology/app-bundle). This is a new format recommended for submitting a release to Google Play Store. Android App Bundle may contain multiple precompiled versions of the app and assets, and Google Play Store internally generates an installable APK for every specific user depending on user device configuration (such as Android version or screen resolution). AAB format is strictly required to upload a project larger than 100Mb to Play Store.

* Additional iOS-only options:
    * `--package-format=ios-xcode-project`: Create the Xcode project. This is the default package method for iOS.
    * `--package-format=ios-archive-ad-hoc`: Archive and export using the *ad-hoc* method, which results in an IPA file of the application. To install on designated devices, upload to [TestFairy](https://github.com/castle-engine/castle-engine/blob/master/tools/build-tool/data/ios/services/test_fairy/README.md) etc.
    * `--package-format=ios-archive-development`: Archive and export using the *development* method. See the Xcode documentation (and try these options from Xcode interactively) for details.
    * `--package-format=ios-archive-app-store`: Archive and export for the TestFlight and the AppStore. See the Xcode documentation (and try these options from Xcode interactively) for details. Note that this does not upload to the TestFlight / AppStore (although we'd like to extend this someday to do it).

By default output file contains a version number (if `CastleEngineManifest.xml` contained the version). Sometimes this is not comfortable, use `--package-name-no-version` to avoid it.

You can use <code>--mode=xxx</code> option when packaging, just like when compiling. Use <code>--mode=debug</code> or <code>--mode=release</code> for a specific compilation mode. By default, it is "release". You can use <code>--mode=debug</code> to package a debug release, which may be useful to distribute to users interested in beta-testing.

In some cases, the <code>--mode</code> also affects the packaging wrapper. For example, on Android, a *debug apk* is generated. Also, only a *debug apk* may use a debug signing key (our build tool will automatically fallback from *release apk* to *debug apk* if you did not provide a [release key in AndroidSigningProperties.txt](https://github.com/castle-engine/castle-engine/wiki/Android-FAQ#signing-a-release-apk)).

To make sure that we recompile everything in the current mode (e.g. a _release mode_), this does `clean`, and then `compile`, and only then actually packages the result. You can change this behavior:

* Use `--fast` to avoid cleaning at the beginning. In effect, we will recompile only what changed. This is usually much faster, and suitable for the development, if you call the `package` command often (e.g. because you're testing on an actual Android or iOS device). This is especially useful on iOS, when the full compilation takes a while (since it must compile for 4 platforms).

    For the final release builds, it's more reliable to not use this option. This makes sure that we recompile 100% of your code in proper (e.g. release) mode, with proper options and such.

* Use `--assume-compiled` to say that you already compiled the application in proper mode before calling the `package` action. We will not do `clean` and `compile` in this case at all. This is obviously much faster, but you need to make sure to call `compile` beforehand yourself.

Another (independent) way to make packaging faster is to use `--update-only-code`. _For now this is meaningful only for iOS._  If specified, it means that the build tool can assume that _only the Pascal code have changed_ (so you did not change e.g. `data/` directory, or project settings in `CastleEngineManifest.xml`). We can then recompile the code (and update the relevant file in the project, like `libxxx.a`) without changing anything else. This means that the `package` command will finish much faster. It will also be more comfortable -- e.g. no need to close and reopen the project in Xcode, Visual Studio or whatever other software is used to handle the final project.


install
----

Install the application created by previous "package" call.

* This is useful when OS is "android", it installs and runs the apk package created by previous "package" call for Android. Useful for quick testing of your app on a device connected through USB. Note: it's best to first test do you see your device using SDK tools, for example execute <tt>adb devices</tt> and see is your device listed.

* Use --plugin to install a web browser plugin. We install the compiled plugin such that it should be visible by all web browsers supporting NPAPI. (On Windows, this means installing proper registry entries. On Unix, it means copying the library to special directory.)

Pass also additional options reflecting the OS/architecture, mode and package name format. In general, pass to `install` *exactly* the same values as you used for `package`, so that we know which package to install:

- Use <code>--os</code>, <code>--cpu</code> or <code>--target</code> to specify target operating system/processor (by default, we install for the current standalone platform). 
- Use `--mode=xxx` to specify debug or release package. 
- Use `--package-format`, `--package-name-no-version` to determine the package name.

run
----

Run the application.

The log of the application (whatever you write using [WritelnLog, WritelnWarning](https://castle-engine.io/manual_log.php)) will be the output of this command. On some platforms, you can also use regular `Writeln`, but to be cross-platform better stick to CGE `WritelnLog` / `WritelnWarning`, they will work in all cases (Android, iOS, Windows GUI applications etc.).

As usual, use <code>--os</code>, <code>--cpu</code> or <code>--target</code> options to specify target operating system/processor. By default, we run the normal (exe) application on the current platform.

On some platforms, it requires packaging and installing the application first. This applies to Android: we install and run on a device connected through USB. Use the "package" and "install" commands before this. For example, on Android you can package and install and run your application like this:

~~~~
castle-engine package --target=android
castle-engine install --target=android
castle-engine run --target=android
~~~~

On other platforms (e.g. standalone Windows, Linux, Mac OS X...), this simply runs the last compiled application. So just "compile" the application first, like this:

~~~~
castle-engine compile
castle-engine run
~~~~

You can specify parameters to pass to the application after the special "--" parameter. For example,

~~~~
castle-engine run -- --fullscreen
~~~~

This will run your application with command-line parameters <code>--fullscreen</code>. In your application, you can read command-line parameters with the help of `CastleParameters` unit. (The `--fullscreen` option, used as an example here, is actually handled automatically, if only your program calls `Application.ParseStandardParameters`.) The command-line parameters are not supported in non-desktop environments (e.g. there's no way to pass them to an Android or iOS application).

On Unix desktop platforms (like Linux, FreeBSD..), we can run your game through a "wrapper script". This is useful e.g. to set `LD_LIBRARY_PATH` before running the application. The build tool simply looks for `run.sh` or `<application_name>_run.sh` script in the project directory, and executes it if found (instead of executing the compiled binary directly).

package-source
----

Package source code, which means just to package whole project directory (cleaned up first).

It creates xxx-VERSION-src.tar.gz archive, with VERSION obtained following the &lt;version&gt; element in the link:pass:[CastleEngineManifest.xml examples][CastleEngineManifest.xml].

It accepts the `--package-format` and `--package-name-no-version` options, just like the `package` command. By default we just pack to zip (that is, `--package-format=zip` is equivalent to `--package-format=zip`).

clean
----

Clean compilation and packaging temporary stuff. This does *not* remove final packaging files.


simple-compile
----

Compile the Object Pascal file (unit/program/library) given as a parameter. This does not search for the Castle Game Engine project's manifest in the <code>CastleEngineManifest.xml</code> file. It merely calls "fpc" with proper command-line options for units/programs/libraries using our engine.

Use this instead of "compile" only if there's some good reason you don't want to use <code>CastleEngineManifest.xml</code> to manage your project.


auto-generate-textures
----

Create GPU-compressed versions of textures, and downscaled textures, for the textures mentioned in &lt;auto_generated_textures&gt; inside the file <code>data/material_properties.xml</code>. Such GPU-compressed and downscaled textures can then be automatically used in your application. See https://castle-engine.io/creating_data_material_properties.php for instructions how to use it and example <code>data/material_properties.xml</code>.

If the output textures are already created, they are updated only if the output timestamp is earlier than input. This usually allows to automatically do only the necessary work, similar to how Makefiles operate. To force recreating all textures, just call `castle-engine auto-generate-clean --all` first.

The information about created textures is stored in `data/CastleAutoGenerated.xml` file. If you use version control, you should either:

- ignore the `data/CastleAutoGenerated.xml` file and ignore all `auto_generated` directories.
- or commit both the `data/CastleAutoGenerated.xml` file and all `auto_generated` directories.


auto-generate-clean
----

Clear `auto_generated` subdirectories. They should contain only the output created by `castle-engine auto-generate-textures` target. In the future, it is possible that more things will be placed there (for example, modern GPUs allow mesh data compression).

Run without any arguments to only clean the _unused_ files in `auto_generated` subdirectories. This may be useful after moving/renaming some subdirectories, as the `castle-engine auto-generate-textures` command never removes previous files, it only adds new files. Having unused files is not a problem -- but they waste disk space, and can be safely removed.

Run with `--all` argument to clean _all_ files from the `auto_generated` subdirectories. This is useful e.g. if you want to force regenerating them all by next `castle-engine auto-generate-textures` command.

generate-program
----

Generates:

1. _standalone (desktop) Pascal program code_ in the file `xxx_standalone.dpr`. It uses the `game_units` defined in the link:pass:[CastleEngineManifest.xml examples][CastleEngineManifest.xml] to determine the correct `uses` clause of the program file.

    You can use this program code to compile the project using any tool you want, not necessarily our build tool. E.g. maybe you like using Lazarus or `fpmake`. You can use it as a `standalone_source` in the `CastleEngineManifest.xml` to make sure build tool also uses it (in case you will modify it), although build tool can also generate such source code automatically.

    If the `standalone_source` is specified in link:pass:[CastleEngineManifest.xml examples][CastleEngineManifest.xml], then we overwrite it, regardless if it is called `xxx_standalone.dpr` or not.

2. _Lazarus project information_ in the file `xxx_standalone.lpi`. 

    Together with `xxx_standalone.dpr`, this allows you to open this project in [Lazarus](http://www.lazarus-ide.org/) to edit, compile, debug and run it from Lazarus. You can also compile it using `lazbuild`.

    If the `standalone_source` is specified in link:pass:[CastleEngineManifest.xml examples][CastleEngineManifest.xml], then we overwrite the respective LPI file, regardless if it is called `xxx_standalone.lpi` or not.

3. _Delphi project information_ in the file `xxx_standalone.dproj`. 

    Together with `xxx_standalone.dpr`, this allows you to open this project in [Delphi](https://www.embarcadero.com/products/Delphi) to edit, compile, debug and run it from Delphi.

    If the `standalone_source` is specified in link:pass:[CastleEngineManifest.xml examples][CastleEngineManifest.xml], then we overwrite the respective DPROJ file, regardless if it is called `xxx_standalone.dproj` or not.

4. `CastleAutoGenerated` unit in `castleautogenerated.pas` unit. It is used by the program file created above (`xxx_standalone.dpr`) and also by all program or library files created automatically by the build tool to build project on all platforms. It defines some project properties, to parse standard command-line properties, to initialize logging.

editor
----

Run the _Castle Game Engine Editor_ within this project. The executed editor will include possible project-specific components.

* In simple cases, this is just a shortcut for running `castle-editor` and opening the current project, which can also be done by calling `castle-editor ../path-to-project/CastleEngineManifest.xml`.

* If your link:pass:[CastleEngineManifest.xml examples][CastleEngineManifest.xml] contains `editor_units` attribute, then this automatically builds and runs a custom CGE editor (a fork of the CGE editor, for use in your project), with your custom components included.

     The custom editor is compiled and present inside `castle-engine-output/editor/`, so it is tied to your project, cleared with `castle-engine clean` and so on. You should always execute it using `castle-engine editor` command. Or using the _"Project -> Restart Editor (may rebuild editor with custom controls)"_ menu from the CGE editor (vanilla build or custom build, doesn't matter, the "Restart editor" will always build custom editor if project uses `editor_units`).
