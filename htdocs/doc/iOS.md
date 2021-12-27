Table of Contents:

* [Supported iOS Devices](#supported-ios-devices)
* [Overview and Examples](#overview-and-examples)
* [Building the App](#building-the-app)
  * [Get macOS and Xcode](#get-macos-and-xcode)
  * [Install FPC for macOS](#install-fpc-for-macos)
  * [Installing FPC cross-compilers for iPhone and iPhoneSimulator](#installing-fpc-cross-compilers-for-iphone-and-iphonesimulator)
  * [Testing FPC cross-compilers](#testing-fpc-cross-compilers)
  * [iPhone Simulator is not an iPhone emulator](#iphone-simulator-is-not-an-iphone-emulator)
  * [Using the build tool](#using-the-build-tool)
  * [Archive (deploy on iOS)](#archive-deploy-on-ios)
  * [Upload to TestFlight and AppStore](#upload-to-testflight-and-appstore)
* [Debugging the Pascal Code without iOS](#debugging-the-pascal-code-without-ios)
* [Known problems](#known-problems)
* [More information about FPC and iOS](#more-information-about-fpc-and-ios)
* [Alternative: Using FPC trunk (unstable)](#alternative-using-fpc-trunk-unstable)

Supported iOS Devices
=====================

The engine should work with all devices running iOS 4.2 or newer. It was successfully tested with iOS 5.1, 7, 10, 11, 13, 14. The engine runs flawlessly on iPhone Simulator as well.

Overview and Examples
=====================

There are two ways to use the engine on iOS:

1. The advised (and easier) way is to use the link:pass:[build tool][] to compile and package your game for iOS. Doing it is a matter of preparing link:pass:[CastleEngineManifest.xml examples][CastleEngineManifest.xml] for your project (with `game_units` or `ios_source` defined), and then running <code>castle-engine package --target=ios</code>. This gives you a ready project that you can open, run and publish from Xcode. **This is the advised approach if you develop games using Object Pascal**.

    Various engine examples demonstrate this approach. Try e.g. `examples/2d_dragon_spine_game/` or `examples/2d_standard_ui/zombie_fighter/` .

2. An alternative way is to use the engine as a library that exposes a simple functionality of a VRML/X3D viewer. You can use this in your own custom Xcode projects. **This approach is useful if you want to use the engine as a VRML/X3D file viewer, and wrap it in a custom user-interface in Xcode**.

    The library API is defined in the `src/library` directory of the engine. An example application using this approach is located in `examples/library/ios_tester`.

    This approach is documented in more details on link:pass:[iOS Using the Custom Xcode Project][] page.

Building the App
================

## Get macOS and Xcode

You will need any Mac computer with Xcode installed (available in Mac App Store, free).
<!-- While parts of the process can be done on any platform, but ultimately building and running requires Apple proprietary tools (Xcode and friends), which are available only on macOS. -->

Note: If you browse the documentation on http://wiki.freepascal.org/iPhone/iPod_development , it will talk about Xcode templates and proprietary iOS SDK headers. For the sake of developing games using Castle Game Engine, this can be ignored -- we don't need these headers or templates (since our entire GUI is drawn using GLES through Castle Game Engine).

Install also [CocoaPods](https://github.com/castle-engine/castle-engine/wiki/iOS-Services#common-notes-for-services-using-cocoapods) by `sudo gem install cocoapods` in the terminal.

## Install FPC for macOS

You will need to install this before installing the cross-compiler.

- Go to https://sourceforge.net/projects/freepascal/files/Mac%20OS%20X/
- Select 3.2.0. (Usually we suggest here using the latest stable version, which is 3.2.2 now, but right now 3.2.2 has no precompiled cross-compiler for iOS.)
- Download the regular compiler (for macOS) from there (like `fpc-3.2.0.intel-macosx.dmg` at the time of writing this).

The FPC compiler needs the "Xcode command-line developer tools" installed. In short, open */Applications/Utilities/Terminal* and execute `xcode-select --install`.

Alternative approach to the installation is to use link:pass:[fpcupdeluxe][fpcupdeluxe]. This works now great with latest stable FPC 3.2.2. You can install the regular FPC/Lazarus, and then the necessary cross-compilers (iOS/arm, iOS/aarch64, i-sim/x86_64).

See https://castle-engine.sourceforge.io/macosx_requirements.php for more information about developing desktop applications for macOS using Castle Game Engine.

## Installing FPC cross-compilers for iPhone and iPhoneSimulator

You will need to have FPC cross-compilers able to create these OS/CPU combinations:

- `iphonesim/i386` and `iphonesim/x86_64` applications (for the iPhone simulator)
- `darwin/arm` and `darwin/aarch64` applications (for the actual iPhone device). Note: `aarch64` is also called `arm64`.

It's easiest to get them from the FPC:

- Go to https://sourceforge.net/projects/freepascal/files/Mac%20OS%20X/
- Select latest stable version (like `3.2.0` at the time of writing this)
- Download the cross-compiler for iOS from there, `xxx-macosx.cross.ios.dmg` from there (like `fpc-3.2.0.intel-macosx.cross.ios.dmg` at the time of writing this). Install this.

**Old note for FPC 3.0.4/3.0.5**: `fpc-3.0.5.intel-macosx.cross.ios.dmg` seems to have problems with the latest XCode versions, that don't create `/usr/lib/crt1.o`. This file is required to finish the installation (and later to compile libraries), by https://svn.freepascal.org/cgi-bin/viewvc.cgi/trunk/install/macosx/packaging/packages/fpc-3.0.4.intel-macosx.pkgproj?view=co&revision=1379&root=fpcbuild , otherwise the installer thinks that "XCode command-line tools" are not present. The workaround:

1. Check whether you have [SIP (System Integrity Protection)](https://support.apple.com/en-gb/HT204899) enabled using `csrutil status`. 

2. If SIP is enabled, fix it by running these commands as `root` in the terminal:

    ```
    # boot into recovery terminal
    csrutil disable
    reboot 

    # Boot into recovery terminal
    # The <Mac> part of the path below varies depending on your Mac
    cd /Volumes/<Mac>/usr/lib/
    cp ../../Library/Developer/CommandLineTools/SDKs/MacOSX10.15.sdk/usr/lib/*.o .
    csrutil enable
    reboot
    ```

3. If SIP is disabled, it's simpler:

    ```
    sudo ln -s /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib/crt1.o /usr/lib/
    sudo ln -s /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib/dylib1.o /usr/lib/
    ```

With FPC 3.2.0, the problems are gone.

## Testing FPC cross-compilers

Before attempting the compilation of a full program, we advise testing that your `fpc` is installed OK and supports the necessary platforms.

*First, test that you can compile for the necessary CPUs*. The cross-compiler for each CPU is actually a different FPC executable, so the lines below will make an error immediately if you cannot cross-compile to the given CPU. The desired result is that they should answer _No source file name in command line_.

~~~~
fpc -Pi386    -l # old note: maybe add -V3.0.5, if you use FPC 3.0.4/3.0.5 combination
fpc -Px86_64  -l # old note: maybe add -V3.0.5, if you use FPC 3.0.4/3.0.5 combination
fpc -Parm     -l 
fpc -Paarch64 -l 
~~~~

Note: Add `-V3.0.5` to the lines marked above, if you use the official "FPC for iOS" installed from `fpc-3.0.5.intel-macosx.cross.ios.dmg` file. See the `Getting Started - iOS.rtf` file inside for explanation. This is no longer needed for FPC 3.2.0.

<!-- You can add `-it` to see the list of supported OSes for each one. -->

*Second, test the actual compilation.*

~~~~
cd /tmp/
echo 'library test_compilation; begin end.' > test_compilation.lpr

SIMULATOR_SDK='/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk'
fpc -Pi386    -Tiphonesim -WP7.0 -XR${SIMULATOR_SDK} test_compilation.lpr # old note: maybe add -V3.0.5, if you use FPC 3.0.4/3.0.5 combination
fpc -Px86_64  -Tiphonesim -WP7.0 -XR${SIMULATOR_SDK} test_compilation.lpr # old note: maybe add -V3.0.5, if you use FPC 3.0.4/3.0.5 combination

DEVICE_SDK='/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk'
# Use below -TiOS instead of -Tdarwin with FPC >= 3.2.2.
fpc -Parm     -Tdarwin    -WP7.0 -Cfvfpv3 -Cparmv7 -XR${DEVICE_SDK} test_compilation.lpr
fpc -Paarch64 -Tdarwin    -WP7.0                   -XR${DEVICE_SDK} test_compilation.lpr
~~~~

Note that in FPC 3.2.2 the iOS targets should be called with `-TiOS`, not `-Tdarwin`. See [FPC 3.2.2 new features](https://wiki.freepascal.org/FPC_New_Features_3.2.2#Support_for_macOS.2FAArch64), [FPC 3.2.2 user changes](https://wiki.freepascal.org/User_Changes_3.2.2#The_Darwin_targets_corresponding_to_iOS_have_been_renamed_to_iOS).

Every `fpc` invocation should create `libtest_compilation.dylib`.

The reasons behind some of these compiler options:
- We test by compiling a library, not a program. Compiling a program fails at linking:
    - iphonesim/i386 error: Undefined symbols for architecture i386: "___keymgr_dwarf2_register_sections", referenced from:
    - iphonesim/x86_64 error: Undefined symbols for architecture x86_64: "___keymgr_dwarf2_register_sections"
    - darwin/arm error: Undefined symbols for architecture armv7: "start"
- We use -WP5.1, otherwise the `darwin/arm` fails at linking (error: *symbol dyld_stub_binding_helper not found, normally in crt1.o/dylib1.o/bundle1.o for architecture armv7*). 
- Actually we use later `-WP7.0`, corresponding to what CGE uses now.
- The additional parameters (the -XRxxx, and -Cfvfpv3 -Cparmv7) come from the `Getting Started - iOS.rtf` file from the FPC for iOS package.

## iPhone Simulator is not an iPhone emulator

The <i>iPhone Simulator</i> is *not* an emulator of a real iPhone device, i.e. it does not emulate the processor (like ARM) inside the iPhone. Rather, it's a modified version of the normal (desktop) macOS system, running on a normal (i386 or x86_64) CPU.

See "iPhone Simulator is not iPhone" on http://wiki.freepascal.org/iPhone/iPod_development for more information.

This should help you understand why we did some things above, e.g. why do we have a special compilation target for <i>iPhone Simulator</i> (because we cannot just run in the simular the application compiled for an actual iPhone), and why it works fast (because it doesn't emulate the iPhone CPU).

This is in contrast to the Android emulator.

## Using the build tool

1. Make sure the link:pass:[build tool][build tool] is available on $PATH, so you can call `castle-engine` in the terminal.
2. Run in terminal:
    ~~~~
    cd <castle-engine>/examples/2d_dragon_spine_game/
    castle-engine package --target=ios
    ~~~~
    **Tip: You can speedup this process**: The `package` command by default cleans and recompiles everything, to make sure everything is recompiled for the current mode (which is a _release mode_ by default). When developing, it's often useful to make this process quicker, and recompile only what changed. Do this by adding `--fast` option:
    ~~~~
    cd <castle-engine>/examples/2d_dragon_spine_game/
    castle-engine package --target=ios --fast
    ~~~~
    Another way to speedup the process is to rebuild *only code*. Add the `--update-only-code` option for this.

    **Tip: You can include iOS simulator support**: Simply add the `--ios-simulator` command-line option. By default this is off, as including simulator support makes build longer (2 more platforms to compile for) and often it is not necessary. Like this:
    ~~~~
    cd <castle-engine>/examples/2d_dragon_spine_game/
    castle-engine package --target=ios --ios-simulator
    ~~~~
3. Run *Xcode* and open the resulting project in `<castle-engine>/examples/2d_dragon_spine_game/castle-engine-output/ios/xcode_project/`. 

    Open the file with `.xcworkspace` extension in that directory. For now, all iOS projects use [CocoaPods](https://github.com/castle-engine/castle-engine/wiki/iOS-Services#common-notes-for-services-using-cocoapods) and so you need to open them through the `.xcworkspace` file, not just `.xcodeproj`.

4. Run the project from Xcode (*Command + R*), to see your game working in an *iPhone simulator*.

    **Tip: Running on a real, physical device:** It works out of the box! You only need to set the *Development Team* in Xcode, or set `<ios team="xxx" />` attribute in link:pass:[CastleEngineManifest.xml examples][CastleEngineManifest.xml].

    <a href="https://castle-engine.sourceforge.io/images/original_size/xcode_select_team.png"><img src="https://castle-engine.sourceforge.io/images/thumb_size/xcode_select_team.png" alt="Selecting a team in Xcode"></a>

## Archive (deploy on iOS)

You can perform further automatic packaging using the command-line CGE link:pass:[Build Tool][]. Call this:

```
castle-engine package --target=iOS --package-format=ios-archive-ad-hoc
``` 

This generates an IPA file which you can distribute to your testers e.g. using [TestFairy](https://github.com/castle-engine/castle-engine/blob/master/tools/build-tool/data/ios/services/test_fairy/README.md). The resulting IPA file will be inside the `castle-engine-output/ios/build/` subdirectory. It is somewhat similar to Android APK, although distributing IPA to testers is more cumbersome (their devices need to be registered in your Apple developer account, and Xcode must know them at build-time to sign IPA for them).

Other options are available: 

- `ios-archive-development`
- `ios-archive-app-store`. Note that this *does not* upload to the AppStore (although we'd like to extend this someday to do it, but it's unsure whether it is actually possible). In effect this is not very useful in practice, and you *do not* need to use this to release your iOS application on the TestFlight / AppStore. 

Note that these options automatically sign the application. To make it work,

1. Register as a (paid) Apple Developer,
2. Place your "team ID" in the iOS section of the link:pass:[CastleEngineManifest.xml-examples][CastleEngineManifest.xml],
3. Release from Xcode *once* manually. Xcode will ask you to configure your team, store the team password in your keychain etc. Once it works, next time you can do it all automatically.
4. If you do it through SSH (not in an interactive GUI session) you need to call 
    ```
    security unlock-keychain login.keychain
    ``` 
    or
    ```
    security unlock-keychain -p YOUR-PASSWORD login.keychain
    ``` 
    to unlock the keychain before building.
5. And now `castle-engine package --target=iOS --package-format=ios-archive-...` will work smoothly.

## Upload to TestFlight and AppStore

To upload the application to the AppStore:

- Create and configure your iOS application on https://appstoreconnect.apple.com/ . You will link there the uploaded build number later.

- Package project for iOS (any `--package-format=`, including default `--package-format=ios-xcode-project`, is OK).

- Open resulting project in `castle-engine-output/ios/` in Xcode. Switch target to _"Any device"_, press _"Archive"_. In the resulting window (once archive is done) press _"Distribute"_ selecting the option to upload to the store.

# Debugging the Pascal Code without iOS

Same instructions as for link:pass:[https://github.com/castle-engine/castle-engine/wiki/Android-FAQ#testing-mobile-opengl-es-rendering-without-an-android][Testing mobile (OpenGL ES) rendering without an Android] apply here too, as the Pascal code is platform independent.

# Known problems

- When compiling with FPC 3.2.2, there are a lot of warnings

    ```
    clang: warning: using sysroot for 'MacOSX' but targeting 'iPhone' [-Wincompatible-sysroot]
    ```

    They can be ignored. We link with proper iOS libraries, don't worry. We don't yet know how to avoid this warning.

- The event loop on iOS must be controlled by the main program in Objective-C, not in Pascal. This means that `Application.ProcessMessages` does not work. The library cannot force the main process to handle some events, and wait for something to happen. This means that you cannot use `Application.ProcessMessages`, or things depending on them: 

    * `CastleMessages` (functions in this unit make modal windows, running a message loop inside and only returning when user exits --- similar to `ShowMessage` in Lazarus LCL / Delphi VCL),
    * `CastleWindowProgress` (it processes messages to redraw the screen inside `Progress.Step`).

    To make modal windows on iOS, use [CastleDialogStates](https://michalis.ii.uni.wroc.pl/cge-www-preview/apidoc/html/CastleDialogStates.html) unit with modal windows using `TUIState` class. You can also set [MessageOKPushesState](https://michalis.ii.uni.wroc.pl/cge-www-preview/apidoc/html/CastleMessages.html#MessageOKPushesState) to `true`. To show progress bar, use `TCastleProgressBar` control, which you will need to continuously update yourself, for example in your `Window.OnUpdate` event.

- On iOS, the OpenGL(ES) context is created and destroyed solely by the Objective-C code, not Pascal. You cannot use the `Window.Open` and `Window.Close` from the Pascal code to force recreating OpenGL context. This follows our [cross-platform code guidelines](https://castle-engine.io/manual_cross_platform.php).

- The [touchesEnded](https://developer.apple.com/documentation/uikit/uiresponder/1621084-touchesended?language=objc) / [touchesCancelled](https://developer.apple.com/documentation/uikit/uiresponder/1621116-touchescancelled?language=objc) events on iOS (that in turn determine CGE events like `TUIState.Release`, `TCastleWindowBase.OnRelease`) have poor behavior.

    It seems to be implemented like _"report that touch ends, or is cancelled, when the finger position doesn't change for the 0.36 seconds"_. It seems to be done regardless of **when** did the user actually released the finger, and **if** the user actually released the finger. You can test this unfortunate behavior even with simple CGE example `examples/user_interface/test_all_state_events/` . As a result:

    - If you press and release the finger, you will notice that CGE `Release` event is reported with small (0.36 sec) delay instead of being instant. This small delay sometimes matters in games -- you will e.g. see `buttonLeft in Container.MousePressed` for a short time, even after user physically released the finger.

    - If you keep pressing your finger, but without any movement, then the CGE `Release` event will be reported (and the touch will no longer be tracked). Even if you keep holding the finger down.

    There's no reliable solution, it seems to be just the way iOS behaves. Trying various, even crazy, things (like not calling `super` in `touchXxx` events, or `touchesCancelled`) in `OpenGLController.m` code doesn't help. Your application simply must be ready for this iOS weirdness -- the CGE `Release` event may happen too early, or too soon, in case user presses the screen quickly, or presses and holds without moving.

# More information about FPC and iOS

* http://wiki.freepascal.org/iPhone/iPod_development
* http://wiki.lazarus.freepascal.org/Portal:iOS
* Building FPC and cross-compiling: http://www.stack.nl/~marcov/buildfaq/
<!-- Old: http://www.ragnemalm.se/lightweight/iphone-install/iphone-installation.html -->

# Alternative: Using FPC trunk (unstable)

Instead of using the stable FPC, you can try using the latest FPC trunk version (3.3.1 now). You need to install 4 cross-compilers, for all 4 platforms that are included in the _"iOS target"_:

1. arm / darwin `# use iOS instead of darwin with FPC >= 3.2.2`
2. aarch64 / darwin `# use iOS instead of darwin with FPC >= 3.2.2`
3. iPhoneSimulator / i386
4. iPhoneSimulator / x86_64

An easy way to install cross-compilers is to use link:pass:[fpcupdeluxe][fpcupdeluxe]. Notes:

* iPhoneSimulator is called _i-sim_ in the fpcupdeluxe UI.
* Our build tool uses the `fpc` binary available on your `$PATH` environment variable. Make sure you create an appropriate symlink to make it call the FPC installed by link:pass:[fpcupdeluxe][fpcupdeluxe (this wiki page describes this)].

When you compiled your own FPC version, you will want to use `--fpc-version-iphone-simulator ""` on the command-line to the build tool. Otherwise the build tool may try to call FPC for iPhone Simulator target with a different FPC version (because the standard FPC 3.0.4 / 3.0.5 mix requires this). So you will usually call this in your project:

```
$ fpc -v
Free Pascal Compiler version 3.0.4 [2017/11/26] for i386
...
$ castle-engine package --target=ios --fpc-version-iphone-simulator "" --fast
...
Compiling project "drawing_toy" for target "ios" in mode "release".
FPC version: 3.1.1
FPC executing...
Compiling Release Version
...
Target OS: Darwin/iPhoneSim for i386
...
Target OS: Darwin/iPhoneSim for x86_64
...
Target OS: Darwin for ARM
...
Target OS: Darwin for AArch64
```

