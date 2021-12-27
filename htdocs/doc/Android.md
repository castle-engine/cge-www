You can create games (and other applications) for Android using our Castle Game Engine.

* [Overview and examples](#overview-and-examples)
* [Installing Android tools](#installing-android-tools)
  * [Install Java](#install-java)
  * [Install Android SDK](#install-android-sdk)
  * [Adjust environment after installing SDK](#adjust-environment-after-installing-sdk)
  * [Install Android SDK components](#install-android-sdk-components)
  * [Install Android NDK](#install-android-ndk)
  * [Adjust environment after installing NDK](#adjust-environment-after-installing-ndk)
  * [Test Android SDK + NDK independently of Pascal and CGE](#test-android-sdk--ndk-independently-of-pascal-and-cge)
  * [FPC for Android](#fpc-for-android)
  * [Add paths to NDK libraries for FPC configuration](#add-paths-to-ndk-libraries-for-fpc-configuration)
  * [Test compiling and running mobile demo](#test-compiling-and-running-mobile-demo)
* link:pass:[Android FAQ][]
* link:pass:[Android SDK and NDK troubleshooting][]

# Overview and examples

The description how to create a cross-platform project, that can work on Android (and other platforms) is in the manual: https://castle-engine.io/manual_mobile.php . This page describes tools you need to install to make the building for Android work.

The engine sources include many examples, and many of them are cross-platform and can be tested on Android. In particular, check out:

* examples/mobile/simple_3d_demo/ - shows 3D world and navigation and graphic effects on Android,
* examples/2d_dragon_spine_android_game/ - shows 2D animation and Google Play Games integration on Android,
* https://castle-engine.io/darkest_before_dawn.php - a complete 3D game, with full source code, for Android and standalone, using our engine.

Use the link:pass:[Build Tool][Build Tool] to easily compile these programs for Android (or other platforms --- they all can be compiled as standalone games too!). But first be sure to install the Android tools described below. They will be called by the link:pass:[Build Tool][Build Tool] under the hood.

# Installing Android tools

To be able to develop Android application, you need some tools from Google, and you need FPC that can compile code to an Android.

## Install Java

Java is required by the Android SDK. Install Java *JDK (Java Development Kit)*. 

* On Debian or Ubuntu, you can install the <code>default-jdk</code> package that will pull in (as dependencies) the best *Java Development Kit* version. Or install directly a package like <code>openjdk-X-jdk</code>.
* On Windows, download the latest version from http://www.oracle.com/technetwork/java/javase/downloads/index.html . Just google for "Java JDK download" if this link doesn't work:) You usually want to install the latest version, like *Java SE Development Kit 8uXXX*. By default, it will be installed in `C:\Program Files\Java\jdk1.8.0_XXX`.

*For users who install full Android Studio*: You do not need to install Java separately. The complete _Android Studio_ will already include Java version suitable for Android SDK. You will want to set `JAVA_HOME` environment variable later to something like
- `C:/Program Files/Android/Android Studio/jre` on Windows
- `<wherever-you-unpacked-android-studio-targz>/jre` on Linux

## Install Android SDK

Download and install Android SDK from http://developer.android.com/sdk/index.html .

You can:

* Install the whole _Android Studio and SDK Tools_ (main download on the above page). This is easier (some configuration can be then done using GUI), and thus is advised for new users. 

    Location of the Android SDK is visible in the Android Studio _Settings -> Android SDK_, on Windows it looks like `C:\Users\<my-user-name>\AppData\Local\Android\Sdk`. The NDK will be installed there too, in `ndk-bundle` subdirectory. You may need to know this location for the next steps.

* Or you can install only the _Command line tools only_ (see the bottom of the above page). The rest can be installed using the `sdkmanager` on the command-line, see our instructions below. This allows to download less, but you will have to work more with the command-line.

    Make sure to unpack the tools under `cmdline-tools` subdirectory of the `$ANDROID_HOME`, and move everything under additional subdirectory `latest`, so you have a file like `$ANDROID_HOME/cmdline-tools/latest/bin/sdkmanager`.

## Adjust environment after installing SDK

* Set `$ANDROID_HOME` environment variable, to point to your Android SDK directory. This is recognized by both our build tool, and some Android tools.
* Consider adding to `$PATH` the `<android-sdk>/platform-tools/` directory. In Castle Game Engine > 6.0.2, this is not necessary (engine doesn't need it), but still advised: it allows you to call `adb` on the command-line easily, which is often useful.
* Set `$JAVA_HOME` environment variable, to point to your newly installed *Java Development Kit (JDK)*. This is required by the Android build tools.

If you don't know what an *"environment variable"* is, google *"how to set environment variable in windows/linux/..."* :) Remember to reopen the relevant application (e.g. close and reopen the terminal) to see the newly set environment variables.

## Install Android SDK components

Install some components of the Android _SDK Manager_:

* The GUI version of the _SDK Manager_ is part of the _Android Studio_. 
    * Run _Android Studio_. If you see a wizard prompting you to create a new project -- you can cancel it. Then choose _Configure -&gt; SDK Manager_.

        <a href="https://castle-engine.io/images/original_size/sdk_manager_1.png"><img src="https://castle-engine.io/images/thumb_size/sdk_manager_1.png" alt="Android SDK Manager 1"></a>
    * Make sure that it knows the correct path to the Android SDK. You may need to _Edit_ the SDK location if you installed (or want to install) to a non-standard path.
    * On the *"SDK Platforms"* tab (1st tab) select *"Android 10.0 (Q) - API 29"* (be sure to use this exact version). Check _"Show Package Details"_ at the bottom to see more, make sure that inside the *"Android 10.0 (Q) - API 29"* section the *"Android SDK Platform 29"* and *"Google APIs"* subcomponents are selected.

        TODO: Screenshots below show older situation, for API 23. In new CGE 6.5, just use API 29 instead.

        <a href="https://castle-engine.io/images/original_size/sdk_manager_2.png"><img src="https://castle-engine.io/images/thumb_size/sdk_manager_2.png" alt="Android SDK Manager 2"></a>

    * On the *"SDK Tools"* tab (2nd tab), select the _"Android SDK Build-tools"_ version 29.0.3 (actually any 29.0.x should be OK). Check _"Show Package Details"_ at the bottom to be able to select the exact _"Android SDK Build-tools"_ version. Be sure to install *this exact version* (not earlier, not later). You can install other versions too, they don't conflict, but you must install this exact version too. 

        TODO: Screenshots below show older situation, for API 23. In new CGE 6.5, just use API 29 instead.

        <a href="https://castle-engine.io/images/original_size/sdk_manager_3.png"><img src="https://castle-engine.io/images/thumb_size/sdk_manager_3.png" alt="Android SDK Manager 3"></a>
    * Also on the *"SDK Tools"* tab (2nd tab), select "NDK".
    * Also on the *"SDK Tools"* tab (2nd tab), from the section *"Support Repository"* (usually at the bottom of the list), make sure *"Android Support Repository"* and *"Google Repository"* are selected.

        <a href="https://castle-engine.io/images/original_size/sdk_manager_4.png"><img src="https://castle-engine.io/images/thumb_size/sdk_manager_4.png" alt="Android SDK Manager 4"></a>
    * Click _OK_, choose _Accept_ at the license question.

* If you prefer to <b>use the command-line</b>, you can alternatively download only _Android SDK command-line tools_. Set `$ANDROID_HOME` environment variable, and then execute
    ```
    cd $ANDROID_HOME
    ./cmdline-tools/latest/bin/sdkmanager --install \
      'platform-tools' \
      'platforms;android-29' \
      'extras;google;google_play_services' \
      'build-tools;29.0.3' \
      'extras;android;m2repository' \
      'ndk-bundle'
    ```
    Remember to use apostrophes as shown above.

If you know what you're doing, you can actually use other versions of SDK components (other versions of *"SDK Platform"* and *"Android SDK Build-tools"*). You will need to declare them on a per-project basis in <code>CastleEngineManifest.xml</code>, see link:pass:[Build Tool][Build Tool]. But it's probably easiest if you use their default versions at the beginning.

## Install Android NDK

The Android NDK should already be installed under the `ndk-bundle` subdirectory of Android SDK, if you followed the above steps :) Both the _GUI version of the SDK Manager_ and the command-line `sdkmanager` install NDK, as described above.

## Adjust environment after installing NDK

* Set `$ANDROID_NDK_HOME` environment variable, to indicate the directory of NDK. It should be just inside the `<android-sdk>/ndk-bundle` directory. Our build tool can figure it out automatically (testing `$ANDROID_HOME/ndk-bundle`), but Google tools really need `$ANDROID_NDK_HOME`. 

* To `$PATH` add:

    * The NDK subdirectory containing Android/Arm binutils (like `arm-linux-androideabi-as`, `arm-linux-androideabi-ld`) suitable for your source platform. For example <code>&lt;android-sdk&gt;/ndk-bundle/toolchains/arm-linux-androideabi-4.9/prebuilt/linux-x86_64/bin</code> . Where "<code>arm-linux-androideabi-4.9</code>" is the Android platform version, just choose latest "<code>arm-linux-androideabi-XXX</code>". And the "<code>prebuilt/linux-x86_64</code>" corresponds to the real OS where you are now (you probably have only one subdirectory inside "<code>prebuilt/</code>" anyway, the one for which you downloaded NDK). Use "<code>linux-x86_64</code>" if you work on Linux 64-bit, adjust for other systems as necessary.

    * The NDK subdirectory containing Android/Aarch64 binutils. Similar to above, e.g. `${ANDROID_NDK_HOME}toolchains/aarch64-linux-android-4.9/prebuilt/linux-x86_64/bin/`.

    * (Optional, mostly useful if you want to use x86_64 emulators/virtual machines.) The NDK subdirectory containing Android/x86_64 binutils. Similar to above, e.g. `${ANDROID_NDK_HOME}toolchains/x86_64-4.9/prebuilt/linux-x86_64/bin/`.

    * (Optional, as above.) The NDK subdirectory containing Android/x86 binutils. Similar to above, e.g. `${ANDROID_NDK_HOME}toolchains/x86-4.9/prebuilt/linux-x86_64/bin/`.

    * Also consider adding to `$PATH` the main NDK directory (with `ndk-build` and `ndk-gdb` tools inside). In Castle Game Engine > 6.0.2, this is not necessary (the engine build tool doesn't need it), but it may be comfortable for you anyway (if you plan to use command-line `ndk-gdb`).

For example, when compiling Android application from Linux/x86_64, the environment variables as below make sense:

```
export CASTLE_ENGINE_PATH="${HOME}"/castle-engine/
export ANDROID_HOME="${HOME}/android-sdk/"
export ANDROID_NDK_HOME="${ANDROID_HOME}/ndk-bundle/"
export PATH="${PATH}:\
${ANDROID_HOME}platform-tools/:\
${ANDROID_NDK_HOME}:\
${ANDROID_NDK_HOME}toolchains/arm-linux-androideabi-4.9/prebuilt/linux-x86_64/bin/:\
${ANDROID_NDK_HOME}toolchains/aarch64-linux-android-4.9/prebuilt/linux-x86_64/bin/"
${ANDROID_NDK_HOME}toolchains/x86-4.9/prebuilt/linux-x86_64/bin/:\
${ANDROID_NDK_HOME}toolchains/x86_64-4.9/prebuilt/linux-x86_64/bin/:\
export JAVA_HOME="/usr/lib/jvm/java-11-openjdk-amd64/"
```

## Test Android SDK + NDK independently of Pascal and CGE

*While this step is optional, it is often a good idea to try, especially when troubleshooting problems:*

Test compiling (and deploying to your actual Android device) some example code from the Android NDK. 

* This allows to make sure that you configured everything correctly, before you start playing with combining Object Pascal code + Android. This allows to detect problems in your setup (or in the latest Android SDK/NDK -- these things happen) that are not related to FPC or CGE.

* If you run into trouble, consult link:pass:[Android SDK and NDK troubleshooting][].

For this, you can compile and install (on a real Android device, typically connected through USB) the "hello-gl2" demo from Android NDK. 

* Download it from GitHub, like this:
    ~~~~
    git clone https://github.com/googlesamples/android-ndk android-ndk-samples
    ~~~~

* Compile the example code by this command-line:

    ~~~~
    cd android-ndk-samples/hello-gl2/

    # On Windows platforms, type this command:
    gradlew.bat assembleDebug

    # On Mac OS and Linux platforms, type these commands:
    chmod +x gradlew
    ./gradlew assembleDebug

    # See https://developer.android.com/studio/build/building-cmdline.html .
    # The first run will take a long time, as gradle downloads it's components.
    # This should create a file app/build/outputs/apk/app-debug.apk

    # Check that your device is connected, enable "USB debugging" and authorize it if needed
    adb devices 
    # Install the application (use -r to reinstall)
    adb install app/build/outputs/apk/app-debug.apk
    # Or install using gradle:
    ./gradlew installDebug
    ~~~~

And then run the resulting application (it is called "GL2JNI") on your device. It should work equally well on a real Android device, or in the Android emulator.

## FPC for Android

You need a special version of FPC (Free Pascal Compiler, http://freepascal.org/): a cross-compiler to Android. This means that FPC can run on your normal OS/processor (Linux, Windows, macOS...) and can produce binaries for Android. "Android" more precisely means "*Android OS (based on Linux) + Android processor (32-bit `arm` or 64-bit `aarch64`, for emulators also `x64_64` is useful)*".

You need to use **FPC >= 3.0.2** for Android compilation. *Reasons*: The *PIC support* for Android libraries is not implemented in earlier FPC versions, and it's required when using Android SDK >= 23, see http://wiki.freepascal.org/Android . We recommend using latest stable, FPC 3.2.2.

**The easiest way to get a cross-compiler for Android is to use link:pass:[fpcupdeluxe][fpcupdeluxe]. I advise it, instead of the manual process described below. Use link:pass:[fpcupdeluxe][fpcupdeluxe] to get latest FPC for Android.**

As an alternative, below we describe how to manually compile latest FPC 3.3.1 for Android:
* Make sure you have installed the latest stable FPC version (3.2.2 at the time of this writing), it is required to compile (bootstrap) new FPC version.
* Get and compile FPC from GitLab.

Here'a an example how to do it from the command-line.

~~~~
git clone https://gitlab.com/freepascal.org/fpc/source fpcsrc
cd fpcsrc/

# Of course adjust INSTALL_PREFIX below.
# -CfVFPV3 is necessary for hard floats, this way the engine works much faster.
make clean crossall crossinstall OS_TARGET=android CPU_TARGET=arm CROSSOPT="-CfVFPV3" INSTALL_PREFIX=$HOME/installed/fpc/android
make clean crossall crossinstall OS_TARGET=android CPU_TARGET=aarch64 INSTALL_PREFIX=$HOME/installed/fpc/android

# 2 commands below are optional, 
# as x86 Android versions are practically useful in Android emulators/virtual machines and not required otherwise.
make clean crossall crossinstall OS_TARGET=android CPU_TARGET=x86 INSTALL_PREFIX=$HOME/installed/fpc/android
make clean crossall crossinstall OS_TARGET=android CPU_TARGET=x86_64 INSTALL_PREFIX=$HOME/installed/fpc/android

# It is also advised to compile and install a "normal" FPC binaries and units,
# not cross-compiling,
# for compiling to your normal OS with the same FPC version.
make clean all install INSTALL_PREFIX=$HOME/installed/fpc/android

# On Unix (not on Windows), this is also useful for comfort:
cd $HOME/installed/fpc/android/bin
ln -s ../lib/fpc/3.3.1/ppcrossarm .
ln -s ../lib/fpc/3.3.1/ppcrossa64 .
ln -s ../lib/fpc/3.3.1/ppcrossx64 . # this is only relevant if your main FPC is 32-bit
ln -s ../lib/fpc/3.3.1/ppc386     . # this is only relevant if your main FPC is 32-bit

# On Windows, it is useful to copy some helper binaries from the last stable FPC release.
# These include:
# - windres
# - gcc, cpp
~~~~

* See http://wiki.freepascal.org/Android for links to more info.
* Remember to first download the last stable FPC compiler, to bootstrap the compilation.

## Add paths to NDK libraries for FPC configuration

Add paths to NDK dirs to your ~/.fpc.cfg (see http://www.freepascal.org/docs-html/user/usersu10.html if you're unsure where's your configuration file; you can create a new one using "fpcmkcfg" program). This way FPC will find the correct NDK tools to link your programs:

~~~~
#ifdef ANDROID

  # 32-bit Android CPUs
  #ifdef CPUARM
  -Fl<android-sdk>/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/sysroot/usr/lib/arm-linux-androideabi/16/
  #endif
  #ifdef CPU386
  -Fl<android-sdk>/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/sysroot/usr/lib/i686-linux-android/16/
  #endif

  # 64-bit Android CPUs
  #ifdef CPUAARCH64
  -Fl<android-sdk>/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/sysroot/usr/lib/aarch64-linux-android/21/
  #endif
  #ifdef CPUX86_64
  -Fl<android-sdk>/ndk-bundle/toolchains/llvm/prebuilt/linux-x86_64/sysroot/usr/lib/x86_64-linux-android/21/
  #endif

#endif
~~~~

Also, double check that your FPC config does not contain any weird -XP option. FPC can correctly determine it at runtime, and having it set in config file does more harm than good --- unless you know what it does and why you use it.

Note that the lines above point to NDK platform version `16` (for 32-bit CPUs). Yes, use exactly this version. This is the NDK platform version, and it should correspond to the `min_sdk_version` in link:pass:[CastleEngineManifest.xml-examples][CastleEngineManifest.xml]. It is a *different* number than the `compile_sdk_version` (and the SDK platform version you installed).

For 64-bit CPUs, these lines point to NDK platform version `21`, earliest version with 64-bit CPU support.

If you use link:pass:[fpcupdeluxe][fpcupdeluxe], it is safest to remove/rename it's `<fpcupdeluxe>/cross/lib/arm-android/` and `<fpcupdeluxe>/cross/lib/arch64-android/`. This way we make sure to use NDK platform version `16` (32-bit) / `21` (64-bit).

## Test compiling and running mobile demo

You should have everything set up now. Let's try compiling and running our mobile demo application, in `examples/mobile/simple_3d_demo/` .

1. First compile and run a normal standalone version, in simple_3d_demo_standalone.lpr . Just compile and run it as usual, using Lazarus or "simple_3d_demo_standalone_compile.sh" script (which simply executes FPC from command-line with proper options).

2. Then compile a standalone version using our link:pass:[Build Tool][Build Tool], by calling

    ~~~~
    castle-engine compile
    ~~~~

3. Now compile the Android application using our link:pass:[Build Tool][Build Tool], by caling

    ~~~~
    castle-engine compile --target=android
    ~~~~

    This should create `libsimple_3d_demo.so` file, using FPC for Android and Android NDK tools.

4. If all is well, go one step further, create an Android package using
    ~~~~
    castle-engine package --target=android
    ~~~~
    This recompiles a release version and creates a ready `simple_3d_demo.apk` file that you can install on your Android device.

5. You can install and run the apk if your Android phone is connected to your computer (through USB cable).

    Make sure that the "*USB Debugging*" is turned on in the "*Development Settings*" on your Android phone or tablet. The official information how to do it from Google is on https://developer.android.com/studio/run/device.html (ignore mentions of <code>build.gradle</code> there, the build tool hides it from you). In case of trouble, search google about it, some devices have special ways to enable it. Keep trying until <code>"adb devices"</code> shows your device --- it is necessary to make further commands work.

    ~~~~
    adb devices # should show your device in response
    castle-engine install --target=android
    castle-engine run --target=android
    ~~~~

    These commands install and run the application using Android SDK tools underneath. They show the log of your running application, which is very useful for testing.

At this point, you have a ready apk file, which you can upload / install / run just like any other application, using any Android tool. You can also upload it to Google Play and release your game! Go for it!:)
