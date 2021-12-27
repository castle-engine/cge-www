# Android Project Services Integrated with Castle Game Engine

<a href="http://castle-engine.sourceforge.net/images/original_size/android-components-icons.png">
<img align="right" src="http://castle-engine.sourceforge.net/images/thumb_size/android-components-icons.png" alt="Android Services"></a>

When you use our link:pass:[Build tool][build tool] to package your game for Android, you can easily add *services* to the project. They allow to comfortably access various Android services (from Google and other 3rd parties, like ad networks) from your Object Pascal game code using the [Castle Game Engine](http://castle-engine.sourceforge.net/engine.php).

**Table of contents:**

* [General usage](#general-usage)
* [Android project types](#android-project-types)
* [Services](#services)
  * [admob](https://github.com/castle-engine/castle-engine/tree/master/tools/build-tool/data/android/integrated-services/admob/README.md)
  * [apps_flyer](#apps_flyer)
  * [chartboost](#chartboost)
  * [client_server](https://github.com/castle-engine/castle-engine/tree/master/tools/build-tool/data/android/integrated-services/client_server/README.md)
  * [download_urls](https://github.com/castle-engine/castle-engine/tree/master/tools/build-tool/data/android/integrated-services/download_urls/README.md)
  * [facebook](#facebook)
  * [freetype](#freetype)
  * [game_analytics](#game_analytics)
  * [giftiz](#giftiz)
  * [google_analytics](#google_analytics)
  * [google_in_app_purchases](#google_in_app_purchases)
  * [google_play_games](#google_play_games)
  * [google_play_services](#google_play_services)
  * [helpshift](#helpshift)
  * [heyzap](#heyzap)
  * [sound](#sound)
  * [startapp](#startapp)
  * [tenjin](https://github.com/castle-engine/castle-engine/tree/master/tools/build-tool/data/android/integrated-services/tenjin/README.md)
  * [test_fairy](https://github.com/castle-engine/castle-engine/tree/master/tools/build-tool/data/android/integrated-services/test_fairy/README.md)
  * [ogg_vorbis](#ogg_vorbis)
  * [photo_service](#photo_service)
  * [png](https://github.com/castle-engine/castle-engine/blob/master/tools/build-tool/data/android/integrated-services/png/README.md)
  * [read_external_storage](https://github.com/castle-engine/castle-engine/blob/master/tools/build-tool/data/android/integrated-services/read_external_storage/README.md)
  * [vibrate](#vibrate)
  * [write_external_storage](https://github.com/castle-engine/castle-engine/blob/master/tools/build-tool/data/android/integrated-services/write_external_storage/README.md)
* [Adding new services](#adding-new-services)

## General usage

The Android services are declared in link:pass:[CastleEngineManifest.xml examples][CastleEngineManifest.xml] like this:

~~~~xml
<android>
  <services>
    <service name="google_play_services" />
    <service name="google_in_app_purchases" />
  </services>
</android>
~~~~

## Android project types

Using the `<android>` element, you can request a project type:

* `<android project_type="base">`: No extra integration, the final project does not use any Java code at all. Rendering and inputs work 100%, since they don't require anything extra. This may be enough for some games. However _no services can work_.

* `<android project_type="integrated">`: (**This is the default in CGE >= 6.5.**) We include extra Java integration code that allows to use Android project services that communicate with Object Pascal code. This opens the door for the services mentioned below. It also allows to integrate with various native-code libraries (like FreeType and OpenAL). 

Merely declaring the project type as "integrated" (even without any extra services) immediately gives you:

* _Immersive mode_ on newer Android devices. This is a "true" full-screen mode for games on Android, where the back/home buttons are usually hidden.

* You can open URLs in external applications using [OpenURL](https://castle-engine.io/apidoc-unstable/html/CastleOpenDocument.html#OpenURL) from [CastleOpenDocument](https://castle-engine.io/apidoc-unstable/html/CastleOpenDocument.html) unit. Hint: use special `market://` URL to send users to the _Google Play_ store, e.g. to rate your application.

* You can share text and links with other applications. Use [ShareText](http://castle-engine.sourceforge.net/apidoc/html/CastleOpenDocument.html#ShareText) from the [CastleOpenDocument](http://castle-engine.sourceforge.net/apidoc/html/CastleOpenDocument.html) unit.

* You can send Android standard on-screen notification ("toast") using [OnScreenNotification](http://castle-engine.sourceforge.net/apidoc/html/CastleOpenDocument.html#OnScreenNotification) from the [CastleOpenDocument](http://castle-engine.sourceforge.net/apidoc/html/CastleOpenDocument.html) unit.

* Some services (like `sound` and `ogg_vorbis`) will be added automatically to your project if the sound files are detected in your game data. So merely declaring your project type as "integrated" makes sound work on Android.

## Services

### apps_flyer

Integration with [AppsFlyer](https://www.appsflyer.com/).

You need to declare in `CastleEngineManifest.xml` the "dev key" you have from AppsFlyer, like this:

~~~~xml
<service name="apps_flyer">
  <parameter key="dev_key" value="..." />
</service>
~~~~

### chartboost

Enable the [Chartboost](http://chartboost.com/) ads. Use [TAds](http://castle-engine.sourceforge.net/apidoc/html/CastleAds.TAds.html) class from the [CastleAds](http://castle-engine.sourceforge.net/apidoc/html/CastleAds.html) unit to show and control the ads from your Pascal code, with `AdNetwork` set to `anChartboost`.

Requires:
* Using this service requires using also <code>google_play_services</code>.
* The core of this service is a closed-source library distributed by Chartboost. Download it from https://dashboard.chartboost.com/tools/sdk , unpack to some temporary location, and copy the <code>lib/chartboost.jar</code> file from the SDK to the <code>castle_game_engine/tools/build-tool/data/android/integrated-services/chartboost/app/libs/</code> directory.

### facebook

Integrates your application with [Facebook Core SDK](https://developers.facebook.com/docs/android/).

This integration allows to use Facebook Analytics. Various events are logged automatically (like starting the application or making a purchase). You can also log custom events (only _"level achieved"_ for now) by `TFacebook.LogAchievedLevel('level_name')` in Pascal (see `CastleFacebook` unit).

You need to link the application to a Facebook application on http://developers.facebook.com/ : 

- In `CastleEngineManifest.xml`, you need to specify Facebook Application Id, like this:

    ~~~~xml
    <service name="facebook">
      <parameter key="app_id" value="11223344" />
    </service>
    ~~~~

- Fill the necessary information in the Facebook application settings. In particular:

    You need to provide to Facebook a "key hash" derived from your application signing keys. See the https://developers.facebook.com/docs/app-events/getting-started-app-events-android , section _"3. Add Your Development and Release Key Hashes"_. 

    Do also steps mentioned there in _"4. Tell Us about Your Android Project"_.

### freetype

Enable loading the `.ttf` font files on Android (and other font formats handled by the https://www.freetype.org/ library).

You can e.g. pass a `.ttf` file URL to the [TTextureFont.Load](https://castle-engine.io/apidoc/html/CastleFonts.TTextureFont.html#Load) method. See engine examples like [examples/fonts/font_from_texture.lpr](https://github.com/castle-engine/castle-engine/blob/master/examples/fonts/font_from_texture.lpr) to see how to use fonts in CGE.

*There is usually no need to include this service explicitly*, it is automatically added if your game has "dependency" on "FreeType". Such dependency is in turn automatically detected when the build tool notices a <code>.ttf</code> file inside your data, although it can also be explicitly requested by declaring <code>&lt;dependency name="FreeType" /&gt;</code> in link:pass:[CastleEngineManifest.xml examples][CastleEngineManifest.xml].

### game_analytics

Enable the [Game Analytics](http://www.gameanalytics.com/) analytics of your apps. Use the [TAnalytics](http://castle-engine.sourceforge.net/apidoc/html/CastleAnalytics.TAnalytics.html) class from the [CastleAnalytics](http://castle-engine.sourceforge.net/apidoc/html/CastleAnalytics.html) unit to initialize and send data to the analytics from your Pascal code. Also, all the purchases done using `google_in_app_purchases` service are automatically send to analytics, with correct price and currency.

* Using this service requires using also using the <code>google_play_services</code> service.
* The core of this service is a closed-source library distributed by GameAnalytics. It will be automatically downloaded using the *Gradle* build system on the first build, so you don't have to do anything.
* This service requires that you increase the <code>min_sdk_version</code> to 10 or higher in your link:pass:[CastleEngineManifest.xml examples][CastleEngineManifest.xml] file. The <code>min_sdk_version</code> is specified at the <code>&lt;android&gt;</code> element.

### giftiz

Enable the [Giftiz](http://www.giftiz.com/) integration. Your app notifies Giftiz when the user finished a mission inside your game, and Giftiz gives users (real!) gifts. See http://www.giftiz.com/ website for more information, and contact them to get the necessary SDK and partner id.

Use [TGiftiz](http://castle-engine.sourceforge.net/apidoc/html/CastleGiftiz.TGiftiz.html) class from the [CastleGiftiz](http://castle-engine.sourceforge.net/apidoc/html/CastleGiftiz.html) unit to control the integration from your Pascal code. Send the "mission complete" message to Giftiz by calling <code>TGiftiz.MissionComplete;</code>. Display Giftiz button on top of your game UI using the <code>TGiftizButton</code> UI service.

Requires:
* The core of this service is a closed-source library distributed by Giftiz in their SDK. 
  * Unpack the Giftiz SDK to a temporary directory.
  * Copy the <code>GiftizSDKLibrary/libs</code> subdirectory to the <code>castle_game_engine/tools/build-tool/data/android/integrated-services/giftiz/app/libs</code>.
  * Copy the <code>GiftizSDKLibrary/res</code> subdirectory to the <code>castle_game_engine/tools/build-tool/data/android/integrated-services/giftiz/app/src/main/res</code>. Double-check the path: the <code>res</code> goes inside the <code>app/src/main/</code>, while the <code>libs</code> are in higher-level <code>app</code> folder.
  * Copy the button UI images: from <code>GiftizSDK_1.5.0/Customize/Border_white</code> copy the png files to your game's <code>data/giftiz/</code> subdirectory. There's an alternative set of images in <code>Border_no</code>, and some customization of these images is allowed (see Giftiz SDK for details). Our Pascal integration will automatically load and use these images to display Giftiz button.
* You need to specify Giftiz *partner id* for integration. Initially this is <code>TEST_PARTNER_KEY</code>, and if you're accepted --- Giftiz will send you your real partner id. You declare it in your link:pass:[CastleEngineManifest.xml examples][CastleEngineManifest.xml] file like this:

  ~~~~xml
  <android project_type="integrated">
    <services>
      <service name="giftiz">
        <parameter key="partner_key" value="TEST_PARTNER_KEY" />
      </service>
    </services>
  </android>
  ~~~~

### google_analytics

Enable the [Google Analytics](https://www.google.com/analytics/) analytics of your apps. Use the [TAnalytics](http://castle-engine.sourceforge.net/apidoc/html/CastleAnalytics.TAnalytics.html) class from the [CastleAnalytics](http://castle-engine.sourceforge.net/apidoc/html/CastleAnalytics.html) unit to initialize and send data to the analytics from your Pascal code. Also, all the purchases done using `google_in_app_purchases` service are automatically send to analytics, with correct price and currency.

Requires:
* Using this service requires using also the <code>google_play_services</code> service.
* Recent _Google Analytics_ require you to use a special `google-services.json` file in your project. 
  * On [this page](https://developers.google.com/analytics/devguides/collection/android/v4/) click _"Get a 
Configuration File"_
  * On a form that appears, add _"Analytics"_ service to your application, selecting proper _Google Analytics_ account parameters.
  * Download `google-services.json` from the form above.
  * Place the contents of this `google-services.json` file inside the link:pass:[CastleEngineManifest.xml examples][CastleEngineManifest.xml] as the parameter of the `google_analytics`, like this:

    ~~~~xml
    <service name="google_analytics">
      <parameter key="game_services_json">
    {
      "project_info": 
      ......
      "configuration_version": "1"
    }
      </parameter>
    </service>
    ~~~~

Note: You need to create a new "property" in _Google Analytics_ for tracking your mobile application, and obtain a "tracking ID" (like `UA-xxx`). However, since Google now advices using Firebase for tracking mobile applications, if you click on "Mobile" when creating a new Google Analytics property, it will suggest you to connect your application to Firebase. Instead, _create a "Website" property in "Google Analytics" to get a normal tracking ID, and then change the "view" to see mobile application data_. See:

* https://stackoverflow.com/questions/44142353/creating-google-analytics-property-not-using-firebase
* https://stackoverflow.com/questions/45853012/add-google-analytics-to-android-app-without-firebase
* https://stackoverflow.com/questions/44421865/is-firebase-now-mandatory-for-use-of-google-analytics-mobile-properties

### google_in_app_purchases

This service enables the _In-App Purchases_ through _Google Play_. This is the most popular way to purchase stuff on Android. Use [TInAppPurchases](http://castle-engine.sourceforge.net/apidoc/html/CastleInAppPurchases.TInAppPurchases.html) class from the [CastleInAppPurchases](http://castle-engine.sourceforge.net/apidoc/html/CastleInAppPurchases.html) unit to handle the purchases from your Pascal code.

### google_play_games

Enable the [Google Play Games Services](https://developers.google.com/games/services/). Use the [TGameService](http://castle-engine.sourceforge.net/apidoc/html/CastleGameService.TGameService.html) class from the [CastleGameService](http://castle-engine.sourceforge.net/apidoc/html/CastleGameService.html) unit in Pascal to initialize and use it, for example to sign-in, *send scores and achievements, view achievements and leaderboards, load and save savegames in the cloud*.

Requires:
* Using this service requires using also <code>google_play_services</code>
* You must create a project in *"Game Services"* section of the *"Google Developer Console"*. This will give you a special "app_id" for your project. In order for the integration to work, you need to specify this app_id in our link:pass:[CastleEngineManifest.xml examples][CastleEngineManifest.xml], like this:

  ~~~~xml
  <android project_type="integrated">
    <services>
      <service name="google_play_services" />
      <service name="google_play_games">
        <parameter key="app_id" value="PASTE-APP-ID-HERE" />
      </service>
    </services>
  </android>
  ~~~~

  After this, you need to call from Pascal code <code>GameService.Initialize</code> to connect to *Google Play Games* when you want (usually, at the beginning of your game: from <code>TCastleApplication.OnInitialize</code>).

### google_play_services

This service includes the [Google Play Services](https://en.wikipedia.org/wiki/Google_Play_Services) library into your Android projects. It doesn't provide anything immediately useful, but it's required by some other services, like <code>google_play_games</code>, <code>google_analytics</code>, <code>admob</code>, <code>heyzap</code>.

The necessary files from Google will be automatically downloaded, along with all their dependencies, using the *Gradle* build system, at the first build. You only have to declare using the service by

~~~~xml
<service name="google_play_services" />
~~~~

### helpshift

Enable the [Helpshift](https://www.helpshift.com/), a mobile-friendly support system (submitting issues, chat with support, FAQ). Use the trivial [THelpshift](http://castle-engine.sourceforge.net/apidoc/html/CastleHelpshift.THelpshift.html) class from the [CastleHelpshift](http://castle-engine.sourceforge.net/apidoc/html/CastleHelpshift.html) unit to show the Helpshift activities from your application.

Requires:
* Create an account on http://helpshift.com/ , to get the necessary keys for your project. Use them when declaring the service parameters in link:pass:[CastleEngineManifest.xml examples][CastleEngineManifest.xml]:

  ~~~~xml
  <service name="helpshift">
    <parameter key="api_key" value="xxx" />
    <parameter key="domain" value="xxx.helpshift.com" />
    <parameter key="app_id" value="xxx_platform_xxx" />
  </service>
  ~~~~

* The Helpshift library is automatically downloaded at the first build using *Gradle*, so you don't have to do anything yourself.
* Make sure you compile with Android SDK platform 23 or higher (already the default now) and have `min_sdk_version` >= 14 in your link:pass:[CastleEngineManifest.xml examples][CastleEngineManifest.xml]. See the https://developers.helpshift.com/android/getting-started/ . This page also says you need the `target_sdk_version` >= 23, but I found that older versions work too.

### heyzap

Enable the [Heyzap advertisements](https://www.heyzap.com/). Use [TAds](http://castle-engine.sourceforge.net/apidoc/html/CastleAds.TAds.html) class from the [CastleAds](http://castle-engine.sourceforge.net/apidoc/html/CastleAds.html) unit to show and control the ads from your Pascal code, with `AdNetwork` set to `anHeyzap`.

Requires:
* The core of this service is a closed-source library distributed in Heyzap SDK.
  * Download the SDK from https://developers.heyzap.com/docs/android_sdk_setup_and_requirements (select in *Step 1* whether you want to use beta or not, and then in *Step 2. Download the SDK* click on SDK link).
  * Unzip the SDK to a temporary location.
  * Copy the jar file from SDK <code>android-sdk/heyzap-ads-sdk.jar</code> into <code>castle_game_engine/tools/build-tool/data/android/integrated-services/heyzap/app/libs/heyzap-ads-sdk.jar</code>.

*Optional notes to use Heyzap mediation:* You can use Heyzap integration (SDK) to get ads from *many* ad networks --- not only from Heyzap own network, but also from AdMob, Chartboost and many more. To do this, you will need to setup the integration in your Heyzap dashboard. Also you will need to follow the instructions on https://developers.heyzap.com/docs/android_sdk_setup_and_requirements to add additional networks SDKs. Select the additional networks on that page, and follow the instructions:

  * Usually, Heyzap docs will instruct you to download some additional SDK. Do as it says, and place the `.jar` and `.aar` files into the <code>..../integrated-services/heyzap/app/libs/</code> directory, alongside the Heyzap jar.
  * For every `.aar` file that you have added, add appropriate &lt;dependency&gt; line to the <code>..../integrated-services/heyzap/app/build.gradle</code> file. Like this:

    ~~~~xml
    <dependency>compile(name:'AudienceNetwork', ext:'aar')</dependency>
    <dependency>compile(name:'unity-ads', ext:'aar')</dependency>
    ~~~~

  * Make the necessary modifications in the <code>..../integrated-services/heyzap/app/src/main/AndroidManifest.xml</code>. Make sure it contains only (and exactly!) the activities/permissions that Heyzap docs show you.
  * *Note for AdMob*: Using AdMob requires that you also use `"google_play_services"` service. And you should add to the <code>..../integrated-services/heyzap/app/build.gradle</code> file these lines:

    ~~~~xml
    <dependency>compile 'com.google.android.gms:play-services-ads:9.4.0'</dependency>
    <dependency>compile 'com.google.android.gms:play-services-location:9.4.0'</dependency>
    ~~~~
  * *Note*: that some ads SDK may require increasing the `min_sdk_version` in your link:pass:[CastleEngineManifest.xml examples][CastleEngineManifest.xml]. You will get a clear error message when building APK in case this is necessary. Currently, _Facebook Ads (Facebook Audience Network)_ requires min SDK >= 11.
  * *Note*: when you use Heyzap to integrate with some 3rd-party ad network (for example with Chartboost), then you *should not* add a service in `CastleEngineManifest.xml` to also directly interact with this network (in this example "chartboost"). That is: *include **only** the heyzap service in CastleEngineManifest.xml*. Otherwise the SDK underneath (like chartboost) will be initialized and handled by two code paths (one directly in our Java code, and one inside Heyzap jar), which can lead to various troubles.
  * To test your integration, call <code>TAds.StartTestActivity(anHeyzap)</code> in your Pascal code. This will show a special activity where you can test whether the connection with your 3rd-party networks works OK. Optional, but highly suggested!

### sound

Enable the sound service on Android. *There is no need to include this service explicitly*, it is automatically added if your game depends on a sound library. Which in turn may be done by declaring <code>&lt;dependency name="Sound" /&gt;</code> in link:pass:[CastleEngineManifest.xml examples][CastleEngineManifest.xml], but usually it's automatically detected when the build tool notices a sound file inside your data.

### startapp

Enable the [StartApp](http://startapp.com/) ads. Use [TAds](http://castle-engine.sourceforge.net/apidoc/html/CastleAds.TAds.html) class from the [CastleAds](http://castle-engine.sourceforge.net/apidoc/html/CastleAds.html) unit to show and control the ads from your Pascal code, with `AdNetwork` set to `anStartApp`.

* The core of this service is a closed-source library distributed by StartApp.
  * Download the SDK from http://s3.startapp.com.s3.amazonaws.com/Resource/SDK_TEMP/Android-InApp%20SDK-InApp-3.2.2.zip . The original page where this is linked is in your [StartApp publisher panel](https://portal.startapp.com/) in the _Resource Center_ section.
  * Unzip the StartApp SDK to a temporary location.
  * Copy the file <code>StartAppInApp-3.2.2.jar</code> from the SDK into <code>castle_game_engine/tools/build-tool/data/android/integrated-services/startapp/app/libs/</code>.

### ogg_vorbis

Enable the _OggVorbis_ sound format on Android.

*There is no need to include this service explicitly*, it is automatically added if your game depends on "OggVorbis". Which in turn is automatically detected when the build tool notices an <code>.ogg</code> file inside your data, although it can also be explicitly requested by declaring <code>&lt;dependency name="OggVorbis" /&gt;</code> in link:pass:[CastleEngineManifest.xml examples][CastleEngineManifest.xml].

In effect, loading OggVorbis sound files in your Android games *just works* -- you only need to make sure your Android `project_type` is set to `"integrated"`.

We use very fast open-source OggVorbis implementation called *Tremolo*, see https://github.com/michaliskambi/tremolo-android . You can compile *Tremolo* in low-precision mode (`libtremolo-low-precision.so`) to get more efficiency (decoding will be faster, less CPU hungry), although then the quality is quite horrible in my tests.

Note that on Android, the sound engine (with OpenAL underneath) has to be paused / resumed when user switches between applications. If you play some music, and you want resuming to start it again --- you need to register you callback on `SoundEngine.OnOpenClose` to handle this situation. If you play music using `TMusicPlayer`, this is taken care of automatically.

### photo_service

Operations with native picture gallery.

Use the Pascal class `TPhotoService` (use `CastlePhotoService` unit) to store your image into Pictures folder in the system gallery. Using this service automatically adds the `WRITE_EXTERNAL_STORAGE` permission.

### vibrate

Allows to vibrate the device. Use the [Vibrate](http://castle-engine.sourceforge.net/apidoc/html/CastleOpenDocument.html#Vibrate) procedure from [CastleOpenDocument](http://castle-engine.sourceforge.net/apidoc/html/CastleOpenDocument.html) unit to cause vibration from Object Pascal code.

## Adding new services

This documentation has moved to link:pass:[Adding New Android Services][].