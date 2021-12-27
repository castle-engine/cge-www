Table of Contents
=================

* [How to use iOS services in Castle Game Engine projects](#how-to-use-ios-services-in-castle-game-engine-projects)
* [Currently supported services:](#currently-supported-services)
  * [activity\_recognition](https://github.com/castle-engine/castle-engine/tree/master/tools/build-tool/data/ios/services/activity_recognition/README.md)
  * [apple\_game\_center](#apple_game_center)
  * [facebook](#facebook)
  * [fmod](https://github.com/castle-engine/castle-engine/tree/master/tools/build-tool/data/ios/services/fmod/README.md)
  * [freetype](https://github.com/castle-engine/castle-engine/tree/master/tools/build-tool/data/ios/services/freetype/README.md)
  * [game\_analytics](#game_analytics)
  * [google\_analytics](#google_analytics)
  * [icloud\_for\_save\_games](#icloud_for_save_games)
  * [in\_app\_purchases](#in_app_purchases)
  * [ogg\_vorbis](#ogg_vorbis)
  * [photo\_service](#photo_service)
  * [tenjin](https://github.com/castle-engine/castle-engine/tree/master/tools/build-tool/data/ios/services/tenjin/README.md)
  * [test_fairy](https://github.com/castle-engine/castle-engine/tree/master/tools/build-tool/data/ios/services/test_fairy/README.md)
  * [vibrate](https://github.com/castle-engine/castle-engine/tree/master/tools/build-tool/data/ios/services/vibrate/README.md)
* [Common notes for services using CocoaPods](#common-notes-for-services-using-cocoapods)
* [Adding new services](#adding-new-services)

# How to use iOS services in Castle Game Engine projects

Inside the link:pass:[CastleEngineManifest.xml examples][CastleEngineManifest.xml] you can declare which additional services you want to integrate with your iOS application. It looks like this:

~~~~xml
<?xml version="1.0" encoding="utf-8"?>
<project name="castle_spine" game_units="Game">
  <ios>
    <services>
      <service name="apple_game_center" />
    </services>
  </ios>
</project>
~~~~

# Currently supported services:

## apple_game_center

Integration with the [Apple Game Center](https://developer.apple.com/game-center/).

To use this, you will need to configure your application integration with _Game Center_ on the Apple websites:

* Login to https://developer.apple.com/ and create your App ID (if not created already) using the `qualified_name` like `io.castleengine.castlespine`.
* Add _Game Center_ feature to your App ID (see the screenshot below):

    <a href="https://castle-engine.sourceforge.io/images/original_size/app_id_gamekit.png"><img src="https://castle-engine.sourceforge.io/images/thumb_size/app_id_gamekit.png" alt="Add GameKit to App Id"></a>
* Login to https://itunesconnect.apple.com/ and configure the achievements, leaderboars in the _Game Center_ section of _iTunes Connect_. [See the Apple documentation for the details](https://developer.apple.com/library/content/documentation/LanguagesUtilities/Conceptual/iTunesConnectGameCenter_Guide/Introduction/Introduction.html#//apple_ref/doc/uid/TP40013726).

Then from your Pascal code, you can use `TGameService` class from the `CastleGameService` unit to send achievements and more using the _Apple Game Center_. See the [examples/mobile/achievements](https://github.com/castle-engine/castle-engine/tree/master/examples/mobile/achievements) example, in particular the code inside [code/gameachievements.pas](https://github.com/castle-engine/castle-engine/blob/master/examples/mobile/achievements/code/gameachievements.pas) and [code/gamestateplay.pas](https://github.com/castle-engine/castle-engine/blob/master/examples/mobile/achievements/code/gamestateplay.pas).

**Savegames in the cloud** work too. Use simple methods `TGameService.SaveGameSave`, `TGameService.SaveGameLoad` from Pascal code. Notes:

* User must be logged into the iCloud, and have _iCloud Drive_ enabled, [see Apple documentation](https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/GameKit_Guide/SavedGames/SavedGames.html).

* You must enable iCloud for your App ID in your https://developer.apple.com/ account.

* You need to use an additional service `icloud_for_save_games`. In effect, in the application settings in Xcode, you should see that the _iCloud_ is active, with _iCloud Documents_ selected. Like on the screenshot below. (_You should not need to adjust them, the generated Xcode project should be already correct. The screenshot is only to allow you to make sure._)

    <a href="https://castle-engine.sourceforge.io/images/original_size/xcode_icloud_settings.png"><img src="https://castle-engine.sourceforge.io/images/thumb_size/xcode_icloud_settings.png" alt="Xcode iCloud Settings for Apple Game Center with savegames"></a>

## facebook

Integration with [Facebook SDK for iOS](https://developers.facebook.com/docs/ios/). Right now this integration:

- Notifies Facebook when users run your application on iOS. This may be useful to see installation statistics inside _Facebook analytics_.
- The Facebook SDK may also log in-app purchases done by users, if you set appropriate option in the Facebook application config.
- In Pascal, right now you can only call `TFacebook.LoginButton` (use `CastleFacebook` unit) to show the Facebook login button. Users can click it to login/logout from the Facebook application. It does not serve much purpose now except checking that the integration actually works (e.g. you pointed to the correct Facebook app).

When adding this to the `CastleEngineManifest.xml`, set also parameters describing application id and name for Facebook:

~~~~xml
<service name="facebook">
  <parameter key="app_id" value="11223344" />
  <parameter key="app_title" value="My Application Name" />
</service>
~~~~

To use this:

- You will need to create a new application on Facebook: https://developers.facebook.com/apps or you can follow the link from https://developers.facebook.com/docs/ios/getting-started .

    In Facebook application, configure at least the _Bundle ID_. It must match the `qualified_name` / `override_qualified_name` you set in `CastleEngineManifest.xml` for iOS. The [getting started guide](https://developers.facebook.com/docs/ios/getting-started) also advices to turn on _Single Sign On_.

    In `CastleEngineManifest.xml`, set the `app_id` and `app_title` for the `facebook` service to match your Facebook application.

- [We use CocoaPods to download the Facebook frameworks, so make sure you have CocoaPods installed](#common-notes-for-services-using-cocoapods)

- Everything else mentioned on [getting started guide](https://developers.facebook.com/docs/ios/getting-started) is done for you automatically -- the project is out-of-the-box integrated with Facebook, using proper declarations in `Info.plist` and Objective-C code.

    Note that compiling the Facebook SDK in latest Xcode will produce a large number of warnings (64, last time I checked...). That's a problem of Facebook SDK -- we cannot help it. You can just ignore them (or submit them to Facebook devs).

## game_analytics

Integration with the [Game Analytics](https://gameanalytics.com/). 

From you game Pascal code, use [TAnalytics class in the CastleAnalytics unit](https://castle-engine.sourceforge.io/apidoc/html/CastleAnalytics.TAnalytics.html). Initialize them with [InitializeGameAnalytics method](https://castle-engine.sourceforge.io/apidoc/html/CastleAnalytics.TAnalytics.html#InitializeGameAnalytics) and send events to the analytics service using various methods like [Event](https://castle-engine.sourceforge.io/apidoc/html/CastleAnalytics.TAnalytics.html#Event). **Also, all the purchases done using `in_app_purchases` service are automatically send to analytics, with correct price and currency.**

Note that you _can_ have both _Game Analytics_ and _Google Analytics_ initialized at the same time. We will send all events to both of them.

[This service uses CocoaPods, so make sure you have CocoaPods installed.](#common-notes-for-services-using-cocoapods)

## google_analytics

Integration with the [Google Analytics](https://analytics.google.com/). 

From you game Pascal code, use [TAnalytics class in the CastleAnalytics unit](https://castle-engine.sourceforge.io/apidoc/html/CastleAnalytics.TAnalytics.html). Initialize them with [InitializeGoogleAnalytics method](https://castle-engine.sourceforge.io/apidoc/html/CastleAnalytics.TAnalytics.html#InitializeGoogleAnalytics) and send events to the analytics service using various methods like [Event](https://castle-engine.sourceforge.io/apidoc/html/CastleAnalytics.TAnalytics.html#Event). **Also, all the purchases done using `in_app_purchases` service are automatically send to analytics, with correct price and currency.**

Note that you _can_ have both _Game Analytics_ and _Google Analytics_ initialized at the same time. We will send all events to both of them.

[This service uses CocoaPods, so make sure you have CocoaPods installed.](#common-notes-for-services-using-cocoapods)

Note: You need to create a new "property" in _Google Analytics_ for tracking your mobile application, and obtain a "tracking ID" (like `UA-xxx`). However, since Google now advices using Firebase for tracking mobile applications, if you click on "Mobile" when creating a new Google Analytics property, it will suggest you to connect your application to Firebase. Instead, _create a "Website" property in "Google Analytics" to get a normal tracking ID, and then change the "view" to see mobile application data_. See:

* https://stackoverflow.com/questions/44142353/creating-google-analytics-property-not-using-firebase
* https://stackoverflow.com/questions/45853012/add-google-analytics-to-android-app-without-firebase
* https://stackoverflow.com/questions/44421865/is-firebase-now-mandatory-for-use-of-google-analytics-mobile-properties

## icloud_for_save_games

This service enhances the `apple_game_center` service to be able to store savegames in the cloud. See the `apple_game_center` documentation above.

## in_app_purchases

Allows to sell products within the iOS application, through the _Apple AppStore_.

In your game, use the [TInAppPurchases class from the CastleInAppPurchases unit](https://castle-engine.sourceforge.io/apidoc/html/CastleInAppPurchases.TInAppPurchases.html) to communicate with the AppStore, requesting information about the products, purchasing etc.

You need (paid) Apple developer account to use this. And you will need to sign some legal forms to enable in-app purchases. See the [Apple documentation](https://developer.apple.com/in-app-purchase/) for all the details. And you will need to configure the products that can be purchased on the [Apple iTunes Connect website](https://itunesconnect.apple.com/). 

Be sure to check that the capability _"In-App Purchase"_ is also _Enabled_ for given _App ID_, in your _Apple Developer Account_.

## ogg_vorbis

This integrates your project with Tremolo to allow loading and playing OggVorbis music on iOS.

*This service is automatically added to your project if it has a dependency on OggVorbis, which in turn is automatic if your game data includes some `.ogg` file.* So there's seldom a need to request this service explicitly.

## photo_service

Integration with PhotoLibrary.

Use the Pascal class `TPhotoService` (use `CastlePhotoService` unit) to store your image into the system Photos app.

# Common notes for services using CocoaPods

Some of the services use [CocoaPods](https://cocoapods.org/) for installing 3rd-party dependencies easily (e.g. _Game Analytics_ or _Google Analytics_ or _Facebook SDK_ libraries).

In order to use such sevices:

1. You need to have [CocoaPods](https://cocoapods.org/) installed on your system. Just execute `sudo gem install cocoapods` in the terminal, it should make the `pod` command available on your `$PATH`.

    The build tool will internally use `pod` to download and install service dependencies. This happens completely automatically. If you never used CocoaPods before, be aware that the 1st run may take a while (even a couple of minutes) as a large CocoaPods repository is downloaded.

2. You should no longer open the project using `my_project_name.xcodeproj` file. This will not work, as the `libPods...` library will not be built in this case. Instead, open and run in Xcode the `my_project_name.xcworkspace` file (it is in the same directory as `my_project_name.xcodeproj`). Using this will correctly build and run the project with dependencies.

# Adding new services

Adding new iOS service is deliberately very consistent with creating new service for Android, which is documented on link:pass:[Adding New Android Services][]. 

- In case of iOS you write code using Objective-C. 

- Similarly to Android, we have a base class in Objective-C `ServiceAbstract` with methods you can override, that correspond to typical iOS application lifecycle.

- See existing service code `build-tool/data/ios/services` for examples.

- Same as on Android, you can use `CastleMessaging` to communicate with Pascal code asynchronously.

- On iOS, use static libraries `libxxx.a` (instead of dynamic `libxxx.so`).

- Note that [CocoaPods](https://cocoapods.org/) has a lot of common libraries available. You can trivially use `Podfile` inside a service to reference any library from CocoaPods. See e.g. [freetype service on iOS](https://github.com/castle-engine/castle-engine/tree/master/tools/build-tool/data/ios/services/freetype) for an example.