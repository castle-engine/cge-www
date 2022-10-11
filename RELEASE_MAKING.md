## Release making (for CGE engine or application, like view3dscene)

- Test as described in RELEASE_MAKING_TESTS.md

- Bump versions:

    - For applications (like view3dscene):

        - Make sure Version constant in the program's source code is incremented.

        - Make sure also CastleEngineManifest.xml <version value="..."/> is updated for a program.

    - For the engine, update version numbers in these files:

        - make sure to update to "X.<even>" for release
        - all ../castle-engine/packages/*.lpk
        - ../castle-engine/src/base/castleversion.inc
        - ../castle-engine/tools/build-tool/CastleEngineManifest.xml

           and recompile castle-engine tool:

           ```
           cd ../castle-engine/tools/build-tool
           ./castle-engine_compile.sh
           mv -f castle-engine ~/bin/
           castle-engine -v # check
           ```

        - ../castle-engine/doc/pasdoc/html-parts/body-end.php
          (and run "make clean && make" in doc/pasdoc/html-parts/ to refresh API docs extra HTML).
        - check: grep "7.0-alpha"

        - Change ../castle-engine/tools/internal/pack_release/github_update_release.txt
          to empty.
          Optional: hack Jenkinsfile to not do "Build Examples" as it takes some time.

- Call scripts/generate_versions.sh script.

  - Before, you should recompile program for the current (source) OS.
    That's because generate_versions.sh actually calls program with --version
    to determine version number.

  - You should run generate_versions script to update
    generated_versions.php (this makes version number on WWW page)
    generated_versions.sh (this makes version number for some binary archives, like on itch.io).

- Tag releases using scripts/make_tags.sh script.
  Leave uncommented only the lines for newly released programs, and run it.

- Make new releases from existing tags on GitHub, like

  https://github.com/castle-engine/castle-view-image/releases/new
  https://github.com/castle-engine/castle-engine/releases/new
  https://github.com/castle-engine/view3dscene/releases/new

  Save new release as merely a draft now.

- Uploading new release:

  - For castle-engine:
    Change ../castle-engine/tools/internal/pack_release/github_update_release.txt
    to proper release, like 7.0-alpha.2.
    Commit, push, wait for Jenkins.

  - For pasdoc, view3dscene and most other projects under castle-engine org:
    Do it semi-automatically using scripts in cge-github-update-release:

    ssh jenkins.castle-engine.io
      sudo -u jenkins-cge-uploader -i
        # if cge-github-update-release missing:
        git clone git@gitlab.com:castle-engine/cge-github-update-release.git
        cd cge-github-update-release/

        # if cge-github-update-release present:
        cd cge-github-update-release/
        mr up

        # For castle-engine tools: adjust ./generic-update-github-releases
        ./generic-update-github-releases
        # For pasdoc:
        ./pasdoc-update-github-releases

    Note: This workflow is not perfect, since we upload the builds from latest (master)
    as the "last tagged" project version.
    In case there were some commits since make_tags.sh, the binary builds will be a bit newer.
    This is not a problem in practice, just be aware of it.
    Eventually, you can set tag again and force-push it (this is what cge-github-update-release
    also does to keep "snapshot" a moving tag).

  - Everything else:
    Manually upload to releases on GitHub latest builds by Jenkins of the appropriate application/engine.
    The Jenkinsfile for each project has perfect commands to make a build of every application,
    using advised FPC version etc. So we just upload these builds as releases.

- Publish releases on GitHub

- Update the URLs:
  - view3dscene links in htdocs/view3dscene.php (see lower about "how to update www content")
    if you want to update snapshots, update
    SNAPSHOTS_BASE
    SNAPSHOTS_VERSION
    if you want to update stable version, make sure the new links are OK
    (VERSION_VIEW3DSCENE should already be OK)

  - CGE links in `cge::download-engine[....]` in htdocs/doc/download.adoc ,
    update version/tag there.

    Note: If you change something outside of ADOC,
    to force regenerate https://castle-engine.io/download , on CGE SSH:

    ```
    cd ~/cge-www/htdocs/doc
    rm -f output/download.html
    make
    ```

  - Grep to make sure all things changed
    7.0-alpha.snapshot
    7.0-alpha-snapshot # mistake
    7.0-alpha.1
    7.0-alpha1 # mistake
    to make sure everything is updated.

  After:
  - Download.
    If you manually uploaded: also compare are the downloaded files the same.

- if you released new castle_game_engine version:
  - make sure new apidoc is already uploaded.

      Alt: you can upload it manually

      ```
      cd ../castle-engine/doc/pasdoc/
      make clean upload
      ```

  - update $engine_ver in htdocs/all_programs_sources.php, to reflect
    with what engine ver each sources were tested.
  - update FPC/Lazarus requirements if needed:
    https://castle-engine.io/supported_compilers.php

  - update CGE version of Docker `cge-stable` by
    - ssh jenkins-linux-slave.castle-engine.io
      as michalis
        update in castle-engine-cloud-build-tools/ all versions to point to 7.0-alpha.2
        ./build-local.sh

- In case of CGE and view3dscene, upload them also to http://castle-engine.itch.io/ :
  Run

  ```
  # This is to make sure you have itch.io butler installed and authenticated.
  # Follow https://itch.io/docs/butler/installing.html if not yet,
  # you can actually install it using itch.io GUI app: https://itch.io/app ,
  # just "chmod +x ..." the butler binary and add it to $PATH.
  butler status castle-engine/castle-game-engine

  pack/upload_itch_io.sh
  ```

  Once `butler status castle-engine/view3dscene` is good,
  - test downloading it from https://castle-engine.itch.io/view3dscene (using itch.io app)
  - write a new devlog there (copy-paste from GitHub release notes)

  Once `butler status castle-engine/castle-game-engine` is good,
  - test downloading it from https://castle-engine.itch.io/castle-game-engine (using itch.io app)
  - write a new devlog there (copy-paste from GitHub release notes)

- In case of CGE and view3dscene, upload them also to https://sourceforge.net/projects/castle-engine/ files:
  Run

  ```
  pack/upload_sourceforge.sh
  ```

  Mark on SourceForge default download for Linux/Windows as latest CGE.

  Login to
  https://sourceforge.net/projects/castle-engine/

  Check this contains new files:
  https://sourceforge.net/projects/castle-engine/files/view3dscene/
  https://sourceforge.net/projects/castle-engine/files/castle_game_engine/

  Mark new CGE files as default for Linux/Windows.
  After a moment, Ctrl+R and "Looking for the latest version?" will show new version.

- bump versions/code afterwards, to again make nice snapshots:

    - engine: version to `x.<odd>.0` or `alpha.snapshot`

      (redo again the section above about bumping version,
      also recheck "bump version" commit that we touched the same files)

      Also revert ../castle-engine/tools/internal/pack_release/github_update_release.txt
      to snapshot.

      Also revert Jenkinsfile to do full "Build Examples" (in case you hacked it for release).

    - view3dscene: version to `x.<odd>.0`

      also in htdocs/view3dscene.php now bump SNAPSHOTS_VERSION

## Website updating

- Note: after large website changes it is a good idea to check
    linkchecker.sh
    validate_html.sh

- if you change some content managed inside GIT (cge-www):
  - commit and push your changes to cge-www
  - ssh-cge-sync (to update on castle-engine.io)

- make sure apidoc in https://castle-engine.io/apidoc/html/
  looks OK. It is now automatically updated by a Jenkins job,
  though if you want you can upload it manually by

    cd ~/sources/castle-engine/castle-engine/doc/pasdoc
    make upload

  You can also update https://castle-engine.io/engine_doc.php (HTML, PDF) by

    cd ~/sources/castle-engine/cge-documentation/vrml_engine_internals
    make upload

  You can also update https://castle-engine.io/compositing_shaders.php (HTML, PDF) by

    cd ~/sources/castle-engine/cge-documentation/compositing_shaders_doc
    make upload

## Announcing release

- Regular annoucements sites:
  - Our Wordpress: https://castle-engine.io/wp/
  - Patreon: https://www.patreon.com/castleengine
  - Facebook: https://facebook.com/castleengine/
  - Twitter: https://twitter.com/castleengine
  - Mastodon: https://mastodon.social/@michalis_kambi

- https://opencollective.com/castle-engine

- engine source code release means updating FPC contrib units info
  (see fpc_contrib_units_data.txt,
  http://www.freepascal.org/contrib/contribs.html).

- new release of some programs/engine may be announced on freshcode:

  http://freshcode.club/projects/view3dscene
  http://freshcode.club/projects/castle-engine

  (Old: our projects are also on freshmeat, but it's dead now:
  http://freecode.com/projects/view3dscene/
  http://freecode.com/projects/castlegame/
  http://freecode.com/projects/castle-game-engine)

- http://www.pascalgamedevelopment.com/
  go to the 'Home' section of the site and create 'News' article
  Make sure that when you are creating a new News "article" you are in the News section of the front page. If you are in the Articles section, it'll create an "article" instead.
  (instructions on http://www.pascalgamedevelopment.com/showthread.php?6406-Posting-News-on-the-front-page-not-the-Forums ).
  When you've got something ready for publishing you can make a publish request in the site feedback section of the forums (using the thread prefix options).

  Create also a forum post in "Your projects", like on
  http://www.pascalgamedevelopment.com/showthread.php?15067-Castle-Game-Engine-4-0-0-released
  This might make it easier to discuss (suggested on
  http://www.lazarus.freepascal.org/index.php/topic,19686.0.html).

- x3d-public mailing list

- if new VRML/X3D nodes supported:
  - remember to post to Don Brutzman about them,
    to update "node inventory spreadsheet"
      http://www.web3d.org/specifications/X3dNodeInventoryComparison.xlsx
      http://www.web3d.org/specifications/X3dNodeInventoryComparison.pdf
  - update http://www.web3d.org/wiki/index.php/Player_support_for_X3D_components

- http://www.lazarus.freepascal.org/
  Examples on:
  http://forum.lazarus.freepascal.org/index.php?topic=26927.msg166141
  http://www.lazarus.freepascal.org/index.php/topic,15653.0.html
  https://forum.lazarus.freepascal.org/index.php/topic,53399.0.html

- Post to community on FB:

  https://www.facebook.com/groups/lazide
  https://www.facebook.com/groups/freepascallazarusdelphi
  https://www.facebook.com/groups/DelphiDevelopers/
  https://www.facebook.com/groups/170041353008050 - Pascal Game Development
  https://www.facebook.com/groups/2326037874 - Pascal programming language
  https://www.facebook.com/groups/137012246341854 - Delphi developer

  (Note that you cannot post as Castle Game Engine to community on FB,
  just post as Michalis.)

  Sample post: https://www.facebook.com/groups/lazide/posts/3320318151558389/

- really large new features may cause updates of project description.
  Change project_description.txt text, and see there for a list of places
  where it's used.

  Also Lazarus wiki contains longer description of our project:
    http://wiki.freepascal.org/Castle_Game_Engine
    http://wiki.lazarus.freepascal.org/Components_and_Code_examples#Packages_for_FPC.2FLazarus_.28not_hosted_here.29
    http://wiki.lazarus.freepascal.org/Projects_using_Lazarus#Castle_Game_Engine
    http://wiki.freepascal.org/Developing_with_Graphics
    http://wiki.freepascal.org/Graphics_libraries

  Also FSF wiki has descriptions:
    https://directory.fsf.org/wiki/Castle_Game_Engine
    https://directory.fsf.org/wiki/View3dscene # or sthg like that

  https://www.indiedb.com/engines/castle-game-engine

  https://castle-engine.itch.io/castle-game-engine

  https://castle-engine.itch.io/view3dscene

- http://www.web3d.org/news/submit/
  ... but it's pretty much ignored, giving up on this.
  As well as submitting to web3d.org list of progs.

- An incredibly large release may be announced on fpc-pascal and lazarus
  mailing lists, but let's not abuse it (I did it only for 4.0.0 release
  when it was justified, as a large release and focused on devs).
  These lists aren't normally for announcements from external projects.

- Watch http://planetdev.freegamedev.net/ , they aggregate our news feed.

- Submit devlog on itch.io ( https://michaliskambi.itch.io/ )
  about Castle Game Engine and view3dscene releases.

- https://en.delphipraxis.net/forum/13-delphi-third-party/
  Sample: https://en.delphipraxis.net/topic/7480-castle-game-engine-70-alpha2-release/

  and browse links on
  https://blogs.embarcadero.com/community/
  for other places where it may be good to post.
