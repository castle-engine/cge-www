## Release making (for CGE engine or application, like castle-model-viewer)

- Test as described in RELEASE_MAKING_TESTS.md

- Bump versions (commit but do not push yet):

    - For applications (like castle-model-viewer):

        - Make sure Version constant in the program's source code is incremented.

        - Make sure also CastleEngineManifest.xml <version value="..."/> is updated for a program.

    - For the engine, update version numbers in these files:

        - make sure to update to "X.<even>" for release
        - all ../castle-engine/packages/*.lpk
        - ../castle-engine/src/base/castleversion.inc
        - ../castle-engine/tools/build-tool/CastleEngineManifest.xml

           and recompile castle-engine tool:

           ```
           # cge-prepare # private Michalis script, or use below:

           cd $CASTLE_ENGINE_PATH/tools/build-tool
           ./castle-engine_compile.sh
           mkdir -p $CASTLE_ENGINE_PATH/bin
           mv -f castle-engine $CASTLE_ENGINE_PATH/bin
           export PATH="$CASTLE_ENGINE_PATH/bin:$PATH"
           castle-engine -v # check
           ```

        - ../castle-engine/doc/pasdoc/html-parts/body-end.php
          (and run "make clean && make" in doc/pasdoc/html-parts/ to refresh API docs extra HTML).
        - check: grep "7.0-alpha"

- Call in cge-www scripts/generate_versions.sh script.

  - Note: This is less important in new work.
    The version numbers stored here matter less and less,
    and we plan to just remove them eventually.

  - Before, you should recompile program for the current (source) OS.
    That's because generate_versions.sh actually calls program with --version
    to determine version number.

  - You should run generate_versions script to update
    generated_versions.php (this makes version number on WWW page - OBSOLETE)
    generated_versions.sh (this makes version number for some binary archives,
    like on itch.io).

- Change GitHub Actions YAML to upload to vX.Y.Z tag.

    In GitHub Actions (~/.github/workflows/*.yml) find line like this:

    ```
    env:
      # To which GitHub release tag should we upload artifacts.
      # Can be "snapshot" or "vX.Y.Z" (latter when we make stable release).
      release_tag: snapshot
    ```

    Change to `release_tag: vX.Y.Z` and commit.

- Create and push GIT tag vX.Y.Z, pushing also all above changes.

    Notes:
    - Needs to be made by GIT, *not* using GitHub release "create tag on publish",
      because we want to push files to this tag before publishing.
    - Pushing tag to repo (see below) requires that related commit be also pushed.
      Our workflow can update the tag later anyway to newer commit,
      so don't worry, don't be afraid to push the tag :)

    ```
    git tag -a vX.Y.Z -m "Tagging the version X.Y.Z."
    git push # push all commits
    git push origin vX.Y.Z # push tag to repo
    ```

- Create new *draft* release from existing tags on GitHub, like

  https://github.com/castle-engine/castle-engine/releases/new
  https://github.com/castle-engine/castle-image-viewer/releases/new
  https://github.com/castle-engine/castle-model-viewer/releases/new

  Set proper GIT tag.
  The release will not have files -- we will let GitHub Actions
  to fill it with files next.

- Wait for GH Actions to update the release.

- Write release notes, for GH and Wordpress:

  - Write using AsciiDoc:
    - Make a separate page on our website like `release_7.0-alpha.3.adoc`.
    - Link to it from `castle_engine_functions.php` and `release.adoc`.
  - Convert to Markdown:
    - Take the "summary" part to separate temp file `summary.adoc`
    - Make links absolute (link: -> https://castle-engine,io/)
    - Convert to Markdown (for GH release) using https://github.com/opendevise/downdoc
      ```
      npm i downdoc
      npx downdoc summary.adoc
      ```
    - Markdown is good for GH release text and Wordpress.

- Publish release on GitHub - fill with final description, press "Publish".

- Update the webpage:
  - Update versions in .adoc:

    - section names like `Download (version 2.1.0)`
    - usage of `cge::download-application`
    - for engine, usage of `cge::download-engine`

    Note: If you changed something outside of ADOC, that affects HTML generation,
    do (on server) `touch xxx.adoc` to force regeneration of given page.
    *Do not* do `rm -f output/download.html` to force this (as this means
    the page gets temporarily broken).

  - Grep to make sure all things changed
    7.0-alpha.snapshot
    7.0-alpha-snapshot # mistake
    7.0-alpha.1
    7.0-alpha1 # mistake
    to make sure everything is updated.

- Download and make last tests.

- if you released new castle_game_engine version:
  - make sure new apidoc is already uploaded.

      Alt: you can upload it manually

      ```
      cd ../castle-engine/doc/pasdoc/
      make clean upload
      ```

  - update FPC/Lazarus requirements if needed:
    https://castle-engine.io/supported_compilers.php

  - update CGE version of Docker `cge-stable`:

    Open https://github.com/castle-engine/castle-engine-docker/ .

    Grep for 7.0-alpha, change to new version, commit and push.
    GHA will build new Docker image for `cge-stable`.

- In case of CGE and castle-model-viewer, upload them also to http://castle-engine.itch.io/

  ```
  # This is to make sure you have itch.io butler installed and authenticated.
  # Follow https://itch.io/docs/butler/installing.html if not yet,
  # you can actually install it using itch.io GUI app: https://itch.io/app ,
  # just "chmod +x ..." the butler binary and add it to $PATH.
  butler status castle-engine/castle-game-engine
  ```

  - To upload CGE, run script in this repo:
      pack/upload_itch_io.sh

  - To upload castle-model-viewer, run script from castle-model-viewer repo:
      cd ..../castle-model-viewer/pack/itch_io/
      ./upload_itch_io.sh

  Once `butler status ...` is good,
  - test downloading it from
      https://castle-engine.itch.io/castle-model-viewer
      https://castle-engine.itch.io/castle-game-engine
    (using itch.io app).
  - write a new devlog on itch.io (copy-paste from GitHub release notes)

  Once `butler status castle-engine/castle-game-engine` is good,
  - test downloading it from  (using itch.io app)
  - write a new devlog there (copy-paste from GitHub release notes)

- In case of CGE and castle-model-viewer, upload them also to https://sourceforge.net/projects/castle-engine/ files:

  Run

  ```
  pack/upload_sourceforge.sh
  ```

  Mark on SourceForge default download for Linux/Windows as latest CGE.

  Login to
  https://sourceforge.net/projects/castle-engine/

  Check this contains new files:
  https://sourceforge.net/projects/castle-engine/files/castle-model-viewer/
  https://sourceforge.net/projects/castle-engine/files/castle_game_engine/

  Mark new CGE files as default for Linux/Windows.
  After a moment, Ctrl+R will show new version.

- bump versions/code afterwards, to again make nice snapshots:

    - engine: version to `x.<odd>.0` or `alpha.snapshot`

      (redo again the section above about bumping version,
      also recheck "bump version" commit that we touched the same files)

      Also revert ../castle-engine/tools/internal/pack_release/github_update_release.txt
      to snapshot.
      TODO: This file will be removed soon.

    - applications: version to `x.<odd>.0`

- Fix snapshots:

    - change GitHub Actions YAML to upload to `snapshot` tag.
    - remove from `snapshot` release the old versions (new versions have new names).
    - make sure new snapshot done OK.

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

- Regular announcements sites:
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

  http://freshcode.club/projects/castle-model-viewer
  http://freshcode.club/projects/castle-engine

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
    https://directory.fsf.org/wiki/View3dscene
      # Note: Unsure how to rename, but I did edit the name for "Castle Model Viewer".
      # Note: No permissions to upload screenshots.

  https://www.indiedb.com/engines/castle-game-engine

  https://castle-engine.itch.io/castle-game-engine
  https://castle-engine.itch.io/castle-model-viewer

- http://www.web3d.org/news/submit/
  ... but it's pretty much ignored, giving up on this.
  As well as submitting to web3d.org list of progs.

- An incredibly large release may be announced on fpc-pascal and lazarus
  mailing lists, but let's not abuse it (I did it only for 4.0.0 release
  when it was justified, as a large release and focused on devs).
  These lists aren't normally for announcements from external projects.

- Watch http://planetdev.freegamedev.net/ , they aggregate our news feed.

- Submit devlog on itch.io ( https://castle-engine.itch.io/ )
  about Castle Game Engine and castle-model-viewer releases.

- https://en.delphipraxis.net/forum/13-delphi-third-party/
  Sample: https://en.delphipraxis.net/topic/7480-castle-game-engine-70-alpha2-release/

  and browse links on
  https://blogs.embarcadero.com/community/
  for other places where it may be good to post.
