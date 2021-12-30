# Code and data of Castle Game Engine website

## Layout

htodcs/ is exactly what goes into https://castle-engine.io/

screenshots/ are only a copy (sometimes with better resolution) of the
same screenshots that are available on SF screenshot page of our project.

scripts/ are various scripts, usually in bash, helpful to manage the website.

* scripts/remote/ are meant to be run when logged on castle-engine.io
  through ssh.

* Rest of scripts/ can be run on a local version,
  cloned somewhere on your local computer.

## Testing Locally

* Install Apache (or any other web server that can handle PHP)
* Install PHP
* Install MySQL
* To regenerate AsciiDoctor automatically, install AsciiDoctor and CodeRay.

    On Debian-like systems, just do `apt install asciidoctor coderay`.

* For local testing, create empty Wordpress database

    ```
    CREATE DATABASE cgewp;
    GRANT ALL PRIVILEGES ON cgewp.* TO 'cgewp'@'localhost' IDENTIFIED BY 'devpassword';
    ```

* Make htdocs/ here known to Apache.
  There are may ways to do this, I advise to do this on Linux and use userdir module:
  - enable Apache "userdir" module
  - make sure it works, and that PHP inside works, by
    - creating a file `/home/USERNAME/public_html/a.html` with contents `Test <b>bold</b>`. It should be visible on http://localhost/~USERNAME/a.html . If this works -> Apache + userdir works.
    - creating a file `/home/USERNAME/public_html/a.php` with contents `<?php phpinfo(); ?>`. It should be visible on http://localhost/~USERNAME/a.php . If this works -> then PHP (in user directory) works too.
  - allow `.htaccess` in cge-www, by adding something like this to Apache config:
      ```
      <Directory "/home/USERNAME/public_html">
          AllowOverride All
      </Directory>
      ```
  - Symlink cge-www:
    ```
    cd ~/public_html
    ln -s ~/sources/castle-engine/cge-www/htdocs/ castle-engine/
    ```
  - in the end, http://localhost/~michalis/castle-engine/ should work.
  - adjust RewriteBase in htdocs/.htaccess to make the rewrite rule work (to access e.g. http://localhost/~michalis/castle-engine/Build_Tool )

## Release making (for CGE engine or application, like view3dscene)

- Test as described in TESTS.txt

- Bump versions:

    - For applications (like view3dscene):

        - Make sure Version constant in the program's source code is incremented.

        - Make sure also CastleEngineManifest.xml <version value="..."/> is updated for a program.

    - For the engine, update version numbers in these files:

        - make sure to update to "X.<even>" for release
        - all ../../castle-engine/packages/*.lpk
        - ../../castle-engine/src/base/castleversion.inc
        - ../../castle-engine/tools/build-tool/CastleEngineManifest.xml
        - ../../castle-engine/tools/castle-editor/macos/create_bundle.sh
          (and recompile castle-engine tool:
               cd ~/sources/castle-engine/castle-engine/tools/build-tool
               ./castle-engine_compile.sh
               mv -f castle-engine ~/bin/
               castle-engine -v # check
          ).
        - doc/pasdoc/html-parts/body-end.php
          (and run "make" in doc/pasdoc/html-parts/ to refresh API docs extra HTML).

- Call scripts/generate_versions.sh script.

  - Before, you should recompile program for the current (source) OS.
    That's because generate_versions.sh actually calls program with --version
    to determine version number.

  - You should run generate_versions script to update
    generated_versions.php (this makes version number on WWW page)
    generated_versions.sh (this makes version number for some binary archives, like on itch.io).

- Tag releases using scripts/make_tags.sh script.
  Leave uncommented only the lines for newly released programs, and run it.

- Make new releases (save as draft now) from existing tags, like
  https://github.com/castle-engine/castle-view-image/releases/new
  https://github.com/castle-engine/castle-engine/releases/new
  https://github.com/castle-engine/view3dscene/releases/new
  https://github.com/castle-engine/glplotter/releases/new

- Upload to releases on GitHub latest builds by Jenkins of the appropriate application/engine.
  The Jenkinsfile for each project has perfect commands to make a build of every application,
  using advised FPC version etc. So we just upload these builds as releases.

  Do it automatically:

  ssh jenkins.castle-engine.io
    sudo -u jenkins-cge-uploader -i
      cd cge-update-github-snapshot/
      # adjust ./cge-update-github-releases
      ./cge-update-github-releases

  TODO: This process is not perfect, since we upload the builds from latest (master)
  as the "last tagged" project version.
  In case there were some commits since make_tags.sh, the binary builds will be a bit newer.
  Not a big problem in practice, just manually watch it.
  You can uncomment special lines in `scripts/make_tags.sh` to delete tags beforehand, to set new ones.
  But it would be best if Jenkins would build tags.

  Exception: for glplotter, I just used pack.sh in glplotter code to build last release.

- Publish releases

- Update the URLs:
  - view3dscene links in htdocs/view3dscene.php (see lower about "how to update www content")

  After:
  - Download and compare are the files the same.

  TODO: uploading dmg on GitHub is not possible.
  The current solution for view3dscene was to zip the dmg, which is non-standard.
  Should we just use zip instead of dmg?
  https://daringfireball.net/2009/09/how_should_mac_apps_be_distributed

- if you released new castle_game_engine version:
  - update ../castle-engine/doc/pasdoc/html-parts/body-end.php with new versions,
  - update apidoc by
    cd ../castle-engine/doc/pasdoc/html-parts/
    make clean default
    cd ../
    make clean upload
  - update $engine_ver in htdocs/all_programs_sources.php, to reflect
    with what engine ver each sources were tested.
  - update engine version linked on htdocs/index.php
  - update FPC/Lazarus requirements:
    https://castle-engine.io/supported_compilers.php

- In case of CGE and view3dscene, upload them also to http://castle-engine.itch.io/ :
  Run

  ```
  pack/upload_itch_io.sh
  ```

- In case of CGE and view3dscene, upload them also to https://sourceforge.net/projects/castle-engine/ files:
  Run

  ```
  pack/upload_sourceforge.sh
  ```

  Mark on SourceForge default download for Linux/Windows as latest CGE.

- bump versions afterwards for snapshots:

    - engine: version to `x.<odd>.0` or `alpha.snapshot`

    - view3dscene: version to `x.<odd>.0`

## Website updating

- Good practice after large changes is to check
  linkchecker.sh and
  validate_html.sh

- if you change some content managed inside GIT:
  - commit and push your changes, of course
  - git pull --rebase on castle-engine.io (ssh-ceup)

- if you change some content outside GitHub cge-www repository:
  (These are some files that are automatically generated and it would
  be a waste to keep them inside a version control repository... so they have to
  be copied in a normal way.) :
  - reference (API docs) and vrml_engine_doc can be automatically uploaded by
    "make upload" in the appropriate sources dir.
  - make sure that "other" users don't have uncessesary permissions:
    call secure_permissions.sh on server
  - Old (no longer regularly checked):
    - run mk_sums_md5.sh locally. This calculates md5 sums on your local
      files.
    - on remote server, run check_sums_md5.sh. This will check md5 sums
      uploaded in previous step, thus checking that all files
      were uploaded correctly.

## Announcing release

- Regular annoucements sites:
  - Our Wordpress: https://castle-engine.io/wp/
  - Patreon: https://www.patreon.com/castleengine
  - Facebook: https://facebook.com/castleengine/
  - Twitter: https://twitter.com/castleengine
  - Mastodon: https://mastodon.social/@michalis_kambi

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

  https://www.facebook.com/groups/LazarusFPCDevForum/
  https://www.facebook.com/groups/DelphiDevelopers/
  https://www.facebook.com/groups/170041353008050/

  (Note that you cannot post as Castle Game Engine to community on FB,
  just post as Michalis.)

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

  https://michaliskambi.itch.io/castle-game-engine

  https://michaliskambi.itch.io/view3dscene

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
