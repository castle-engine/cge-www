Code and data of Castle Game Engine website
===========================================

Layout
------

htodcs/ is exactly what goes into https://castle-engine.io/

screenshots/ are only a copy (sometimes with better resolution) of the
same screenshots that are available on SF screenshot page of our project.

scripts/ are various scripts, usually in bash, helpful to manage the website.

* scripts/shell.sf.net are meant to be run when logged on
  shell.sourceforge.net server, i.e. these scripts are supposed to work
  on currently "online" version on the website on SF.

* Rest of scripts/ should usually be run on your local version,
  checkouted somewhere on your local computer. Running them on
  shell.sf.net is not preferred, sometimes has little sense.

Procedure to update WWW content
-------------------------------

- Making a release:
  1. testing is described in TESTS.txt
  2. compiling and packing releases is described in pack/README.txt

- When release is packed, it's nice to tag it, see scripts/make_tags.sh script.
  Leave uncommented only the lines for newly released programs, and run it.

- Release on SourceForge.

  New FRS (http://p.sf.net/sourceforge/FRS):
  - Move older versions to subdirs in old_versions
    (Do it first, to avoid showing on SF project summary "last changes list"
    these moves as "latest actions")

  - Upload to
    sftp://kambi,castle-engine@frs.sourceforge.net/home/frs/project/c/ca/castle-engine
    (following https://sourceforge.net/p/forge/documentation/SCP/)
    See pack/upload_sourceforge.sh

  - Only for CGE: On https://sourceforge.net/projects/castle-engine/files/
    mark new CGE released zip as default download for all OSes.
    This is only for default download on given OS, we suggest here
    CGE as our "default package".

  After release:
  - Download and compare are the files the same.

- Release on GitHub.

  Upload a new release to
  https://github.com/castle-engine/castle-engine/releases
  https://github.com/castle-engine/view3dscene/releases
  and so on
  (a tag should already be created in the previous step, by scripts/make_tags.sh script)

  Update the URLs:
  - CGE_LATEST_DOWNLOAD in htdocs/castle_engine_functions.php
  - view3dscene links in htdocs/view3dscene.php

  After:
  - Download and compare are the files the same.

  TODO: uploading dmg on GitHub is not possible.
  The current solution for view3dscene was to zip the dmg, which is non-standard.
  Should we just use zip instead of dmg?
  https://daringfireball.net/2009/09/how_should_mac_apps_be_distributed

- if you modified castle_game_engine:
  - update apidoc/
  - update $engine_ver in htdocs/all_programs_sources.php, to reflect
    with what engine ver each sources were tested.

- good practice after large changes is to check
  linkchecker.sh and validate_html.sh

- if you change some content managed inside GIT:
  - commit and push your changes, of course
  - git pull --rebase on SF within cge-www

- if may be good to fix old links leading to
  http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/docs/ now
  to go to https://castle-engine.io/ .
  (M-x kam-castle-engine-snapshot-docs-to-normal in Michalis Emacs.)

- After updating CGE,
  update also FPC/Lazarus requirements:
  http://castle-engine.io/fpc_and_lazarus.php

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

- a release of engine or view3dscene or some other tools can also be
  uploaded to http://itch.io/ , see http://michaliskambi.itch.io/

  See pack/upload_itch_io.sh

Announcing release
------------------

- Regular annoucements sites:
  - Our Wordpress: https://castle-engine.io/wp/
  - Patreon: https://www.patreon.com/castleengine
  - Facebook: https://facebook.com/castleengine/
  - Google+: https://plus.google.com/+CastleGameEngineX3d/
  - Twitter: https://twitter.com/castleengine

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

- if new VRML/X3D extensions implemented:
  remember to post to Joerg (White Dune) about them

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

- Post new topic on our forum, to encourage some discussion.
  Possibly resign from it? Posting on our g+ page is easier,
  and there are also instant comments.

- Post to community on g+:

  FPC/Lazarus:
  https://plus.google.com/communities/114860965042324270757
  As Castle Game Engine:
  https://plus.google.com/b/101185352355602218697/communities/114860965042324270757
  Use tags #FreePascal #ObjectPascal #GameDevelopment

  Delphi:
  https://plus.google.com/b/101185352355602218697/communities/103113685381486591754

- Post to community on FB:
  https://www.facebook.com/groups/LazarusFPCDevForum/?ref=browser
  We cannot post as Castle Game Engine to community? So just post as Michalis.

  https://www.facebook.com/groups/DelphiDevelopers/

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

- http://www.web3d.org/news/submit/
  ... but it's pretty much ignored, giving up on this.
  As well as submitting to web3d.org list of progs.

- Update http://www.devmaster.net/engines/
  update devmaster page about view3dscene

- An incredibly large release may be announced on fpc-pascal and lazarus
  mailing lists, but let's not abuse it (I did it only for 4.0.0 release
  when it was justified, as a large release and focused on devs).
  These lists aren't normally for announcements from external projects.

- watch http://planet.objpas.org/ (not active anymore?) and
  http://planetdev.freegamedev.net/ , they aggregate our news feed.
