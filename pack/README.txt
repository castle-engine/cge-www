Scripts to automatically create release archives
(binary, for various platforms, and source).
It's *enormously* useful to have this process automated :)

These are bash scripts (under Windows basic Cygwin environment is required;
MinGW is probably also OK, although minor adjustments may be needed).

Procedure to release a program:

- Make sure Version constant in the program's source code is incremented.

  Make sure also CastleEngineManifest.xml <version value="..."/> is updated
  for a program.

  For the engine, update version numbers in these files:
  - make sure to update to "X.<even>" for release
  - all castle_game_engine/packages/*.lpk
  - castle_game_engine/src/base/castleversion.inc
  - castle_game_engine/tools/CastleEngineManifest.xml
    (and recompile castle-engine tool:
         cd ~/sources/castle-engine/castle-engine/tools/build-tool
         ./castle-engine_compile.sh
         mv -f castle-engine ~/bin/
         castle-engine -v # check
    ).

- Call ../scripts/generate_versions.sh script.
  - Before, you should recompile program for the current (source) OS.
    That's because generate_versions.sh actually calls program with --version
    to determine version number.

    In case of glinformation_glut, remember to recompile glinformation first, as
    `glinformation --version` determines version number used for glinformation_glut.
    (In general, see generate_versions.sh for other such possible exceptions).

  - You should run generate_versions script to update
    generated_versions.php (this makes version number on WWW page)
    generated_versions.sh (this makes version number for binary and
    source archives created by pack_binary and pack_pascal_src)

- Make sure you did `svn update' of program sources.
  And make sure no local modifications remain - `svn status' should return empty.

- Recompile the program with *release* settings:
  - make sure you're using correct FPC version (fpc -l).
    This will be actually checked by pack_binary, so also make sure
    that $REQUIRED_FPC_VERSION inside pack_binary.sh is Ok.
  - cd ~/sources/castle-engine/trunk/ && ./clean_everything.sh
  - compile program with release settings: see compile.sh scripts in program dirs
  - move executable to $EXEC_PATH/os-architecture/ defined in pack_binary.sh

- Call pack_binary.sh and pack_pascal_src.sh with proper options.
  You may use do_everything_example.sh as a starting point.

  Unpack and check resulting archives: unpack, and run.
  Run documentation in browser.

  Look at archive size, compare with size for previous version ---
  if any drastic change, investigate why (possibly it's as expected,
  possibly we packed too little / too much).

- make sure to update engine version to "X.<odd> (unstable)" for release

After doing this, follow SF update procedure on ../NOTES.txt .
