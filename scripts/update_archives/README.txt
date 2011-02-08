Here are the scripts to automatically create release archives
(binary, for various platforms, and source).
It turned out *enormously* usefull to have this process automated :)

These are bash scripts (under Windows basic Cygwin environment is required;
MinGW is probably also OK, although minor adjustments may be needed).

Notes:
- Architectures names (since I have 32 bit i386 and x86_64 now):
  Debian calls Intel x86 (i386) / AMD64 (amd64)
  FPC calls i386 / x86_64
  I choose i386 / x86_64, seems better than amd64 name.

Common things to do before calling ./update_archives.sh
on some program:

- Make sure Version constant in the program's source code in updated.

- Make sure you did `svn update' of program sources.
  And make sure no local modifications remain - `svn status' should return empty.

- Recompile the program with *release* settings:
  - make sure you're using correct FPC version (fpc -l)
  - cd ~/sources/vrmlengine/trunk/ && ./clean_everything.sh
  - compile program with release settings, and move binary where appropriate:
    if using Emacs, open program's .lpr file, and compile with C-F10
    if not, see kam-compile-release-command* at the bottom of .lpr file

- If the program has version number:
  - You should recompile program both for target OS (the OS for which you
    want to pack) and the OS where you're going to call generate_versions.sh.
    That's because generate_versions.sh actually calls program with --version
    to determine version number.

    In case of glinformation_glut, remember to recompile glinformation first, as
    `glinformation --version` determines version number used for glinformation_glut.
    (In general, see generate_versions.sh for other such possible exceptions).

  - You should run generate_versions script to update
    generated_versions.php (this makes version number on WWW page)
    generated_versions.sh (this makes version number for binary and
    source archives created by update_archives and update_pascal_src)

- Call ./update_archives.sh with proper options

  You can check generated archives again, by unpacking,
  look into executable files for correct FPC version and OS/arch.
  Run documentation in browser.

- Call ./update_pascal_src.sh with proper options

After this, see SF update procedure on
../../NOTES.txt
