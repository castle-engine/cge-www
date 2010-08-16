In this directory are the scripts that automatically create archives
of my programs (binary, for various platforms, and source).
Needless to say, it's *enormously* usefull to have this process automated :)

These are bash scripts (under Windows basic Cygwin environment is required;
MinGW is probably also OK, although minor adjustments may be needed).

Some notes about implementation:
- I use my mk_archive functions.
  I tried to implement every target as a sequence of commands like
    mk_archive_begin
    add_component_1
    add_component_2
    ...
    mk_archive_pack
    mk_archive_end
  This makes a flexible implementation: it's often easy to customize some
  target by adding appropriate add_component_* calls, and everything
  else stays as it is. This is important, as I pack various things,
  and some of them require various treatment.

  This also makes it easy to see how packing of given target goes.
  Instead of writing and calling one complicated bash function
  to handle various cases, I just create and call a couple of simple
  add_component_* functions.

- mk_archive functions create temporary directory to prepare archive
  contents. This prooved to be very usefull for a number of reasons:
  1. dziêki temu ¿e nie u¿ywamy na potrzeby archiwum jakiego¶ istniej±cego ju¿
     katalogu (jak kiedy¶ uzywa³em $PROGRAM_PATH)
     nie musimy siê przejmowaæ tym ¿e pliki naszego archiwum mog±
     kolidowaæ z jakimi¶ istniej±cymi ju¿ plikami, i mo¿emy pó¼niej usun±æ
     wszystkie ¶miecie po prostu usuwaj±c TEMP_ARCHIVE_PATH za jednym zamachem,
  2. mo¿na ³atwo testowaæ skrypt - wystarczy usun±æ rm -Rf $TEMP_ARCHIVE_PATH
     na koñcu aby zobaczyæ co dok³adnie znalaz³o siê w archiwum
  3. pod UNIXem musimy skopiowac caly katalog na jakis system plikow POSIXowy
     zeby ustawic dobre permissions : 755 dla binarki i katalogow,
     dla innych plikow 644. Tego sie kurwa naprawde nie da zrobic
     uzywajac --mode z tar (bo nie mam mu jak powiedziec zeby moje
     pliki, ktore maja execute bo sa na FATcie, nie mialy trybu execute,
     ale zeby katalogi mialy; to jest dumbness tar'a i chmod'a i mount'a
     razem). Zak³adanie TEMP_ARCHIVE_PATH w /tmp za³atwia to - /tmp jest na pewno
     na POSIXowym systemie plików.

- Architectures notes (since I have 32 bit i386 and x86_64 now):
  Debian calls Intel x86 (i386) / AMD64 (amd64)
  FPC calls i386 / x86_64
  I choose i386 / x86_64, seems better than amd64 name.

Common things to do before calling ./update_archives.sh
on some program:

- Make sure Version constant in the program's source code in updated.

- Make sure you did `svn update' of program sources.
  And make sure no local modifications remain - `svn status' should return empty.

  Under Linux, take care to update both ~/sources/vrmlengine/trunk/ and
  /mnt/fat/mojepasy/vrmlengine/trunk/, as update_xxx scripts work with
  the latter. So be sure that they are in sync, just to be safe.

- Recompile the program with *release* settings:
  - make sure you're using correct FPC version
  - (g)make -C ~/sources/vrmlengine/trunk/kambi_vrml_game_engine/ clean
  - open program's .lpr file in Emacs
  - compile it with C-F10, this will compile with proper *release* options

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

  Before packing new kambi_vrml_game_engine version,
  check tests: ~/sources/vrmlengine/trunk/www/TESTS

After this, see SF update procedure on
~/sources/vrmlengine/trunk/www/NOTES
