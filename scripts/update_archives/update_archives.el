(defun create-compile-script (progs-list &optional uses-glwindow)
  "This function inserts text that is a valid bash (or sh) script
that calls fpc a couple times to compile some programs.
E.g.

  #! /bin/sh
  set -eu

  # Some comments

  cd ../castle_game_engine/

  fpc -dRELEASE @kambi.cfg ../rayhunter/rayhunter.dpr

Such scripts are then embedded by update_archives.sh script
inside archives of my Pascal programs' sources.

Header lines (#! /bin/sh,  set -eu, some comments, cd ../)
are constant. Rest of lines is determined by PROGS-LIST.
PROGS-LIST is just a list of strings, each string S
produces line
(concat \"fpc -dRELEASE @kambi.cfg \" S)

So each item on PROGS-LIST is a relative filename of program's
dpr file (actually, it could also be a unit if you're going
to compile a unit; but, for now, I use this only to generate
compile.sh scripts that compile programs). And it can be prefixed
with things like \"-dNOT_USE_LIBPNG \".

If USES-GLWINDOW is non-nil then also a line to remove glwindow.{o,ppu}
files will be added. This is handy to make sure that recompilation
works with good options. Compilation speed will suffer (since GLWindow
will be always recompiled), but correctness is more important here."
  (interactive)

  (insert "#! /bin/sh
set -eu

# This is automatically generated script that should compile
# all programs in this archive. It simply calls FPC
# with proper command-line options.
#
# We must do cd ../castle_game_engine/ (and call FPC from that directory)
# because kambi.cfg file is there and it contains paths relative
# to that directory.

cd ../castle_game_engine/

")

  (when uses-glwindow
    (insert
"# This program uses GLWindow unit. GLWindow unit may be compiled
# with various back-ends (e.g. under Unices two most useful back-ends
# are XLIB and GTK). To make sure that compilation of this program
# will produce exactly what you need, below we make sure that
# unit GLWindow will be *always* *rebuild*.
#
# Of course this means that compilation time will suffer a little,
# since GLWindow unit will be possibly rebuild without any real need.
# Comment out line below if you want.
rm -f opengl/glwindow.o \\
      opengl/glwindow.ppu \\
      opengl/GLWindow.o \\
      opengl/GLWindow.ppu

"))

  (dolist (item progs-list)
    (insert (concat "fpc -dRELEASE @kambi.cfg " item "\n"))
  )
)

;; temp:
;; (create-compile-script '("-Fusniffer/units_net/ sniffer/sniffer.dpr"))