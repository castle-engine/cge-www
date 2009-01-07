#!/bin/bash
set -eu

# -v mo¿na z ciekawo¶ci w³±czyæ (¿eby wiedzieæ co on w³a¶ciwie sprawdza)
#    chocia¿ wtedy to naprawdê wypisuje masê tekstu
#
# -o colored te¿ jest dobre (tylko wymaga konsoli, nie shella Emacsa)
#
# --anchor is needed, although slows down enormously...
#

do_linkchecker ()
{
  linkchecker --config=/home/michalis/sources/vrmlengine/trunk/www/scripts/linkchecker.conf "$@"
}

do_linkchecker http://127.0.0.1/~michalis/vrmlengine/

# Test also do offline docs have links ok

cd offline_docs/
make clean all

# Ignore
#   images/kambi_lines/
#     It's Ok that they are missing: when actually generating kambi_lines,
#     they will be available.

do_linkchecker --ignore-url=images/kambi_lines/ *.html
