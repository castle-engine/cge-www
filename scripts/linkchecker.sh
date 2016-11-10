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
  linkchecker --config=`pwd`/linkchecker.conf "$@"
}

do_linkchecker http://127.0.0.1/~michalis/castle-engine/

# do_linkchecker *.html
