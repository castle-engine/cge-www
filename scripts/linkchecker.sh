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

#do_linkchecker https://castle-engine.io/
do_linkchecker http://localhost:8777/

# If you place CGE website in non-root, remember to adjust .htaccess to make redirects work OK.
# Otherwise linkchecker will make a lot of invalid errors, that are handled as redirects
# in proper CGE WWW installation.
#do_linkchecker http://localhost/~michalis/castle-engine/

# do_linkchecker *.html
