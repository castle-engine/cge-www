#!/bin/bash
set -eu

# -v mo¿na z ciekawo¶ci w³±czyæ (¿eby wiedzieæ co on w³a¶ciwie sprawdza)
#    chocia¿ wtedy to naprawdê wypisuje masê tekstu
#
# -o colored te¿ jest dobre (tylko wymaga konsoli, nie shella Emacsa)
#
# --anchor is needed, although slows down enormously...
#
# URLs ignored (because I decided that it's OK, their warnings/errors
# may be ignored) :
#
#   http://validator.w3.org/check/referer
#   http://en.wikipedia.org/wiki/Ray-tracing
#     disallow entry by robots.txt
#
#   http://stoma.name/michalis/castle-*,
#   http://127.0.0.1/vrmlengine/unofficial/,
#   http://127.0.0.1/vrmlengine/kambi_vrml_extensions.php#ext_worldinfo
#     some old links changes_log, that's OK, they can stay as dead
#     links for historical purposes.
#
#   http://www.ijg.org/
#     Host is unreachable. I would like to correct this link,
#     but I don't know to where.
#     http://en.wikipedia.org/wiki/Libjpeg doesn't help too.
#
#   http://rasmus.uib.no/~st01369/filarkiv/lyder.html
#     Page not available anymore. I don't care about it so much.
#
#   http://www.rush3d.com/reference/opengl-redbook-1.1/
#     I should correct these links one day...

linkchecker \
  --ignore-url=http://validator.w3.org/check/referer \
  --ignore-url=http://stoma.name/michalis/castle-with-sources-0.5.9.tar.gz \
  --ignore-url=http://stoma.name/michalis/castle-0.5.8.tar.gz \
  --ignore-url=http://stoma.name/michalis/castle-0.5.7.tar.gz \
  --ignore-url=http://127.0.0.1/vrmlengine/unofficial/ \
  --ignore-url=http://127.0.0.1/vrmlengine/kambi_vrml_extensions.php#ext_worldinfo \
  --ignore-url=http://www.ijg.org/ \
  --ignore-url=http://rasmus.uib.no/~st01369/filarkiv/lyder.html \
  --ignore-url=mailto:michalis.kambi%20AT%20gmail.com \
  --ignore-url=mailto:kambi%20AT%20users.sourceforge.net \
  --ignore-url=http://www.rush3d.com/reference/opengl-redbook-1.1/ \
  --ignore-url=http://www.rush3d.com/reference/opengl-bluebook-1.0/ \
  http://127.0.0.1/vrmlengine/