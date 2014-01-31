Libraries (dll files) required by Castle Game Engine
( http://castle-engine.sourceforge.net/ ) programs on Windows 32-bit (i386).

These may be useful for developers using Castle Game Engine to develop/distribute
their own programs, and for people trying our snapshot releases
( http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/ or from SVN).
If you simply downloaded a stable binary release of one of our programs,
like view3dscene ( http://castle-engine.sourceforge.net/view3dscene.php ),
they you don't need this.
All binary releases of our programs already contain all the necessary libraries.

Note that not all programs require all the libraries.
For example, OpenAL and OggVorbis is only used if you want 3D sound.
FreeType is only used if you want to read font files (like .ttf).
See http://castle-engine.sourceforge.net/apidoc/html/index.html
for details what is required for what purpose.

Sources of these DLLs:
- libpng, zlib, freetype: http://gnuwin32.sourceforge.net/ .
  - libpng depends on zlib.
  - freetype depends on zlib.
    It was renamed from freetype6.dll to freetype-6.dll,
    to follow name in FPC unit packages/fcl-image/src/freetypeh.pp.

- OpenAL: as installed by Creative's OpenAL Windows installer,
  from http://www.openal.org/downloads.html

- OggVorbis: http://www.vorbis.com/ (don't remember exactly where,
  but I found some archive with precompiled vorbis libs for Windows
  looking from http://www.vorbis.com/)

Michalis
