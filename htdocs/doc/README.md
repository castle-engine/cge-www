Castle Game Engine documentation using AsciiDoctor.
Going forward, we want to maintain all CGE docs like this -- AsciiDoctor is really comfortable.

Right now, most of these pages come from GitHub wiki, but remade to AsciiDoctor.

These pages are rendered using ../doc.php . Access them just like

  https://castle-engine.io/build_tool

-> this is converted (using mod_rewrite in ../.htaccess) to

  https://castle-engine.io/doc.php?page=build_tool

-> in effect, this shows

  doc/build_tool.html (on production)

  doc/build_tool.adoc | processed by asciidoctor (on development)

After doing changes, be sure to "make" (to refresh HTMLs for development,
also it allows to see AsciiDoctor errors).

Naming convention:
- all lowercase
- with underscore (not space, not -) inside
- avoid special chars like ( ) , in filenames, even when actually allowed
