Castle Game Engine documentation using AsciiDoctor.

Going forward, we want to maintain all CGE docs like this -- AsciiDoctor is really comfortable.
See our annoucement and Michalis thoughts about ways to write documentation on
https://castle-engine.io/wp/2021/12/31/many-documentation-upgrades-using-asciidoctor-as-our-primary-way-to-write-documentation-michalis-thoughts-and-plans-about-our-documentation/ .

These pages are rendered using ../doc.php . Users access them just like

  https://castle-engine.io/build_tool

-> this is converted (using mod_rewrite in ../.htaccess) to

  https://castle-engine.io/doc.php?page=build_tool

-> in effect, this shows

  `doc/build_tool.html` (on production, already processed to HTML)

  `doc/build_tool.adoc` (on development, this processed by AsciiDoctor to HTML each time you access the `doc.php?page=xxx`)

On production, after doing any changes, we make sure to execute `make`
(to refresh HTMLs, also it allows to see AsciiDoctor errors).

Naming convention:
- all lowercase
- with underscore (not space, not -) inside
- avoid special chars like ( ) , in filenames, even when actually allowed
