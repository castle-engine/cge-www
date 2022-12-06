# Code and data of Castle Game Engine website

## Editing

We use a few ways to edit the content:

### AsciiDoctor for static pages

Example of page written this way: https://castle-engine.io/gltf , content in `htdocs/doc/gltf.adoc` .

To edit: Fork this repository (cge-www) and edit the `*.adoc` pages in `htdocs/doc/` subdirectory. Then create a pull request.

See https://asciidoctor.org/ for an overview of the AsciiDoctor language. To preview your edits:

- look at GitHub preview of your file
- or run `asciidoctor xxx.adoc`
- or (more complicated but presents complete preview) follow the _"Testing Locally"_ instructions below to set up your own local copy of CGE website.

We support additional macros within AsciiDoctor:

- `cgeref`: Place link to API reference of some Pascal identifier, with optional customized title.
  The Pascal identifier is automatically found in global identifiers in latest
  CGE API docs (using https://pasdoc.github.io/PhpOutput underneath).
  Use like this:

  ```
  cgeref:TCastleScene[]
  cgeref:TCastleScene[link text for TCastleScene]
  ```

  If you want to write `[` or `]` inside the link title,
  write `{{{` or `}}}` instead. They will be replaced with `[` or `]` . Like

  ```
  cgeref:TMessaging.Send[Messaging.Send({{{'message-1','some-parameter'}}})]
  ```

- `cgeimg`: Place images, using CGE style. Images can be floating (on the right)
  or be a block within page content flow.

  ```
  cgeimg::block[aaa.png|Description of AAA,bbb.png|Description of BBB]
  cgeimg::float[aaa.png|Description of AAA,bbb.png|Description of BBB]

  // This example has image filenames that actually exist, so go ahead and try pasting it some .adoc
  cgeimg::block[dragon_0_wire.png|Dragon,gamma_nogamma_helmet.png|Gamma Correction]
  cgeimg::float[dragon_0_wire.png|Dragon,gamma_nogamma_helmet.png|Gamma Correction]

  // Additional whitespace and newlines are allowed for cgeimg, so you can write it like this too
  cgeimg::block[
    dragon_0_wire.png|Dragon,
    gamma_nogamma_helmet.png|Gamma Correction
  ]
  ```

  Note: you can use backslash to write an ordinary comma within the image description.

  ```
  cgeimg::block[aaa.png|A very\, very\, very long description]

  cgeimg::block[aaa.png|Can be used to escape bar too\| <- like this]

  cgeimg::block[aaa.png|Double your backslashes to be safe, \\ <- this outputs a backslash]
  ```

  We expect to find referenced images (aaa.png,bbb.png in examples below) in
  `htdocs/images/original_size/`.
  You can regenerate all thumbnails using `cd htdocs/images/ && make`,
  though at development they will also be regenerated as-needed by PHP.
  To allow the PHP to do this,

    - install ImageMagick (`convert` must be on $PATH),

    - GNU `make`,

    - and make sure PHP has permissions to write within proper images/ subdirs: `chmod -R a+rwX htdocs/images/*_size/`.

  The idea is that (at development) you

    - Just drop new images to `htdocs/images/original_size/` (hint: make it a bookmark in your file manager)

    - Add `cgeimg` to .adoc source code to view them

    - Refresh the local page like http://localhost:8777/curves_tool , which will (in development) automatically regenerate AsciiDoctor -> HTML, and will generate thumbnails if missing. (hint: set up a key shortcut in your text editor to refresh the www browser view of this page)

- Note: AsciiDoctor macros above are not really implemented as AsciiDoctor macros in Ruby,
  following

    - https://docs.asciidoctor.org/asciidoctor/latest/extensions/inline-macro-processor/
    - https://docs.asciidoctor.org/asciidoctorj/latest/extensions/block-macro-processor/
    - https://docs.asciidoctor.org/asciidoctor/latest/extensions/block-macro-processor/

  Instead they are implemented using PHP, as replacements on top of the AsciiDoctor processing.
  Why? Because we needed to implement them in PHP too,
  as our pages need these functions in PHP too, and for Wordpress shortcodes too.

### PHP for static pages

Example of page written this way: https://castle-engine.io/manual_state_events.php , content in `htdocs/manual_state_events.php` .

To edit: Fork this repository, edit php file in `htdocs/`, and create a pull request.

To test your modifications, you can follow the _"Testing Locally"_ instructions below. If these instructions look too complicated, just ignore them for simple modifications --- the PHP files are mostly using trivial HTML with small PHP additions (like PHP `castle_header`, `cgeRef` functions) so if you understand basic HTML, you can probably figure out what is going on.

Most important PHP functions are:

- `castle_header`, `castle_footer` to start every CGE webpage

- `cgeRef`, `cgeImg` -- consistent with above-described AsciiDoctor macros. Example usage:

    ```
    <?php
    echo cgeImg('float', array(
      array('filename' => 'state_events_screen.png', 'titlealt' => 'Plane flying on the mountain background - game'),
      array('filename' => 'state_events_biplane_4_resized.png', 'titlealt' => 'Plane flying on the mountain background - design'),
    ));
    ?>

    Something descends from <?php echo cgeRef('TUIState'); ?> and manages something.
    ```

### Wordpress for news

Example page: https://castle-engine.io/wp/ .

To edit: contact Michalis Kamburelis (michalis@castle-engine.io) to get a Wordpress account. Page contents are in Wordpress database.

Most important Wordppress shortcodes are:

- `[gallery...]` - modified Wordpress gallery to use castle_thumbs (quite like cgeImg) to display Wordpress images

- `[cgeref...]`, `[cgeimg....]` -- consistent with above-described AsciiDoctor macros. Example usage:

    ```
    [cgeimg block images="dragon_0_wire.png|Dragon,gamma_nogamma_helmet.png|Gamma Correction"]

    [cgeref id=TCastleScene]
    [cgeref id=TCastleScene title="link text for TCastleScene"]
    ```

## Layout of files

* `htdocs/` is exactly what goes into https://castle-engine.io/

    * `htdocs/doc/` are documents in AsciiDoctor. Subset of https://castle-engine.io/ is generated from AsciiDoctor.

    * `htdocs/wp/` is our Wordpress installation, used right now to manage news.

* `screenshots/` are only a copy (sometimes with better resolution) of the same screenshots that are available on SF screenshot page of our project.

* `scripts/` are various scripts, usually in bash, helpful to manage the website.

    * `scripts/remote/` are meant to be run when logged on castle-engine.io through ssh.

    * Rest of `scripts/` can be run on a local version, cloned somewhere on your local computer.

## Testing Locally

* Install Apache (or any other web server that can handle PHP)

    - Enable `mod_rewrite`. On Debian-based systems, do `sudo a2enmod rewrite && sudo service apache2 restart`.

* Install PHP

* Make htdocs/ referenced from Apache. There are may ways to do this. I advise to do this on Linux and make the website root accessible under URL like http://localhost:8777/ (which would open `htdocs/index.php` file in this repo).

    - Listen on port 8777. Edit `/etc/apache2/ports.conf` and add `Listen 127.0.0.1:8777`.

        Note: There's nothing special about the 8777 port. It just seems unused by most other software (looking at https://en.wikipedia.org/wiki/List_of_TCP_and_UDP_port_numbers ), so it is likely non-conflicting with anything else on your system.

    - Create new site, following `apache-sample.conf` example. You can copy and adjust it to `/etc/apache2/sites-available/cge-www.conf` . Or even symlink it, if you use the same path as me: `cd /etc/apache2/sites-available && sudo ln -s ~/sources/castle-engine/cge-www/apache-sample.conf cge-www.conf`.

    - Enable it:

        - On Debian-based systems, do `sudo a2ensite cge-www && sudo service apache2 restart`.

        - On Arch Linux, follow https://wiki.archlinux.org/title/Apache_HTTP_Server .

    In the end, http://localhost:8777/manual_intro.php should work. Note that the main page, http://localhost:8777/ , depends on a working Wordpress installation (including database) -- see below, it may require additional work to see it.

    Note: It is also possible to set this up in non-root, you could even use Apache "userdir" to place it e.g in `http://localhost/~michalis/castle-engine/` . We used to even advise this here, but we don't advise it anymore, as it's a bit more work (need to make sure PHP works in userdir) and needs adjusting .htaccess to make rewrites/redirects work and `/` to link to main page (which we use now from both PHP and AsciiDoctor) will not work.

* To enable testing of AsciiDoctor pages, like http://localhost:8777/build_tool :

    - Install AsciiDoctor and CodeRay.

        On Debian-like systems, just do `apt install asciidoctor coderay`.

* To enable viewing the main page and other pages depending on Wordpress (http://localhost:8777/, http://localhost:8777/wp/):

    * Install MySQL

    * Create empty Wordpress database

        ```
        CREATE DATABASE cgewp;
        CREATE USER 'cgewp'@'localhost' IDENTIFIED BY 'devpassword';
        GRANT ALL PRIVILEGES ON cgewp.* TO 'cgewp'@'localhost';
        ```

    * Install PHP mysql extension.

    * When the main page is first opened, configure Wordpress.

    * If you need to import the real content of our Wordpress, contact Michalis.
