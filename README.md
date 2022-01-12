# Code and data of Castle Game Engine website

## Editing

We use a few ways to edit the content:

### AsciiDoctor for static pages

Example of page written this way: https://castle-engine.io/gltf , content in `htdocs/doc/gltf.adoc` .

To edit: Fork this repository (cge-www) and edit the `*.adoc` pages in `htdocs/doc/` subdirectory. Then create a pull request.

See https://asciidoctor.org/ for an overview of the AsciiDoctor language. To preview your edits:

- look at GitHub preview of your file
- or run `asciidoctor xxx.adoc`
- or (complicated but full-blows) follow the _"Testing Locally"_ instructions below to set up your own local copy of CGE website.

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

    - and make sure PHP has permissions to write within proper images/ subdirs: `cd htdocs/images/ && chmod -R a+rwX thumb_size/ thumb_const_height_size/ teaser_size/ gallery_size/`.

  The idea is that (at development) you

    - Just drop new images to `htdocs/images/original_size/` (hint: make it a bookmark in your file manager)

    - Add `cgeimg` to .adoc source code to view them

    - Refresh the local page like http://localhost/~michalis/castle-engine/curves_tool , which will (in development) automatically regenerate AsciiDoctor -> HTML, and will generate thumbnails if missing. (hint: set up a key shortcut in your text editor to refresh the www browser view of this page)

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

To test your modifications, you can follow the _"Testing Locally"_ instructions below. If these instructions look too complicated, just ignore them for simple modifications --- the PHP files are mostly using trivial HTML with small PHP additions (like PHP castle_header, api_link functions) so if you understand basic HTML, you can probably figure out what is going on.

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

* `htodcs/` is exactly what goes into https://castle-engine.io/

    * `htdocs/doc/` are documents in AsciiDoctor. Subset of https://castle-engine.io/ is generated from AsciiDoctor.

    * `htdocs/wp/` is our Wordpress installation, used right now to manage news.

* `screenshots/` are only a copy (sometimes with better resolution) of the same screenshots that are available on SF screenshot page of our project.

* `scripts/` are various scripts, usually in bash, helpful to manage the website.

    * `scripts/remote/` are meant to be run when logged on castle-engine.io through ssh.

    * Rest of `scripts/` can be run on a local version, cloned somewhere on your local computer.

## Testing Locally

* Install Apache (or any other web server that can handle PHP)

* Install PHP

* Make htdocs/ referenced from Apache.

    There are may ways to do this. I advise to do this on Linux and use userdir module, such that the website is accessible under URL like http://localhost/~michalis/castle-engine/xxx.php (which would open `htdocs/xxx.php` file in this repo).

    - Enable Apache "userdir" module.

        On Debian-based systems, just doing `sudo a2enmod userdir && sudo service apache2 restart` is enough.

        On Arch Linux, follow https://wiki.archlinux.org/title/Apache_HTTP_Server .

    - Make sure it works, and that PHP inside works, by

        - creating a file `/home/USERNAME/public_html/a.html` with contents `Test <b>bold</b>`. It should be visible on http://localhost/~USERNAME/a.html . If this works -> Apache + userdir works.

        - creating a file `/home/USERNAME/public_html/a.php` with contents `<?php phpinfo(); ?>`. It should be visible on http://localhost/~USERNAME/a.php . If this works -> then PHP (in user directory) works too.

            Note: On some systems, it may be necessary to edit userdir module configuation (and restart Apache), to enable PHP in user directories. That is why we advise 2 tests above. If HTML works, but PHP fails -> this applies to you.

    - Allow `.htaccess` in cge-www, by adding something like this to Apache config:

        ```
        <Directory "/home/USERNAME/public_html">
            AllowOverride All
            # Makes .htaccess assume that website is under /~michalis/castle-engine/
            Define MichalisLocalTest
        </Directory>
        ```

    - Symlink cge-www:

        ```
        cd ~/public_html
        ln -s ~/sources/castle-engine/cge-www/htdocs/ castle-engine/
        ```

    - In the end, http://localhost/~michalis/castle-engine/manual_intro.php should work. Note that the main page, http://localhost/~michalis/castle-engine/ , depends on a working Wordpress installation (including database) -- see below.

* To enable testing of AsciiDoctor pages, like http://localhost/~michalis/castle-engine/build_tool :

    - Adjust RewriteBase in htdocs/.htaccess to make the rewrite rule work:

    - AsciiDoctor and CodeRay.

        On Debian-like systems, just do `apt install asciidoctor coderay`.

* To enable viewing the main page and other pages depending on Wordpress (http://localhost/~michalis/castle-engine/, http://localhost/~michalis/castle-engine/wp/):

    * Install MySQL

    * Create empty Wordpress database

        ```
        CREATE DATABASE cgewp;
        GRANT ALL PRIVILEGES ON cgewp.* TO 'cgewp'@'localhost' IDENTIFIED BY 'devpassword';
        ```

    * Install PHP mysql extension.

    * When the main page is first opened, configure Wordpress.

    * If you need to import the real content of our Wordpress, contact Michalis.
