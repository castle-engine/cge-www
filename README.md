# Code and data of Castle Game Engine website

## Editing

We use a few ways to edit the content:

- Wordpress for news: https://castle-engine.io/wp/ .

    Michalis Kamburelis (michalis@castle-engine.io) will give you an account.

- AsciiDoctor for static pages:

    Fork this repository (cge-www) and edit the `*.adoc` pages in `htdocs/doc/`
    subdirectory. Then create a pull request.

    See https://asciidoctor.org/ for an overview of the AsciiDoctor language.
    To preview what you do, you can

    - look at GitHub preview of your file
    - run `asciidoctor xxx.adoc`
    - or (complicated but full) follow the "Testing Locally" instructions below.

    We support additional shortcuts within AsciiDoctor:

    - Paste link to API docs for TCastleScene, with optional customized text.
      The TCastleScene is automatically found in global identifiers in latest
      CGE API docs.

      ```
      cgeref:TCastleScene[]
      cgeref:TCastleScene[link text for TCastleScene]
      ``

      If you want to write `[` or `]` inside the link title,
      write '{{{` or `}}}` instead. They will be replaced with `[` or `]` . Like

      ```
      cgeref:TMessaging.Send[Messaging.Send({{{'message-1','some-parameter'}}})]
      ```

    - Paste gallery of images, using CGE style. Can be floating (on the right)
      or (default) within page content.
      We expect to find referenced images (aaa.png,bbb.png in examples below) in
      htdocs/images/original_size/ .
      You should regenerate all thumbnails using `cd htdocs/images/ && make`,
      though at development they will also be regenerated as-needed by PHP.

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

    - Note: AsciiDoctor macros above are not really implemented as AsciiDoctor macros in Ruby,
      following
      https://docs.asciidoctor.org/asciidoctor/latest/extensions/inline-macro-processor/
      https://docs.asciidoctor.org/asciidoctorj/latest/extensions/block-macro-processor/
      https://docs.asciidoctor.org/asciidoctor/latest/extensions/block-macro-processor/

      Instead they are implemented using PHP, as replacements on top of the AsciiDoctor processing.
      Why? Because we needed to implement them in PHP too,
      as our pages need these functions in PHP too.

- PHP for static pages:

    Same as above, you can fork this repository, edit php file in `htdocs/`,
    and create a pull request.

    To test your modifications, you can follow the "Testing Locally" instructions below.
    If these instructions look too complicated, just ignore them for simple modifications
    --- the PHP files are mostly using trivial HTML with small PHP additions
    (like PHP castle_header, api_link functions) so if you understand basic HTML,
    you can probably figure out what is going on.

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
