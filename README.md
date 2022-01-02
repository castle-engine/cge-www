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
