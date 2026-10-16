# Testing the website with Docker

This is an optional way to preview PHP pages locally. From this directory, run:

```sh
docker compose up --build
```

Open <http://localhost:8777/additional_components.php>. The repository's
`htdocs/` directory is mounted into the container, so PHP and content changes
are available after refreshing the page. Stop the server with `Ctrl+C`.

The container includes PHP, Apache with `mod_rewrite`, AsciiDoctor, CodeRay,
and Rouge. AsciiDoctor pages are regenerated when previewed. The container
does not include a WordPress database or the extra tools needed to regenerate
thumbnails or AsciiDoctor output files, so the main page and WordPress-dependent
pages need additional services and setup.
