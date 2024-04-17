This directory contains utilities for our online tool
"Convert everything to X3D" on https://castle-engine.io/convert.php .

# Architecture

- The PHP files implementing it are part of the CGE website,
  in ../htdocs/convert*.php .

- PHP (more exactly, ../htdocs/convert-output.php) calls a script convert-to-x3d.sh
  in this directory.

  Security: www-data has permissions to call this script through
  "sudo -u convert-to-x3d convert-to-x3d.sh ...".
  This way www-data doesn't have permissions to use "docker ..." in general,
  it can only use Docker through "convert-to-x3d.sh".

- convert-to-x3d.sh uses Docker image inside docker-image/ .

  This Docker image contains the latest version of
  [castle-model-viewer](https://castle-engine.io/model-viewer) and [castle-model-converter](https://castle-engine.io/model-converter), using latest Castle Game Engine.

  Rebuild this image (and upload it to Docker hub)
  by "cd docker-image/ && ./build.sh"

  It is available on Docker hub publicly, on https://hub.docker.com/r/kambi/convert-to-x3d .

# Installing on server

Install Docker (see
https://www.docker.com/ ,
https://docs.docker.com/install/ ,
https://castle-engine.io/docker ).

Configure some server directories and users:

```
sudo -i

    # create /var/convert-to-x3d/
    rm -Rf /var/convert-to-x3d/
    mkdir -p /var/convert-to-x3d/output/
    for I in {1..10}; do mkdir -p /var/convert-to-x3d/volumes/$I/; done
    chown -R www-data:www-data /var/convert-to-x3d/

    # create convert-to-x3d user
    adduser convert-to-x3d --system --ingroup docker --home /nonexistent --no-create-home --disabled-password
    # adduser convert-to-x3d www-data # allow convert-to-x3d to write in dirs created by www-data (not useful for now)
    # test that convert-to-x3d can run Docker:
    sudo -u convert-to-x3d docker run --rm kambi/convert-to-x3d castle-model-converter --version
    # test that www-data cannot run Docker:
    sudo -u www-data docker run --rm kambi/convert-to-x3d castle-model-converter --version

    # allow www-data to execute convert-to-x3d.sh
    visudo -f /etc/sudoers.d/convert-to-x3d
      www-data ALL = (convert-to-x3d) NOPASSWD: /home/michalis/cge-www/convert-to-x3d/convert-to-x3d.sh

    # test convert-to-x3d.sh script
    sudo mkdir -p                /var/convert-to-x3d/volumes/1/contents/
    sudo chown www-data:www-data /var/convert-to-x3d/volumes/1/contents/
    sudo chmod a+w               /var/convert-to-x3d/volumes/1/contents/
    sudo bash -c 'echo "#VRML V2.0 utf8" > /var/convert-to-x3d/volumes/1/contents/input.wrl'
    sudo -u www-data sudo -u convert-to-x3d /home/michalis/cge-www/convert-to-x3d/convert-to-x3d.sh 1 input.wrl xml testoutputid
    sudo ls -Flah /var/convert-to-x3d/volumes/1/contents/
    sudo cat /var/convert-to-x3d/volumes/1/contents/error.log
    sudo cat /var/convert-to-x3d/volumes/1/contents/testoutputid
```

Increase upload size in Apache:

```
emacs /sudo:root@localhost:/etc/php/7.3/apache2/php.ini
  upload_max_filesize = 100M
  post_max_size = 100M
sudo /etc/init.d/apache2 restart
```

Make sure to adjust cge-www-path in ../htdocs/castle_engine_config.php .
Use ../htdocs/castle_engine_config_sample.php as template,
if it doesn't exist.

Create a cron job, by creating `/etc/cron.d/convert-to-x3d`:

```
*/5 * * * *     www-data    cd /home/michalis/cge-www/htdocs/ && php < convert-cron.php
```

# License

Author: Michalis Kamburelis

License: GPL >= 2.
