# Online Model Converter

This directory contains utilities for our online 3D and 2D model converter
available on https://castle-engine.io/convert.php .

## Architecture

- The PHP files implementing it are part of the CGE website,
  in ../htdocs/convert*.php .

- PHP (more exactly, ../htdocs/convert-output.php) calls a script online-model-converter.sh
  in this directory.

  Security: www-data has permissions to call this script through
  "sudo -u online-model-converter online-model-converter.sh ...".
  This way www-data doesn't have permissions to use "docker ..." in general,
  it can only use Docker through "online-model-converter.sh".

- online-model-converter.sh uses Docker image inside docker-image/ .

  This Docker image contains the latest version of
  [castle-model-viewer](https://castle-engine.io/model-viewer) and [castle-model-converter](https://castle-engine.io/model-converter), using latest Castle Game Engine.

  Rebuild this image (and upload it to Docker hub)
  by "cd docker-image/ && ./build.sh"

  It is available on Docker hub publicly, on https://hub.docker.com/r/kambi/online-model-converter .

## Installing on server

Install Docker (see
https://www.docker.com/ ,
https://docs.docker.com/install/ ,
https://castle-engine.io/docker ).

Configure some server directories and users:

```
sudo -i

    # create /var/online-model-converter/
    rm -Rf /var/online-model-converter/
    mkdir -p /var/online-model-converter/output/
    for I in {1..10}; do mkdir -p /var/online-model-converter/volumes/$I/; done
    chown -R www-data:www-data /var/online-model-converter/

    # create online-model-converter user
    adduser online-model-converter --system --ingroup docker --home /nonexistent --no-create-home --disabled-password
    # adduser online-model-converter www-data # allow online-model-converter to write in dirs created by www-data (not useful for now)
    # test that online-model-converter can run Docker:
    sudo -u online-model-converter docker run --rm kambi/online-model-converter castle-model-converter --version
    # test that www-data cannot run Docker:
    sudo -u www-data docker run --rm kambi/online-model-converter castle-model-converter --version

    # allow www-data to execute online-model-converter.sh
    visudo -f /etc/sudoers.d/online-model-converter
      www-data ALL = (online-model-converter) NOPASSWD: /home/michalis/cge-www/online-model-converter/online-model-converter.sh

    # test online-model-converter.sh script
    sudo mkdir -p                /var/online-model-converter/volumes/1/contents/
    sudo chown www-data:www-data /var/online-model-converter/volumes/1/contents/
    sudo chmod a+w               /var/online-model-converter/volumes/1/contents/
    sudo bash -c 'echo "#VRML V2.0 utf8" > /var/online-model-converter/volumes/1/contents/input.wrl'
    sudo -u www-data sudo -u online-model-converter /home/michalis/cge-www/online-model-converter/online-model-converter.sh 1 input.wrl xml testoutputid
    sudo ls -Flah /var/online-model-converter/volumes/1/contents/
    sudo cat /var/online-model-converter/volumes/1/contents/error.log
    sudo cat /var/online-model-converter/volumes/1/contents/testoutputid
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

Create a cron job, by creating `/etc/cron.d/online-model-converter`:

```
*/5 * * * *     www-data    cd /home/michalis/cge-www/htdocs/ && php < convert-cron.php
```

## License

Author: Michalis Kamburelis

License: GPL >= 2.
