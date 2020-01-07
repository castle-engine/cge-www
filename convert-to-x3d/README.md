This directory contains utilities for our online tool
"Convert everything to X3D" on https://castle-engine.io/convert.php .

Architecture:

- The PHP files implementing it are part of the CGE website,
  in ../htdocs/convert*.php .

- PHP (more exactly, ../htdocs/convert-output.php) calls a script convert-to-x3d.sh
  in this directory.

  Security: www-data has permissions to call this script through
  "sudo -u convert-to-x3d convert-to-x3d.sh ...".
  This way www-data doesn't have permissions to use "docker ..." in general,
  it can only use Docker through "convert-to-x3d.sh".

- convert-to-x3d.sh uses Docker image inside docker-image/ .

  This Docker image contains the latest version of view3dscene and tovrmlx3d,
  using latest Castle Game Engine.
  See https://castle-engine.io/ and https://castle-engine.io/view3dscene.php .

  Rebuild this image (and upload it to Docker hub)
  by "cd docker-image/ && ./build.sh"

  It is available on Docker hub publicly, on https://hub.docker.com/r/kambi/convert-to-x3d .

Author: Michalis Kamburelis

License: GPL >= 2.
