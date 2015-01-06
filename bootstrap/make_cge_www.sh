#!/bin/bash
set -eu

grunt dist
rm -Rf ../htdocs/kambi-php-lib/bootstrap/
cp -R dist/ ../htdocs/kambi-php-lib/bootstrap/
chmod -R a+rX ../htdocs/kambi-php-lib/bootstrap/
