#!/bin/bash
set -eu

# Update patreon.json that contains information about Patreon displayed on CGE website.

php -f castle_patreon.php > new_patreon.json

# override patreon.json only if php execution succeeded
mv -f new_patreon.json patreon.json
