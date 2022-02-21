#!/bin/bash
set -eu

# Update patreon.json that contains information about Patreon displayed on CGE website.

php -f castle_patreon.php > patreon.json
