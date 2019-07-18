#!/bin/sh
set -e

if [ "$(whoami)" == "root" ]; then
    chown -R mole:mole /home/mole/workspace
    exec su-exec mole "$@"
fi

