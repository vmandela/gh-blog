#!/usr/bin/env bash

DEST_DIR="$1"

if [ "$#" -ne 1 ]; then
    echo "No arguments supplied exiting"
    exit 1
fi


if [ -d "$DEST_DIR" ]; then

    echo "Syncing blog to $DEST_DIR"
    rsync -av --exclude .git _site/ "$DEST_DIR"

fi
