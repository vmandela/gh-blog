#!/usr/bin/env bash

convert favicon.svg favicon.ico

echo '<link rel="icon" type="image/svg+xml" href="/images/favicon/favicon.svg"/>'
ICON_SIZES="16 24 32 40 48 64 96 128 192 256"
for SIZE in $ICON_SIZES; do
    SIZESTR="${SIZE}x${SIZE}"
    FNAME="favicon-${SIZESTR}.png"
    convert favicon.svg -size "$SIZESTR" "$FNAME"
    echo "<link rel=\"icon\" type=\"image/png\" href=\"/images/favicon/$FNAME\" sizes=\"$SIZESTR\">"
done

echo '<link rel="icon" type="image/x-icon" href="/images/favicon/favicon.ico"/>'
