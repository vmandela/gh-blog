#!/usr/bin/env bash

# Run this script from the root of the repo

BULMA_PATH="../bulma"
SASS_PATH="$BULMA_PATH/sass"
# nix-shell -p pkgs.sassc
cat <<EOF > bulma.mini.scss
@charset "utf-8";
@import "utilities/_all";
@import "base/minireset.sass";
@import "base/generic.sass";
@import "base/helpers.sass";
@import "elements/container.sass";
@import "elements/content.sass";
@import "elements/title.sass";
@import "elements/image.sass";
@import "elements/table.sass";
@import "components/navbar.sass";
@import "components/menu.sass";
@import "grid/columns.sass";
@import "layout/footer.sass";
EOF
sassc -t compressed -I ../bulma/sass bulma.mini.scss css/bulma.custom.css

rm -f bulma.mini.scss
