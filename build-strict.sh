#!/bin/sh
set -eu
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
CC=${CC:-gcc}
CFLAGS=${CFLAGS:--Wall -Wextra -pedantic -std=gnu99 -fsanitize=address -Og -g}
version="$(git describe --always --tags --dirty 2>/dev/null || true)"
build_date="$(date '+%Y-%m-%dT%H:%M%z' | sed 's@^.\{19\}@&:@')"
set -x
$CC $CFLAGS \
    -D SCHEME_VERSION=\""$version"\" \
    -D SCHEME_BUILD_DATE=\""$build_date"\" \
    -o discheme discheme.c
