#!/bin/sh
set -eu
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
CC=${CC:-gcc}
CFLAGS=${CFLAGS:--Wall -Wextra -pedantic -std=gnu99 -fsanitize=address -Og -g}
build_date="$(date '+%Y-%m-%dT%H:%M%z' | sed 's@^.\{19\}@&:@')"
git_commit="$(git rev-parse --short HEAD 2>/dev/null || true)"
set -x
$CC $CFLAGS \
    -D SCHEME_BUILD_DATE=\""$build_date"\" \
    -D SCHEME_GIT_COMMIT=\""$git_commit"\" \
    -o discheme discheme.c
