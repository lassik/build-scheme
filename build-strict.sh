#!/bin/sh
set -eu
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
CC=${CC:-gcc}
CFLAGS=${CFLAGS:--Wall -Wextra -pedantic -std=gnu99 -fsanitize=address -Og -g}
version="$(git describe --always --tags --dirty 2>/dev/null || true)"
set -x
$CC $CFLAGS -D SCHEME_VERSION=\""$version"\" -o discheme discheme.c
