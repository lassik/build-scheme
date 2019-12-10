#!/bin/sh
set -eu
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
CC=${CC:-gcc}
CFLAGS=${CFLAGS:--Wall -Wextra -pedantic -std=c99 -Og -g}
set -x
$CC $CFLAGS -o scheme scheme.c
