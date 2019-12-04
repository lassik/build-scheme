#!/bin/sh
CC=${CC:-clang}
CFLAGS=${CFLAGS:--Wall -Wextra -pedantic -std=c99 -Og -g}
$CC $CFLAGS -o scheme scheme.c
