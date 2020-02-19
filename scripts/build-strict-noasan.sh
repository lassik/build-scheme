#!/bin/sh
set -eu
cd "$(dirname "$0")"
export CFLAGS="-Wall -Wextra -pedantic -std=gnu99 -Og -g"
exec ./build-strict.sh "$@"
