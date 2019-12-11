#!/bin/bash
set -eu -o pipefail
cd "$(dirname "$0")"
grep '^///' discheme.c | sed -e 's@^///@@' -e 's@^ *@@' >discheme.adoc
asciidoctor discheme.adoc
