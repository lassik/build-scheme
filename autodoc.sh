#!/bin/bash
set -eu -o pipefail
cd "$(dirname "$0")"
grep '^///' scheme.c | sed -e 's@^///@@' -e 's@^ *@@' >autodoc.adoc
asciidoctor autodoc.adoc
