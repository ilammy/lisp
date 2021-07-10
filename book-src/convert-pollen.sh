#!/usr/bin/env bash
#
# Convert files from *.tex to *.poly.pm by applying sed magic.
# You still need to hand-tune them afterwards, it's just some
# tedious replacements that I have to do all the time.
#
# This is intended to work with BSD sed, not GNU sed.

set -eu

convert() {
    local file="$1"
    sed -E -i '' \
        -e 's/<</«/g' \
        -e 's/>>/»/g' \
        -e 's/([^ ]\.) /\1\
/g' \
        -e 's/ "--- /~— /' \
        -e 's/"---/—/' \
        -e 's/"~/-/' \
        -e 's/"=/-/' \
        -e 's/\\#/#/' \
        "$1"
    sed -E -i '' \
        -e 's/\\(chapter|[a-z]*section\*?)\{([^{}]*)\}\\label\{([^{}]*)\}/◊\1[#:label "\3"]{\2}/g' \
        -e 's/\\begin\{code:lisp\}/◊code:lisp{/g' \
        -e 's/\\end\{code:lisp\}/}/g' \
        -e 's/\$([^$]*)\$/◊${\1}/g' \
        "$1"
    sed -E -i '' \
        -e 's/\\/◊/g' \
        "$1"
}

while [[ $# -ge 1 ]]
do
    convert "$1"
    shift
done
