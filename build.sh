#!/bin/bash

set -exuo pipefail
shopt -s extglob

realname() {
    if [[ $1 == *self.org ]]; then
        echo "$(basename "$(dirname "$1")").org"
    else
        basename "$1"
    fi
}

restore_timestamp() {
    local file
    file="$(cut -f3- -d/ <<<"$1")"
    touch -md "$(
        cd "$(cut -f2 -d/ <<<"$1")"
        git log -1 --format='%cI' "${file}"
    )" "${1}"
}

rm -vrf ./public/* ./org/notes
mkdir -p public org/notes

# org/notes/ - copy .org + media
notes=(
    ./programming-notes/terminal/languages/awk/awk.org
    ./programming-notes/terminal/languages/sed/self.org
    ./programming-notes/terminal/languages/jq/jq.org
    ./programming-notes/terminal/languages/m4/self.org
    ./programming-notes/terminal/languages/dc/self.org
    ./programming-notes/terminal/languages/bc/self.org
    ./programming-notes/terminal/tools/roff/pic/self.org
    ./programming-notes/languages/stack/postscript/self.org
    # ./programming-notes/languages/sql/sqlite/self.org
    ./programming-notes/languages/compiled/c/makefile/self.org
)
for note in "${notes[@]}"; do
    restore_timestamp "${note}"
    cp -vp "${note}" "org/notes/$(realname "${note}")"
    cp -vp "$(dirname "${note}")/"*.@(jpg|png|jpeg|gif|ps|svg) org/notes/ || true
done

emacs -Q --script build.el

# public/ - Copy media from org/
find org/ -type f \( -not -name '*.org' -and -not -name '*.html' \) |
    while read -r file; do
        mkdir -p "$(dirname "public/${file#*/}")"
        cp "${file}" "public/${file#*/}"
    done

touch ./*.css # force .css regeneration after nuking everything
