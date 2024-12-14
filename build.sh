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
    file="$(echo "$1" | cut -f3- -d/)"
    cd "$(echo "$1" | cut -f2 -d/)"
    touch -d "$(git log -1 --format='%aI' "${file}")" "${file}"
    cd -
}

rm -vrf ./public ./org/notes
mkdir -p public org/notes

# org/notes/ - Copy .org files and media
find -L . -name '*.org' -exec awk 'NR == 1 && /TITLE/ { print FILENAME }' {} \; |
    grep -v org/ |
    while read -r file; do
        restore_timestamp "${file}"
        cp -vp "${file}" "org/notes/$(realname "${file}")"
        cp -vp "$(dirname "${file}")/"*.@(jpg|png|jpeg|gif) org/notes/ || true
    done

emacs -Q --script build.el

# public/ - Copy media from org/
find org/ -type f -not -name '*.org' |
    while read -r file; do
        cp "${file}" "public/${file:4}"
    done
