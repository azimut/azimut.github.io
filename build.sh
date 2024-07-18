#!/bin/bash

set -exuo pipefail

realname() {
	[[ $1 == *self.org ]] &&
		echo "$(basename "$(dirname $1)").org" ||
		basename $1
}

restore_timestamp() {
	local file
	file="$(echo "$1" | cut -f3- -d/)"
	cd "$(echo "$1" | cut -f2 -d/)"
	touch -d "$(git log -1 --format='%aI' ${file})" "${file}"
	cd -
}

rm -vrf ./public ./org/notes
mkdir -p public org/notes

find -L . -name '*.org' -exec awk 'NR == 1 && /TITLE/ { print FILENAME }' {} \; |
	grep -v org/ |
	while read -r file; do
		restore_timestamp "${file}"
		cp -p "${file}" "org/notes/$(realname ${file})"
	done

emacs -Q --script build.el

cp style.css public/
find org -type f -not -name '*.org' | while read -r file; do
	cp "${file}" public/"${file:3}"
done
