#!/bin/bash

set -exuo pipefail

realname() {
	[[ $1 == *self.org ]] &&
		echo "$(basename "$(dirname $1)").org" ||
		basename $1
}

rm -vrf ./public ./content
mkdir -p public content

find -L . -name '*.org' -exec awk 'NR == 1 && /TITLE/ { print FILENAME }' {} \; |
	grep -v content/ |
	while read -r file; do
		cp -p "${file}" "content/$(realname ${file})"
	done

cp -p style.css public/
emacs -Q --script build.el
