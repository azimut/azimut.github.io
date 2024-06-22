#!/bin/bash

set -exuo pipefail

rm -vrf ./public ./content
mkdir -p public content

realname() {
	[[ $1 == *self.org ]] &&
		echo "$(basename "$(dirname $1)").org" ||
		basename $1
}

find -L . -name '*.org' -exec awk 'NR == 1 && /TITLE/ { print FILENAME }' {} \; |
	grep -v content/ |
	while read -r file; do
		cp "${file}" "content/$(realname ${file})"
	done

emacs -Q --script build.el
