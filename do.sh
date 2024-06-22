#!/bin/sh -exu

mkdir -p public
rm -vf ./public/*

find . -name '*.org' -exec awk 'NR == 1 && /TITLE/ { print FILENAME }' {} \; |
	while read -r file; do
		emacs --batch --eval "(require 'org)" "${file}" --funcall org-html-export-to-html
		case "$file" in
		*self.org) mv -v "${file%.*}.html" "public/$(basename "$(dirname $file)").html" ;;
		*) mv -v "${file%.*}.html" public/ ;;
		esac
	done
