#!/bin/sh -e

mkdir -p public
rm -vf ./public/*
cd public/

find . -type f -name '*.org' \
	-exec mawk 'NR == 1 && /TITLE/ { print FILENAME }' {} \; |
	while read -r file; do
		emacs --batch --eval "(require 'org)" "${file}" --funcall org-html-export-to-html
		case "$file" in
		*self.org) mv -v "${file%.*}.html" "$(basename "$(dirname $file)").html" ;;
		*) mv -v "${file%.*}.html" . ;;
		esac
	done
