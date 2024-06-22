#!/bin/bash -xeu

mkdir -p public
rm -vf ./public/*

realname() {
	[[ $1 == *self.org ]] &&
		echo "$(basename "$(dirname $1)").html" ||
		echo "$(basename "$1" .org).html"
}

echo "<html><body><ul>" >>public/index.html
find -L . -name '*.org' \
	-exec awk 'NR == 1 && /TITLE/ { print FILENAME }' {} \; |
	while read -r file; do
		emacs --batch --eval "(require 'org)" "${file}" --funcall org-html-export-to-html
		echo "<li><a href='$(realname $file)'>$(head -n1 "${file}" | cut -f2- -d' ')</a></li>" >>public/index.html
		mv -v "${file%.*}.html" "public/$(realname "${file}")"
	done
echo "</ul></body></html>" >>public/index.html
