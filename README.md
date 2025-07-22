# My Personal Website

Currently I am building this site with Emacs's org-mode [html-exporter](https://orgmode.org/worg/exporters/ox-overview.html). Which means that it translates my markup language of choice (Org) to HTML.

The site is build and published through custom `Github Actions`.

## Build

Current build process:

- Custom shell script to create a new directory structure from different sources that will be used by Emacs on the build.
- Custom  `Javascript`, `CSS` and `HTML` is referenced and included later.
- Custom Emacs Lisp script to tell Emacs how to do the *org to html* conversion
- imagemagick: to draw the favicon

## Development

Current development setup uses the following tech:

- [GNU Make](http://www.gnu.org/software/make/): to coordinate concurrent dev tasks and update the favicon
- [live-server](https://www.npmjs.com/package/live-server): to start a server that will reload on the client side on file changes
- [postcss](https://www.npmjs.com/package/postcss): to be able to use new CSS features and still be able to support old browsers
