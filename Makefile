all: docs/index.html

docs/index.html: index.org
	cask emacs -Q --script ./build.el
