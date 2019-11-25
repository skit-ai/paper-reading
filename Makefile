all: README.org docs/index.html

docs/index.html: index.org
	cask emacs -Q --script ./build.el

README.org: library.bib
	cask emacs --batch -l ./gen.el --eval "(gen)"
