all: README.org

README.org: library.bib
	cask emacs --batch -l ./gen.el --eval "(gen)"
