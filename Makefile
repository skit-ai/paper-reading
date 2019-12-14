all: docs/index.html

.PHONY: deploy
deploy: docs/index.html
	aws s3 cp ./docs/ s3://backyard/paper-reading --recursive --acl public-read

docs/index.html: index.org
	cask emacs -Q --script ./build.el
