PWD := $(shell pwd)

all:
	mv ~/.emacs.d ~/.emacs.d.backup-`date +%s`
	ln -s $(PWD) ~/.emacs.d

compile:
	emacs --batch --eval '(byte-recompile-directory "~/.emacs.d")'
