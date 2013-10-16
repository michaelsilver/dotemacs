PWD := $(shell pwd)

all:
	mv ~/.emacs.d ~/.emacs.d.backup-`date +%s`
	ln -s $(PWD) ~/.emacs.d

# Will compile everything!
compile:
	emacs --batch --eval '(byte-recompile-directory "~/.emacs.d" 0 t)'
