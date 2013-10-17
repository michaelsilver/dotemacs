PWD := $(shell pwd)
ELC_FILES=$(shell find . -name "*.el" | sed -e 's/\.el$$/.elc/')

clean:
	rm -rf $(ELC_FILES)

%.elc: %.el
	emacs --batch --eval '(load-file "fd-essential.el") (byte-compile-file "$^")'

compile: $(ELC_FILES)
all: compile

print-elc:
	@echo "ELC_FILES: $(ELC_FILES)"

.PHONY: all compile print-elc clean
