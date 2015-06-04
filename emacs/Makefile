EMACS ?= emacs
SANDBOX ?= ./sandbox
PACKAGE := $(shell $(EMACS) -Q --batch -l scripts/package-name.el -f my-package-version)

elc:
	$(EMACS) -q -l package -f package-initialize \
	         --eval "(add-to-list 'load-path \".\")" \
	         --batch -f batch-byte-compile *.el

clean-elc:
	rm -rf *.elc

dafny-docs.html: rebuild-dafny-docs.py
	python3 ./scripts/rebuild-dafny-docs.py

package-contents: clean-elc elc dafny-docs.html

package: clean-package package-contents
	mkdir -p build/$(PACKAGE)
	cp -R *.el etc/ build/$(PACKAGE)
	cd build && tar -cf $(PACKAGE).tar $(PACKAGE)

clean-package:
	rm -rf build

sandbox: clean-sandbox package
	mkdir -p $(SANDBOX)
	$(EMACS) -Q --eval '(setq user-emacs-directory "$(SANDBOX)")' -l scripts/sandbox-setup.el

clean-sandbox:
	rm -rf $(SANDBOX)

test: elc
	$(EMACS) tests.dfy

clean: clean-package clean-elc
	rm -f dafny-docs.html

all: elc dafny-docs package

.PHONY: elc clean-elc package-contents package clean-package sandboz clean-sandbox clean all
