EMACS ?= $(shell which emacs)
SRC=$(shell cask files)
ELCFILES = $(SRC:.el=.elc)

.PHONY: autoloads
autoloads:
	-rm -f lisp/ein-loaddefs.el
	$(EMACS) -Q --batch \
		--eval "(let ((generated-autoload-file (expand-file-name \"lisp/ein-loaddefs.el\"))) (update-directory-autoloads (expand-file-name \"lisp\")))"

.PHONY: clean
clean:
	cask clean-elc

.PHONY: test-compile
test-compile: clean autoloads
	! ( cask build 2>&1 | awk '{if (/^ /) { gsub(/^ +/, " ", $$0); printf "%s", $$0 } else { printf "\n%s", $$0 }}' | egrep "not known|Error|free variable|error for|Use of gv-ref" )
	cask clean-elc

.PHONY: quick
quick: test-compile test-unit

.PHONY: test
test: quick test-int

.PHONY: test-int
test-int:
	cask exec ert-runner -L ./lisp -L ./test -l test/testfunc.el test/test-func.el
	cask exec ecukes

.PHONY: test-unit
test-unit:
	cask exec ert-runner -L ./lisp -L ./test -l test/testein.el test/test-ein*.el
