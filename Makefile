.PHONY: test load clean

# Both recipes pipe SBCL through another command. Without pipefail the recipe
# exits with that command's status, so a failing check or a compile error
# cannot fail the build. `set -o pipefail` is written into each recipe rather
# than .SHELLFLAGS because make 3.81 (the version macOS ships) ignores
# .SHELLFLAGS.
SHELL := /bin/bash

QUICKLISP_HOME ?= $(HOME)/quicklisp
XDG_CACHE_HOME := $(CURDIR)/.cache
SBCL := XDG_CACHE_HOME=$(XDG_CACHE_HOME) sbcl --dynamic-space-size 2048 --noinform --no-userinit --non-interactive
LOAD := --load $(QUICKLISP_HOME)/setup.lisp --eval '(push (truename ".") asdf:*central-registry*)'

test:
	@set -o pipefail; $(SBCL) $(LOAD) \
		--eval '(asdf:load-system :agento11y-cl/t)' \
		--eval '(multiple-value-bind (ok pass fail) (agento11y-cl/t:run-tests) (declare (ignore pass fail)) (uiop:quit (if ok 0 1)))' \
		2>&1 | grep -E "^(=|---|  [✓✗]|TOTAL)"

load:
	@set -o pipefail; $(SBCL) $(LOAD) \
		--eval '(asdf:load-system :agento11y-cl)' \
		--eval '(format t "~%agento11y-cl loaded OK~%")' \
		2>&1 | tail -5

clean:
	@find . -name '*.fasl' -delete
	@rm -rf .cache
