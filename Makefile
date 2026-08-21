.PHONY: test load clean conformance-refresh

# Both recipes pipe SBCL through another command. Without pipefail the recipe
# exits with that command's status, so a failing check or a compile error
# cannot fail the build. `set -o pipefail` is written into each recipe rather
# than .SHELLFLAGS because make 3.81 (the version macOS ships) ignores
# .SHELLFLAGS.
SHELL := /bin/bash

QUICKLISP_HOME ?= $(HOME)/quicklisp
AGENTO11Y_REPO ?= $(HOME)/projects/agento11y
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

# Re-copy the vendored cross-SDK fixtures from an agento11y checkout. Record the
# new upstream commit in t/fixtures/README.md afterwards.
conformance-refresh:
	@set -e; \
	for f in request-preflight request-postflight-guard responses; do \
		cp $(AGENTO11Y_REPO)/conformance/hooks/$$f.json t/fixtures/hooks/$$f.json; \
	done; \
	for f in inputs ids requests responses; do \
		cp $(AGENTO11Y_REPO)/conformance/experiments/$$f.json t/fixtures/experiments/$$f.json; \
	done; \
	for f in strings generations; do \
		cp $(AGENTO11Y_REPO)/redaction/fixtures/$$f.json t/fixtures/redaction/$$f.json; \
	done; \
	cp $(AGENTO11Y_REPO)/redaction/patterns.json t/fixtures/redaction/patterns.json; \
	for f in openai_sync anthropic_stream gemini_sync; do \
		cp $(AGENTO11Y_REPO)/go/agento11y/testdata/otlpwire/$$f.generation.json \
			t/fixtures/otlpwire/$$f.generation.json; \
		cp $(AGENTO11Y_REPO)/go/agento11y/testdata/otlpwire/$$f.span.json \
			t/fixtures/otlpwire/$$f.span.json; \
	done
	@git -C $(AGENTO11Y_REPO) log -1 --format='refreshed from agento11y %H'
