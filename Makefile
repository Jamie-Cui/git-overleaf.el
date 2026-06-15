EMACS ?= emacs
COVERAGE_DIR ?= coverage
COVERAGE_MIN ?= 0
PACKAGE_INIT = (progn (setq load-prefer-newer t) (require 'package) (package-initialize) (add-to-list 'load-path default-directory))
EMACS_BATCH = $(EMACS) -Q --batch -L . --eval "$(PACKAGE_INIT)"
MODULE_ELC_FILES = \
	git-overleaf-log.elc \
	git-overleaf-core.elc \
	git-overleaf-http.elc \
	git-overleaf-sync.elc \
	git-overleaf-firefox.elc \
	git-overleaf-auth.elc
ELC_FILES = $(MODULE_ELC_FILES) git-overleaf.elc git-overleaf-magit.elc
TEST_FILES = \
	test/git-overleaf-test.el \
	test/git-overleaf-git-test.el \
	test/git-overleaf-sync-tree-test.el \
	test/git-overleaf-command-test.el \
	test/git-overleaf-http-auth-test.el \
	test/git-overleaf-async-test.el \
	test/git-overleaf-magit-test.el

.PHONY: all compile test coverage help clean

all: compile

compile: $(ELC_FILES)

test:
	$(EMACS_BATCH) $(foreach file,$(TEST_FILES),-l $(file)) -f ert-run-tests-batch-and-exit

coverage:
	$(EMACS_BATCH) \
		--eval "(setq git-overleaf-coverage-directory \"$(COVERAGE_DIR)\" git-overleaf-coverage-min $(COVERAGE_MIN))" \
		-l test/coverage.el

git-overleaf-log.elc: git-overleaf-log.el
	$(EMACS_BATCH) -f batch-byte-compile git-overleaf-log.el

git-overleaf-core.elc: git-overleaf-core.el git-overleaf-log.elc
	$(EMACS_BATCH) -f batch-byte-compile git-overleaf-core.el

git-overleaf-http.elc: git-overleaf-http.el git-overleaf-core.elc
	$(EMACS_BATCH) -f batch-byte-compile git-overleaf-http.el

git-overleaf-sync.elc: git-overleaf-sync.el git-overleaf-core.elc git-overleaf-http.elc
	$(EMACS_BATCH) -f batch-byte-compile git-overleaf-sync.el

git-overleaf-firefox.elc: git-overleaf-firefox.el git-overleaf-core.elc
	$(EMACS_BATCH) -f batch-byte-compile git-overleaf-firefox.el

git-overleaf-auth.elc: git-overleaf-auth.el git-overleaf-core.elc git-overleaf-http.elc git-overleaf-firefox.elc
	$(EMACS_BATCH) -f batch-byte-compile git-overleaf-auth.el

git-overleaf.elc: git-overleaf.el $(MODULE_ELC_FILES)
	$(EMACS_BATCH) -f batch-byte-compile git-overleaf.el

git-overleaf-magit.elc: git-overleaf-magit.el git-overleaf.elc
	$(EMACS_BATCH) -f batch-byte-compile git-overleaf-magit.el

help:
	@printf '%s\n' \
		'Targets:' \
		'  make        Byte-compile the package files.' \
		'  make test   Run the ERT test suite.' \
		'  make coverage   Run ERT under built-in testcover.' \
		'  make help   Show this help message.' \
		'  make clean  Remove generated .elc files.' \
		'' \
		'Variables:' \
		'  EMACS=/path/to/emacs   Emacs executable to use (default: emacs).' \
		'  COVERAGE_DIR=coverage  Directory for coverage reports.' \
		'  COVERAGE_MIN=0         Minimum coverage percent required.'

clean:
	rm -f $(ELC_FILES)
	rm -rf $(COVERAGE_DIR)
