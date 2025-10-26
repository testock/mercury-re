MMC = mmc

# The expected outputs do not use CRLF line endings, so if we are on
# Windows we need to account for this.
# We assume the use of GNU diff if this is the case.
#
ifdef WINDIR
DIFF_FLAGS=--diff-flags "--strip-trailing-cr"
else
DIFF_FLAGS=
endif

ifdef GRADE
GRADEFLAGS=--grade $(GRADE)
else
GRADEFLAGS=
endif

.PHONY: runtests
runtests: test_regex
	@./test_regex $(DIFF_FLAGS)

.PHONY: runtests-verbose
runtests-verbose: test_regex
	@./test_regex $(DIFF_FLAGS) -v

Mercury.modules: $(wildcard *.m) $(wildcard ../src/*.m)
	$(MMC) -f $(wildcard *.m) $(wildcard ../src/*.m)

test_regex: Mercury.modules
	$(MMC) $(GRADEFLAGS) --make $@

tags: $(wildcard *.m) $(wildcard ../src/*.m)
	mtags $(wildcard *.m) $(wildcard ../src/*.m)

# We're only interested in coverage information from the regex library modules.
#
COVERAGE_RESTRICTION = \
	-m regex

.PHONY: coverage
coverage: test_regex
	mtc --coverage-test -o test_regex.trace_counts ./test_regex
	mcov $(COVERAGE_RESTRICTION) test_regex.trace_counts -o test_regex.coverage-procs
	mcov $(COVERAGE_RESTRICTION) test_regex.trace_counts -d -o test_regex.coverage-labels

.PHONY: realclean
realclean:
	$(MMC) --make test_regex.realclean
	/bin/rm -rf Mercury
	/bin/rm -f Mercury.modules tags
	/bin/rm -f FAILED_TESTS ABORTED_TESTS
	/bin/rm -f test_regex.trace_counts test_regex.coverage-procs
	/bin/rm -f test_regex.coverage-labels