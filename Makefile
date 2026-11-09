# Test pipeline for the Core Erlang extraction plugin.
#
# Conventions:
#   test/<name>.v        Rocq input; must extract to test_core/<name>, i.e.
#                        contain  CoreErlang Extraction "test_core/<name>" ...
#                        producing test_core/rocq_<name>.core
#   test/<name>_tests.erl  EUnit suite for module rocq_<name>
#
# The rules below are abstract: adding a new test/<name>.v (following the
# convention) and test/<name>_tests.erl is enough to include it in the run.

ROCQ    ?= rocq
ERLC    ?= erlc
ERL     ?= erl
PKGDIR  ?= build/pkg
OUT     := test_core
VFILES  := $(wildcard test/*.v)
TESTS   := $(patsubst test/%.erl,%,$(wildcard test/*_tests.erl))

# list syntax for -eval: "a, b, c"
empty    :=
space    := $(empty) $(empty)
comma    := ,
TESTLIST := $(subst $(space),$(comma),$(TESTS))

CORES  := $(patsubst test/%.v,$(OUT)/%.core,$(VFILES))
BEAMS  := $(CORES:.core=.beam) $(patsubst %,$(OUT)/%.beam,$(TESTS))

# keep the intermediate .core files (they are the extraction output)
.SECONDARY: $(CORES)

.PHONY: all check build clean
all: check

# --- plugin: build + stage it for `Declare ML Module` -----------------

PKG   := $(PKGDIR)/coreerlang_plugin
STAMP := $(PKG)/.staged

$(PKG):
	mkdir -p $@

$(STAMP): src/*.ml src/*.mlg src/META.coreerlang_plugin | $(PKG)
	dune build
	cp src/META.coreerlang_plugin $(PKG)/META
	for f in cmxs cma cmxa a; do \
	  ln -sf $(CURDIR)/_build/default/src/coreerlang_plugin.$$f $(PKG)/; \
	done
	touch $@

build: $(STAMP)

# --- Rocq -> Core Erlang (abstract rule over test/*.v) ----------------

$(OUT):
	mkdir -p $@

$(OUT)/%.core: test/%.v $(STAMP) | $(OUT)
	$(ROCQ) c -I $(PKGDIR) -R test "" $<

# --- Core Erlang / EUnit suites -> BEAM (abstract rules) --------------

$(OUT)/%.beam: $(OUT)/%.core | $(OUT)
	$(ERLC) -o $(OUT) $<

$(OUT)/%_tests.beam: test/%_tests.erl | $(OUT)
	$(ERLC) -o $(OUT) $<

# --- run the test suite ------------------------------------------------

check: $(BEAMS)
	$(ERL) -pa $(OUT) -noshell \
	  -eval "eunit:test([$(TESTLIST)], [verbose])" -s init stop

clean:
	rm -rf $(OUT) $(PKGDIR)
