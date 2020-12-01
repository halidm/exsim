REBAR = ./rebar3

DEPS_PATH = ./_build/default/lib/*/ebin

ERL_LIBS= ./lib

CT_SUITES = 
.PHONY: all compile clean distclean dialyze tests

all: compile

compile:
	$(REBAR) compile

clean:
	rm -rf ebin/* test/*.beam logs log
	$(REBAR) clean

distclean: clean
	rm -rf _build priv/*.so logs log

dialyze:
	$(REBAR) dialyzer

tests:
	$(REBAR) ct --suite=$(CT_SUITES)
