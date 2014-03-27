DEPS_DIR=deps
EBIN_DIR=ebin

all: compile

clean:
	rm -rf $(DIST_DIR)
	rm -rf $(EBIN_DIR)

distclean: clean
	rm -rf $(DEPS_DIR)

compile: $(DEPS_DIR)
	./rebar compile

deps:
	./rebar get-deps

run:
	@erl -pz deps/epgsql/ebin/ ebin/ -noinput -noshell +B -eval "application:start(erlln)."

test:
	./rebar compile eunit
