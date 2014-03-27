-module(erlln_tests).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    ok = application:start(erlln),
    ?assertNot(undefined == whereis(erlln_sup)).
