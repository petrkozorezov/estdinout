-module(estdinout_tests).
-include_lib("eunit/include/eunit.hrl").

run_test() ->
    {ok, <<"hello">>} = estdinout:run("cat", <<"hello">>),
    {ok, <<"hello">>} = estdinout:run("echo -n hello", <<>>).
