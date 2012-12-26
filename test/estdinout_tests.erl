-module(estdinout_tests).
-include_lib("eunit/include/eunit.hrl").

run_test() ->
    {ok, <<"hello">>} = estdinout:run("/bin/cat", <<"hello">>),
    {ok, <<"hello">>} = estdinout:run("/bin/echo -n hello", <<>>).
