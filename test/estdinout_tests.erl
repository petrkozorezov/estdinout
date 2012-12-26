-module(estdinout_tests).
-include_lib("eunit/include/eunit.hrl").

run_test() ->
    {ok, <<"hello">>} = estdinout:run("cat", <<"hello">>),
    {ok, <<"hello">>} = estdinout:run("echo -n hello", <<>>).

    % for big data tests
    % {ok, Data} = file:read_file(filename:join(estdinout_utils:priv_dir(estdinout), "test.jpg")),
    % {ok, Data1} = estdinout:run("convert -quality 95 -resize 2048x2048 - jpg:-", Data),
    % ok = file:write_file("test.jpg", Data1).
