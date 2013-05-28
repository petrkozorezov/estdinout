-module(estdinout_tests).
-include_lib("eunit/include/eunit.hrl").

run_test() ->
    {ok, <<"hello">>} = estdinout:run("cat", <<"hello">>),
    {ok, <<"hello">>} = estdinout:run("echo -n hello", <<>>),

    BigDataMbLength = 500,
    BigDataKb = [random:uniform(255) || _ <- lists:seq(1, 1024)],
    BigDataMb = [BigDataKb || _ <- lists:seq(1, 1024)],
    random:seed(erlang:now()),
    BigData = list_to_binary([BigDataMb || _ <- lists:seq(1, BigDataMbLength)]),
    {ok, BigData} = estdinout:run("cat", BigData).

    % for big data tests
    % {ok, Data} = file:read_file(filename:join(estdinout_utils:priv_dir(estdinout), "logo.png")),
    % {ok, Data1} = estdinout:run("convert -quality 80 -resize 100x100 - png:-", Data),
    % io:format("Data: ~p", [Data1]),
    % % exit(1),
    % ok = file:write_file("../logo.png", Data1).
