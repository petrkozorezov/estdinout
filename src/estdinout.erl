-module(estdinout).

-export([run/2, run/3]).

%%
%% API
%%
%% 6ms on mac book pro 2012
run(Cmd, InputData) ->
    run(Cmd, InputData, 5000).

run(Cmd, InputData, Timeout) ->
    ForcerCmd = filename:join(estdinout_utils:priv_dir(?MODULE), "stdin_forcer ") ++ Cmd,
    Port = erlang:open_port({spawn, ForcerCmd}, [exit_status, binary, use_stdio, {packet, 4}]),
    true = erlang:port_command(Port, InputData),

    read_response(Port, Timeout).

%%
%% local
%%
read_response(Port, Timeout) ->
    TRef = erlang:start_timer(Timeout, self(), timeout),
    Resp = read_response_loop(Port, [], TRef),
    erlang:cancel_timer(TRef), Resp.

read_response_loop(Port, Acc, TRef) ->
    receive
        {Port, {data, Data}} ->
            read_response_loop(Port, [Data|Acc], TRef);
        {Port, closed} ->
            {error, closed};
        {Port, {exit_status, 0}} ->
            {ok, iolist_to_binary(lists:reverse(Acc))};
        {Port, {exit_status, Status}} ->
            {error, {exit_status, Status}};
        {'EXIT', Port, Reason} ->
            {error, {interrupted, Reason}};
        {timeout, TRef, timeout} ->
            port_close(Port),
            {error, timeout}
    end.
