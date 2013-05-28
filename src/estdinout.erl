-module(estdinout).
-include_lib("kernel/include/file.hrl").

-export([run/2, run/3]).

%%
%% API
%%
%% 6ms on mac book pro 2012
run(Cmd, InputData) ->
    run(Cmd, InputData, 5000).

run(Cmd, InputData, Timeout) ->
    ForcerCmd = filename:join(priv_dir(?MODULE), "estdinout ") ++ Cmd,
    Port = erlang:open_port({spawn, ForcerCmd}, [exit_status, binary, use_stdio, {packet, 4}]),
    true = erlang:port_command(Port, InputData),

    read_response(Port, Timeout).

%%
%% local
%%
read_response(Port, Timeout) ->
    TRef = erlang:start_timer(Timeout, self(), timeout),
    Resp = read_response_loop(Port, [], TRef),
    erlang:cancel_timer(TRef),
    Resp.

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


%% utils
priv_dir(AppName) ->
    case code:priv_dir(AppName) of
        Value when is_list(Value) ->
            Value ++ "/";
        _Error ->
            filename:join(select_priv_dir([["apps", atom_to_list(AppName), "priv"], ["priv"]]))
     end.

select_priv_dir([]) ->
    erlang:throw({error, failed_to_find_priv_dir});
select_priv_dir([Path | Tail]) ->
    case test_priv_dir(Path) of
        true  -> Path;
        false -> select_priv_dir(Tail)
    end.

test_priv_dir(Path) ->
    case file:read_file_info(Path) of
        {ok, #file_info{type=directory}} ->
            true;
        _ ->
            false
    end.
