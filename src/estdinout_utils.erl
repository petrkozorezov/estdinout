-module(estdinout_utils).
-include_lib("kernel/include/file.hrl").
-export([priv_dir/1]).

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
