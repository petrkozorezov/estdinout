estdinout
=========

simple middleware for resolving erlang eof port problem

example:

        {ok, <<"hello">>} = estdinout:run("/bin/cat", <<"hello">>),
        {ok, <<"hello">>} = estdinout:run("/bin/echo -n hello", <<>>).
