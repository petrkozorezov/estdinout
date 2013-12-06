estdinout
=========

Simple middleware for resolving erlang eof port problem. Erlang ports can't close stdout stream when no more data, but shell operators '|' and '<' do and some programs wait for this. For exmple, ImageMagic console command "convert", and it can be useful for server side image converting without a temp files.

Example:

        {ok, <<"hello">>} = estdinout:run("/bin/cat", <<"hello">>).
        {ok, <<"hello">>} = estdinout:run("/bin/echo -n hello", <<>>).
        {ok, SmallJpeg}   = estdinout:run("convert -resize 1024x1024 - jpg:-", BigJpeg).


:herb::pig2::dash::boom:
