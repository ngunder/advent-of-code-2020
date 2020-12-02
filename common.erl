-module(common).
-export([readlines/1]).

readlines(Day) ->
    {ok, Bin} = file:read_file(Day),
    binary:split(Bin, [<<"\r\n">>], [global]).