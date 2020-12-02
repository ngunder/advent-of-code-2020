-module(common).
-export([readlines/1, elm_count/2, print_day/2]).

readlines(Day) ->
    {ok, Bin} = file:read_file(Day),
    binary:split(Bin, [<<"\r\n">>], [global]).

elm_count(List, Elm) when is_list(Elm) ->
    elm_count(List, hd(Elm), 0);
elm_count(List, Elm) ->
    elm_count(List, Elm, 0).
elm_count([], _Elm, Acc) ->
    Acc;
elm_count([Elm|T], Elm, Acc) ->
    elm_count(T, Elm, Acc + 1);
elm_count([_|T], Elm, Acc) ->
    elm_count(T, Elm, Acc).

print_day(Part, Str) ->
    io:format("Part ~w: ~w~n", [Part, Str]).