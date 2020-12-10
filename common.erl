-module(common).
-export([readlines/1, elm_count/2, print_day/3, better_split/2, 
    is_whole_int/1, is_hex/1, maximum/2]).

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

print_day(Day, Part, Str) ->
    io:format("~p ~p: ~p~n", [Day, Part, Str]).

% Not sure why the lists:split fails on odd lengths, but I will make my own so 
% it doesn't
better_split(Where, List) ->
    better_split(Where, List, []).
better_split(Where, List,  Acc) when Where =< 0 orelse List =:= []->
    {lists:reverse(Acc), List};
better_split(Where, [H|T], Acc) ->
    better_split(Where-1, T, [H|Acc]).

is_whole_int(String) ->
    is_whole_int(String, false).
is_whole_int([], Result) ->
    Result;
is_whole_int([H|T], _Result) when H >= $0 andalso H =< $9->
    is_whole_int(T, true);
is_whole_int(_, _) ->
    false.

is_hex(C) when (C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $f) ->
    true;
is_hex(_) ->
    false. 

maximum(Max, [H|T]) ->
    case Max > H of
        true -> maximum(Max, T);
        false -> maximum(H, T)
    end;
maximum(Max, []) -> Max.