-module(day10).
-export([run/0]).

run() ->
    Data = lists:map(fun binary_to_integer/1, common:readlines("inputs/day10.txt")),
    Result = find_diffs(lists:sort(Data)),
    Val1 = common:elm_count(Result, 1),
    Val2 = common:elm_count(Result, 3),
    %io:format("1's : ~p~n", [Val1]),
    %io:format("3's : ~p~n", [Val2]),
    common:print_day(?MODULE, 1, Val1 * Val2),
    LongList = lists:sort(find_combos(lists:sort(Data))),
    common:print_day(?MODULE, 2, calculate_perms(LongList)).


find_diffs(List) ->
    First = hd(List) - 0,
    find_diffs(List, [First]).
find_diffs([], Acc)->
    Acc;
find_diffs([_], Acc) ->
    find_diffs([], [3|Acc]);
find_diffs([H1, H2 | T], Acc)->
    find_diffs([H2|T], [H2-H1|Acc]).

find_combos(List) ->
    find_combos([0]++List++[hd(lists:reverse(List))+3], []).

find_combos([], Acc) ->
    Acc;
find_combos([_,_], Acc) ->
    Acc;
find_combos([H1, H2, H3, H4, H5 | T], Acc) when H5 - H1 =:= 4 ->
    find_combos([H1, H5 | T], [H2, H3, H4 | Acc]);
find_combos([H1, H2, H3, H4 | T], Acc) when H4 - H1 < 4 ->
    find_combos([H1, H4 | T], [H2, H3 | Acc]);
find_combos([H1, H2, H3 | T], Acc) when H3 - H1 < 4 ->
    find_combos([H1, H3 | T], [H2 | Acc]);
find_combos([_,H2,H3|T], Acc) ->
    find_combos([H2,H3|T], Acc).

calculate_perms(List) ->
    calculate_perms(List, 1).
calculate_perms([], Sum) ->
    Sum;
calculate_perms([H1,H2,H3|T], Sum) when H2 - H1 == 1 andalso H3 - H2 == 1 ->
    calculate_perms(T, Sum*7);
calculate_perms([H1,H2|T], Sum) when H2 - H1 == 1 ->
    calculate_perms(T, Sum*4);
calculate_perms([_|T], Sum) ->
    calculate_perms(T, Sum*2).