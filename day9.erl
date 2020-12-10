-module(day9).
-export([run/0]).
%55732936
run() ->
    BinList = common:readlines("inputs/day9.txt"),
    NumList = lists:foldr(fun(E,Acc) -> [binary_to_integer(E)|Acc] end, 
                           [], BinList),
    PartA = check_sum_list(NumList),
    common:print_day(?MODULE, 1, PartA),
    ResultList = lists:sort(find_contiguous_list(NumList, PartA)),
    Part2 = hd(ResultList) + hd(lists:reverse(ResultList)),
    common:print_day(?MODULE, 2, Part2).


check_sum_list([N1, N2, N3, N4, N5, N6, N7, N8, N9, N10,
                N11, N12, N13, N14, N15, N16, N17, N18, N19, N20,
                N21, N22, N23, N24, N25, Sum | T]) ->
    case check_sums([N1, N2, N3, N4, N5, N6, N7, N8, N9, N10,
    N11, N12, N13, N14, N15, N16, N17, N18, N19, N20,
    N21, N22, N23, N24, N25], Sum) of
        true ->
            check_sum_list([N2, N3, N4, N5, N6, N7, N8, N9, N10,
                            N11, N12, N13, N14, N15, N16, N17, N18, N19, N20,
                            N21, N22, N23, N24, N25, Sum|T]);
        false ->
            Sum
    end.

check_sums(SumList, Sum) ->
    GenList = [{X,Y} || Y <- SumList, X <- SumList],
    check_sum_list(GenList, Sum).

check_sum_list([{X,Y}|_], Sum) when X+Y == Sum andalso X=/=Y ->
    true;
check_sum_list([_|T], Sum) ->
    check_sum_list(T, Sum);
check_sum_list([], _Sum) ->
    false.

find_contiguous_list(Set, Num) ->
    find_contiguous_list(Set, Set, Num, []).
find_contiguous_list([], _, _, _) ->
    error;
find_contiguous_list([H|T], Set, Num, Acc) ->
    CurrentCount = lists:sum(Acc),
    case {CurrentCount + H == Num, CurrentCount + H < Num} of
        {true, _} ->
            lists:reverse([H|Acc]);
        {false, false} ->
            [_|NewT] = Set,
            find_contiguous_list(NewT, NewT, Num, []);
        {_, true} ->
            find_contiguous_list(T, Set, Num, [H|Acc])
    end.