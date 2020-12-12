-module(day11).
-export([run/0]).

run() ->
    Data = lists:map(fun binary_to_list/1, common:readlines("inputs/day11.txt")),
    InitalState = make_data_set(Data),
    {done, _, ResultA, _, _} = do_rounds(part_a, InitalState, 0),
    {done, _, ResultB, _, _} = do_rounds(part_b, InitalState, 0),
    common:print_day(?MODULE, 1, ResultA),
    common:print_day(?MODULE, 2, ResultB).


make_data_set(Data) ->
    make_data_set_y(Data, 1, gb_trees:empty()).
make_data_set_y([], _, Tree) ->
    Tree;
make_data_set_y([H|T], Y, Tree) ->
    make_data_set_y(T, Y+1, make_data_set_x(H, 1, Y, Tree)).

make_data_set_x([], _, _, Tree) ->
    Tree;
make_data_set_x([$L|T], X, Y, Tree) ->
    make_data_set_x(T, X+1, Y, gb_trees:enter({X,Y}, free, Tree));
make_data_set_x([$.|T], X, Y, Tree) ->
    make_data_set_x(T, X+1, Y, Tree).

do_rounds(Part, SeatTree, Round) ->
    SeatList = gb_trees:to_list(SeatTree),
    NewTree = update_seats(Part, SeatList, SeatTree, SeatTree),
    case SeatList == gb_trees:to_list(NewTree) of
        true ->
            {done, Round, check_occupied_count(SeatList, 0), gb_trees:smallest(NewTree), gb_trees:largest(NewTree)};
        false ->
            io:format("."),
            do_rounds(Part, NewTree, Round+1)
    end. 

update_seats(_, [], _, SeatTree) ->
    SeatTree;
update_seats(part_a, [{{X,Y},_} | T], BaseTree, SeatTree) ->
    case check_surrounding_part_a(X,Y,BaseTree) of
        free_to_sit ->
            update_seats(part_a, T, BaseTree, gb_trees:enter({X,Y},occupied,SeatTree));
        free_up ->
            update_seats(part_a, T, BaseTree, gb_trees:enter({X,Y},free,SeatTree));
        do_nothing ->
            update_seats(part_a, T, BaseTree, SeatTree)
    end;
update_seats(part_b, [{{X,Y},_} | T], BaseTree, SeatTree) ->
    case check_surrounding_part_b(X, Y, BaseTree) of
        free_to_sit ->
            update_seats(part_b, T, BaseTree, gb_trees:enter({X,Y},occupied,SeatTree));
        free_up ->
            update_seats(part_b, T, BaseTree, gb_trees:enter({X,Y},free,SeatTree));
        do_nothing ->
            update_seats(part_b, T, BaseTree, SeatTree)
    end.

check_surrounding_part_a(X,Y,SeatTree) ->
        Neighbors = [
            gb_trees:lookup({X-1,Y-1}, SeatTree),
            gb_trees:lookup({X-1,Y}, SeatTree),
            gb_trees:lookup({X-1,Y+1}, SeatTree),
            gb_trees:lookup({X,Y-1}, SeatTree),
            gb_trees:lookup({X,Y+1}, SeatTree),
            gb_trees:lookup({X+1,Y-1}, SeatTree),
            gb_trees:lookup({X+1,Y}, SeatTree),
            gb_trees:lookup({X+1,Y+1}, SeatTree)],
        NumberOccupied = check_occupied_count(Neighbors, 0),
        case {NumberOccupied >=4, NumberOccupied == 0} of
            {true, _} ->
                free_up;
            {_, true} ->
                free_to_sit;
            {_,_} ->
                do_nothing
        end.

check_surrounding_part_b(X,Y,SeatTree) ->
    {{LX, LY},_} = gb_trees:largest(SeatTree),
    Val = look({0,-1},X,Y,SeatTree, {LX, LY}) 
          + look({0,1},X,Y,SeatTree, {LX, LY}) 
          + look({1,0},X,Y,SeatTree, {LX, LY}) 
          + look({-1,0},X,Y,SeatTree, {LX, LY}) 
          + look({1,-1},X,Y,SeatTree, {LX, LY}) 
          + look({1,1},X,Y,SeatTree, {LX, LY}) 
          + look({-1,-1},X,Y,SeatTree, {LX, LY}) 
          + look({-1,1},X,Y,SeatTree, {LX, LY}),

    case {Val >= 5, Val == 0} of 
        {true, _} -> 
            free_up;
        {_, true} ->
            free_to_sit;
        {_,_} ->
            do_nothing
    end.


check_occupied_count([], Count) ->
    Count;
check_occupied_count([{_,occupied}|T], Count) ->
    check_occupied_count(T, Count+1);
check_occupied_count([_|T], Count) ->
    check_occupied_count(T, Count).

look(_,X,Y,_SeatTree,{LargestX, LargestY}) 
  when X > LargestX orelse Y > LargestY ->
    0;
look(_,X,Y,_SeatTree,{_LargestX, _LargestY}) when X < 0 orelse Y < 0 ->
    0;
look({IncX, IncY},X,Y,SeatTree, Largest) ->
    case gb_trees:lookup({X+IncX, Y+IncY}, SeatTree) of
        {value,free} ->
            0;
        {value,occupied}->
            1;
        none->
            look({IncX, IncY},X+IncX,Y+IncY,SeatTree, Largest)
    end.