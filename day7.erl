%% 32
%% 180
-module(day7).
-export([run/0]).

-record(bag, {num, description, color, contains=[]}).

run() ->
    Data = common:readlines("inputs/day7.txt"),
    Result = process_lines({Data,Data}, #bag{num=1, description="shiny", 
        color="gold", contains=[]}, part_a),
    common:print_day(?MODULE, 1, 
        length(lists:usort(count_colors(Result))--[{"shiny", "gold"}])),
    Result2 = process_lines({Data,Data}, #bag{num=1, description="shiny", 
        color="gold", contains=[]}, part_b),
    common:print_day(?MODULE, 2, count_bags(Result2)-1).

process_lines({[],_}, TopBag, _) ->
    TopBag;
process_lines({[<<>>|T], Data}, TopBag, Part) ->
    process_lines({T, Data}, TopBag, Part);
process_lines({[H|T], Data}, TopBag, part_a) ->
    process_lines({T, Data}, process_line_a(H, TopBag, Data), part_a);
process_lines({[H|T], Data}, TopBag, part_b) ->
    process_lines({T, Data}, process_line_b(H, TopBag, Data), part_b).

%%%%%%%%%%%%%%%%%%% PART B
process_line_b(Binary, 
    TopBag=#bag{description=Desc1, color=Color1}, Data) ->
    String = binary_to_list(Binary),
    [First, Last] = string:split(String, " contain "),
    case string:tokens(First, " ") of
        [Desc1, Color1, _] ->
            TopBag#bag{contains = 
                process_part_b(string:split(Last, ", ", all), Data, [])};
        _ ->
            TopBag
    end.

process_part_b([], _Data, Acc) ->
    Acc;
process_part_b([H|T], Data, Acc) ->
    case string:tokens(H, " ") of
        [Number, Desc, Color, _] ->
            process_part_b(T, Data, [process_lines({Data,Data},
                #bag{num=list_to_integer(Number), description=Desc, 
                    color=Color, contains=[]}, part_b)|Acc]);
        ["no", "other", "bags."] ->
            []
    end.

%%%%%%%%%% PART A
process_line_a(Binary, Bag, Data) ->
    String = binary_to_list(Binary),
    [First, Last] = string:split(String, " contain "),
    [Desc1, Color1, _] = string:tokens(First, " "),
    process_part_a(string:split(Last, ", ", all),  
        #bag{description=Desc1, color=Color1}, Bag, Data).

process_part_a([], _, Bag, _) ->
    Bag;
process_part_a([H|T], FirstBag, 
    Bag=#bag{description=Desc, color=Color, contains=Contains}, Data) ->
    case string:tokens(H, " ") of
        [Number, Desc, Color, _] ->
            process_part_a(T, FirstBag, 
                Bag#bag{contains=
                    [process_lines({Data,Data},
                        FirstBag#bag{num=list_to_integer(Number)}, part_a)
                        |Contains]}, Data);
        _Other ->
            process_part_a(T, FirstBag, Bag, Data)
    end.

count_colors(#bag{description=D, color=C, contains=List}) ->
    count_colors(List, [{D,C}]).
count_colors([], Acc) ->
    Acc;
count_colors([#bag{description=D, color=C, contains=List} | T], Acc) ->
    count_colors(List, []) ++ count_colors(T, []) ++ [{D,C} | Acc].

%% Will also count the source bag, so remember to (-)
count_bags(List) when is_list(List) ->
    count_bags(List, 0);
count_bags(Bag) ->
    count_bags([Bag], 0).
count_bags([], Count) ->
    io:format("~p~n", [Count]),
    Count;
count_bags([#bag{num=Num, contains=Contains}|T], Count) ->
   %io:format("~p + (~p * ~p)))+~p ~n", [Num, Num, count_bags(Contains), Count]),
    count_bags(T,  (Num + (Num * count_bags(Contains))))+Count.
