-module(day13).
-export([run/0, quick_test/2]).

run() ->
    [Time, Schedule] =
        lists:map(fun binary_to_list/1, common:readlines("inputs/day13.txt")),
    R = bus_list(string:tokens(Schedule ,","), []),

    {Bus, Wait} = get_bus_times(R, list_to_integer(Time), {Time, Time}),
    common:print_day(?MODULE, 1, Bus * Wait),

    R2 = quick_test(0,0),
    common:print_day(?MODULE, 2, R2).

bus_list([], Acc) ->
    lists:reverse(Acc);
bus_list(["x"|T], Acc) ->
    bus_list(T, [x|Acc]);
bus_list([Num|T], Acc) ->
    bus_list(T, [list_to_integer(Num)|Acc]).

get_bus_times([], _, {Bus, Smallest}) ->
    {Bus, Smallest};
get_bus_times([x|T], Start, Best)->
    get_bus_times(T, Start, Best);
get_bus_times([Bus|T], Start, {_OldBus, Smallest}) 
  when Bus - (Start rem Bus) < Smallest ->
    get_bus_times(T, Start, {Bus, Bus - (Start rem Bus)});
get_bus_times([_|T], Start, Best) ->
    get_bus_times(T, Start, Best).
    
%% Gave up on using the input to generate this function, as I had to try a lot
%% of things and this would have slowed me down on this one. So it will not
%% work on other inputs (only my own). I will try to fix this to be more 
%% generic later when I get sometime
quick_test(N, _Acc) when N rem 983 == 0 
                         andalso (N + 31) rem 397 == 0 
                         andalso (N + 2) rem 29  == 0 
                         andalso (N + 19) rem 19 == 0 
                         andalso (N + 23) rem 23 == 0  
                         andalso (N + 37) rem 37 == 0 
                         andalso (N + 44) rem 13 == 0 
                         andalso (N - 17) rem 17 == 0 
                         andalso (N - 10) rem 41 == 0 ->
       N - 17;
% There is a quicker way to get this, but I just took from 6 here and it seems
% To be ok to get to a solution without waiting too long
quick_test(N, Acc) when (N+31) rem 397 == 0 
                         andalso (N + 2) rem 29  == 0 
                         andalso (N + 19) rem 19 == 0 
                         andalso (N + 23) rem 23 == 0 
                         andalso (N + 37) rem 37 == 0  ->
     io:format(".", []),
     case Acc of
         0 ->
             quick_test(N+983, N);
         _ ->
             quick_test(N+(N-Acc), N)
     end;
quick_test(N, Acc) ->
    quick_test(N+983, Acc). 