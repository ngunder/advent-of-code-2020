-module(day12).
-export([run/0]).

run() ->
    Data =lists:map(fun binary_to_list/1, common:readlines("inputs/day12.txt")),
    {{X1,Y1}=_ShipPos1, _Dir} = get_pos(Data),
    common:print_day(?MODULE, 2, abs(X1) + abs(Y1)),
    {{X2,Y2}=_ShipPos2, _WaypointPos} = get_pos_2(Data),
    common:print_day(?MODULE, 2, abs(X2) + abs(Y2)).

get_pos(Data) ->
    get_pos(Data, {0,0}, 90).
get_pos([], Pos, Dir) ->
    {Pos, Dir};
get_pos([[$N|Amount]|T], {X,Y}, Dir) ->
    Int = list_to_integer(Amount),
    get_pos(T, {X, Y-Int}, Dir);
get_pos([[$S|Amount]|T], {X,Y}, Dir) ->
    Int = list_to_integer(Amount),
    get_pos(T, {X, Y+Int}, Dir);
get_pos([[$E|Amount]|T], {X,Y}, Dir) ->
    Int = list_to_integer(Amount),
    get_pos(T, {X+Int, Y}, Dir);
get_pos([[$W|Amount]|T], {X,Y}, Dir) ->
    Int = list_to_integer(Amount),
    get_pos(T, {X-Int, Y}, Dir);
get_pos([[$L|Amount]|T], {X,Y}, Dir) ->
    Int = list_to_integer(Amount),
    get_pos(T, {X, Y}, (Dir-Int) rem 360);
get_pos([[$R|Amount]|T], {X,Y}, Dir) ->
    Int = list_to_integer(Amount),
    get_pos(T, {X, Y}, (Dir+Int) rem 360);
get_pos([[$F|Amount]|T], {X,Y}, Dir) ->
    Int = list_to_integer(Amount),
    get_pos(T, process_f(Int, {X,Y}, Dir), Dir).

get_pos_2(Data) ->
    get_pos_2(Data, {0,0}, {10,-1}).
get_pos_2([], ShipPos, WaypointPos) ->
    {ShipPos, WaypointPos};
get_pos_2([[$N|Amount]|T], {ShipX, ShipY}, {WaypointX, WaypointY}) ->
    Int = list_to_integer(Amount),
    get_pos_2(T, {ShipX, ShipY}, {WaypointX, WaypointY-Int});
get_pos_2([[$S|Amount]|T], {ShipX, ShipY}, {WaypointX, WaypointY}) ->
    Int = list_to_integer(Amount),
    get_pos_2(T, {ShipX, ShipY}, {WaypointX, WaypointY+Int});
get_pos_2([[$E|Amount]|T], {ShipX, ShipY}, {WaypointX, WaypointY}) ->
    Int = list_to_integer(Amount),
    get_pos_2(T, {ShipX, ShipY}, {WaypointX+Int, WaypointY});
get_pos_2([[$W|Amount]|T], {ShipX, ShipY}, {WaypointX, WaypointY}) ->
    Int = list_to_integer(Amount),
    get_pos_2(T, {ShipX, ShipY}, {WaypointX-Int, WaypointY});
get_pos_2([[$L|Amount]|T], {ShipX, ShipY}, {WaypointX, WaypointY}) ->
    Int = list_to_integer(Amount),
    get_pos_2(T, {ShipX, ShipY}, rotate_waypoint(l, Int, {WaypointX, WaypointY}));
get_pos_2([[$R|Amount]|T], {ShipX, ShipY}, {WaypointX, WaypointY}) ->
    Int = list_to_integer(Amount),
    get_pos_2(T, {ShipX, ShipY}, rotate_waypoint(r, Int, {WaypointX, WaypointY}));
get_pos_2([[$F|Amount]|T], {ShipX, ShipY}, {WaypointX, WaypointY}) ->
    Int = list_to_integer(Amount),
    NewShipX = ShipX + (Int * WaypointX),
    NewShipY = ShipY + (Int * WaypointY),
    get_pos_2(T, {NewShipX, NewShipY}, {WaypointX, WaypointY}).

process_f(Amount, {X,Y}, Dir) when Dir == 90 orelse Dir == -270 ->
    {X+Amount, Y};
process_f(Amount, {X,Y}, Dir) when Dir == 0 ->
    {X, Y-Amount};
process_f(Amount, {X,Y}, Dir) when Dir == -90 orelse Dir == 270 ->
    {X-Amount, Y};
process_f(Amount, {X,Y}, Dir) when Dir == 180 orelse Dir == -180 ->
    {X, Y+Amount}.

rotate_waypoint(_, 0, {X,Y}) ->
    {X,Y};
rotate_waypoint(l, Int, {X,Y}) ->
    rotate_waypoint(l, Int-90, {Y, -X});
rotate_waypoint(r, Int, {X,Y}) ->
    rotate_waypoint(r, Int-90, {-Y, X}).



