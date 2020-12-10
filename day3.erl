-module(day3).
-export([run/0]).
% --- Day 3: Toboggan Trajectory ---
% With the toboggan login problems resolved, you set off toward the airport. 
% While travel by toboggan might be easy, it's certainly not safe: there's 
% very minimal steering and the area is covered in trees. You'll need to see 
% which angles will take you near the fewest trees.

% Due to the local geology, trees in this area only grow on exact integer 
% coordinates in a grid. You make a map (your puzzle input) of the open squares 
% (.) and trees (#) you can see. For example:

% ..##.......
% #...#...#..
% .#....#..#.
% ..#.#...#.#
% .#...##..#.
% ..#.##.....
% .#.#.#....#
% .#........#
% #.##...#...
% #...##....#
% .#..#...#.#
% These aren't the only trees, though; due to something you read about once 
% involving arboreal genetics and biome stability, the same pattern repeats 
% to the right many times:

% ..##.........##.........##.........##.........##.........##.......  --->
% #...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
% .#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
% ..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
% .#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
% ..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....  --->
% .#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
% .#........#.#........#.#........#.#........#.#........#.#........#
% #.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
% #...##....##...##....##...##....##...##....##...##....##...##....#
% .#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
% You start on the open square (.) in the top-left corner and need to reach 
% the bottom (below the bottom-most row on your map).

% The toboggan can only follow a few specific slopes (you opted for a cheaper 
% model that prefers rational numbers); start by counting all the trees you 
% would encounter for the slope right 3, down 1:

% From your starting position at the top-left, check the position that is right 
% 3 and down 1. Then, check the position that is right 3 and down 1 from there, 
% and so on until you go past the bottom of the map.

% The locations you'd check in the above example are marked here with O where 
% there was an open square and X where there was a tree:

% ..##.........##.........##.........##.........##.........##.......  --->
% #..O#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
% .#....X..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
% ..#.#...#O#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
% .#...##..#..X...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
% ..#.##.......#.X#.......#.##.......#.##.......#.##.......#.##.....  --->
% .#.#.#....#.#.#.#.O..#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
% .#........#.#........X.#........#.#........#.#........#.#........#
% #.##...#...#.##...#...#.X#...#...#.##...#...#.##...#...#.##...#...
% #...##....##...##....##...#X....##...##....##...##....##...##....#
% .#..#...#.#.#..#...#.#.#..#...X.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
% In this example, traversing the map using this slope would cause you to 
% encounter 7 trees.

% Starting at the top-left corner of your map and following a slope of right 3 
% and down 1, how many trees would you encounter?

% --- Part Two ---
% Time to check the rest of the slopes - you need to minimize the probability 
% of a sudden arboreal stop, after all.

% Determine the number of trees you would encounter if, for each of the 
% following slopes, you start at the top-left corner and traverse the map 
% all the way to the bottom:

% Right 1, down 1.
% Right 3, down 1. (This is the slope you already checked.)
% Right 5, down 1.
% Right 7, down 1.
% Right 1, down 2.
% In the above example, these slopes would find 2, 7, 3, 4, and 2 tree(s) 
% respectively; multiplied together, these produce the answer 336.

% What do you get if you multiply together the number of trees encountered on 
% each of the listed slopes?

-define(TREE, $#).
-define(OPEN, $.).
-record(position, {pos, val}).

run() ->
    Input = common:readlines("inputs/day3.txt"),
    Parsed = parse_input(Input),
    Part1 = process_run(3, 1, Parsed),
    common:print_day(?MODULE, 1, Part1),
    Part2 = Part1 * process_run(1, 1, Parsed) * process_run(5, 1, Parsed) * 
            process_run(7,1,Parsed) * process_run(1,2,Parsed),
    common:print_day(?MODULE, 2, Part2).

process_run(Right, Down, Input) ->
    process_run(length(hd(Input)), Right, Down, Input, {0, 0}).
process_run(RunSize, Right, Down, Input=[In|_], {X, TreeCount}) ->
    NextX = (X+Right) rem RunSize,
    {_, NewTail} = common:better_split(Down, Input),
    {value, #position{val=Val}} = lists:keysearch(X, #position.pos, In),
    NewTreeCount = case Val of
        ?TREE ->
            TreeCount+1;
        ?OPEN ->
            TreeCount
        end,
        process_run(RunSize, Right, Down, NewTail, {NextX, NewTreeCount});
process_run(_RunSize, _Right, _Down, [], {_X, TreeCount}) ->
    TreeCount.

parse_input(List) ->
    parse_input(List, [], 0).
parse_input([Bin|T], Acc, Y) ->
    parse_input(T, [parse_bin(Bin,Y)|Acc], Y+1);
parse_input([], Acc, _) ->
    lists:reverse(Acc).

parse_bin(Bin,Y) ->
    parse_bin(Bin, [], {0,Y}).
parse_bin(<<>>, Acc, {_,_}) ->
    lists:reverse(Acc);
parse_bin(<<Char:8, Rest/binary>>, Acc, {X,Y}) ->
    parse_bin(Rest, [#position{pos=X, val=Char}|Acc], {X+1,Y}).

