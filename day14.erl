-module(day14).
-export([run/0]).

run() ->
    R=lists:map(fun binary_to_list/1, common:readlines("inputs/day14.txt")),
    common:print_day(?MODULE, 1, read_program(part_a, R, "", gb_trees:empty())),
    common:print_day(?MODULE, 2, read_program(part_b, R, "", gb_trees:empty())).

read_program(_, [], _CurrentMask, Tree) ->
    lists:sum(gb_trees:values(Tree));
read_program(Part, ["mask = " ++ Rest | T], _, Tree) ->
    read_program(Part, T, Rest, Tree);
read_program(part_a, ["mem[" ++ Rest | T], CurrentMask, Tree) ->
    {Mem, Val} = read_mem_address(Rest, num, [], []),
    NewVal = apply_mask(CurrentMask, Val),
    read_program(part_a, T, CurrentMask, gb_trees:enter(Mem, NewVal, Tree));
read_program(part_b, ["mem[" ++ Rest | T], CurrentMask, Tree) ->
    {Mem, Val} = read_mem_address(Rest, num, [], []),
    Mems = float_mask(CurrentMask, Mem),
    NewTree =
        lists:foldl(fun(M, Tree) ->
            gb_trees:enter(M, Val, Tree) end, Tree, Mems),
    read_program(part_b, T, CurrentMask, NewTree);
read_program(Part, [Issue|T], CurrentMask, Tree) ->
    io:format("Formatting issue ~p~n", [Issue]),
    read_program(Part, T, CurrentMask, Tree).

read_mem_address([], _, Num, Val) ->
    {list_to_integer(lists:reverse(Num)), list_to_integer(lists:reverse(Val))};
read_mem_address([$], $ |T], num, Num, Val) ->
    read_mem_address(T, num, Num, Val);
read_mem_address([$=, $ |T], num, Num, Val) ->
    read_mem_address(T, val, Num, Val);
read_mem_address([V|T], num, Num, Val) ->
    read_mem_address(T, num, [V|Num], Val);
read_mem_address([V|T], val, Num, Val) ->
    read_mem_address(T, val, Num, [V|Val]).

apply_mask(Mask, Val) ->
    apply_mask(Mask, 35, Val).
apply_mask([], _, Val) ->
    Val;
apply_mask([$X|T], Bit, Val) ->
    apply_mask(T, Bit-1, Val);
apply_mask([$1|T], Bit, Val) ->
    apply_mask(T, Bit-1, round(math:pow(2, Bit)) bor Val);
apply_mask([$0|T], Bit, Val) ->
    Bitval = round(math:pow(2, Bit)),
    apply_mask(T, Bit-1, 
        (Val bor Bitval) bxor Bitval).

float_mask(Mask, MemVal) when is_integer(MemVal) ->
    <<B1:1, B2:1, B3:1, B4:1, B5:1, B6:1, B7:1, B8:1, B9:1, B10:1, B11:1, B12:1, 
      B13:1, B14:1, B15:1, B16:1, B17:1, B18:1, B19:1, B20:1, B21:1, B22:1, 
      B23:1, B24:1, B25:1, B26:1, B27:1, B28:1, B29:1, B30:1, B31:1, B32:1, 
      B33:1, B34:1, B35:1, B36:1>> = <<MemVal:36>>,
    float_mask(Mask, [B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, 
      B13, B14, B15, B16, B17, B18, B19, B20, B21, B22, B23, B24, B25, B26, 
      B27, B28, B29, B30, B31, B32, B33, B34, B35, B36], [<<>>]).
float_mask([], _MemBits, Bins) ->
    make_vals(Bins, []);
float_mask([$0|T1], [V|T2], Bins) ->
    float_mask(T1, T2, apply_bin((V bor 0) bxor 0, Bins));
float_mask([$1|T], [V|T2], Bins) ->
    float_mask(T, T2, apply_bin(V bor 1, Bins));
float_mask([$X|T], [_V|T2], Bins) ->
    float_mask(T, T2, apply_bin(floater, Bins)).

apply_bin(1, Bins) ->
    apply_bin(1, Bins, []);
apply_bin(0, Bins) ->
    apply_bin(0, Bins, []);
apply_bin(floater, Bins) ->
    float_binary(Bins).


apply_bin(_, [], Acc) ->
    Acc;
apply_bin(Val, [<<Bin/bits>>|T], Acc) ->
    apply_bin(Val, T, [<<Bin/bits, Val:1>>|Acc]).

float_binary(List) ->
    float_binary(List, []).
float_binary([], Acc) ->
    Acc;
float_binary([<<Bin/bits>>|T], Acc) ->
    float_binary(T, [<<Bin/bits, 1:1>>,<<Bin/bits, 0:1>>|Acc]).

make_vals([], Vals) ->
    Vals;
make_vals([<<BinVal:36>>|T], Vals) ->
    make_vals(T, [BinVal | Vals]).