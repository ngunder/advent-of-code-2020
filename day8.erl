-module(day8).
-export([run/0]).

run() ->
    Ins = common:readlines("inputs/day8.txt"),
    PartA=run_program(Ins, 1, 0, []),
    PartB=fix_program(Ins, []),
    common:print_day(?MODULE, 1, PartA),
    common:print_day(?MODULE, 2, PartB).


fix_program([I = <<"nop ", Rest/binary>>|T], Ins) ->
    case run_program(lists:reverse(Ins)++[<<"jmp ", Rest/binary>>|T], 1, 0, []) of
        {halt,_} ->
            fix_program(T, [I|Ins]);
        {done,Acc} ->
            {done,Acc}
    end;
fix_program([I = <<"jmp ", Rest/binary>>|T], Ins) ->
    case run_program(lists:reverse(Ins)++[<<"nop ", Rest/binary>>|T], 1, 0, []) of
        {halt,_} ->
            fix_program(T, [I|Ins]);
        {done, Acc} ->
            {done,Acc}
    end;
fix_program([I|T], Ins) ->
    fix_program(T, [I|Ins]).


run_program(Ins, Pos, Acc, Places) ->
    case Pos > length(Ins) of
        true ->
            {done, Acc};
        false ->

    case lists:member(Pos, Places) of
        true->
            {halt, Acc};
        false ->
            eval(lists:nth(Pos, Ins), Ins, Pos, Acc, [Pos|Places])
    end
end.


eval(<<"nop ", _/binary>>, Ins, Pos, Acc, Places) ->
    run_program(Ins, Pos+1, Acc, Places);
eval(<<"jmp +", Amount/binary>>, Ins, Pos, Acc, Places) ->
    run_program(Ins, Pos+binary_to_integer(Amount), Acc, Places);
eval(<<"acc +", Amount/binary>>, Ins, Pos, Acc, Places) ->
    run_program(Ins, Pos+1, Acc+binary_to_integer(Amount), Places);
eval(<<"jmp -", Amount/binary>>, Ins, Pos, Acc, Places) ->
    run_program(Ins, Pos-binary_to_integer(Amount), Acc, Places);
eval(<<"acc -", Amount/binary>>, Ins, Pos, Acc, Places) ->
    run_program(Ins, Pos+1, Acc-binary_to_integer(Amount), Places).
