-module(day4).
-export([run/0]).

% --- Day 4: Passport Processing ---
% You arrive at the airport only to realize that you grabbed your North Pole 
% Credentials instead of your passport. While these documents are extremely 
% similar, North Pole Credentials aren't issued by a country and therefore 
% aren't actually valid documentation for travel in most of the world.

% It seems like you're not the only one having problems, though; a very long 
% line has formed for the automatic passport scanners, and the delay could 
% upset your travel itinerary.

% Due to some questionable network security, you realize you might be able to 
% solve both of these problems at the same time.

% The automatic passport scanners are slow because they're having trouble 
% detecting which passports have all required fields. The expected fields 
% are as follows:

% byr (Birth Year)
% iyr (Issue Year)
% eyr (Expiration Year)
% hgt (Height)
% hcl (Hair Color)
% ecl (Eye Color)
% pid (Passport ID)
% cid (Country ID)
-record(passport, {byr, iyr, eyr, hgt, hcl, ecl, pid, cid=none}).

% Passport data is validated in batch files (your puzzle input). Each passport 
% is represented as a sequence of key:value pairs separated by spaces or 
% newlines. Passports are separated by blank lines.

% Here is an example batch file containing four passports:

% ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
% byr:1937 iyr:2017 cid:147 hgt:183cm

% iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
% hcl:#cfa07d byr:1929

% hcl:#ae17e1 iyr:2013
% eyr:2024
% ecl:brn pid:760753108 byr:1931
% hgt:179cm

% hcl:#cfa07d eyr:2025 pid:166559648
% iyr:2011 ecl:brn hgt:59in
% The first passport is valid - all eight fields are present. The second 
% passport is invalid - it is missing hgt (the Height field).

% The third passport is interesting; the only missing field is cid, so it 
% looks like data from North Pole Credentials, not a passport at all! Surely, 
% nobody would mind if you made the system temporarily ignore missing cid 
% fields. Treat this "passport" as valid.

% The fourth passport is missing two fields, cid and byr. Missing cid is fine, 
% but missing any other field is not, so this passport is invalid.

% According to the above rules, your improved system would report 2 valid 
% passports.

% Count the number of valid passports - those that have all required fields. 
% Treat cid as optional. In your batch file, how many passports are valid?

% 211 is too low
% 212 ?? 
% 225 is not correct
% 254 someone else's #
% 255 is incorrect

% --- Part Two ---
% The line is moving more quickly now, but you overhear airport security 
% talking about how passports with invalid data are getting through. Better 
% add some data validation, quick!

% You can continue to ignore the cid field, but each other field has strict 
% rules about what values are valid for automatic validation:

% byr (Birth Year) - four digits; at least 1920 and at most 2002.
% iyr (Issue Year) - four digits; at least 2010 and at most 2020.
% eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
% hgt (Height) - a number followed by either cm or in:
% If cm, the number must be at least 150 and at most 193.
% If in, the number must be at least 59 and at most 76.
% hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
% ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
% pid (Passport ID) - a nine-digit number, including leading zeroes.
% cid (Country ID) - ignored, missing or not.
% Your job is to count the passports where all required fields are both present 
% and valid according to the above rules. Here are some example values:

% byr valid:   2002
% byr invalid: 2003

% hgt valid:   60in
% hgt valid:   190cm
% hgt invalid: 190in
% hgt invalid: 190

% hcl valid:   #123abc
% hcl invalid: #123abz
% hcl invalid: 123abc

% ecl valid:   brn
% ecl invalid: wat

% pid valid:   000000001
% pid invalid: 0123456789
% Here are some invalid passports:

% eyr:1972 cid:100
% hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

% iyr:2019
% hcl:#602927 eyr:1967 hgt:170cm
% ecl:grn pid:012533040 byr:1946

% hcl:dab227 iyr:2012
% ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

% hgt:59cm ecl:zzz
% eyr:2038 hcl:74454a iyr:2023
% pid:3556412378 byr:2007
% Here are some valid passports:

% pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
% hcl:#623a2f

% eyr:2029 ecl:blu cid:129 byr:1989
% iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

% hcl:#888785
% hgt:164cm byr:2001 iyr:2015 cid:88
% pid:545766238 ecl:hzl
% eyr:2022

% iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
% Count the number of valid passports - those that have all required fields and 
% valid values. Continue to treat cid as optional. In your batch file, how many 
% passports are valid?
run() ->
    Data = common:readlines("inputs/day4.txt"),
   
    Recs = enter_passport_data(Data),
     io:format("~p~n", [Recs]),
    lists:foldl(fun(E, Acc) -> Acc + valid_rec(E) end, 0, Recs).

enter_passport_data(Data) ->
    enter_passport_data(Data, #passport{}, []).

enter_passport_data([], Rec, TotalAcc) ->
    [Rec|TotalAcc];
enter_passport_data([<<>>|T], Rec, TotalAcc) ->
    enter_passport_data(T, #passport{}, [Rec|TotalAcc]);
enter_passport_data([RecInfo|T], Rec, TotalAcc) ->
    enter_passport_data(T, parse_rec_info(RecInfo, Rec), TotalAcc).

parse_rec_info(Bin, Rec) ->
    String = binary_to_list(Bin),
    ListOfElms = lists:foldl(fun(E, Acc)-> 
        [string:tokens(E, ":")|Acc] end, [], string:tokens(String, " ")),
    build_rec(ListOfElms, Rec).


valid_rec(#passport{byr=undefined}) ->
    0;
valid_rec(#passport{iyr=undefined}) ->
    0;
valid_rec(#passport{eyr=undefined}) ->
    0;
valid_rec(#passport{hgt=undefined}) ->
    0;
valid_rec(#passport{hcl=undefined}) ->
    0;
valid_rec(#passport{ecl=undefined}) ->
    0;
valid_rec(#passport{pid=undefined}) ->
    0;
valid_rec(_)->
    1.

build_rec([], Rec) ->
    Rec;
build_rec([["byr", Val]|T], Rec) ->
    build_rec(T, Rec#passport{byr = check_byr(Val)});
build_rec([["iyr", Val]|T], Rec) ->
    build_rec(T, Rec#passport{iyr = check_iyr(Val)});
build_rec([["eyr", Val]|T], Rec) ->
    build_rec(T, Rec#passport{eyr = check_eyr(Val)});
build_rec([["hgt", Val]|T], Rec) ->
    build_rec(T, Rec#passport{hgt = check_hgt(Val)});
build_rec([["hcl", Val]|T], Rec) ->
    build_rec(T, Rec#passport{hcl = check_hcl(Val)});
build_rec([["ecl", Val]|T], Rec) ->
    build_rec(T, Rec#passport{ecl = check_ecl(Val)});
build_rec([["pid", Val]|T], Rec) ->
    build_rec(T, Rec#passport{pid = check_pid(Val)});
build_rec([["cid", Val]|T], Rec) ->
    build_rec(T, Rec#passport{cid = Val});
build_rec(Unknown, Rec) ->
    io:format("Unknown ~p~n", [Unknown]),
    Rec.

% byr (Birth Year) - four digits; at least 1920 and at most 2002.
check_byr(Year) when is_list(Year) ->
    check_byr(list_to_integer(Year));
check_byr(Year) when Year >= 1920 andalso Year =< 2002 ->
    Year;
check_byr(_) ->
    undefined.
% iyr (Issue Year) - four digits; at least 2010 and at most 2020.
check_iyr(Year) when is_list(Year) ->
    check_iyr(list_to_integer(Year));
check_iyr(Year) when Year >= 2010 andalso Year =< 2020 ->
    Year;
check_iyr(_) ->
    undefined.
% eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
check_eyr(Year) when is_list(Year) ->
    check_eyr(list_to_integer(Year));
check_eyr(Year) when Year >= 2020 andalso Year =< 2030 ->
    Year;
check_eyr(_) ->
    undefined.

% hgt (Height) - a number followed by either cm or in:
check_hgt(Str) ->
    case {lists:suffix("cm", Str), lists:suffix("in", Str)} of 
        {true, false} ->
            check_cm(list_to_integer(lists:subtract(Str, "cm")));
        {false, true} ->
            check_in(list_to_integer(lists:subtract(Str, "in")));
        _ ->
            undefined
    end.
% If cm, the number must be at least 150 and at most 193.
check_cm(Val) when Val >= 150 andalso Val =< 193 ->
    Val;
check_cm(_)->
    undefined.

% If in, the number must be at least 59 and at most 76.
check_in(Val) when Val >= 59 andalso Val =< 76 ->
    Val;
check_in(_)->
    undefined.

% hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
check_hcl([$#, C1, C2, C3, C4, C5, C6] = Str) ->
    case 
        common:is_hex(C1) andalso 
        common:is_hex(C2) andalso 
        common:is_hex(C3) andalso 
        common:is_hex(C4) andalso 
        common:is_hex(C5) andalso 
        common:is_hex(C6) of 
            true ->
                Str;
            false ->
                undefined
    end;
check_hcl(_) ->
    undefined.

% ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
check_ecl("amb") ->
    "amb";
check_ecl("blu") ->
    "blu";
check_ecl("brn") ->
    "brn";
check_ecl("gry") ->
    "gry";
check_ecl("grn") ->
    "grn";
check_ecl("hzl") ->
    "hzl";
check_ecl("oth") ->
    "oth";
check_ecl(_)->
    undefined.

% pid (Passport ID) - a nine-digit number, including leading zeroes.
check_pid([_, _, _, _, _, _, _, _, _] = NumStr) ->
    case common:is_whole_int(NumStr) of
        true ->
            list_to_integer(NumStr);
        false ->
            undefined
    end;
check_pid(_) ->
    undefined.

% cid (Country ID) - ignored, missing or not.


