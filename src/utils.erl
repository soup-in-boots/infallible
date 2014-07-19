-module(utils).
-export([pl_to_record/3]).
-export([grab_field/3, assign_field/4]).
-export([md5/1, bin_to_hex/1]).
-export([get_value/2, get_value/3, ldef/1]).
-export([get_available_races/0]).
-export([re_match/2,re_match/3]).
-export([pretty_direction/1,pretty_stats/1]).
-export([num_to_list/1]).
-export([term_to_json/1, term_from_json/1]).
-include("infallible.hrl").

pretty_direction(north) -> "North";
pretty_direction(south) -> "South";
pretty_direction(east)  -> "East";
pretty_direction(west)  -> "West";
pretty_direction(up)    -> "Up";
pretty_direction(down)  -> "Down".

pretty_stats(Field)     -> string:left(do_pretty_stats(Field), 24).

do_pretty_stats(health)    ->  "Health";
do_pretty_stats(mana)      ->  "Mana";
do_pretty_stats(stamina)   ->  "Stamina";
do_pretty_stats(hps)       ->  "Health Regen";
do_pretty_stats(mps)       ->  "Mana Regen";
do_pretty_stats(sps)       ->  "Stamina Regen";
do_pretty_stats(str)       ->  "Strength";
do_pretty_stats(dex)       ->  "Dexterity";
do_pretty_stats(con)       ->  "Constitution";
do_pretty_stats(int)       ->  "Intelligence";
do_pretty_stats(spr)       ->  "Spirit".

atomize_pl(PL) ->
    [ {atomize_key(K), V} || {K, V} <- PL ].

atomize_key(Key) when is_binary(Key) -> binary_to_existing_atom(Key, utf8);
atomize_key(Key) when is_list(Key) -> list_to_existing_atom(Key).

pl_to_record(Tag, Fields, PL) ->
    Atomized = atomize_pl(PL),
    Tuple = list_to_tuple([Tag|Fields]),
    lists:foldl(fun({Field, Value}, Tuple) -> assign_field(Field, Value, Fields, Tuple) end, Tuple, Atomized).

grab_field(Field, [Field|_Fields], [Value|_Values]) ->
    Value;
grab_field(Field, [_Other|Fields], [_Value|Values]) ->
    grab_field(Field, Fields, Values);
grab_field(Field, Fields, Record) when is_tuple(Record) ->
    grab_field(Field, Fields, tl(tuple_to_list(Record)));
grab_field(Field, [], []) ->
    undefined.

assign_field(Field, Assign, [Field|_Fields], [_OldValue|Values]) ->
    [Assign|Values];
assign_field(Field, Assign, [_Other|Fields], [Value|Values]) ->
    [Value|assign_field(Field, Assign, Fields, Values)];
assign_field(Field, _Assign, [], []) ->
    exit({no_field, Field});
assign_field(Field, Assign, Fields, Record) when is_tuple(Record) ->
    [Tag|Values] = tuple_to_list(Record),
    NewValues = assign_field(Field, Assign, Fields, Values),
    list_to_tuple([Tag|NewValues]).

md5(Data) ->
    Bin     = erlang:md5(Data),
    lists:flatten(lists:reverse(bin_to_hex(Bin))).

bin_to_hex(Bin) -> bin_to_hex(Bin, "").

bin_to_hex(<<B:8/integer,Rest/binary>>, S) ->
    bin_to_hex(Rest, [integer_to_list(B, 16)|S]);
bin_to_hex(<<>>, S) ->
    S.

get_value(K, L) -> 
    get_value(K, L, undefined).
get_value(K, L, D) ->
    case lists:keyfind(K, 1, L) of
        false       -> D;
        {K, V}      -> V
    end.

ldef([])            -> undefined;
ldef([undefined|T]) -> ldef(T);
ldef([A|_])         -> A.

get_available_races() ->
    [ Name || _ = #race{name = Name} <- inf_race:fetch_all() ].

re_match(Subject, RE) -> re_match(Subject, RE, []).
re_match(Subject, RE, Options) ->
    case re:run(Subject, RE, Options) of
        match       -> true;
        {match, _}  -> true;
        _           -> false
    end.

num_to_list(N) when is_integer(N) -> integer_to_list(N);
num_to_list(F) when is_float(F) -> float_to_list(F, [{decimals,2}, compact]).

term_to_json(Term) when is_list(Term) ->
    case is_struct(Term) of
        true -> lists:map(fun struct_to_json/1, Term);
        false -> lists:map(fun term_to_json/1, Term)
    end;
term_to_json(Term) when is_tuple(Term) ->
    {[{tuple, tuple_to_list(Term)}]};
term_to_json(Term) ->
    Term.

is_struct(Struct) ->
    case catch lists:foreach(fun check_is_struct/1, Struct) of
        ok -> true;
        _ -> false
    end.

check_is_struct({_,_}) -> ok;
check_is_struct(Key) when is_binary(Key); is_atom(Key) -> ok.

struct_to_json({Key, Value}) -> {Key, term_to_json(Value)};
struct_to_json(Key) -> {Key, true}.

term_from_json({Properties}) when is_list(Properties) ->
    lists:map(fun struct_from_json/1, Properties);
term_from_json(Entries) when is_list(Entries) ->
    lists:map(fun term_from_json/1, Entries);
term_from_json(Term) ->
    Term.

struct_from_json({Key, Value}) -> {Key, term_from_json(Value)}.
