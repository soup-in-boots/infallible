-module(utils).
-export([md5/1, bin_to_hex/1]).
-export([get_value/2, get_value/3, ldef/1]).
-export([get_available_races/0, get_available_classes/0]).
-export([re_match/2,re_match/3]).
-include("infallible.hrl").

md5(Data) ->
    Bin     = erlang:md5(Data),
    lists:flatten(lists:reverse(bin_to_hex(Bin))).

bin_to_hex(Bin) -> bin_to_hex(Bin, "").

bin_to_hex(<<B:8/integer,Rest/binary>>, S) ->
    bin_to_hex(Rest, [integer_to_list(B, 16)|S]);
bin_to_hex(<<>>, S) ->
    S.

get_value(K, L)     -> get_value(K, L, undefined).
get_value(K, L, D)  ->
    case lists:keyfind(K, 1, L) of
        false       -> D;
        {K, V}      -> V
    end.

ldef([])            -> undefined;
ldef([undefined|T]) -> ldef(T);
ldef([A|_])         -> A.

get_available_races()   -> mnesia:dirty_all_keys(race).
get_available_classes() -> mnesia:dirty_all_keys(class).

re_match(Subject, RE) -> re_match(Subject, RE, []).
re_match(Subject, RE, Options) ->
    case re:run(Subject, RE, Options) of
        match       -> true;
        {match, _}  -> true;
        _           -> false
    end.

