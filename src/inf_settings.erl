-module(inf_settings).
-export([
        get_labels/0,
        from_json/1,
        to_json/1,
        to_proplist/1,

        new/0,

        initialize_table/0,
        fetch_all/0,
        fetch/1,
        write/1,
        update/2,
        delete/1
    ]).
-include("infallible.hrl").

initialize_table() ->
    mnesia:create_table(setting, [
            {attributes, record_info(fields, setting)},
            {disc_copies, [node()]}
        ]),
    set_defaults().

set_defaults() ->
    ok.

get_labels() ->
    Keys = mnesia:dirty_all_keys(setting),
    lists:zip(Keys, Keys).

fetch_all() ->
    F = case mnesia:is_transaction() of
        true -> fun mnesia:all_keys/1;
        false -> fun mnesia:dirty_all_keys/1
    end,
    [ fetch(ID) || ID <- mnesia:dirty_all_keys(setting) ].

fetch(ID) ->
    F = case mnesia:is_transaction() of
        true -> fun mnesia:read/2;
        false -> fun mnesia:dirty_read/2
    end,
    case F(setting, ID) of
        [] -> undefined;
        [Setting] -> Setting
    end.

delete(_ = #setting{id = ID}) ->
    delete(ID);
delete(ID) ->
    F = case mnesia:is_transaction() of
        true -> fun mnesia:delete/2;
        false -> fun mnesia:dirty_delete/2
    end,
    F(setting, ID).

write(Setting = #setting{}) ->
    F = case mnesia:is_transaction() of
        true -> fun mnesia:write/1;
        false -> fun mnesia:dirty_write/1
    end,
    F(Setting).

update(undefined, Value) ->
    write(from_json(Value));
update(ID, Value) ->
    Original    = fetch(ID),
    Updated     = from_json(Value),
    Fields      = record_info(fields, setting),
    ListOrig    = tl(tuple_to_list(Original)),
    ListUpdate  = tl(tuple_to_list(Updated)),
    ListNew     = lists:zipwith3(fun do_update/3, Fields, ListOrig, ListUpdate),
    list_to_tuple([setting|ListNew]).

do_update(_Key, Old, undefined) ->
    Old;
do_update(id, Old, Old) ->
    Old;
do_update(id, Old, New) ->
    delete(Old),
    New;
do_update(_Key, _Old, New) ->
    New.

new() -> {ok, #setting{}}.

from_json(JSON) ->
    io:format("[~p][~s:~p] From JSON: ~s~n", [?MODULE, ?FILE, ?LINE, JSON]),
    {Properties} = jiffy:decode(JSON),
    lists:foldl(fun from_json/2, #setting{}, Properties).

from_json({<<"id">>, ID}, Setting) ->
    Setting#setting{id = ID};
from_json({<<"type">>, TypeDesc}, Setting) ->
    Setting#setting{type_desc = TypeDesc};
from_json({<<"value">>, Value}, Setting) ->
    Setting#setting{value = Value}.

to_json(Setting) ->
    Fields      = record_info(fields, setting),
    Values      = tl(tuple_to_list(Setting)),
    Properties  = lists:zipwith(fun to_json/2, Fields, Values),
    jiffy:encode({Properties}).

to_json(Key, Value) ->
    {Key, Value}.

to_proplist(Setting) ->
    io:format("[~p][~s:~p] Setting: ~p", [?MODULE, ?FILE, ?LINE, Setting]),
    Fields = record_info(fields, setting),
    Values = tl(tuple_to_list(Setting)),
    lists:zipwith(fun to_proplist/2, Fields, Values).

to_proplist(Key, Value) ->
    {Key, Value}.
