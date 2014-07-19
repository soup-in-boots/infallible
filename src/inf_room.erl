-module(inf_room).
-export([
        %% API
        get_labels/0,
        new/0,
        fetch_all/0,
        fetch/1,
        write/1,

        to_proplist/1,
        to_json/1,
        update/2,

        set_globals/1
    ]).
-include("infallible.hrl").

get_labels() ->
    [ {Obj:get_value("id"), Obj:get_value("label")} || Obj <- fetch_all() ].

new() ->
    VM = inf_world:vm(),
    erlv8_vm:run(VM, "new Room();").

fetch_all() ->
    Objects = inf_world:fetch_all(<<"Room">>),
    lists:map(fun(_ = #object{id = ID}) -> fetch(ID) end, Objects).

fetch(ID) ->
    case inf_room_manager:lookup(ID) of
        undefined ->
            case do_fetch(ID) of
                undefined ->
                    undefined;
                Object ->
                    initialize(Object)
            end;
        Obj ->
            Obj
    end.

do_fetch(ID) ->
    inf_world:fetch(ID).

update(undefined, Object) ->
    Obj = initialize(#object{id = undefined, type = <<"Room">>, json = Object}),
    JSONFun = Obj:get_value("json"),
    JSON = Obj:call(JSONFun, []),
    ID = Obj:get_value("id"),
    write(#object{id = ID, type = <<"Room">>, json = JSON}),
    Obj;
update(ID, Object) ->
    NewObject = inf_room_manager:lookup(ID),
    NewObject:call(NewObject:get_value("update"), [Object]),
    JSON = NewObject:call(NewObject:get_value("json", [])),
    write(#object{id = ID, type = <<"Room">>, json = JSON}),
    NewObject.


write(Object) when is_record(Object, object) ->
    inf_world:write(Object);
write(Object) ->
    JSON = Object:call(Object:get_value("json"), []),
    ID = Object:get_value("id"),
    write(#object{id = ID, type = <<"Room">>, json = JSON}).

set_globals(VM) ->
    ok.

initialize(_ = #object{json = JSON}) ->
    VM      = inf_world:vm(),
    G       = erlv8_vm:global(VM),
    Room    = G:get_value("Room"),
    Load    = Room:get_value("Load"),
    case Load:call([JSON]) of
        {throw, {error, Error}} -> 
            io:format("Failed to load: ~p", [Error:proplist()]),
            throw(Error);
        Obj ->
            ID      = Obj:get_value("id"),
            inf_room_manager:register_room(ID, Obj)
    end.

to_proplist(Obj) ->
    [ to_proplist(K, V) || {K, V} <- Obj:proplist() ].

to_proplist(K = <<"exits">>, V) ->
    {K, V:proplist()};
to_proplist(K = <<"occupants">>, V) ->
    {K, V:list()};
to_proplist(K, V) ->
    {K, V}.

to_json(Obj) ->
    Obj:call(Obj:get_value("json"), []).
