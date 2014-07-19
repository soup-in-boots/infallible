-module(inf_entity).
-export([
        %% Opaque API
        get_labels/0,
        new/0,
        fetch_all/0,
        fetch/1,
        write/1,
        update/2,

        to_json/1,

        set_globals/1,
        to_proplist/1,
        is_active/1
    ]).
-include("infallible.hrl").
-define(ENTITY_TIMEOUT, 5000).

new() ->
    VM = inf_world:vm(),
    erlv8_vm:run(VM, "new Entity();").

get_labels() ->
    [ {Obj:get_value("id"), Obj:get_value("label")} || Obj <- fetch_all() ].

fetch_all() ->
    Objects = inf_world:fetch_all(<<"Entity">>),
    lists:map(fun(_ = #object{id = ID}) -> fetch(ID) end, Objects).

fetch(ID) ->
    case inf_entity_manager:lookup(ID) of
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

write(Object = #object{}) ->
    inf_world:write(Object);
write(Object) ->
    JSON = Object:call(Object:get_value("json"), []),
    ID = Object:get_value("id"),
    write(#object{id = ID, type = <<"Entity">>, json = JSON}).

to_json(Obj) ->
    Obj:call(Obj:get_value("json"), []).

update(undefined, Object) ->
    Obj = initialize(#object{id = undefined, type = <<"Entity">>, json = Object}),
    JSONFun = Obj:get_value("json"),
    JSON = Obj:call(JSONFun, []),
    ID = Obj:get_value("id"),
    write(#object{id = ID, type = <<"Entity">>, json = JSON}),
    Obj;
update(ID, Object) ->
    NewObject = inf_entity_manager:lookup(ID),
    NewObject:call(NewObject:get_value("update"), [Object]),
    JSON = NewObject:call(NewObject:get_value("json", [])),
    write(#object{id = ID, type = <<"Entity">>, json = JSON}).

set_globals(VM) ->
    io:format("Setting entity globals~n").

to_proplist(Obj) ->
    Properties = [ to_proplist(K, V) || {K, V} <- Obj:proplist() ],
    lists:filter(fun
            (ignore) -> 
                false; 
            (_) -> 
                true 
        end, Properties).


to_proplist(K = <<"base">>, V) ->
    {K, V:proplist()};
to_proplist(K = <<"active">>, V) ->
    {K, V:proplist()};
to_proplist(K = <<"active_max">>, V) ->
    {K, V:proplist()};
to_proplist(K, V) ->
    {K, V}.

initialize(_ = #object{json = JSON}) ->
    VM      = inf_world:vm(),
    G       = erlv8_vm:global(VM),
    Entity  = G:get_value("Entity"),
    Load    = Entity:get_value("Load"),
    case Load:call([JSON]) of
        {throw, {error, Error}} ->
            io:format("Failed to load: ~p", [Error:proplist()]), 
            throw(Error);
        Obj ->
            ID      = Obj:get_value("id"),
            inf_entity_manager:register_entity(ID, Obj)
    end.

is_active(Entity) ->
    case Entity:get_value("handler") of
        Pid when is_pid(Pid) -> is_process_alive(Pid);
        _ -> false
    end.
