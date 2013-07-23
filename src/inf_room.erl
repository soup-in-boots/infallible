-module(inf_room).
-export([
        %% API
        get_id/1,

        join/2,     %% Login/Spawn
        depart/2,   %% Logout/Death
        look/1,

        % Object Functions
        initialize_table/0,
        fetch_all/0,
        fetch/1,
        write/1
    ]).
-include("infallible.hrl").
-define(DEFAULT_ROOM, #room{
        id          = "default",
        label       = "An Unremarkable Room",
        description = "You see four walls, a ceiling, and a floor.",
        exits       = [],
        occupants   = []
    }).

get_id(_ = #room{id = ID}) ->
    ID;
get_id(Ref) ->
    {ok, Room} = inf_reference:read(Ref),
    get_id(Room).

look(Ref) ->
    {ok, Room} = inf_reference:read(Ref),
    {ok, do_look(Room)}.

do_look(_Room = #room{description = Desc, occupants = Occupants}) ->
    {Desc, Occupants}.

join(Ref, Entity) ->
    inf_reference:transform(Ref, fun(Room) -> do_join(Room, Entity) end).

do_join(Room = #room{description = Desc, occupants = Occupants}, Entity) ->
    notify(Room, {entered, Entity}),
    Room#room{occupants = [Entity|Occupants]}.

depart(Ref, Entity) ->
    inf_reference:transform(Ref, fun(Room) -> do_depart(Room, Entity) end).

do_depart(Room = #room{occupants = Occupants}, Entity) ->
    NewOccupants = lists:delete(Entity, Occupants),
    inf_entity:notify(exited),
    lists:foreach(fun(Occ) -> inf_entity:notify(Occ, {departed, Entity}) end, NewOccupants),
    Room#room{occupants = NewOccupants}.

initialize_table() ->
    mnesia:create_table(room, [
            {attributes, record_info(fields, room)},
            {disc_copies, [node()]}
        ]),
    write(?DEFAULT_ROOM).

fetch_all() ->
    ?dirty_read_all(room).

fetch(ID) ->
    case inf_room_manager:lookup(ID) of
        undefined ->
            Room = do_fetch(ID),
            initialize(Room);
        Ref ->
            Ref
    end.

do_fetch(ID) ->
    ?dirty_read(ID, room).

write(Room) ->
    mnesia:dirty_write(Room).

initialize(Room = #room{id = ID}) ->
    {ok, Ref} = inf_reference:new(Room),
    inf_room_manager:register_room(ID, Ref).

notify(_ = #room{occupants = Occupants}, Message) ->
    lists:foreach(fun(Entity) -> inf_entity:notify(Entity, Message) end, Occupants).
