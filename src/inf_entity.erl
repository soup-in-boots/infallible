-module(inf_entity).
-export([
        %% Opaque API
        get_health/1,
        get_mana/1,
        get_stamina/1,
        get_id/1,
        get_room/1,
        look/1,
        look_at/1,
        move/2,

        set_handler/2,
        notify/2,

        %% Database Methods
        initialize_table/0,
        fetch_all/0,
        fetch/1,
        write/1
    ]).
-include("infallible.hrl").

get_id(_ = #entity{id = ID}) ->
    ID;
get_id(Ref) ->
    {ok, Entity} = inf_reference:read(Ref),
    get_id(Entity).

get_room(_ = #entity{room = Room}) ->
    Room;
get_room(Ref) ->
    {ok, Entity} = inf_reference:read(Ref),
    get_room(Entity).

get_health(_ = #entity{active = _Active = #stats{health = Current}, base = _Base = #stats{health = Max}}) ->
    {Current, Max};
get_health(Ref) ->
    {ok, Entity} = inf_reference:read(Ref),
    get_health(Entity).

get_mana(_ = #entity{active = _Active = #stats{mana = Current}, base = _Base = #stats{mana = Max}}) ->
    {Current, Max};
get_mana(Ref) ->
    {ok, Entity} = inf_reference:read(Ref),
    get_mana(Entity).

get_stamina(_ = #entity{active = _Active = #stats{stamina = Current}, base = _Base = #stats{stamina = Max}}) ->
    {Current, Max};
get_stamina(Ref) ->
    {ok, Entity} = inf_reference:read(Ref),
    get_stamina(Entity).

look(_ = #entity{id = ID, room = Room}) ->
    {ok, {Desc, Occupants}} = inf_room:look(Room),
    error_logger:error_msg("Occupants: ~p", [Occupants]),
    OccDesc = do_look(ID, Occupants),
    error_logger:error_msg("OccDesc: ~p", [OccDesc]),
    {ok, {Desc, OccDesc}};
look(Ref) ->
    {ok, Entity} = inf_reference:read(Ref),
    look(Entity).

do_look(ID, Occupants) ->
    do_look(ID, Occupants, []).

do_look(_ID, [], Looked) ->
    lists:reverse(Looked);
do_look(ID, [Occupant|Rest], Looked) ->
    OccupantID = get_id(Occupant),
    error_logger:error_msg("Shall I skip? ~p | ~p == ~p", [ID, OccupantID, ID == OccupantID]),
    case OccupantID of
        ID -> 
            error_logger:error_msg("Skipped."),
            do_look(ID, Rest, Looked);
        _Other ->
            error_logger:error_msg("Included."),
            do_look(ID, Rest, [look_at(Occupant)|Looked])
    end.

look_at(_ = #entity{label = Label, description = Description, equipment = Equipment}) ->
    {Label, Description, Equipment};
look_at(Ref) ->
    {ok, Entity} = inf_reference:read(Ref),
    look_at(Entity).

notify(_ = #entity{handler = undefined}, _Message) ->
    ok;
notify(_ = #entity{id = ID, handler = Handler}, Message) ->
    Handler ! {entity_notice, ID, Message};
notify(Ref, Message) ->
    {ok, Entity} = inf_reference:read(Ref),
    notify(Entity, Message).

set_handler(Ref, Pid) ->
    inf_reference:transform(Ref, fun(Entity) -> Entity#entity{handler = Pid} end).

move(Ref, Direction) ->
    inf_reference:transform(Ref, fun(Entity) -> do_move(Entity, Direction) end).

do_move(Entity = #entity{room = Room}, Direction) ->
    Exits = inf_room:get_exits(Room),
    case lists:keyfind(Direction, 1, Exits) of
        false -> 
            throw(no_exit);
        {Direction, Destination} ->
            inf_room:depart(Room, Entity),
            inf_room:join(Destination, Entity),
            Entity#entity{room = Destination}
    end.

initialize_table() ->
    mnesia:create_table(entity, [
            {attributes, record_info(fields, entity)},
            {disc_copies, [node()]}
        ]).

fetch_all() ->
    IDs = mnesia:dirty_all_keys(entity),
    [ fetch(ID) || ID <- IDs ].

fetch(ID) ->
    case inf_entity_manager:lookup(ID) of
        undefined ->
            Entity = do_fetch(ID),
            initialize(Entity);
        Ref ->
            Ref
    end.

do_fetch(ID) ->
    ?dirty_read(ID, entity).

write(Entity = #entity{room = Room, race = Race}) ->
    error_logger:error_msg("Current room: ~p", [Room]),
    RoomID  = inf_room:get_id(Room),
    RaceID  = inf_race:get_id(Race),
    mnesia:dirty_write(Entity#entity{
            room        = RoomID, 
            handler     = undefined,
            listeners   = [],
            race        = RaceID
        });
write(Ref) ->
    {ok, Entity} = inf_reference:read(Ref),
    write(Entity).

initialize(Entity = #entity{id = ID, room = RoomID, race = RaceID}) ->
    Race = inf_race:fetch(RaceID),
    Room = inf_room:fetch(RoomID),
    Entity1 = calculate_stats(Entity#entity{room = Room, race = Race}),
    {ok, Ref} = inf_reference:new(Entity1),
    inf_entity_manager:register_entity(ID, Ref).

calculate_stats(Entity = #entity{base = Base, race = _Race = #race{stats = RaceStats}}) ->
    Base1 = Base#stats{
        hps         = RaceStats#stats.hps,
        mps         = RaceStats#stats.mps,
        sps         = RaceStats#stats.sps,

        str         = Base#stats.str + RaceStats#stats.str,
        dex         = Base#stats.dex + RaceStats#stats.dex,
        con         = Base#stats.con + RaceStats#stats.con,
        int         = Base#stats.int + RaceStats#stats.int,
        spr         = Base#stats.spr + RaceStats#stats.spr
    },
    Base2 = Base1#stats{
        health      = calculate_health(Base1),
        mana        = calculate_mana(Base1),
        stamina     = calculate_stamina(Base1),

        hps         = calculate_hps(Base1),
        mps         = calculate_mps(Base1),
        sps         = calculate_sps(Base1)
    },
    calculate_stats_2(Entity#entity{base = Base2}).

calculate_stats_2(Entity = #entity{base = Base, active = undefined}) ->
    Entity#entity{active = Base};
calculate_stats_2(Entity = #entity{base = Base, active = Active}) ->
    Entity#entity{active = Base#stats{
            health      = lists:min([Active#stats.health, Base#stats.health]),
            mana        = lists:min([Active#stats.mana, Base#stats.mana]),
            stamina     = lists:min([Active#stats.stamina, Base#stats.stamina])
        }}.

calculate_health(_ = #stats{str = Str, con = Con}) ->
    (2 * Str) + (6 * Con).

calculate_mana(_ = #stats{int = Int, spr = Spr}) ->
    (1 * Int) + (3 * Spr).

calculate_stamina(_ = #stats{dex = Dex, con = Con}) ->
    (1 * Dex) + (3 * Con).

calculate_hps(_ = #stats{hps = HPS, con = Con}) ->
    HPS + (Con * 2).

calculate_mps(_ = #stats{mps = MPS, spr = Spr}) ->
    MPS + (Spr * 1).

calculate_sps(_ = #stats{sps = SPS, con = Con}) ->
    SPS + (Con * 1).
