-ifndef(INFALLIBLE_HRL).
-record(stats, {
        health   = 0    :: float(),     % Damage which can be taken
        mana     = 0    :: float(),     % Energy for spells
        stamina  = 0    :: float(),     % Energy for techniques

        hps      = 0    :: float(),     % Health Per Second
        mps      = 0    :: float(),     % Mana Per Second
        sps      = 0    :: float(),     % Stamina Per Second

        str      = 0    :: integer(),   % Strength
        dex      = 0    :: integer(),   % Dexterity
        con      = 0    :: integer(),   % Constitution
        int      = 0    :: integer(),   % Intelligence
        spr      = 0    :: integer()    % Spirit
    }).
-type item_type() :: consumable | wieldable | equipable | trophy.
-type item_slot() :: head | neck | shoulders | torso | arms | hands | finger | waist | legs | feet | left_hand | right_hand | quiver.
-record(command, {
        command         :: string(),
        permission      :: integer(),
        execution       :: string() | binary()
    }).
-record(item, {
        id              :: integer(),
        type            :: item_type(),
        slot            :: undefined | item_slot(),
        stats           :: #stats{},
        quantity        :: integer(),
        stack           :: boolean()
    }).
-record(equipment, {
        head            :: #item{},
        neck            :: #item{},
        shoulders       :: #item{},
        torso           :: #item{},
        arms            :: #item{},
        hands           :: #item{},
        finger          :: #item{},
        waist           :: #item{},
        legs            :: #item{},
        feet            :: #item{},
        left_hand       :: #item{},
        right_hand      :: #item{},
        quiver          :: #item{}
    }).
-record(modifier, {
        name            :: string(),
        stack           :: string(),
        level           :: integer(),
        duration        :: integer() | infinity,
        stats           :: #stats{}
    }).
-record(race, {
        name            :: string(),
        stats           :: #stats{},
        buffs           :: [#modifier{}]
    }).
-record(user, {
        username        :: list(),
        password        :: list(),
        entity          :: string() | term()
    }).
-record(entity, {
        id              :: string(),
        label           :: string(),
        description     :: string(),
        level           :: integer(),
        experience      :: integer(),

        handler         :: pid(),
        listeners       :: [pid()],
        room            :: string() | term(),

        base            :: #stats{},
        race            :: #race{},
        equipment       :: #equipment{},
        active_max      :: #stats{},
        active          :: #stats{},

        modifiers       :: [{atom()|list(), #modifier{}}],
        inventory       :: [{string(),integer()}]
    }).
-type direction() :: north | south | east | west | up | down.
-record(room, {
        id              :: string(),
        label           :: string(),
        description     :: string(),
        exits           :: [{direction(), string()}],
        occupants       :: list()
    }).

-define(dirty_read(Key, Tab),
    case mnesia:dirty_read(Tab, Key) of
        []      -> undefined;
        [Val]   -> Val
    end).
-record(object, {
        id,
        type,
        json
    }).
-record(setting, {
        id              :: string(),
        type_desc       :: term(),
        value           :: term()
    }).

-define(dirty_read_all(Tab),
    lists:map(
            fun(Key) -> ?dirty_read(Key,Tab) end, 
            mnesia:dirty_all_keys(Tab)
        )).

-define(TICK, 5000).
-define(SUPER_TICK, 300000).

-endif.
