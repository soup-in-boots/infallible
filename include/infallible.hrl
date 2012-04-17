-ifndef(INFALLIBLE_HRL).
-record(equipment, {
        head            :: undefined | list(),
        neck            :: undefined | list(),
        shoulders       :: undefined | list(),
        torso           :: undefined | list(),
        arms            :: undefined | list(),
        hands           :: undefined | list(),
        finger          :: undefined | list(),
        waist           :: undefined | list(),
        legs            :: undefined | list(),
        feet            :: undefined | list(),
        left_hand       :: undefined | list(),
        right_hand      :: undefined | list(),
        quiver          :: undefined | list()
    }).
-record(stats, {
        health          :: float(),
        mana            :: float(),
        stamina         :: float(),

        hrps            :: float(),
        mrps            :: float(),
        srps            :: float(),

        strength        :: integer(),
        dexterity       :: integer(),
        constitution    :: integer(),
        intelligence    :: integer(),
        spirit          :: integer()
    }).
-type modifier_type()   :: race | class | profession | blessing | curse | damage_over_time.
-record(modifier, {
        id              :: atom() | list(),
        life            :: permanent | temporary,
        type            :: modifier_type(),
        acquired        :: undefined | integer(),
        duration        :: undefined | integer(),
        manifest        :: neutral | poison | fire | ice | electric | wind,
        stats           :: undefined | #stats{}
    }).
-record(class, {
        id              :: atom(),
        block_prof      :: [atom()],
        initial_stats   :: #stats{},
        modifiers       :: [#modifier{}]
    }).
-record(user, {
        username        :: list(),
        password        :: list(),

        level           :: integer(),
        experience      :: integer(),
        level_weight    :: float(),

        stats           :: #stats{},
        equipment       :: #equipment{},
        modifiers       :: [{atom()|list(), #modifier{}}],
        inventory       :: [list()]
    }).
-record(room, {
        id              :: integer(),
        name            :: list(),
        description     :: list(),
        exits           :: [{atom(), integer()}]
    }).

-define(dirty_read(Key, Tab),
    case mnesia:dirty_read(Tab, Key) of
        []      -> undefined;
        [Val]   -> Val
    end).

-define(dirty_read_all(Tab),
    lists:map(
            fun(Key) -> ?dirty_read(Key,Tab) end, 
            mnesia:dirty_all_keys(Tab)
        )).

-endif.
