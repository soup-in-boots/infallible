-module(inf_race).
-export([
        get_id/1,

        initialize_table/0,
        fetch_all/0,
        fetch/1,
        write/1,
        delete/1
    ]).
-include("infallible.hrl").
-define(DEFAULT_RACES, [
        #race{
            name        = "human",
            stats       = #stats{
                str         = 20,
                dex         = 20,
                con         = 20,
                int         = 20,
                spr         = 20
            },
            buffs       = [
                #modifier {
                    name        = "Sense of Urgency",
                    stack       = race_stamina_recovery,
                    level       = 1,
                    duration    = infinity,
                    stats       = #stats{
                        sps        = 30.0
                    }
                }
            ]
        },
        #race{
            name        = "elf",
            stats       = #stats{
                str         = 15,
                dex         = 25,
                con         = 15,
                int         = 25,
                spr         = 20
            },
            buffs       = [
                #modifier{
                    name        = "Nature's Roots",
                    stack       = "race_mana_recovery",
                    level       = 1,
                    duration    = infinity,
                    stats       = #stats{
                        mps        = 30.0
                    }
                }
            ]
        },
        #race{
            name        = "dwarf",
            stats       = #stats{
                str         = 25,
                dex         = 15,
                con         = 25,
                int         = 15,
                spr         = 20
            },
            buffs       = [
                #modifier{
                    name        = "Born of Stone",
                    stack       = "race_health_recovery",
                    level       = 1,
                    duration    = infinity,
                    stats       = #stats{
                        hps        = 30.0
                    }
                }
            ]
        }
    ]).

get_id(_ = #race{name = Name}) ->
    Name.

initialize_table() ->
    mnesia:create_table(race, [
            {attributes, record_info(fields, race)},
            {disc_copies, [node()]}
        ]),
    lists:foreach(fun write/1, ?DEFAULT_RACES).

fetch_all() ->
    ?dirty_read_all(race).

fetch(Name) ->
    ?dirty_read(unicode:characters_to_list(Name), race).

write(Race = #race{name = Name}) ->
    delete(Name),
    mnesia:dirty_write(Race#race{name = unicode:characters_to_list(Name)}).

delete(Name) ->
    mnesia:dirty_delete(race, unicode:characters_to_list(Name)).
