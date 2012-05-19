-module(infallible_mod).
-include("infallible.hrl").
-export([make_tables/0,populate_tables/0,merge_stats/1]).
-export([get_race/1, get_class/1, get_profession/1, get_modifier/1, get_room/1]).
-export([get_all_races/0, get_all_classes/0, get_all_professions/0, get_all_modifiers/0, get_all_rooms/0]).
-export([get_filtered_classes/1, get_filtered_professions/2]).

-define(DEFAULT_RACES, [
        #race{
            id              = "human",
            block_class     = [],
            block_prof      = [],
            initial_stats   = #stats{
                strength        = 20,
                dexterity       = 20,
                constitution    = 20,
                intelligence    = 20,
                spirit          = 20
            },
            modifiers       = []
        }
    ]).

-define(DEFAULT_CLASSES, [
        #class{
            id              = "warrior",
            block_prof      = [],
            initial_stats   = #stats{ 
                strength        = 5,
                dexterity       = 0,
                constitution    = 5,
                intelligence    = -10,
                spirit          = 0
            },
            modifiers       = []
        },
        #class{
            id              = "rogue",
            block_prof      = [],
            initial_stats   = #stats{
                strength        = 0,
                dexterity       = 5,
                constitution    = 0,
                intelligence    = 5,
                spirit          = -10
            },
            modifiers       = []
        }
    ]).

-define(DEFAULT_PROFESSIONS, [
    ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initialization

make_tables() ->
    lists:foreach(fun create_table/1, [modifier, race, class, profession, room]).

populate_tables() ->
    lists:map(fun mnesia:dirty_write/1, ?DEFAULT_RACES),
    lists:map(fun mnesia:dirty_write/1, ?DEFAULT_CLASSES),
    lists:map(fun mnesia:dirty_write/1, ?DEFAULT_PROFESSIONS).

create_table(modifier)      -> create_table(modifier, record_info(fields, modifier));
create_table(race)          -> create_table(race, record_info(fields, race));
create_table(class)         -> create_table(class, record_info(fields, class));
create_table(profession)    -> create_table(profession, record_info(fields, profession));
create_table(room)          -> create_table(room, record_info(fields, room)).

create_table(Name, Fields) ->
    mnesia:create_table(Name, [
            {disc_copies, [node()]},
            {attributes, Fields}
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Access

get_race(ID)            -> do_read(race, ID).
get_class(ID)           -> do_read(class, ID).
get_profession(ID)      -> do_read(profession, ID).
get_modifier(ID)        -> do_read(modifier, ID).
get_room(ID)            -> do_read(room, ID).

get_all_races()         -> do_read_all(race).
get_all_classes()       -> do_read_all(class).
get_all_professions()   -> do_read_all(profession).
get_all_modifiers()     -> do_read_all(modifier).
get_all_rooms()         -> do_read_all(room).

do_read(Table, Key) -> mnesia:dirty_read(Table, Key).
do_read_all(Table)  -> lists:map(fun (Key) -> do_read(Table, Key) end, mnesia:dirty_all_keys(Table)).

get_filtered_classes(Race) when is_list(Race) -> get_filtered_classes(get_race(Race));
get_filtered_classes(_ = #race{block_class = Blocked}) ->
    lists:filter(fun (_ = #class{id = ID}) -> lists:member(ID, Blocked) end, get_all_classes()).

get_filtered_professions(Race, Class) when is_list(Race) -> get_filtered_professions(get_race(Race), Class);
get_filtered_professions(Race, Class) when is_list(Class) -> get_filtered_professions(Race, Class);
get_filtered_professions(_ = #race{block_prof = Race}, Class = #class{block_prof = Class}) ->
    Blocked = lists:merge(lists:sort(Race), lists:sort(Class)),
    lists:filter(fun (_ = #profession{id = ID}) -> lists:member(ID, Blocked) end, get_all_professions()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Manipulation

merge_stats([]) -> #stats{};
merge_stats(L = [_|_]) -> lists:foldl(fun merge_stats/2, #stats{}, L).

merge_stats(CStat, AccStat) ->
    L = lists:seq(2, record_info(size, stats)),
    lists:foldl(fun(Field, Stats) ->
                setelement(Field, Stats, element(Field, CStat) + element(Field, Stats))
        end, AccStat, L).
