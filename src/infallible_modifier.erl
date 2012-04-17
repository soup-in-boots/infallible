-module(infallible_modifier).
-export([initialize_table/0]).
-export([get_modifier/1, get_modifiers/1, get_modifiers/0, save_modifier/1]).
-include("infallible.hrl").
-define(RACES, [
        #modifier{
            id          = human,
            life        = permanent,
            type        = race,
            manifest    = neutral,
            stats       = #stats{
                strength        = 20,
                dexterity       = 20,
                constitution    = 20,
                intelligence    = 20,
                spirit          = 20
            }
        }
    ]).

initialize_table() ->
    mnesia:create_table(modifier, [
            {attributes,        record_info(fields, modifier)},
            {index,             [#modifier.type, #modifier.manifest]},
            {disc_copies,       [node()]}
        ]).

get_modifier(ID)            -> ?dirty_read(ID, modifier).
get_modifiers(L = [_|_])    -> [?dirty_read(ID, modifier) || ID <- L].
get_modifiers()             -> ?dirty_read_all(modifier).

save_modifier(M) when is_record(M, modifier) ->
    mnesia:dirty_write(M);
save_modifier(_) ->
    throw(badarg).
