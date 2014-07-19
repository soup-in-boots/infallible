-module(inf_user).
-export([new/0]).
-export([grab/2, assign/3]).
-export([look_room/1]).
-export([initialize_table/0]).
-export([check_login/2]).
-export([fetch_all/0,fetch/1,get_labels/0,write/1]).
-export([to_proplist/1,to_json/1]).
-include("infallible.hrl").

look_room(Pid) ->
    gen:call(Pid, look_room).

initialize_table() ->
    mnesia:create_table(user, [
            {attributes, record_info(fields, user)},
            {disc_copies, [node()]}
        ]).

new() ->
    {ok, #user{}}.

grab(User, Field) when is_record(User, user) ->
    Fields = record_info(fields, user),
    utils:grab_field(Field, Fields, User).

assign(User, Field, Value) ->
    Fields = record_info(fields, user),
    utils:assign_field(Field, Value, Fields, User).

fetch_all() ->
    [ fetch(Key) || Key <- mnesia:dirty_all_keys(user) ].

fetch(UserName) -> ?dirty_read(unicode:characters_to_binary(UserName), user).

get_labels() ->
    Keys = [ unicode:characters_to_binary(Key) || Key <- mnesia:dirty_all_keys(user) ],
    lists:zip(Keys, Keys).

to_proplist(User) when is_record(User, user) ->
    lists:zip(record_info(fields, user), tl(tuple_to_list(User))).

to_json(User) when is_record(User, user) ->
    Keys    = record_info(fields, user),
    Values  = tl(tuple_to_list(User)),
    jiffy:encode({lists:zipwith(fun to_json/2, Keys, Values)}).

to_json(username, Username) ->
    {username, unicode:characters_to_binary(Username)};
to_json(password, Password) ->
    {password, unicode:characters_to_binary(Password)};
to_json(entity, Entity) when is_list(Entity); is_binary(Entity) ->
    {entity, unicode:characters_to_binary(Entity)};
to_json(entity, Entity) ->
    ID = Entity:get_value("id"),
    to_json(entity, ID).

write(User = #user{entity = Entity}) ->
    %% Store entity ID
    ID = Entity:get_value("id"),
    mnesia:dirty_write(User#user{entity = ID}).

check_login(undefined, _) ->
    false;
check_login(_User = #user{password = Correct}, Password) ->
    MD5 = utils:md5(Password),
    if
        MD5 == Correct ->
            true;
        true ->
            false
    end;
check_login(UserName, Password) ->
    check_login(fetch(UserName), Password).
