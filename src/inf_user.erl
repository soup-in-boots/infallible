-module(inf_user).
-export([look_room/1]).
-export([initialize_table/0]).
-export([check_login/2]).
-export([fetch/1,write/1]).
-include("infallible.hrl").

look_room(Pid) ->
    gen:call(Pid, look_room).

initialize_table() ->
    mnesia:create_table(user, [
            {attributes, record_info(fields, user)},
            {disc_copies, [node()]}
        ]).

fetch(UserName) -> ?dirty_read(UserName, user).

write(User = #user{entity = Entity}) ->
    %% Store entity ID
    EntityID = inf_entity:get_id(Entity),
    mnesia:dirty_write(User#user{entity = EntityID}).

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
