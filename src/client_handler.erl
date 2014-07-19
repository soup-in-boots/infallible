-module(client_handler).
-export([init/2, handle_data/3, handle_info/3, terminate/2]).
-export([cursor/2]).
-define(COMMANDS, [
        {"look", fun do_look/3},
        {"north", fun do_move/4, [north]},
        {"south", fun do_move/4, [south]},
        {"east", fun do_move/4, [east]},
        {"west", fun do_move/4, [west]},
        {"up", fun do_move/4, [up]},
        {"down", fun do_move/4, [down]},
        {"stats", fun do_stats/3},
        {"say", fun do_say/3}
    ]).
-include("infallible.hrl").

cursor(Entity, _Client) ->
    [ inf_protocol:fg_color(red), Entity:get_value("id"), inf_protocol:reset_code(), "> "].

init(Opts, Client) ->
    EntityID = utils:get_value(entity, Opts),
    case inf_entity:fetch(EntityID) of
        undefined ->
            Client2 = inf_protocol:send_message(Client, "You can't seem to be...\r\n"),
            {stop, {no_exists, entity, EntityID}, undefined, Client2};
        Entity ->
            case inf_entity:is_active(Entity) of
                true ->
                    Client2 = inf_protocol:send_message(Client, "Something is amiss! You find your body within the world, housing another consciousness!\r\n"),
                    {stop, already_logged_in, undefined, Client2};
                false ->
                    Entity:call(Entity:get_value("setHandler"), [self()]),
                    Client2 = inf_protocol:send_message(Client, io_lib:format("A strange sensation washes over you, as your body forms bit by bit from nothingness. Standing, you blink away a wave of dizziness as your senses start to fire.~n", [])),
                    self() ! join_world,
                    {ok, Entity, Client2}
            end
    end.

handle_data(RawCommand, Entity, Client) ->
    [Command|Arguments] = re:split(RawCommand, "\\s+", [{return, list}, trim]),
    case Command of
        "" ->
            {ok, Entity, inf_protocol:send_message(Client, "")};
        _ ->
            Client2 = do_command(Client, Entity, Command, Arguments),
            {ok, Entity, Client2}
    end.

handle_info({system, Message}, Entity, Client) ->
    Client2 = inf_protocol:send_message(Client, [Message,"\r\n"]),
    {ok, Entity, Client2};
handle_info(join_world, Entity, Client) ->
    %% 1. Set up our cursor
    Client2 = inf_protocol:set_cursor(Client, fun cursor/2),
    Entity:call(Entity:get_value("login"), []),
    {ok, Entity, Client2};
handle_info(Data, State, Client) ->
    {ok, State, Client}.

terminate(connection_lost, Entity) ->
    Entity:call(Entity:get_value("logout"), []);
terminate(Reason, _State) ->
    ok.

do_command(Client, Entity, Command, Arguments) ->
    Found = find_command(Command),
    case Found of
        {_C, F} -> 
            case F:call(Entity, Arguments) of
                {ok, _Res} -> 
                    ok;
                undefined ->
                    ok;
                {Exception, {error, Obj}} ->
                    Error = Obj:proplist(),
                    throw({Exception, Error});
                Other ->
                    throw(Other)
            end,
            Client;
        undefined ->
            inf_protocol:send_message(Client, "Huh?\r\n")
    end.

find_command(Command) ->
    seek_command(Command, ets:tab2list(command_funs)).

seek_command(_Command, []) -> undefined;
seek_command([], [Res|_]) -> Res;
seek_command([C|Command], Commands) -> seek_command(Command, filter_commands(C, Commands)).

filter_commands(_, []) ->
    [];
filter_commands(C, [Command|Commands]) ->
    case element(1, Command) of
        [C|Rest] -> [setelement(1, Command, Rest)|filter_commands(C, Commands)];
        _Else -> filter_commands(C, Commands)
    end.
