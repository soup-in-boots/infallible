-module(client_handler).
-export([init/2, handle_data/3, handle_info/3, terminate/2]).
-export([cursor/2]).
-include("infallible.hrl").

cursor(Entity, _Client) ->
    {CurH, _MaxH}    = inf_entity:get_health(Entity),
    {CurM, _MaxM}    = inf_entity:get_mana(Entity),
    {CurS, _MaxS}    = inf_entity:get_stamina(Entity),
    io_lib:format("~p/~p/~p> ", [CurH, CurM, CurS]).

init(Opts, Client) ->
    EntityID = utils:get_value(entity, Opts),
    Entity = inf_entity:fetch(EntityID),
    inf_entity:set_handler(Entity, self()),
    inf_protocol:send_message(Client, io_lib:format("A strange sensation washes over you, as your body forms bit by bit from nothingness. Standing, you blink away a wave of dizziness as your senses start to fire.~n", [])),
    self() ! join_world,
    {ok, Entity, Client}.

handle_data("look", Entity, Client) ->
    do_look(Entity, Client),
    {ok, Entity, Client};
handle_data(Data, State, Client) ->
    error_logger:info_msg("[~p][~p:handle_data] Handling Data: ~p~n", [self(), ?MODULE, Data]),
    {ok, State, Client}.

handle_info({entity_notice, _ID, {entered, Occupant}}, Entity, Client) ->
    {Name, _Desc, _Equipment} = inf_entity:look_at(Occupant),
    inf_protocol:send_message(Client, [Name, " has entered the room.\r\n"]),
    {ok, Entity, Client};
handle_info(join_world, Entity, Client) ->
    Room = inf_entity:get_room(Entity),
    Client2 = inf_protocol:set_cursor(Client, fun cursor/2),
    inf_room:join(Room, Entity),
    do_look(Entity, Client),
    {ok, Entity, Client2};
handle_info(Data, State, Client) ->
    error_logger:info_msg("[~p][~p:handle_info] Handling Info: ~p~n", [self(), ?MODULE, Data]),
    {ok, State, Client}.

terminate(Reason, _State) ->
    error_logger:info_msg("[~p][~p:terminate] Terminating Because: ~p~n", [self(), ?MODULE, Reason]),
    ok.

do_look(Entity, Client) ->
    {ok, {Desc, Occupants}} = inf_entity:look(Entity),
    OccCount = length(Occupants),
    OccJoined = [ ["    ", OccLabel, ", ", OccDesc, "\r\n"] || {OccLabel, OccDesc, _Equipment} <- Occupants ],
    inf_protocol:send_message(Client, [Desc, "\r\n"]),
    error_logger:error_report([
            {occupant_count, OccCount},
            {occupants, Occupants}
        ]),
    if
        OccCount > 0 ->
            inf_protocol:send_message(Client, ["You also see: \r\n", OccJoined]);
        true ->
            ok
    end.
