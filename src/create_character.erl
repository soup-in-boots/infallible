-module(create_character).
-export([init/2, handle_data/3, handle_info/3, terminate/2]).
-include("infallible.hrl").

init(Opts, Client) ->
    error_logger:error_msg("Initializing create_character handler"),
    User    = utils:get_value(user, Opts),
    error_logger:error_msg("Got user."),
    Races   = utils:get_available_races(),
    error_logger:error_msg("Got races:"),
    error_logger:error_report([{races, Races}]),
    Client2 = inf_protocol:send_message(Client, ["\nWhat is your race? [", [ [Race, ","] || Race <- Races ], "] "], [{echo, on}]),
    error_logger:error_msg("Requested selection."),
    {ok, {select_race, User}, Client2}.

handle_data(Data, {thinking, User}, Client) ->
    Client2 = inf_protocol:send_message(Client, "Huh? Sorry, give me a moment. I have to mull this over. "),
    {ok, {thinking, User}, Client2};
handle_data(Data, {select_race, User = #user{username = Name}}, Client) ->
    case inf_race:fetch(Data) of
        undefined ->
            Client2 = inf_protocol:send_message(Client, "Don't be a smartass. Try again.\n"),
            {ok, {select_race, User}, Client2};
        _Race -> 
            %% 1. Finish filling out user information and save
            {ok, Entity} = inf_entity:new(),
            Entity:set_value("race", Data),
            Entity:set_value("label", Name),
            NewUser = User#user{
                entity = Entity
            },

            error_logger:error_msg("Saving user and entity."),
            inf_user:write(NewUser),
            Entity2     = inf_entity:update(undefined, inf_entity:to_json(Entity)),
            EntityID    = Entity2:get_value("id"),

            %% 2. Prepare and submit closing statements
            Messages = [
                io_lib:format("Very well, ~s. You shall be born an ~s.~n", [Name, Data]),
                io_lib:format("Look, I don't really have time to chat. So, uh... good luck.~n", [])
            ],
            inf_protocol:send_message(Client, Messages),

            %% 3. Upgrade to standard client interface
            {upgrade, client_handler, [{entity, EntityID}], Client}
    end;
handle_data(Data, State, Client) ->
    error_logger:info_msg("[~p][~p:handle_info] Unhandled Info: ~p~n", [self(), ?MODULE, Data]),
    {ok, State, Client}.

handle_info(_Data, State, Client) ->
    error_logger:info_msg("[~p][~p:handle_info] Unhandled Info: ~p~n", [self(), ?MODULE, State]),
    {ok, State, Client}.

terminate(_, _) -> ok.
