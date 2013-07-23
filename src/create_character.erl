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
    inf_protocol:send_message(Client, ["\nWhat is your race? [", string:join(Races, ", "), "] "], [{echo, on}]),
    error_logger:error_msg("Requested selection."),
    {ok, {select_race, User}, Client}.

handle_data(Data, {thinking, User}, Client) ->
    inf_protocol:send_message(Client, "Huh? Sorry, give me a moment. I have to mull this over. "),
    {ok, {thinking, User}, Client};
handle_data(Data, {select_race, User = #user{username = Name}}, Client) ->
    Races = inf_race:fetch_all(),
    case lists:keyfind(Data, #race.name, Races) of
        Race when is_record(Race, race) -> 
            %% 1. Finish filling out user information and save
            Room = inf_room:fetch("default"),
            EntityID = uuid:to_string(uuid:v4()),
            Entity = #entity{
                id              = EntityID,
                label           = Name,
                description     = "A nondescript being.",
                room            = Room,
                listeners       = [],

                level           = 1,
                experience      = 0,

                base            = #stats{
                    health          = 0,
                    mana            = 0,
                    stamina         = 0,

                    hps             = 0,
                    mps             = 0,
                    sps             = 0,

                    str             = 0,
                    dex             = 0,
                    con             = 0,
                    int             = 0,
                    spr             = 0
                },
                race            = Race,
                equipment       = #equipment{},
                active          = undefined,

                modifiers       = [],
                inventory       = []
            },
            NewUser = User#user{
                entity = Entity
            },

            error_logger:error_msg("Saving user and entity."),
            inf_user:write(NewUser),
            inf_entity:write(Entity),

            %% 2. Prepare and submit closing statements
            Messages = [
                io_lib:format("Very well, ~s. You shall be born an ~s.~n", [Name, Data]),
                io_lib:format("Look, I don't really have time to chat. So, uh... good luck.~n", [])
            ],
            inf_protocol:send_message(Client, Messages),

            %% 3. Upgrade to standard client interface
            {upgrade, client_handler, [{entity, EntityID}], Client};
        false ->
            inf_protocol:send_message(Client, "Don't be a smartass. Try again.\n"),
            {ok, {select_race, User}, Client}
    end;
handle_data(Data, State, Client) ->
    error_logger:info_msg("[~p][~p:handle_info] Unhandled Info: ~p~n", [self(), ?MODULE, Data]),
    {ok, State, Client}.

handle_info(_Data, State, Client) ->
    error_logger:info_msg("[~p][~p:handle_info] Unhandled Info: ~p~n", [self(), ?MODULE, State]),
    {ok, State, Client}.

terminate(_, _) -> ok.
