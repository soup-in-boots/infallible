-module(create_character).
-export([init/1, handle_data/2, handle_info/2, terminate/2]).
-include("infallible.hrl").

init(Opts) ->
    User    = infallible_utils:get_value(user, Opts),
    Races   = infallible_utils:get_available_races(),
    {send_message, ["What is your race? [", string:join(", ", Races), "] "], [{echo, on}], {select_race, User}}.

handle_data(Data, {select_race, User = #user{modifiers = Modifiers}}) ->
    RaceID  = list_to_existing_atom(Data),
    Race    = infallible_modifier:get_modifier(RaceID),
    Classes = infallible_utils:get_available_classes(),
    {send_message, ["I see. Well, what's your job? [", string:join(Classes, ", "), "] "], {select_class, User#user{
                modifiers = [{race, Race}|Modifiers]
            }}};
handle_data(Data, {select_class, User = #user{modifiers = Modifiers}}) ->
    ClassID = list_to_existing_atom(Data),
    Class   = infallible_modifier:get_modifier(ClassID),
    CompleteUser = infallible_utils:generate_stats(User#user{modifiers = [{class, Class}|Modifiers]}),
    [
        {send_message, ["Good. Let's get you started. \n\n"], complete},
        {upgrade, client_handler, [{user, CompleteUser}]}
    ].

handle_info(Data, State) ->
    error_logger:info_msg("[~p][~p:handle_info] Unhandled Info: ~p~n", [self(), ?MODULE, State]),
    {ok, State}.

terminate(_, _) -> ok.
