-module(login_handler).
-export([init/1, handle_data/2, handle_info/2, terminate/2]).
-record(state, {
        username    = undefined,
        password    = undefined
    }).

init(_) ->
    {send_message, "Welcome to infallible! What is your name? ",  #state{}}.

handle_data(Data, State = #state{username = undefined}) ->
    {send_message, "What is your password?", [{echo, off}], State#state{username = Data}};
handle_data(Data, State = #state{username = Username, password = undefined}) ->
    error_logger:info_msg("[~p][~p:handle_data] Todo: Validate login credentials/create new character.~n", [self(), ?MODULE]),
    {upgrade, client_handler, [{username, Username},{password, Data}]}.

handle_info(Data, State) ->
    error_logger:info_msg("[~p][~p:handle_info] Handling info: ~p~n", [self(), ?MODULE, Data]),
    {ok, State}.

terminate(Reason, State) ->
    error_logger:info_msg("[~p][~p:terminate] Terminating for reason: ~p~n", [self(), ?MODULE, Reason]),
    ok.
